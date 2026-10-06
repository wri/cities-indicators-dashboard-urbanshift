#!/usr/bin/env bash
#
# Rebuild and restart one of the dashboard containers on the app server.
# Run from the repository checkout on i-08cfa066a7178527b (reach it with SSM
# Session Manager).

set -euo pipefail

usage() {
    cat <<'USAGE'
Usage: ./deploy.sh <branch> <image> [port] [container]

  branch     git branch to deploy, e.g. staging, combined-app
  image      docker image tag to build, e.g. staging-image, combined-app
  port       host port to publish (default: 4949)
  container  container name (default: the image name, with a trailing
             -image rewritten to -container)

Examples:
  ./deploy.sh staging staging-image 4949
  ./deploy.sh combined-app combined-app 3838
USAGE
}

if [ $# -lt 2 ]; then
    usage >&2
    exit 1
fi
case "$1" in
    -h|--help) usage; exit 0 ;;
esac

BRANCH=$1
IMAGE=$2
PORT=${3:-4949}

if [ $# -ge 4 ]; then
    CONTAINER=$4
elif [ "${IMAGE%-image}" != "$IMAGE" ]; then
    CONTAINER="${IMAGE%-image}-container"
else
    CONTAINER="$IMAGE"
fi

REPO_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$REPO_DIR"

say() { printf '\n==> %s\n' "$*"; }

say "Deploying branch '$BRANCH' as image '$IMAGE' -> container '$CONTAINER' on port $PORT"
say "Repository: $REPO_DIR"

# --- refuse to clobber local edits ------------------------------------------
# The checkout on the server has historically carried uncommitted changes.
# Switching branches would silently discard them, so stop and let a human look.
if [ -n "$(git status --porcelain)" ]; then
    say "ERROR: working tree has uncommitted changes:"
    git status --short
    cat <<'MSG'

Refusing to continue. Deal with these first:

    git stash             # keep them for later
    git checkout -- .     # throw them away

MSG
    exit 1
fi

# --- update the checkout ----------------------------------------------------
say "Fetching"
git fetch --all --prune

say "Checking out $BRANCH"
git checkout "$BRANCH"
git pull --ff-only

say "Now at: $(git log -1 --oneline)"

# --- build ------------------------------------------------------------------
# Rebuilding reuses the tag, so keep the current image under a second name
# first; it is the rollback target if the new one does not come up.
PREVIOUS=""
if docker image inspect "$IMAGE" >/dev/null 2>&1; then
    PREVIOUS="${IMAGE}-previous"
    say "Tagging current $IMAGE as $PREVIOUS for rollback"
    docker tag "$IMAGE" "$PREVIOUS"
fi

say "Building $IMAGE (slow: R packages compile from source)"
docker build . -t "$IMAGE"

# --- swap the container -----------------------------------------------------
if docker ps -a --format '{{.Names}}' | grep -qx "$CONTAINER"; then
    say "Removing existing container $CONTAINER"
    docker rm -f "$CONTAINER"
fi

# Anything else holding this port would make docker run fail.
OCCUPANT=$(docker ps --filter "publish=${PORT}" --format '{{.Names}}' | grep -vx "$CONTAINER" || true)
if [ -n "$OCCUPANT" ]; then
    say "ERROR: port $PORT is already published by: $OCCUPANT"
    say "Stop or rename it first, or pass a different port."
    exit 1
fi

say "Starting $CONTAINER"
docker run -d -p "${PORT}:3838" --name "$CONTAINER" --restart on-failure "$IMAGE"

# --- verify -----------------------------------------------------------------
say "Waiting for the app to answer on :$PORT"
CODE=""
for _ in $(seq 1 45); do
    CODE=$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 "http://localhost:${PORT}/" || true)
    if [ "$CODE" = "200" ]; then
        break
    fi
    sleep 2
done

if [ "$CODE" != "200" ]; then
    say "FAILED: no 200 from http://localhost:${PORT}/ (last response: ${CODE:-none})"
    say "Container logs:"
    docker logs --tail 40 "$CONTAINER" || true
    if [ -n "$PREVIOUS" ]; then
        cat <<MSG

To roll back:

    docker rm -f $CONTAINER
    docker run -d -p ${PORT}:3838 --name $CONTAINER --restart on-failure $PREVIOUS

MSG
    fi
    exit 1
fi

say "OK: serving 200 on :$PORT"

# The data host is rendered into the page, so this shows which bucket or
# distribution the running build actually reads from.
say "Data host in the served page:"
curl -s --max-time 10 "http://localhost:${PORT}/" \
    | grep -oE 'src="https://[^"]*logo[^"]*"' \
    | sed 's/^/    /' \
    || echo "    (no logo URLs found)"

say "Done. '$BRANCH' is live as '$CONTAINER' on port $PORT."
if [ -n "$PREVIOUS" ]; then
    say "Previous image kept as $PREVIOUS - 'docker rmi $PREVIOUS' once you are happy."
fi
