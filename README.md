# cities-indicators-dashboard-urbanshift

An R [Shiny](https://shiny.posit.co/) dashboard presenting city-level sustainability
indicators — land use, tree cover, biodiversity, air quality, flooding, heat, population
and more — for the cities participating in [UrbanShift](https://www.shiftcities.org/)
and [Cities4Forests](https://cities4forests.com/).

The whole application is a single Shiny file, `dashboard-urbanshift/app.R` (~3,800
lines), which builds seven tabs: **Indicators**, **Map**, **Table**, **Chart**,
**Benchmark**, **Definitions** and **About**. It holds no data of its own — every
indicator table, boundary file, raster and image is fetched over HTTPS at startup and
on demand, from the location described in [Data hosting](#data-hosting-s3--cloudfront)
below. There is no database and no backend service.

A `selected_project` flag near the top of `app.R` switches the branding and city list
between the two projects:

```r
selected_project = "urbanshift"
# selected_project = "cities4forests"
```

## Repository layout

| Path | Purpose |
| --- | --- |
| `dashboard-urbanshift/app.R` | The entire application |
| `dashboard-urbanshift/www/` | Static assets served by Shiny (logos, Weglot translation scripts) |
| `Dockerfile` | Builds the container: `rocker/shiny:4.2.1` + R packages pinned to a 2022-09-02 CRAN snapshot |
| `deploy/` | Terraform for the EC2 host (instance, security group, elastic IP, SSM role) |
| `.github/workflows/deploy.yml` | Deploys `staging` and production — see [Deployment](#deployment) |

## Branches

`combined-app` is the default branch and runs in production. `staging` runs on the
staging port, and changes reach production by a pull request from `staging` into
`combined-app`. `main` is not deployed anywhere.

| Branch | Where it runs |
| --- | --- |
| `combined-app` | **Production** — port 3838, container `combined-app` |
| `staging` | **Staging** — port 4949, container `staging` |
| `main` | Not deployed. Still points at the retired `cities-urbanshift` bucket. |

## Running locally

Requires R with the packages listed at the top of `app.R`, plus GDAL, GEOS, PROJ and
udunits for `sf`/`raster`:

```sh
brew install gdal geos proj udunits
Rscript -e 'install.packages(c("shiny","plotly","leaflet","leaflet.extras","plyr",
  "dplyr","shinyWidgets","rnaturalearth","tidyverse","sf","httr","jsonlite","raster",
  "data.table","DT","RColorBrewer","shinydisconnect","shinyjs","shinycssloaders"))'

Rscript -e 'shiny::runApp("dashboard-urbanshift", port = 3838, launch.browser = TRUE)'
```

`app.R` also calls `library(rgdal)` and `library(rgeos)`. Both packages were retired
from CRAN in October 2023 and cannot be installed on current R. Neither is used
anywhere else in the file, so the two `library()` calls have to be removed (or the app
run on R old enough to still have them) before it will start locally. The container is
unaffected because it pins a 2022 CRAN snapshot that still contains both.

Data is read from the live CloudFront distribution, so a local run needs no AWS
credentials but does need network access.

## Deployment

Both containers run on a single EC2 instance (`IndicatorsAppServerInstance`) in
WRI's AWS account, region `us-east-1`.

Public traffic reaches it through two load balancers, neither of which is managed by
the Terraform in `deploy/`:

```
citiesindicators.wri.org
  -> NLB CitiesIndicatorStaticLB  (listeners 80, 443, 4949)
  -> ALB CitiesIndicatorLB
  -> IndicatorsAppServerInstance  :3838 (production) / :4949 (staging)
```

Staging is reachable at `https://citiesindicators.wri.org:4949/`.

### Deploying a change

Deploys run from GitHub Actions via `.github/workflows/deploy.yml`. Pushing to a
branch deploys it:

| | `staging` | `combined-app` (production) |
| --- | --- | --- |
| Trigger | push to `staging` | push to `combined-app`, then approval |
| GitHub environment | `staging` (no approval) | `production` (approval required) |
| Server checkout | `~/cities-indicators-dashboard-urbanshift-staging` | `~/cities-indicators-dashboard-urbanshift-combined-app` |
| Image / container | `staging` / `staging` | `combined-app` / `combined-app` |
| Port | 4949 | 3838 |

Changes go into `staging` first and reach production through a pull request from
`staging` into `combined-app` (the default branch, which only accepts changes via pull
request). The workflow can also be started by hand with **Run workflow** in the Actions
tab. It refuses to deploy any other branch.

Each deploy SSHes to the instance and, for its branch:

1. Pulls the branch into that branch's checkout (one folder per branch).
2. Builds the image (named after the branch, as is the container) before touching the running container, so a failed build leaves
   the app up. The R package build can take a while; the job allows 60 minutes.
3. Keeps the outgoing image as `<image>-previous`, then replaces the container with
   `--restart unless-stopped`, so it comes back after a crash or reboot.
4. Health-checks the app for an HTTP 200. If it doesn't come up, the deploy rolls back
   to `<image>-previous` and fails.
5. Prunes untagged images with `docker image prune -f`. Never use `prune -a` on this
   host: it deletes the `rocker/shiny` base image and cached R package layers, making
   every rebuild slow.

Only one deploy per branch runs at a time. The SSH connection details are repo secrets
(`AWS_IP`, `AWS_USERNAME`, `SSH_PORT`, `SSH_KEY`); the repo is public, so keep server
details out of workflow logs and this file.

To roll back by hand, revert the change on the branch and let the deploy run.

### Terraform

`deploy/` provisions the instance, its security group, elastic IP and SSM IAM role.
State has drifted from reality: the security group opens ports 80 and 3838 but not
4949, and neither load balancer nor the CloudFront setup below is represented. Treat
`deploy/` as the original bootstrap rather than an accurate model of what is running.

A legacy `dashboard-urbanshift/rsconnect/` profile targets shinyapps.io
(`wri-cities/indicators-urbanshift-dashboard`) from an earlier hosting arrangement. It
is not part of the current deployment.

## Data hosting: S3 + CloudFront

The dashboard reads all of its data (indicator CSVs, boundaries, rasters, logos,
analytics snippets) over HTTPS at startup and on demand. It is served from a private
S3 bucket behind CloudFront, replacing direct reads from the old public
`cities-indicators` bucket (#27, re-landed in #43).

`aws_s3_path` in `dashboard-urbanshift/app.R` is the single source of truth for this
base URL. Every data reference goes through it — do not reintroduce hardcoded bucket
URLs.

```
aws_s3_path = "https://cities-indicators-shiny.wridata.org/"
```

### AWS resources

All in region `us-east-1`. Look up resource IDs in the AWS console rather than
recording them here, since the repo is public.

| Resource | Notes |
| --- | --- |
| S3 bucket `wri-cities-indicators-shiny` | The origin. Private, with all four public-access blocks on, so it is reachable only through CloudFront. |
| CloudFront distribution | Alias `cities-indicators-shiny.wridata.org`. |
| Origin Access Control | Named `wri-cities-indicators-shiny`; sigv4, always sign. |
| ACM certificate | For `cities-indicators-shiny.wridata.org`, expires **2027-04-15**. |
| S3 bucket policy | Grants `s3:GetObject` to `cloudfront.amazonaws.com`, scoped by `AWS:SourceArn` to the distribution. |

Distribution settings (mirroring the cities-socio-economic-vulnerability
distribution):

- Viewer protocol policy: `redirect-to-https`
- Allowed methods: `HEAD, GET, OPTIONS` (cached: `HEAD, GET`)
- Compression: enabled
- Cache policy: `Managed-CachingDisabled`. **CloudFront does not cache; every
  request is proxied to S3.**
- Origin request policy: `Managed-CORS-S3Origin`
- Response headers policy: `Managed-CORS-with-preflight-and-SecurityHeadersPolicy`
- Price class: `PriceClass_All`
- WAF: **none attached** (the SEV distribution has one; see the Dockerfile note below)

### DNS (managed outside AWS)

`wridata.org` is not in a Route 53 hosted zone in this account, so
`cities-indicators-shiny.wridata.org` is a CNAME to the distribution's
`cloudfront.net` domain, added by hand by whoever administers that domain.

**Certificate renewal caveat.** ACM re-checks its DNS validation CNAME when it
auto-renews. That record was removed after the certificate was issued, so the
automatic renewal ACM will attempt around **2027-02-14** (60 days before expiry) is
expected to fail. Before then, either re-add the validation record (the certificate
in ACM shows its name and value) or plan to re-validate manually. The same gap exists
on `cities-socio-economic-vulnerability.wridata.org`, whose certificate expires
2027-01-21.

### Dockerfile: GDAL environment variables

```dockerfile
RUN echo 'GDAL_HTTP_USERAGENT=GDAL' >> /usr/local/lib/R/etc/Renviron.site && \
    echo 'GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR' >> /usr/local/lib/R/etc/Renviron.site
```

Rasters are read through GDAL's `/vsicurl/` driver. `GDAL_HTTP_USERAGENT` exists
because a CloudFront WAF will block requests that send no user agent — this
distribution has no WAF today, but the setting is kept so attaching one later does not
break the app. `GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR` stops GDAL from listing the
whole prefix on every open, which the bucket does not permit and which costs a request
per read regardless.

### Verifying

```sh
# object fetch
curl -I https://cities-indicators-shiny.wridata.org/indicators/definitions_dev.csv

# raster read through the same path the app uses
GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR GDAL_HTTP_USERAGENT=GDAL \
  gdalinfo /vsicurl/https://cities-indicators-shiny.wridata.org/data/population/worldpop/ARG-Buenos_Aires-ADM2union-WorldPop-population-2020.tif
```
