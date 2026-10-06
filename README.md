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
| `deploy.sh` | Builds and restarts a container on the app server — see [Deployment](#deployment) |
| `Dockerfile` | Builds the container: `rocker/shiny:4.2.1` + R packages pinned to a 2022-09-02 CRAN snapshot |
| `deploy/` | Terraform for the EC2 host (instance, security group, elastic IP, SSM role) |
| `.github/workflows/` | Staging deploy workflow — see the caveat under [Deployment](#deployment) |

## Branches

Several long-lived branches are deployed independently. There is no single "current"
branch, and `main` is not deployed anywhere.

| Branch | Where it runs |
| --- | --- |
| `combined-app` | **Production** — port 3838, container `combined-app` |
| `staging` | **Staging** — port 4949, container `staging-container` |
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

Both containers run on a single EC2 instance in account **540362055257**
(`us-east-1`):

| | |
| --- | --- |
| Instance | `i-08cfa066a7178527b` (`IndicatorsAppServerInstance`) |
| Public IP | `52.72.202.129` · private `172.31.96.97` |
| Access | **SSM Session Manager** (there is no SSH key workflow) |

Public traffic reaches it through two load balancers, neither of which is managed by
the Terraform in `deploy/`:

```
citiesindicators.wri.org
  -> NLB CitiesIndicatorStaticLB  (44.212.116.171; listeners 80, 443, 4949)
  -> ALB CitiesIndicatorLB
  -> i-08cfa066a7178527b  :3838 (production) / :4949 (staging)
```

Staging is reachable at `https://citiesindicators.wri.org:4949/`.

### Deploying a change

Deployment is **manual**. Connect to the instance with SSM Session Manager, then run
`deploy.sh` from the repository checkout:

```sh
./deploy.sh <branch> <image> [port] [container]
```

```sh
# staging  (port 4949)
./deploy.sh staging staging-image 4949

# production  (port 3838)
./deploy.sh combined-app combined-app 3838
```

`port` defaults to `4949`. `container` defaults to the image name, with a trailing
`-image` rewritten to `-container`, which gives `staging-image` -> `staging-container`
and `combined-app` -> `combined-app`.

The script checks out and pulls the branch, builds the image, replaces the container
(always with `--restart on-failure`, so it survives an instance reboot), then polls the
port until the app answers and prints the data host from the served page so you can see
which bucket or distribution the new build reads from.

Two safeguards worth knowing about:

- It **refuses to run if the working tree is dirty**, rather than let `git checkout`
  discard uncommitted work. The checkout on the server has carried stray edits before.
- It **retags the outgoing image** as `<image>-previous` before building. If the new
  container fails to serve, the script prints the logs and the exact command to roll
  back to that image.

Doing it by hand is the same four steps:

```sh
cd ~/cities-indicators-dashboard-urbanshift
git fetch --all && git checkout <branch> && git pull
docker build . -t <image>
docker rm -f <container>
docker run -d -p <port>:3838 --name <container> --restart on-failure <image>
```

### The GitHub Actions workflow does not run

`.github/workflows/deploy-staging.yml` is triggered by pushes to `staging`, but the
file exists only on `main` and `github-action`. GitHub reads `push` workflows from the
branch being pushed to, so pushing to `staging` fires nothing — the workflow has never
run. To make it work, the workflow file has to be present on `staging` itself, and its
four secrets (`AWS_IP`, `AWS_USERNAME`, `SSH_KEY`, `SSH_PORT`) have to be populated and
an SSH path opened to a host that is currently SSM-only.

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
analytics snippets) over HTTPS at build/startup time. As of 2026-09-30 that data is
served from a private S3 bucket fronted by CloudFront, replacing direct public-S3
reads from the old `cities-indicators` bucket.

`aws_s3_path` in `dashboard-urbanshift/app.R` is the single source of truth for this
base URL. Every data reference goes through it — do not reintroduce hardcoded bucket
URLs.

```
aws_s3_path = "https://cities-indicators-shiny.wridata.org/"
```

### AWS resources

All in account **540362055257**, region **us-east-1**.

| Resource | Identifier | Notes |
| --- | --- | --- |
| S3 bucket (origin) | `wri-cities-indicators-shiny` | Private. All four public-access blocks on. Reachable only via CloudFront. |
| CloudFront distribution | `E2SZ050RAH064K` | Domain `d1i466oq03q1hq.cloudfront.net`, alias `cities-indicators-shiny.wridata.org` |
| Origin Access Control | `E3O18XXD5MW5J0` | Name `wri-cities-indicators-shiny`, sigv4, always sign |
| ACM certificate | `b6abf7f9-47c6-4a41-962f-9af96b4a5322` | `cities-indicators-shiny.wridata.org`, expires **2027-04-15** |
| S3 bucket policy | on `wri-cities-indicators-shiny` | Grants `s3:GetObject` to `cloudfront.amazonaws.com`, scoped by `AWS:SourceArn` to the distribution above |

Distribution settings (mirrors the socio-economic-vulnerability distribution
`E3B5G0FBPJA96I`):

- Viewer protocol policy: `redirect-to-https`
- Allowed methods: `HEAD, GET, OPTIONS` (cached: `HEAD, GET`)
- Compression: enabled
- Cache policy: `Managed-CachingDisabled` — **CloudFront does not cache; every
  request is proxied to S3**
- Origin request policy: `Managed-CORS-S3Origin`
- Response headers policy: `Managed-CORS-with-preflight-and-SecurityHeadersPolicy`
- Price class: `PriceClass_All`
- WAF: **none attached** (the SEV distribution does have one — see the Dockerfile note below)

### DNS records (managed outside this AWS account)

`wridata.org` is not in a Route 53 hosted zone in account 540362055257, so these
records were added by hand by whoever administers that domain.

| Name | Type | Value | Status |
| --- | --- | --- | --- |
| `cities-indicators-shiny.wridata.org` | CNAME | `d1i466oq03q1hq.cloudfront.net` | **Required.** Routes traffic to the distribution. |
| `_a97cd8dabf9fd81124a53286562c3280.cities-indicators-shiny.wridata.org` | CNAME | `_f661aff2cdd496d08db9df6b64a7a644.wzccmgtwzk.acm-validations.aws.` | Added for ACM validation, since **deleted**. See renewal caveat below. |

**Certificate renewal caveat.** ACM re-checks the DNS validation CNAME when it
auto-renews. That record has been removed, so the automatic renewal ACM will attempt
around **2027-02-14** (60 days before expiry) is expected to fail. Before then, either
re-add the validation record or plan to re-validate manually. The same gap exists on
`cities-socio-economic-vulnerability.wridata.org`, whose certificate expires
2027-01-21.

An earlier certificate for the name `cities-indicators.wridata.org` was created and
then deleted when the domain was renamed; no DNS records for that name remain in use.

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
