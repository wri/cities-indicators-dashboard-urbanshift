# cities-indicators-dashboard-urbanshift

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
