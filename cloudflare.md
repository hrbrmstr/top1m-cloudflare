# CloudFlare use in Alexa Top 1m

Let's take a 1% sample of domains in the Alexa top 1m and see how many have CloudFlare IPv4s in their DNS A records.




```r
library(sys)
library(iptools)
library(stringi)
library(tidyverse)
```

We can get a list of CloudFlare IPV4s here direct from the source:


```r
cf_ranges_url <- "https://www.cloudflare.com/ips-v4"
cf_range <- read_lines(cf_ranges_url)
is_cloudflare <- function(x) { any(ip_in_any(x, cf_range)) }
```

And, get download a copy of the Alexa Top 1m:


```r
top_1m_url <- "http://s3.amazonaws.com/alexa-static/top-1m.csv.zip"
fil <- basename(top_1m_url)
if (!file.exists(fil)) download.file(URL, fil)
top_1m_fil <- unzip(fil)
```

Read in the top 1m and take a 1% sample:


```r
top_1m <- read_csv(top_1m_fil, col_names = c("rank", "domain"),  col_types = "ic")
set.seed(1492)
samp_1m <- top_1m[sample(nrow(top_1m), 0.01 * nrow(top_1m)),]
tf <- tempfile(fileext=".txt")
write_lines(samp_1m$domain, tf)
```

This uses [slookup](https://github.com/hrbrmstr/slookup) which allows for parallel DNS lookups. There are better utilities to use for this but I had it handy and am not doing too many lookups (bulk lookups should use something like [MassDNS](https://github.com/blechschmidt/massdns) and a fairly large list of resolvers).

We'll also cache the result to avoid doing this resource-intensive task again:


```r
slookup_fil <- "slookup_output.rds"
if (!file.exists(slookup_fil)) {
  res <- exec_internal("slookup", c("-f", "4", "-p" , "-t", "A", "-i", tf))
  write_rds(res, slookup_fil)
} else {
  res <- read_rds(slookup_fil)
}
```

The output of `slookup` is uniform but ugly, so we clean it up, make it a data frame and test for CloudFlare range inclusion:


```r
rawToChar(res$stdout) %>%
  stri_split_lines() %>%
  flatten_chr() %>%
  discard(stri_detect_fixed, "not found") %>%
  stri_replace_all_fixed(" +", "") %>%
  stri_replace_all_fixed(" A ", " ") %>%
  stri_split_fixed(" ", 2, simplify = TRUE) %>%
  as_data_frame() %>%
  set_names(c("domain", "ip")) %>%
  filter(domain != "") %>%
  mutate(ip = stri_split_fixed(ip, " "),
         is_cf = map_lgl(ip, is_cloudflare)) -> samp_df
```

Nearly 10% of our 1% sample are in CloudFlare:


```r
count(samp_df, is_cf) %>%
  mutate(pct=n/sum(n), pct_lab=scales::percent(pct))
```

```
## # A tibble: 2 × 4
##   is_cf     n        pct pct_lab
##   <lgl> <int>      <dbl>   <chr>
## 1 FALSE  8714 0.90903401   90.9%
## 2  TRUE   872 0.09096599    9.1%
```
