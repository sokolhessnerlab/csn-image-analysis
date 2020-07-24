CSN Image Categorization Master Results Table
=============================================

Load packages
-------------

``` r
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hash)
library(scales)
library(readr)
```

Set datapath and load `shlab.imgct`
-----------------------------------

Begin by setting the working directory and important top-level paths to
data and loading necessary packages.

-   NOTE: This will be changed to dynamically account for the package
    `shlab.imgct` via its GitHub instance later. For now, it is using
    development loading.

``` r
# Set the working directory to be part of S Drive (may make dynamic later?)
# Whilst not dynamic, change for own session if mount point is not equivalent on
# local machine
shared_dir <- "~/Projects/shlab/mounts/imgct"
package_dir <- "~/Projects/shlab"

datapath <- file.path(shared_dir, "csn_images")
imgct_package_path <- file.path(package_dir, "shlab.imgct")

# Make sure that devtools, tidyverse are installed before this call
devtools::load_all(imgct_package_path)
```

``` r
results_path <- file.path(datapath, "results")

categorizations_df <- readr::read_tsv(file.path(results_path, "categorizations.tsv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   image_id = col_character(),
    ##   `Animal/Plant` = col_double(),
    ##   Object = col_double(),
    ##   Other = col_double(),
    ##   Person = col_double(),
    ##   Place = col_double(),
    ##   htg_index = col_double(),
    ##   category_max = col_character(),
    ##   category_ties = col_character()
    ## )

``` r
emotion_ratings_df <- readr::read_tsv(file.path(results_path, "general_emotion_ratings.tsv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   db_id = col_character(),
    ##   image_id = col_character(),
    ##   arousal = col_double(),
    ##   valence = col_double(),
    ##   erotic = col_double()
    ## )

``` r
master_df <- dplyr::left_join(categorizations_df, emotion_ratings_df, by = c("image_id"))

master_df %>%
  readr::write_tsv(file.path(results_path, "master_table.tsv"),
                  append = FALSE,
                  col_names = TRUE)

knitr::kable(
  head(master_df, 5)
)
```

| image\_id      |  Animal/Plant|  Object|      Other|     Person|      Place|  htg\_index| category\_max | category\_ties | db\_id |  arousal|  valence|  erotic|
|:---------------|-------------:|-------:|----------:|----------:|----------:|-----------:|:--------------|:---------------|:-------|--------:|--------:|-------:|
| IAPS\_2217.jpg |             0|       0|  0.0000000|  0.5714286|  0.4285714|       0.510| Person        | Person         | IAPS   |  -0.2300|   0.3100|       0|
| IAPS\_2410.jpg |             0|       0|  0.0000000|  0.8571429|  0.1428571|       0.265| Person        | Person         | IAPS   |  -0.2175|  -0.0950|       0|
| IAPS\_2681.jpg |             0|       0|  0.1428571|  0.8571429|  0.0000000|       0.265| Person        | Person         | IAPS   |  -0.0075|  -0.2400|       0|
| IAPS\_3062.jpg |             0|       0|  0.1428571|  0.8571429|  0.0000000|       0.265| Person        | Person         | IAPS   |   0.1950|  -0.7825|       0|
| IAPS\_5831.jpg |             0|       0|  0.0000000|  0.5714286|  0.4285714|       0.510| Person        | Person         | IAPS   |  -0.1425|   0.6575|       0|
