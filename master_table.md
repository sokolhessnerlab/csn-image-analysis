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
    ##   Person_is_max = col_double(),
    ##   `Animal/Plant_is_max` = col_double(),
    ##   Object_is_max = col_double(),
    ##   Place_is_max = col_double(),
    ##   Other_is_max = col_double(),
    ##   rel_max_rating = col_double(),
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

| image\_id      |  Animal/Plant|  Object|  Other|  Person|  Place|  Person\_is\_max|  Animal/Plant\_is\_max|  Object\_is\_max|  Place\_is\_max|  Other\_is\_max|  rel\_max\_rating|  htg\_index| category\_max | category\_ties | db\_id |  arousal|  valence|  erotic|
|:---------------|-------------:|-------:|------:|-------:|------:|----------------:|----------------------:|----------------:|---------------:|---------------:|-----------------:|-----------:|:--------------|:---------------|:-------|--------:|--------:|-------:|
| IAPS\_1033.jpg |             1|       0|      0|       0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   | IAPS   |   0.2825|  -0.2825|       0|
| IAPS\_1310.jpg |             1|       0|      0|       0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   | IAPS   |   0.2500|  -0.1000|       0|
| IAPS\_1390.jpg |             1|       0|      0|       0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   | IAPS   |   0.0725|  -0.1250|       0|
| IAPS\_1617.jpg |             1|       0|      0|       0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   | IAPS   |   0.0850|  -0.1925|       0|
| IAPS\_1660.jpg |             1|       0|      0|       0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   | IAPS   |  -0.1075|   0.3725|       0|
