Emotion Rating Database Analysis
================================

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

Constants
---------

``` r
# Prevalence measures
HOMOSEXUAL_PREVALENCE <- 0.1
HETEORSEXUAL_PREVALENCE <- 0.9
HOMOSEXUAL_TAG <- "_ho"

# Database details
DB_SCALES <- hash()
DB_SCALES[["OASIS"]] <- c(1, 7)
DB_SCALES[["IAPS"]] <- c(1, 9)
DB_SCALES[["NAPS_BE"]] <- c(1, 9)
DB_SCALES[["NAPS_ERO"]] <- c(1, 9)

# Output rating scale
POS_NEG_RANGE <- c(-1, 1)

# For formatting doubles
options(digits = 2)
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

Load Database Imports
---------------------

``` r
oasis_df <- shlab.imgct::load_imported_xlsx(datapath, "oasis_emotion_ratings")
iaps_df <- shlab.imgct::load_imported_xlsx(datapath, "iaps_emotion_ratings")
naps_be_df <- shlab.imgct::load_imported_xlsx(datapath, "naps_be_emotion_ratings")
naps_ero_df <- shlab.imgct::load_imported_xlsx(datapath, "naps_ero_emotion_ratings")
```

Handle General Ratings for NAPS ERO Dataset
-------------------------------------------

Provided that NAPS ERO has specified arousal and valence ratings for
four groups, but no general rating of arousal and valence, we must
choose an averaging strategy. We will attempt a weighted average with
rough prevelance estimates, based on these four groups:

1.  HoF (Homosexual Female)
2.  HoM (Homosexual Male)
3.  HeF (Heterosexual Female)
4.  HeM (Heterosexual Male)

Constants for prevalence are determined above, and below is a function
for weighted ratings based upon the columns of NAPS ERO where "\_ho" and
"\_he" are tags for homosexual and heterosexual, respectively.

``` r
weighted_rating <- function(ratings) {
  columns <- names(ratings)
  
  # Weights are distributed to tags such that:
  #   - FALSE = HETEROSEXUAL_PREVALENCE
  #   - TRUE = HOMOSEXUAL_PREVALENCE
  weights <- c(
    HETEORSEXUAL_PREVALENCE,
    HOMOSEXUAL_PREVALENCE
  )
  
  tags <- stringr::str_detect(columns, HOMOSEXUAL_TAG)
  weights_vec <- weights[1 + tags]

  weighted.mean(
    ratings,
    weights_vec
  )
}
```

Mutate new columns of general arousal and valence ratings for NAPS ERO.

``` r
naps_ero_df <- naps_ero_df %>%
  dplyr::mutate(
    arousal = dplyr::select(., starts_with("arousal")) %>%
      purrr::pmap_dbl(
        .,
        ~ weighted_rating(c(...))
      ),
    valence = dplyr::select(., starts_with("valence")) %>%
      purrr::pmap_dbl(
        .,
        ~ weighted_rating(c(...))
      )
  ) %>%
  dplyr::relocate(
    c(arousal, valence),
    .after = image_id
  )

head(naps_ero_df)
```

    ## # A tibble: 6 x 11
    ##   image_id arousal valence arousal_hof valence_hof arousal_hom valence_hom
    ##   <chr>      <dbl>   <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
    ## 1 NAPS_ER…    4.22    6.10        4.2         6.6         2.45        4.8 
    ## 2 NAPS_ER…    5.08    6.60        5.8         7.5         3.05        5.65
    ## 3 NAPS_ER…    4.90    6.18        5.2         6.95        3.15        5.15
    ## 4 NAPS_ER…    4.64    6.54        5.45        7.35        2.75        5.5 
    ## 5 NAPS_ER…    5.03    5.2         3.55        2.85        4.7         3.95
    ## 6 NAPS_ER…    4.36    6.1         4.35        6.85        2.35        5.35
    ## # … with 4 more variables: arousal_hef <dbl>, valence_hef <dbl>,
    ## #   arousal_hem <dbl>, valence_hem <dbl>

``` r
naps_ero_df %>% 
  summarize(
    mean_arousal = mean(arousal),
    mean_valence = mean(valence)
  )
```

    ## # A tibble: 1 x 2
    ##   mean_arousal mean_valence
    ##          <dbl>        <dbl>
    ## 1         4.28         5.36

Merge dataframes
----------------

``` r
rescale_rating <- function(rating, db) {
  db_scale <- DB_SCALES[[db]]
  return(scales::rescale(rating, from = db_scale, to = POS_NEG_RANGE))
}

all_emotion_ratings_df <- dplyr::bind_rows(
    list(
      oasis_df,
      iaps_df,
      naps_be_df,
      naps_ero_df
    ),
    .id = "db_id"
  ) %>%
  dplyr::select(db_id, image_id, arousal, valence) %>%
  dplyr::mutate_at(
    vars(db_id),
    ~ dplyr::recode(
      .,
      "1" = "OASIS",
      "2" = "IAPS",
      "3" = "NAPS_BE",
      "4" = "NAPS_ERO"
    )
  ) %>%
  dplyr::mutate(
    arousal = pmap_dbl(list(arousal, db_id), rescale_rating),
    valence = pmap_dbl(list(valence, db_id), rescale_rating)
  )

head(all_emotion_ratings_df)
```

    ## # A tibble: 6 x 4
    ##   db_id image_id            arousal valence
    ##   <chr> <chr>                 <dbl>   <dbl>
    ## 1 OASIS OASIS_acorns_1.jpg   -0.551  0.229 
    ## 2 OASIS OASIS_acorns_2.jpg   -0.591  0.173 
    ## 3 OASIS OASIS_acorns_3.jpg   -0.564  0.252 
    ## 4 OASIS OASIS_alcohol_1.jpg  -0.378  0.228 
    ## 5 OASIS OASIS_alcohol_2.jpg  -0.333  0.0833
    ## 6 OASIS OASIS_alcohol_3.jpg  -0.551  0.0556

``` r
p <- ggplot(all_emotion_ratings_df, aes(x=valence, y=arousal, color=db_id)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(limits = POS_NEG_RANGE) +
  scale_y_continuous(limits = POS_NEG_RANGE)

p <- p + labs(
    title = "Arousal vs. Valence of Database Images",
    x = "Valence",
    y = "Arousal",
    color = "Source"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

p
```

![](emotion_ratings_files/figure-markdown_github/plot-emotion-ratings-1.png)

Save to results as general TSV of arousal and valence ratings
-------------------------------------------------------------

``` r
readr::write_tsv(
  all_emotion_ratings_df,
  file.path(datapath, "results", "general_emotion_ratings.tsv"),
  append = FALSE,
  col_names = TRUE
)
```

Other checks on data
--------------------

``` r
all_emotion_ratings_df %>%
  dplyr::group_by(db_id) %>%
  dplyr::count()
```

    ## # A tibble: 4 x 2
    ## # Groups:   db_id [4]
    ##   db_id        n
    ##   <chr>    <int>
    ## 1 IAPS      1165
    ## 2 NAPS_BE   1356
    ## 3 NAPS_ERO   200
    ## 4 OASIS      900
