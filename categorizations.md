Categorization Analysis
=======================

Load packages
-------------

``` r
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(hash)
library(GGally)
```

Constants
---------

``` r
VALIDATION_THRESHOLD = 4

# Colors for plotting
COLORS <- hash()
COLORS[["WHITE"]] <- "#FFFFFF"
COLORS[["BLACK"]] <- "#2F2F2F"
COLORS[["GRAY"]] <- "#5C6D70"
COLORS[["LIGHT_GRAY"]] <- "#CDD5D1"
COLORS[["BLUE"]] <- "#0E79B2"
COLORS[["GREEN"]] <- "#6FD08C"
COLORS[["ORANGE"]] <- "#F39237"

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

Load category data
------------------

``` r
counted_responses_cols = readr::cols(
  image_id = readr::col_character(),
  .default = readr::col_integer()
)

counted_df <- shlab.imgct::load_result(datapath, 
                                       stringr::str_c("categorized_", VALIDATION_THRESHOLD, "_valid"), 
                                       columns = counted_responses_cols)

# Determine category names from table
category_names <- counted_df %>% 
  dplyr::select(-c(image_id, n_ratings)) %>%
  names()

# Include max_rating for further analyses
counted_df <- counted_df %>%
  dplyr::mutate(
    max_rating = pmax(!!!rlang::syms(category_names)),
    rel_max_rating = max_rating / n_ratings
  )

# Include htg_index
counted_df <- counted_df %>%
  dplyr::mutate(
    htg_index = dplyr::select(., all_of(category_names)) %>%
      purrr::pmap_dbl(~shlab.imgct::calculate_htg_index(c(...)))
    )

knitr::kable(
  tail(counted_df)
)
```

| image\_id                  |  Person|  Animal/Plant|  Object|  Place|  Other|  n\_ratings|  max\_rating|  rel\_max\_rating|  htg\_index|
|:---------------------------|-------:|-------------:|-------:|------:|------:|-----------:|------------:|-----------------:|-----------:|
| OASIS\_snake\_5.jpg        |       0|             6|       0|      0|      0|           6|            6|              1.00|        0.00|
| OASIS\_stingray\_3.jpg     |       0|             6|       0|      0|      0|           6|            6|              1.00|        0.00|
| OASIS\_thunderstorm\_9.jpg |       0|             0|       1|      4|      1|           6|            4|              0.67|        0.56|
| OASIS\_tiger\_2.jpg        |       0|             5|       1|      0|      0|           6|            5|              0.83|        0.31|
| OASIS\_toast\_1.jpg        |       0|             0|       6|      0|      0|           6|            6|              1.00|        0.00|
| OASIS\_wall\_4.jpg         |       0|             0|       1|      3|      2|           6|            3|              0.50|        0.67|

Threshold-based Categorization
------------------------------

Functions for evaluating the harsh and semi-harsh threshold types of
categorization for a given image based on number of responses.

``` r
# Function to choose category with harsh threshold for maximum
#   - Does not allow for ties, only unique maximum as named category or Other.
choose_category_max_threshold <- function(ratings) {
  
  # count number of ratings equal to maximum rating
  n_max <- length(which(ratings == max(ratings)))
  if (n_max > 1) {
    return("Other")
  } else {
    return(names(ratings)[which.max(ratings)])
  }
  
}

# Function to choose category with semi-harsh threshold for maximum
#   - Does allow for ties, so unique maximum as named category, tied maxima as list category, or Other.
choose_category_ties_threshold <- function(ratings) {
  return(stringr::str_c(
    names(ratings)[which(ratings == max(ratings))],
    collapse=" & "
  ))
}
```

Determine the category dataframe including both the harsh
(category\_max) and semi-harsh (category\_ties) columns, which are
character and list of character types respectively.

``` r
category_df <- counted_df %>%
  dplyr::mutate(
    
    category_max = dplyr::select(., all_of(category_names)) %>%
      purrr::pmap_chr(
        ., 
        ~ choose_category_max_threshold(c(...))
      ),
    
    category_ties = dplyr::select(., all_of(category_names)) %>%
      purrr::pmap_chr(
        .,
        ~ choose_category_ties_threshold(c(...))
      )
    
  )

# Map the category names to pasted new column names with suffix _is_max,
# then bind those columns into the original category dataframe,
# then fill new columns with 0 and 1 values based on whether the category is
#   a maximum for the image.
category_df <- all_of(category_names) %>%
  purrr::map(
    ~ category_df %>% 
        dplyr::select(.x) %>%
        set_names(paste0(.x, "_is_max"))
  ) %>%
  dplyr::bind_cols(category_df, .) %>%
  dplyr::mutate_at(vars(matches("_is_max")), ~ as.integer(.x == max_rating)) %>%
  dplyr::relocate(matches("_is_max"), .before = n_ratings)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## ℹ Use `all_of(.x)` instead of `.x` to silence this message.
    ## ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
knitr::kable(
  head(category_df)
)
```

| image\_id      |  Person|  Animal/Plant|  Object|  Place|  Other|  Person\_is\_max|  Animal/Plant\_is\_max|  Object\_is\_max|  Place\_is\_max|  Other\_is\_max|  n\_ratings|  max\_rating|  rel\_max\_rating|  htg\_index| category\_max | category\_ties |
|:---------------|-------:|-------------:|-------:|------:|------:|----------------:|----------------------:|----------------:|---------------:|---------------:|-----------:|------------:|-----------------:|-----------:|:--------------|:---------------|
| IAPS\_1033.jpg |       0|             6|       0|      0|      0|                0|                      1|                0|               0|               0|           6|            6|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1310.jpg |       0|             7|       0|      0|      0|                0|                      1|                0|               0|               0|           7|            7|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1390.jpg |       0|             7|       0|      0|      0|                0|                      1|                0|               0|               0|           7|            7|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1617.jpg |       0|             7|       0|      0|      0|                0|                      1|                0|               0|               0|           7|            7|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1660.jpg |       0|             7|       0|      0|      0|                0|                      1|                0|               0|               0|           7|            7|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1750.jpg |       0|             7|       0|      0|      0|                0|                      1|                0|               0|               0|           7|            7|                 1|           0| Animal/Plant  | Animal/Plant   |

### Plot Harsh Categories

``` r
p <- ggplot(category_df, aes(x=category_max)) +
  geom_bar(color=COLORS[["GRAY"]], fill=COLORS[["BLUE"]], alpha=0.7)

p + labs(title="Distribution of Harsh Categorization Counts", x="Category", y="Count") +
  theme_classic() 
```

![](categorizations_files/figure-markdown_github/plot-harsh-categories-1.png)

### Plot Semi-Harsh Categories

``` r
p <- ggplot(category_df, aes(y=category_ties)) +
  geom_bar(color=COLORS[["GRAY"]], fill=COLORS[["ORANGE"]], alpha=0.7)

p + labs(title="Distribution of Semi-Harsh Categorization Counts", x="Count", y="Category") +
  theme_classic() 
```

![](categorizations_files/figure-markdown_github/plot-semi-harsh-categories-1.png)

``` r
only_ties_df <- category_df %>%
  dplyr::filter(!(category_ties %in% c(category_names)))

p <- ggplot(only_ties_df, aes(y=category_ties)) +
  geom_bar(color=COLORS[["GRAY"]], fill=COLORS[["ORANGE"]], alpha=0.7)

p + labs(title="Distribution of Category Ties", x="Count", y="Category") +
  theme_classic()
```

![](categorizations_files/figure-markdown_github/plot-only-ties-1.png)

Normalization-based Categorization
----------------------------------

### Sum normalization

``` r
normed_category_df <- category_df %>%
  dplyr::mutate_at(vars(all_of(category_names)), ~ . / n_ratings) %>%
  dplyr::select(-c(n_ratings, max_rating))

knitr::kable(
  head(normed_category_df)
)
```

| image\_id      |  Person|  Animal/Plant|  Object|  Place|  Other|  Person\_is\_max|  Animal/Plant\_is\_max|  Object\_is\_max|  Place\_is\_max|  Other\_is\_max|  rel\_max\_rating|  htg\_index| category\_max | category\_ties |
|:---------------|-------:|-------------:|-------:|------:|------:|----------------:|----------------------:|----------------:|---------------:|---------------:|-----------------:|-----------:|:--------------|:---------------|
| IAPS\_1033.jpg |       0|             1|       0|      0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1310.jpg |       0|             1|       0|      0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1390.jpg |       0|             1|       0|      0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1617.jpg |       0|             1|       0|      0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1660.jpg |       0|             1|       0|      0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
| IAPS\_1750.jpg |       0|             1|       0|      0|      0|                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |

### Pivot Sum-Normed Categories to Longer

``` r
normed_category_long_df <- normed_category_df %>% 
  tidyr::pivot_longer(
    all_of(category_names),
    names_to = "category",
    values_to = "density"
  ) %>%
  dplyr::arrange(desc(density), category) %>%
  dplyr::mutate(image_sort_id = match(image_id, unique(image_id))) %>%
  dplyr::arrange(category, desc(density)) %>%
  dplyr::relocate(image_sort_id, .after = image_id)
```

### Plot Sum-Normalization

``` r
p <- ggplot(normed_category_long_df, aes(x=image_sort_id, y=category, alpha=density, size=density)) +
  geom_point(aes(color = htg_index))

p <- p + labs(
    title = "Sum-Normed Densities of Image Categorizations",
    x = "Images",
    y = "Category",
    color = "Heterogeneity"
  ) +
  scale_x_continuous(breaks = c(0, 3600)) +
  scale_alpha_continuous(range = c(0.005, 0.1)) +
  scale_size_continuous(range = c(0,2)) +
  scale_color_gradient(low = COLORS[["GREEN"]], high = COLORS[["BLUE"]]) +
  guides(
    alpha = FALSE,
    size = FALSE
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.justification = c("left"),
    legend.key.width = unit(2, "cm")
  )

p
```

![](categorizations_files/figure-markdown_github/plot-sum-normed-categories-1.png)

``` r
p <- ggplot(normed_category_long_df %>% dplyr::filter(htg_index > 0), aes(x=image_sort_id, y=category, alpha=density, size=density)) +
  geom_point(aes(color = htg_index))

p <- p + labs(
    title = "Sum-Normed Densities of Image Categorizations (Non-Homogeneous)",
    x = "Images",
    y = "Category",
    color = "Heterogeneity"
  ) +
  scale_alpha_continuous(range = c(0.005, 0.1)) +
  scale_size_continuous(range = c(0,2)) +
  scale_color_gradient(low = COLORS[["GREEN"]], high = COLORS[["BLUE"]]) +
  guides(
    alpha = FALSE,
    size = FALSE
  ) +
  theme_classic() +
  theme(
    #axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.justification = c("left"),
    legend.key.width = unit(2, "cm")
  )

p
```

![](categorizations_files/figure-markdown_github/plot-sum-normed-nonzero-htg-1.png)

### Combine Categorization Methods

Alphabetize categories and combine sum-normed categorizations with harsh
threshold categorizations.

``` r
normed_category_df
```

    ## # A tibble: 3,600 x 15
    ##    image_id Person `Animal/Plant` Object Place Other Person_is_max
    ##    <chr>     <dbl>          <dbl>  <dbl> <dbl> <dbl>         <int>
    ##  1 IAPS_10…      0              1      0     0     0             0
    ##  2 IAPS_13…      0              1      0     0     0             0
    ##  3 IAPS_13…      0              1      0     0     0             0
    ##  4 IAPS_16…      0              1      0     0     0             0
    ##  5 IAPS_16…      0              1      0     0     0             0
    ##  6 IAPS_17…      0              1      0     0     0             0
    ##  7 IAPS_19…      0              1      0     0     0             0
    ##  8 IAPS_20…      1              0      0     0     0             1
    ##  9 IAPS_20…      1              0      0     0     0             1
    ## 10 IAPS_20…      1              0      0     0     0             1
    ## # … with 3,590 more rows, and 8 more variables: `Animal/Plant_is_max` <int>,
    ## #   Object_is_max <int>, Place_is_max <int>, Other_is_max <int>,
    ## #   rel_max_rating <dbl>, htg_index <dbl>, category_max <chr>,
    ## #   category_ties <chr>

``` r
alphabetized_category_names <- sort(category_names)

combined_categorization_df <- normed_category_df %>%
  #dplyr::inner_join(category_df, by = "image_id") %>%
  dplyr::mutate_at(
    vars(category_max),
    ~ as.factor(.x)
  ) %>%
  dplyr::relocate(all_of(alphabetized_category_names))

knitr::kable(
  head(combined_categorization_df)
)
```

|  Animal/Plant|  Object|  Other|  Person|  Place| image\_id      |  Person\_is\_max|  Animal/Plant\_is\_max|  Object\_is\_max|  Place\_is\_max|  Other\_is\_max|  rel\_max\_rating|  htg\_index| category\_max | category\_ties |
|-------------:|-------:|------:|-------:|------:|:---------------|----------------:|----------------------:|----------------:|---------------:|---------------:|-----------------:|-----------:|:--------------|:---------------|
|             1|       0|      0|       0|      0| IAPS\_1033.jpg |                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
|             1|       0|      0|       0|      0| IAPS\_1310.jpg |                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
|             1|       0|      0|       0|      0| IAPS\_1390.jpg |                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
|             1|       0|      0|       0|      0| IAPS\_1617.jpg |                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
|             1|       0|      0|       0|      0| IAPS\_1660.jpg |                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |
|             1|       0|      0|       0|      0| IAPS\_1750.jpg |                0|                      1|                0|               0|               0|                 1|           0| Animal/Plant  | Animal/Plant   |

### Plot Categorizations by Parallel Coordinates

``` r
parallel_coordinates_plotter <- function(.category = "Person") { # "Person" default
  combined_categorization_df$alpha <- c(0.01, .1)[
      1 + (alphabetized_category_names %in% .category)
    ][combined_categorization_df$category_max]
  
  combined_categorization_df$size <- c(0.1)[combined_categorization_df$category_max]
  
  color_vec <- c(COLORS[["LIGHT_GRAY"]], COLORS[["BLUE"]])[
      1 + (alphabetized_category_names %in% .category)
    ]
  
  p <-  GGally::ggparcoord(
      data = combined_categorization_df,
      columns = alphabetized_category_names,
      groupColumn = "category_max",
      order = 1:(length(alphabetized_category_names) + 1),
      scale = "globalminmax",
      showPoints = FALSE,
      alphaLines = "alpha"
    )
  
  p <- p + labs(
      title = stringr::str_c("Distributions of Category Rating Permutations (", .category, ")"),
      x = "Category",
      y = "Density"
    ) +
    scale_color_manual(values = color_vec) +
    theme_classic() +
    theme(
      legend.position = "none"
    )
  
  p
}
```

``` r
purrr::map(alphabetized_category_names, parallel_coordinates_plotter)
```

![](categorizations_files/figure-markdown_github/plot-all-categorizations-in-parallel-coordinates-1.png)![](categorizations_files/figure-markdown_github/plot-all-categorizations-in-parallel-coordinates-2.png)![](categorizations_files/figure-markdown_github/plot-all-categorizations-in-parallel-coordinates-3.png)![](categorizations_files/figure-markdown_github/plot-all-categorizations-in-parallel-coordinates-4.png)![](categorizations_files/figure-markdown_github/plot-all-categorizations-in-parallel-coordinates-5.png)

### Save categorizations to file

``` r
combined_categorization_df %>%
  dplyr::relocate(image_id) %>%
  readr::write_tsv(
    file.path(datapath, "results", "categorizations.tsv"),
    append = FALSE,
    col_names = TRUE
  )
```
