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
VALIDATION_THRESHOLD = 3

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

tail(counted_df)
```

    ## # A tibble: 6 x 10
    ##   image_id Person `Animal/Plant` Object Place Other n_ratings max_rating
    ##   <chr>     <int>          <int>  <int> <int> <int>     <int>      <int>
    ## 1 OASIS_s…      0              7      0     0     0         7          7
    ## 2 OASIS_s…      0              7      0     0     0         7          7
    ## 3 OASIS_t…      0              0      2     4     1         7          4
    ## 4 OASIS_t…      0              6      1     0     0         7          6
    ## 5 OASIS_t…      0              0      7     0     0         7          7
    ## 6 OASIS_w…      0              0      1     4     2         7          4
    ## # … with 2 more variables: rel_max_rating <dbl>, htg_index <dbl>

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
    
    category_max = dplyr::select(., category_names) %>%
      purrr::pmap_chr(
        ., 
        ~ choose_category_max_threshold(c(...))
      ),
    
    category_ties = dplyr::select(., category_names) %>%
      purrr::pmap_chr(
        .,
        ~ choose_category_ties_threshold(c(...))
      )
    
  ) %>%
  dplyr::select(image_id, category_max, category_ties)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## ℹ Use `all_of(category_names)` instead of `category_names` to silence this message.
    ## ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
head(category_df)
```

    ## # A tibble: 6 x 3
    ##   image_id      category_max category_ties
    ##   <chr>         <chr>        <chr>        
    ## 1 IAPS_1033.jpg Animal/Plant Animal/Plant 
    ## 2 IAPS_1310.jpg Animal/Plant Animal/Plant 
    ## 3 IAPS_1390.jpg Animal/Plant Animal/Plant 
    ## 4 IAPS_1617.jpg Animal/Plant Animal/Plant 
    ## 5 IAPS_1660.jpg Animal/Plant Animal/Plant 
    ## 6 IAPS_1750.jpg Animal/Plant Animal/Plant

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
normalized_sum_df <- counted_df %>%
  dplyr::mutate_at(vars(category_names), ~ . / n_ratings) %>%
  dplyr::select(-c(n_ratings, max_rating))

head(normalized_sum_df)
```

    ## # A tibble: 6 x 8
    ##   image_id     Person `Animal/Plant` Object Place Other rel_max_rating htg_index
    ##   <chr>         <dbl>          <dbl>  <dbl> <dbl> <dbl>          <dbl>     <dbl>
    ## 1 IAPS_1033.j…      0              1      0     0     0              1         0
    ## 2 IAPS_1310.j…      0              1      0     0     0              1         0
    ## 3 IAPS_1390.j…      0              1      0     0     0              1         0
    ## 4 IAPS_1617.j…      0              1      0     0     0              1         0
    ## 5 IAPS_1660.j…      0              1      0     0     0              1         0
    ## 6 IAPS_1750.j…      0              1      0     0     0              1         0

### Pivot Sum-Normed Categories to Longer

``` r
norm_sum_long <- normalized_sum_df %>% 
  tidyr::pivot_longer(
    -c(image_id, htg_index),
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
p <- ggplot(norm_sum_long, aes(x=image_sort_id, y=category, alpha=density, size=density)) +
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
p <- ggplot(norm_sum_long %>% dplyr::filter(htg_index > 0), aes(x=image_sort_id, y=category, alpha=density, size=density)) +
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
alphabetized_category_names <- sort(category_names)

combined_categorization_df <- normalized_sum_df %>%
  dplyr::inner_join(category_df, by = "image_id") %>%
  dplyr::select(-c(rel_max_rating)) %>%
  dplyr::mutate_at(
    vars(category_max),
    ~ as.factor(.x)
  ) %>%
  dplyr::filter(htg_index != 0) %>%
  # relocate by alphabetical order for plotting
  dplyr::relocate(all_of(alphabetized_category_names))

head(combined_categorization_df)
```

    ## # A tibble: 6 x 9
    ##   `Animal/Plant` Object Other Person Place image_id htg_index category_max
    ##            <dbl>  <dbl> <dbl>  <dbl> <dbl> <chr>        <dbl> <fct>       
    ## 1              0      0 0      0.5   0.5   IAPS_22…     0.562 Other       
    ## 2              0      0 0      0.875 0.125 IAPS_24…     0.234 Person      
    ## 3              0      0 0.125  0.875 0     IAPS_26…     0.234 Person      
    ## 4              0      0 0.125  0.875 0     IAPS_30…     0.234 Person      
    ## 5              0      0 0      0.5   0.5   IAPS_58…     0.562 Other       
    ## 6              0      0 0.375  0     0.625 IAPS_59…     0.453 Place       
    ## # … with 1 more variable: category_ties <chr>

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

### Max-Normalization

``` r
normalized_max_df <- counted_df %>%
  dplyr::mutate_at(vars(category_names), ~ . / max_rating) %>%
  dplyr::select(-c(n_ratings, max_rating))

head(normalized_max_df)
```

    ## # A tibble: 6 x 8
    ##   image_id     Person `Animal/Plant` Object Place Other rel_max_rating htg_index
    ##   <chr>         <dbl>          <dbl>  <dbl> <dbl> <dbl>          <dbl>     <dbl>
    ## 1 IAPS_1033.j…      0              1      0     0     0              1         0
    ## 2 IAPS_1310.j…      0              1      0     0     0              1         0
    ## 3 IAPS_1390.j…      0              1      0     0     0              1         0
    ## 4 IAPS_1617.j…      0              1      0     0     0              1         0
    ## 5 IAPS_1660.j…      0              1      0     0     0              1         0
    ## 6 IAPS_1750.j…      0              1      0     0     0              1         0

### Pivot Max-Normed Categories to Longer

``` r
max_long <- normalized_max_df %>% 
  tidyr::pivot_longer(
    -image_id,
    names_to = "category",
    values_to = "density"
  )

head(max_long)
```

    ## # A tibble: 6 x 3
    ##   image_id      category       density
    ##   <chr>         <chr>            <dbl>
    ## 1 IAPS_1033.jpg Person               0
    ## 2 IAPS_1033.jpg Animal/Plant         1
    ## 3 IAPS_1033.jpg Object               0
    ## 4 IAPS_1033.jpg Place                0
    ## 5 IAPS_1033.jpg Other                0
    ## 6 IAPS_1033.jpg rel_max_rating       1
