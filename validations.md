Validation Thresholds Analysis
==============================

Load packages
-------------

``` r
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(hash)
```

Constants
---------

``` r
# Validation details
MAX_THRESHOLD <- 5
THRESHOLD_INDEX <- 1:(MAX_THRESHOLD + 1)

# Colors for plotting
COLORS <- hash()
COLORS[["WHITE"]] <- "#FFFFFF"
COLORS[["BLACK"]] <- "#2F2F2F"
COLORS[["GRAY"]] <- "#5C6D70"
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

Load the dataframe containing each participant and validation totals
under the column header named `total_valid`.

``` r
valid_df <- shlab.imgct::validate_all_participants(datapath)
```

Plot the distribution of validation counts across participants.

``` r
p <- ggplot(valid_df, aes(x=total_valid)) + 
  geom_histogram(binwidth=1, color=COLORS[["GRAY"]], fill=COLORS[["BLUE"]], alpha=0.7)

p + labs(title="Distribution of Validation Counts", x="Total Valid", y="Count") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5)) +
  theme_classic() +
  theme(legend.position="top")
```

![](validations_files/figure-markdown_github/plot-validation-counts-1.png)

Categorize at each threshold and assign to list, where list element
indices are threshold values plus one (to be R-like)

``` r
ct_df_list <- list()
ct_df_list <- purrr::map(THRESHOLD_INDEX, function(x) {
  th <- x - 1
  df <- shlab.imgct::categorize(datapath, threshold = th)
  df$threshold <- as.factor(th)
  ct_df_list[[x]] <- df
})
```

Bind rows of the dataframes from the above list to make a single
dataframe with “image\_id” and “threshold” behaving as a multi-index of
sorts for plotting and analysis. Also, create dataframe “mu” of means of
ratings per threshold.

``` r
ct_df_bind <- dplyr::bind_rows(ct_df_list[THRESHOLD_INDEX]) %>%
  dplyr::arrange(desc(threshold))

mu <- ct_df_bind %>%
  dplyr::group_by(threshold) %>% 
  dplyr::summarise(mu_ratings = mean(n_ratings), .groups = "drop") %>%
  dplyr::arrange(desc(threshold))
```

Plot distribution of rating counts relative to each threshold with
means.

``` r
p <- ggplot(ct_df_bind , aes(x=n_ratings, fill=threshold)) + 
  geom_histogram(binwidth=1, color=COLORS[["GRAY"]], alpha=0.5, aes(y = ..density..), position="identity") +
  geom_vline(data=mu, aes(xintercept=mu_ratings, color=threshold),
             linetype="dashed")

p + labs(title="Distribution of Image Ratings", x="Number of Ratings", y="Density") +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12)) +
  theme_classic() +
  theme(legend.position="top")
```

![](validations_files/figure-markdown_github/plot-ratings-by-threshold-1.png)
Above is the distribution of rating counts per image across the six
possible validation thresholds. As expected, increasing values of
threshold from 0 to 5 move the density of ratings leftward. I’ve
included vertical dashed lines representing means of ratings for each
threshold, where the line color and shade color of bars match. Some
thoughts:

1.  Clearly, we can’t use a threshold of 0.
2.  A threshold of 1, 2, or 3 gets us in the ballpark of about 8-9
    ratings per image on average. A threshold of 4 gets us down to 7-8
    ratings per image, and 5 gets us just below 6 ratings per image.
3.  The variance on a validation threshold of 5 is clearly massive, and
    is the only threshold to lower the number of ratings for any block
    to sub-4.
4.  Thresholds of 3 or 4 will likely be our best bet, but will wait to
    further evaluate which of those based on the relative heterogeneity
    distributions.

Calculate heterogeneity indices
-------------------------------

For all rows in the binded dataframe, calculate the htg\_index and
assign a column for it.

``` r
htg_df <- ct_df_bind %>%
  dplyr::mutate(
    htg_index = select(., -c(image_id, n_ratings, threshold)) %>%
      purrr::pmap_dbl(~shlab.imgct::calculate_htg_index(c(...)))
    )

knitr::kable(
  head(htg_df)
)
```

| image\_id      |  Person|  Animal/Plant|  Object|  Place|  Other|  n\_ratings| threshold |  htg\_index|
|:---------------|-------:|-------------:|-------:|------:|------:|-----------:|:----------|-----------:|
| IAPS\_1033.jpg |       0|             4|       0|      0|      0|           4| 5         |           0|
| IAPS\_1310.jpg |       0|             5|       0|      0|      0|           5| 5         |           0|
| IAPS\_1390.jpg |       0|             5|       0|      0|      0|           5| 5         |           0|
| IAPS\_1617.jpg |       0|             5|       0|      0|      0|           5| 5         |           0|
| IAPS\_1660.jpg |       0|             5|       0|      0|      0|           5| 5         |           0|
| IAPS\_1750.jpg |       0|             5|       0|      0|      0|           5| 5         |           0|

Below are two other visualizations:

1.  The scatter plot visualization we discussed that gives a sense of
    how heterogeneity changes with increasing thresholds of validation,
    and
2.  The distribution of counts of fully homogeneous responses, where

Heterogeneity speaks to the level of variation among ratings for a given
image in the normalized range \[0,1\], where 0 is fully homogeneous and
0 is fully heterogeneous.

In (1) I’ve included the number of ratings per image as a factor,
color-coded in the legend to the right. It further demonstrates that
thresholds of 1, 2, and 5 are probably not ideal, while 3 or 4 look like
prime candidates. For reference, noting exception of one participant
thrown out during cleaning for the multi-selection issue: - Threshold of
3 leaves of us with 147 participants’ responses out of 187, so 78.6% of
responses - Threshold of 4 leaves us with 136 participants’ responses
out of 187, so 72.7% of responses

``` r
factored_htg_df <- htg_df %>%
  dplyr::mutate_at(vars(n_ratings), list(~ as.factor(.x)))

p <- ggplot(factored_htg_df, aes(x=threshold, y=htg_index, color=n_ratings)) + 
  geom_point(alpha=.3)

p <- p + labs(title="Distribution of Heterogeneity By Threshold", x="Validation Threshold", y="Heterogeneity Index") +
  theme_classic() +
  theme(legend.position="right")

p
```

![](validations_files/figure-markdown_github/plot-htg-by-threshold-1.png)

### Plot Heterogeneity given Threshold

Look at Validation Threshold 3 & 4

``` r
threshold_filter <- c(3,4)

htg_pivot_df <- htg_df %>%
  dplyr::filter(threshold %in% threshold_filter) %>%
  tidyr::pivot_wider(
    id_cols = image_id,
    names_from = c(threshold),
    names_prefix = c("th_"),
    names_sort = TRUE,
    values_from = c(htg_index, n_ratings)
  ) %>%
  dplyr::mutate(
    htg_diff = htg_index_th_4 - htg_index_th_3,
    n_ratings_diff = n_ratings_th_4 - n_ratings_th_3
  ) 

p <- ggplot(htg_pivot_df, aes(x=htg_index_th_4, y=htg_index_th_3, size = n_ratings_diff)) +
  geom_point(color = COLORS[["ORANGE"]], alpha = 0.25)

p <- p + labs(
    title = "Heteogeneity Index Comparison, Validation Thresholds (3 vs. 4)",
    x = "Heterogeneity (Validation 4+ of 5)",
    y = "Heterogeneity (Validation 3+ of 5)",
    size = expression(paste(Delta, "n_ratings"))
  ) +
  scale_size_continuous(range = c(2,.5)) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

p
```

![](validations_files/figure-markdown_github/compare-htg-by-threshold-1.png)

### Plot Differences in Heterogeneity

Consider both directional changes in heterogeneity and number of ratings
between validation threshold values

``` r
htg_diff_all_df <- htg_pivot_df %>%
  dplyr::arrange(desc(-htg_diff)) %>%
  dplyr::mutate(image_sort_id = match(image_id, unique(image_id))) %>%
  dplyr::relocate(image_sort_id, .after = image_id) %>%
  dplyr::select(!contains("_th_"))

htg_diff_nonzero_df <- htg_pivot_df %>%
  dplyr::filter(htg_diff != 0) %>%
  dplyr::arrange(desc(-htg_diff)) %>%
  dplyr::mutate(image_sort_id = match(image_id, unique(image_id))) %>%
  dplyr::relocate(image_sort_id, .after = image_id) %>%
  dplyr::select(!contains("_th_"))
  
knitr::kable(
  head(htg_diff_all_df) 
)
```

| image\_id                        |  image\_sort\_id|  htg\_diff|  n\_ratings\_diff|
|:---------------------------------|----------------:|----------:|-----------------:|
| NAPS\_BE\_landscapes\_132\_h.jpg |                1|      -0.39|                -2|
| IAPS\_9830.jpg                   |                2|      -0.31|                -2|
| IAPS\_7340.jpg                   |                3|      -0.31|                -1|
| NAPS\_BE\_objects\_049\_h.jpg    |                4|      -0.31|                -1|
| NAPS\_BE\_people\_168\_h.jpg     |                5|      -0.31|                -1|
| OASIS\_dessert\_1.jpg            |                6|      -0.31|                -1|

``` r
p <- ggplot(htg_diff_all_df, aes(x=image_sort_id, y=htg_diff, color=n_ratings_diff)) + 
  geom_col(alpha=.4)

p <- p + labs(
    title="Heterogeneity Index Difference Between Validation Thresholds 3 & 4", 
    x="Images", 
    y="Heterogeneity Index Difference",
    color=expression(paste(Delta, "n_ratings"))
  ) +
  scale_color_gradient(low = COLORS[["GREEN"]], high = COLORS[["BLUE"]]) +
  theme_classic() +
  theme(
    #axis.text.x = element_blank(),
    legend.position="right"
  )

p
```

![](validations_files/figure-markdown_github/plot-htg-diff-by-threshold-1.png)

``` r
p <- ggplot(htg_diff_nonzero_df, aes(x=image_sort_id, y=htg_diff, color=n_ratings_diff)) + 
  geom_col(alpha=.4)

p <- p + labs(
    title="Heterogeneity Index Difference (Nonzero) Between Validation Thresholds 3 & 4", 
    x="Images", 
    y="Heterogeneity Index Difference",
    color=expression(paste(Delta, "n_ratings"))
  ) +
  scale_color_gradient(low = COLORS[["GREEN"]], high = COLORS[["BLUE"]]) +
  theme_classic() +
  theme(
    #axis.text.x = element_blank(),
    legend.position="right"
  )

p
```

![](validations_files/figure-markdown_github/plot-htg-nonzero-by-threshold-1.png)

Filter for heterogeneity indices of 0, meaning fully homogenous.

``` r
htg_0_df <- htg_df %>% 
  dplyr::filter(htg_index == 0) %>%
  dplyr::mutate_at(vars(threshold), funs(as.integer(as.character(.))))
```

Plot the histogram of counts for homogeneity by varying validation
threshold.

``` r
p <- ggplot(data = htg_0_df , aes(x=threshold)) + 
  geom_histogram(binwidth=1, color=COLORS[["GRAY"]], fill=COLORS[["ORANGE"]], alpha=0.7)

p <- p + labs(title="Distribution of Homogeneity by Threshold", x="Validation Threshold", y="Count") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5)) +
  theme_classic()
p
```

![](validations_files/figure-markdown_github/plot-homogeneity-by-threshold-1.png)

Participant Response Quality Analysis
-------------------------------------

Begin by loading all clean blocks and binding them.

``` r
clean_responses_df_list <- shlab.imgct::load_all_clean_blocks(datapath) 
```

Rename all columns to be the trial response number for each participant,
inclusive of validation images, as opposed to the particular image name.
This avoids confusion, as in this dataframe the images are not
consistent across blocks of participants.

``` r
# from first dataframe element in list, simply extract old_names,
# then determine new_names via "trial_" pattern that applies generally
old_names <- names(clean_responses_df_list[[1]] %>% dplyr::select(-participant_id))
new_names <- paste("trial", which(old_names %in% old_names), sep="_")

all_responses_df <- clean_responses_df_list %>%
  purrr::map(function(df) {
    df %>% dplyr::rename_with(
      ~ new_names, 
      contains(c(".jpg", ".jpeg", ".png"))
    )
  }) %>%
  dplyr::bind_rows()

knitr::kable(
  head(all_responses_df)
)
```

| participant\_id | trial\_1 | trial\_2 | trial\_3 | trial\_4 | trial\_5 | trial\_6 | trial\_7 | trial\_8 | trial\_9 | trial\_10 | trial\_11 | trial\_12 | trial\_13 | trial\_14 | trial\_15 | trial\_16 | trial\_17 | trial\_18 | trial\_19 | trial\_20 | trial\_21 | trial\_22 | trial\_23 | trial\_24 | trial\_25 | trial\_26 | trial\_27 | trial\_28 | trial\_29 | trial\_30 | trial\_31 | trial\_32 | trial\_33 | trial\_34 | trial\_35 | trial\_36 | trial\_37 | trial\_38 | trial\_39 | trial\_40 | trial\_41 | trial\_42 | trial\_43 | trial\_44 | trial\_45 | trial\_46 | trial\_47 | trial\_48 | trial\_49 | trial\_50 | trial\_51 | trial\_52 | trial\_53 | trial\_54 | trial\_55 | trial\_56 | trial\_57 | trial\_58 | trial\_59 | trial\_60 | trial\_61 | trial\_62 | trial\_63 | trial\_64 | trial\_65 | trial\_66 | trial\_67 | trial\_68 | trial\_69 | trial\_70 | trial\_71 | trial\_72 | trial\_73 | trial\_74 | trial\_75 | trial\_76 | trial\_77 | trial\_78 | trial\_79 | trial\_80 | trial\_81 | trial\_82 | trial\_83 | trial\_84 | trial\_85 | trial\_86 | trial\_87 | trial\_88 | trial\_89 | trial\_90 | trial\_91 | trial\_92 | trial\_93 | trial\_94 | trial\_95 | trial\_96 | trial\_97 | trial\_98 | trial\_99 | trial\_100 | trial\_101 | trial\_102 | trial\_103 | trial\_104 | trial\_105 | trial\_106 | trial\_107 | trial\_108 | trial\_109 | trial\_110 | trial\_111 | trial\_112 | trial\_113 | trial\_114 | trial\_115 | trial\_116 | trial\_117 | trial\_118 | trial\_119 | trial\_120 | trial\_121 | trial\_122 | trial\_123 | trial\_124 | trial\_125 | trial\_126 | trial\_127 | trial\_128 | trial\_129 | trial\_130 | trial\_131 | trial\_132 | trial\_133 | trial\_134 | trial\_135 | trial\_136 | trial\_137 | trial\_138 | trial\_139 | trial\_140 | trial\_141 | trial\_142 | trial\_143 | trial\_144 | trial\_145 | trial\_146 | trial\_147 | trial\_148 | trial\_149 | trial\_150 | trial\_151 | trial\_152 | trial\_153 | trial\_154 | trial\_155 | trial\_156 | trial\_157 | trial\_158 | trial\_159 | trial\_160 | trial\_161 | trial\_162 | trial\_163 | trial\_164 | trial\_165 | trial\_166 | trial\_167 | trial\_168 | trial\_169 | trial\_170 | trial\_171 | trial\_172 | trial\_173 | trial\_174 | trial\_175 | trial\_176 | trial\_177 | trial\_178 | trial\_179 | trial\_180 | trial\_181 | trial\_182 | trial\_183 | trial\_184 | trial\_185 | trial\_186 | trial\_187 | trial\_188 | trial\_189 | trial\_190 | trial\_191 | trial\_192 | trial\_193 | trial\_194 | trial\_195 | trial\_196 | trial\_197 | trial\_198 | trial\_199 | trial\_200 | trial\_201 | trial\_202 | trial\_203 | trial\_204 | trial\_205 |
|:----------------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|:-----------|
| ICT\_001        | 1        | 5        | 4        | 4        | 1        | 3        | 1        | 1        | 1        | 1         | 3         | 5         | 1         | 1         | 2         | 1         | 1         | 4         | 1         | 2         | 1         | 1         | 2         | 1         | 1         | 1         | 2         | 1         | 2         | 2         | 1         | 3         | 3         | 3         | 2         | 3         | 2         | 1         | 3         | 1         | 1         | 1         | 2         | 3         | 4         | 2         | 3         | 5         | 3         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 4         | 4         | 4         | 2         | 4         | 3         | 4         | 1         | 5         | 5         | 2         | 2         | 1         | 3         | 3         | 3         | 1         | 4         | 1         | 1         | 2         | 3         | 1         | 3         | 4         | 3         | 1         | 1         | 3         | 2         | 4         | 1         | 4         | 4         | 4         | 1         | 3         | 2         | 5         | 1         | 1         | 3         | 1         | 4          | 1          | 1          | 1          | 1          | 3          | 4          | 3          | 1          | 2          | 2          | 1          | 4          | 3          | 1          | 1          | 5          | 3          | 2          | 1          | 3          | 1          | 4          | 3          | 3          | 1          | 1          | 1          | 4          | 1          | 4          | 3          | 4          | 1          | 1          | 4          | 1          | 1          | 4          | 4          | 3          | 1          | 3          | 2          | 2          | 3          | 1          | 3          | 2          | 3          | 1          | 1          | 4          | 1          | 3          | 1          | 1          | 1          | 2          | 3          | 1          | 1          | 4          | 2          | 3          | 1          | 1          | 1          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 4          | 2          | 3          | 2          | 4          | 4          | 2          | 5          | 1          | 1          | 5          | 4          | 1          | 4          | 3          | 1          | 1          | 1          | 1          | 3          | 1          | 5          | 3          | 3          | 2          | 1          | 4          | 4          |
| ICT\_002        | 1        | 3        | 3        | 3        | 3        | 3        | 1        | 1        | 1        | 1         | 4         | 3         | 1         | 1         | 1         | 1         | 1         | 4         | 4         | 4         | 2         | 1         | 5         | 1         | 1         | 1         | 2         | 2         | 2         | 2         | 3         | 3         | 3         | 1         | 2         | 3         | 2         | 1         | 3         | 1         | 1         | 1         | 1         | 1         | 3         | 2         | 4         | 4         | 4         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 2         | 1         | 2         | 4         | 4         | 4         | 1         | 4         | 2         | 2         | 1         | 1         | 4         | 4         | 3         | 1         | 4         | 1         | 1         | 2         | 3         | 1         | 3         | 1         | 3         | 1         | 1         | 1         | 2         | 4         | 1         | 1         | 4         | 4         | 1         | 4         | 3         | 4         | 1         | 1         | 4         | 1         | 4          | 1          | 1          | 1          | 1          | 1          | 4          | 3          | 2          | 2          | 2          | 1          | 1          | 3          | 1          | 1          | 3          | 3          | 2          | 1          | 3          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 3          | 1          | 1          | 4          | 4          | 1          | 1          | 4          | 1          | 1          | 1          | 1          | 1          | 1          | 1          | 2          | 2          | 1          | 1          | 3          | 2          | 1          | 1          | 2          | 1          | 3          | 2          | 1          | 1          | 1          | 2          | 3          | 1          | 3          | 1          | 1          | 4          | 3          | 1          | 1          | 1          | 1          | 2          | 3          | 1          | 4          | 2          | 3          | 4          | 4          | 4          | 4          | 3          | 2          | 4          | 4          | 2          | 1          | 1          | 1          | 1          | 1          | 2          | 2          | 4          | 1          | 3          | 4          | 4          | 3          | 1          | 5          | 2          | 1          | 3          | 1          | 2          | 3          |
| ICT\_003        | 1        | 5        | 4        | 3        | 1        | 3        | 1        | 1        | 1        | 1         | 3         | 3         | 1         | 1         | 2         | 1         | 1         | 4         | 1         | 2         | 1         | 1         | 2         | 1         | 1         | 1         | 2         | 1         | 2         | 2         | 1         | 3         | 3         | 1         | 2         | 3         | 2         | 1         | 3         | 1         | 1         | 1         | 1         | 3         | 4         | 2         | 4         | 4         | 3         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 2         | 1         | 2         | 4         | 4         | 4         | 1         | 4         | 4         | 2         | 2         | 1         | 4         | 3         | 3         | 1         | 4         | 1         | 1         | 2         | 3         | 1         | 3         | 1         | 3         | 1         | 1         | 1         | 2         | 4         | 1         | 1         | 3         | 4         | 0         | 4         | 2         | 4         | 1         | 1         | 4         | 1         | 4          | 1          | 1          | 1          | 1          | 3          | 4          | 3          | 2          | 2          | 2          | 1          | 1          | 3          | 1          | 1          | 1          | 3          | 2          | 1          | 3          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 2          | 1          | 4          | 4          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 1          | 1          | 1          | 1          | 2          | 2          | 3          | 1          | 3          | 2          | 1          | 1          | 1          | 1          | 1          | 3          | 1          | 1          | 1          | 3          | 3          | 1          | 1          | 4          | 2          | 3          | 1          | 1          | 1          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 4          | 3          | 3          | 2          | 4          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 3          | 1          | 1          | 1          | 1          | 3          | 1          | 5          | 3          | 3          | 2          | 1          | 4          | 4          |
| ICT\_004        | 1        | 3        | 3        | 3        | 1        | 3        | 1        | 1        | 1        | 1         | 3         | 4         | 1         | 1         | 2         | 1         | 1         | 4         | 1         | 2         | 1         | 1         | 3         | 1         | 1         | 1         | 2         | 1         | 2         | 2         | 1         | 3         | 4         | 3         | 2         | 3         | 2         | 1         | 3         | 1         | 1         | 1         | 1         | 3         | 3         | 2         | 3         | 4         | 3         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 2         | 1         | 2         | 4         | 3         | 4         | 1         | 3         | 2         | 2         | 2         | 1         | 4         | 3         | 3         | 1         | 4         | 1         | 1         | 2         | 3         | 1         | 3         | 1         | 3         | 1         | 1         | 3         | 2         | 4         | 1         | 1         | 3         | 4         | 1         | 4         | 2         | 4         | 1         | 1         | 4         | 1         | 4          | 1          | 1          | 1          | 1          | 3          | 4          | 3          | 2          | 2          | 2          | 1          | 4          | 3          | 1          | 1          | 1          | 3          | 2          | 1          | 3          | 1          | 1          | 3          | 4          | 1          | 1          | 1          | 3          | 1          | 3          | 4          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 1          | 1          | 1          | 1          | 2          | 2          | 3          | 1          | 3          | 2          | 3          | 1          | 1          | 1          | 1          | 3          | 1          | 1          | 1          | 2          | 3          | 1          | 1          | 4          | 2          | 3          | 1          | 1          | 1          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 4          | 2          | 3          | 2          | 4          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 3          | 1          | 1          | 1          | 3          | 3          | 1          | 5          | 3          | 5          | 2          | 1          | 4          | 4          |
| ICT\_005        | 1        | 3        | 4        | 3        | 1        | 3        | 1        | 1        | 1        | 1         | 3         | 4         | 1         | 1         | 2         | 1         | 1         | 3         | 1         | 2         | 1         | 1         | 3         | 1         | 1         | 1         | 2         | 1         | 2         | 2         | 1         | 4         | 3         | 1         | 2         | 3         | 2         | 1         | 3         | 1         | 1         | 1         | 1         | 3         | 3         | 2         | 3         | 4         | 3         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 4         | 1         | 2         | 4         | 4         | 4         | 1         | 4         | 4         | 2         | 2         | 1         | 3         | 3         | 3         | 1         | 4         | 1         | 1         | 2         | 3         | 1         | 3         | 1         | 3         | 1         | 1         | 3         | 2         | 4         | 1         | 1         | 4         | 4         | 1         | 3         | 3         | 4         | 1         | 1         | 4         | 1         | 4          | 1          | 1          | 1          | 1          | 1          | 4          | 3          | 2          | 2          | 2          | 1          | 1          | 3          | 4          | 1          | 4          | 3          | 2          | 1          | 3          | 1          | 4          | 3          | 4          | 1          | 1          | 1          | 4          | 1          | 4          | 4          | 4          | 1          | 1          | 4          | 3          | 1          | 4          | 1          | 1          | 1          | 1          | 2          | 2          | 1          | 1          | 3          | 2          | 1          | 1          | 1          | 4          | 1          | 3          | 1          | 4          | 1          | 2          | 3          | 1          | 1          | 4          | 2          | 3          | 1          | 1          | 1          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 4          | 2          | 3          | 2          | 4          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 4          | 3          | 1          | 1          | 1          | 1          | 3          | 1          | 5          | 4          | 3          | 2          | 1          | 4          | 4          |
| ICT\_006        | 1        | 3        | 3        | 3        | 1        | 3        | 1        | 1        | 1        | 1         | 4         | 4         | 1         | 1         | 2         | 1         | 1         | 5         | 1         | 2         | 1         | 1         | 4         | 1         | 1         | 1         | 2         | 1         | 2         | 2         | 1         | 3         | 1         | 1         | 2         | 4         | 2         | 1         | 3         | 1         | 1         | 1         | 1         | 3         | 4         | 2         | 4         | 4         | 4         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 1         | 2         | 1         | 2         | 4         | 4         | 4         | 1         | 4         | 4         | 2         | 2         | 1         | 4         | 4         | 3         | 4         | 4         | 1         | 1         | 2         | 3         | 1         | 3         | 1         | 3         | 1         | 1         | 1         | 2         | 4         | 1         | 1         | 4         | 4         | 1         | 4         | 3         | 4         | 1         | 1         | 4         | 1         | 4          | 1          | 1          | 1          | 1          | 1          | 4          | 3          | 1          | 2          | 2          | 1          | 4          | 3          | 1          | 1          | 1          | 1          | 2          | 1          | 3          | 1          | 1          | 3          | 4          | 1          | 1          | 1          | 4          | 1          | 1          | 4          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 1          | 1          | 1          | 1          | 2          | 2          | 2          | 1          | 3          | 1          | 1          | 1          | 1          | 1          | 1          | 3          | 1          | 1          | 1          | 2          | 3          | 1          | 1          | 4          | 2          | 3          | 1          | 1          | 1          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 1          | 4          | 2          | 3          | 2          | 4          | 4          | 2          | 1          | 1          | 1          | 1          | 4          | 1          | 1          | 3          | 1          | 1          | 1          | 1          | 3          | 1          | 5          | 4          | 4          | 2          | 1          | 4          | 4          |

With a function to get the lengths of consecutive runs for a given
vector, compute the longest (max) run. Apply this to a widened ratings
dataframe that groups by participantCode, and then summarizes the
consecutive maximum runs on ratings for each participant.

``` r
get_consec_max <- function(vec) {
    with(rle(vec), max(lengths))
}

ratings_df <- all_responses_df %>%
  tidyr::pivot_longer(
    cols=starts_with("trial_"),
    names_to="trial",
    names_prefix="trial_",
    values_to="rating"
  ) %>%
  group_by(participant_id)

consecutives_df <- ratings_df %>% 
  dplyr::summarise(consec_max = get_consec_max(rating), .groups = "drop") %>%
  dplyr::arrange(desc(consec_max))

knitr::kable(
  head(consecutives_df)
)
```

| participant\_id |  consec\_max|
|:----------------|------------:|
| ICT\_145        |           11|
| ICT\_083        |            9|
| ICT\_097        |            9|
| ICT\_115        |            9|
| ICT\_116        |            9|
| ICT\_117        |            9|
