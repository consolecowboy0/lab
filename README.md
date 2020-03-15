
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lab <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of **lab** is to provide useful functionality for experimenting
with the development of machine learning and articifical intelligence
related algorithms. Specifically, lab serves as a laboratory for
developing novel ML/RL/AI methods.

## Installation

You can install the released version of **lab** from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("lab") coming soon!
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mtnman38/lab")
```

## Usage

The purpose of **lab** is to help you get started developing,
experimenting, and testing different ML/RL/AI solutions on a variety of
basic problems. What’s the point of this? Well, having a consistent
structure to test a new method on allows you to easily reproduce results
and understand when and how a certain method fails. This is obviously
useful if you are interested in developing novel methods.

Let’s look at a very simple example that illustrates how to use this
package. We will use the built in goose\_game S3 class to see if we can
build a very simple model to allow to goose to navigate the obstacles in
the goose race. **lab** provides you with simple tools to get started,
as well as importing a variety of handy libraries.

We first will initialize a goose\_race and take a look at what we have.

``` r
library(lab)
library(tidyverse)
#> ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
#> ✓ tibble  2.1.3     ✓ dplyr   0.8.5
#> ✓ tidyr   1.0.2     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

gr <- goose_race()
gr
#> $locations
#> # A tibble: 100 x 3
#>        x     y obstacle_present
#>    <int> <int>            <dbl>
#>  1     1     1                0
#>  2     1     2                0
#>  3     1     3                0
#>  4     1     4                0
#>  5     1     5                0
#>  6     1     6                0
#>  7     1     7                0
#>  8     1     8                0
#>  9     1     9                0
#> 10     1    10                0
#> # … with 90 more rows
#> 
#> $starting_point
#> [1] 1 1
#> 
#> $ending_point
#> [1] 100 100
#> 
#> $current_location
#> [1] 1 1
#> 
#> $on_obstacle
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "goose_race"
```

You can see that we initialize a large 10x10 grid (stored in $locations
as a tibble) along with a few other parameters, such as the starting and
ending point of the goose race. The goal is to try and develop an
algorithm to get the goose to the end of the race.

Let’s start by writing a function that extracts some features for us
from our goose’s environment.

``` r
extract_features <- function(gr) {
  obstacle_df <- look(gr)
  if (nrow(obstacle_df) > 0)
    obstacle_df$obstacle_present <- TRUE
  around_df <- tibble::tibble(x = c(gr$current_location[1] + 1,
                                    gr$current_location[1] - 1,
                                    gr$current_location[1] + 0,
                                    gr$current_location[1] + 0),
                              y = c(gr$current_location[2] + 0,
                                    gr$current_location[2] - 0,
                                    gr$current_location[2] - 1,
                                    gr$current_location[2] + 1),
                              direction = c("north", "south", "east", "west"))
  feature_df <- dplyr::left_join(around_df, obstacle_df, by = c("x", "y"))
  if (nrow(obstacle_df) == 0)
    feature_df$obstacle_present <- FALSE
  feature_df$obstacle_present[is.na(feature_df$obstacle_present)] <- 0
  feature_df %>%
    dplyr::select(direction, obstacle_present) %>%
    tidyr::pivot_wider(names_from = direction, values_from = obstacle_present) %>%
    mutate(current_x = gr$current_location[1], current_y = gr$current_location[2])
}
```
