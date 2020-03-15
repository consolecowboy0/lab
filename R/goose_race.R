#' Build an experiment of a goose moving around a grid.
#' This particular function will create the grid with certain elements blocking the path.
#'
#' @param x integer width of the grid
#' @param y integer height of the grid
#' @param n integer number of elements blocking path
#' @param p vector of probability of finding an obstacle and not
#' @param starting_point vector of length two, e.g., \code{c(x, y)} for where to start the goose
#' @param ending_point vector of length two, e.g., \code{c(x, y)} for where to end the race
#' @return A tibble containing the state of the goose race
#'
#' @export
goose_race <- function(x = 10,
                       y = 10,
                       n = 30,
                       p = c(0.1, 0.99),
                       starting_point = c(1, 1),
                       ending_point = c(100, 100)) {
  basic_grid <- tidyr::expand_grid(x = 1:x, y = 1:y)
  n_grid <- nrow(basic_grid)
  basic_grid$obstacle_present <- sample(x = c(1, 0),
                                        size = n_grid,
                                        replace = TRUE,
                                        prob = p)
  try(if(length(starting_point) != 2) stop("Vector length for starting point must be two"))
  try(if(length(ending_point) != 2) stop("Vector length for ending point must be two"))
  basic_grid$goose_location <- ifelse(basic_grid$x == starting_point[1] &
                                      basic_grid$y == starting_point[2], 1, 0)
  basic_grid$ending_point <- ifelse(basic_grid$x == ending_point[1] &
                                    basic_grid$y == ending_point[2], 1, 0)
  return(basic_grid)
}
