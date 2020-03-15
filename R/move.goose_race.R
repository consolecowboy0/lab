#' Method for goose_race class object to move the goose
#'
#' @param df dataframe to adjust
#' @param x integer distance to move along the x axis
#' @param y integer distance to move along the y axis
#'
#' @examples
#' gr <- goose_race()
#' move(gr, x = 1, y = 1)
#'
#' @export
move <- function(df, x = 1, y = 1) UseMethod("move")

#' @describeIn move Applied to a goose_race class
move.goose_race <- function(df, x = 1, y = 1) {
  movement <- c(df$current_location[1] + x, df$current_location[2] + y)
  df$current_location <- movement
  rowx <- which(df$locations$x == movement[1] & df$locations$y == movement[2])
  df$on_obstacle <- df$obstacle_present[rowx] == 1
  return(df)
}
