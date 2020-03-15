#' Method for goose_race class object to move the goose
#'
#' @param gr dataframe to adjust
#' @param x integer distance to move along the x axis
#' @param y integer distance to move along the y axis
#'
#' @examples
#' gr <- goose_race()
#' move(gr, x = 1, y = 1)
#'
#' @export
move <- function(gr, x = 1, y = 1) UseMethod("move")

#' @describeIn move Applied to a goose_race class
move.goose_race <- function(gr, x = 1, y = 1) {
  if(x > 1 | y > 1) stop("Cannot move more than 1 unit in either direction")
  movement <- c(gr$current_location[1] + x, gr$current_location[2] + y)
  gr$current_location <- movement
  rowx <- which(gr$locations$x == movement[1] & gr$locations$y == movement[2])
  gr$on_obstacle <- gr$locations$obstacle_present[rowx] == 1
  if(gr$on_obstacle) stop("Your goose ran in to an obstacle")
  return(gr)
}
