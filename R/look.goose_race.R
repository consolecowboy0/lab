#' Method for goose_race class object to have the goose look around
#'
#' @param gr goose_race object to adjust
#' @param distance integer distance the goose can look
#'
#' @examples
#' gr <- goose_race()
#' look(gr, distance = 2)
#'
#' @export
look <- function(gr, distance = 2) UseMethod("look")

#' @describeIn look Applied to a goose_race class
look.goose_race <- function(gr, distance = 2) {
  test <- gr$locations
  test$current_x <- gr$current_location[1]
  test$current_y <- gr$current_location[2]
  test$distance <- sqrt((test$x - test$current_x)^2 + (test$y - test$current_y)^2)
  return(test[which(test$distance <= distance & test$obstacle_present == 1), c("x", "y")])
}
