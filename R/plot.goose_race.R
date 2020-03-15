#' Method for goose_race class object to see a visual of current environment
#'
#' @param x goose_race object to display
#' @param ... additional arguments
#'
#' @export
#' @method plot goose_race
plot.goose_race <- function(x, ...) {
  y <- legend <- NULL
  for_plot <- x$locations
  for_plot$legend <- ifelse(for_plot$obstacle_present == 1,
                            "obstacle", "no obstacle")
  for_plot$legend[which(for_plot$x == x$current_location[1] &
                        for_plot$y == x$current_location[2])] <- "goose"
  ggplot2::ggplot(for_plot, ggplot2::aes(x, y)) +
    ggplot2::geom_point(ggplot2::aes(color = legend,
                                     size = ifelse(legend %in% c("goose", "obstacle"),
                                                   10, 2))) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::scale_size(guide = FALSE) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values=c("#999999", "#000000", "#56B4E9"))
}
