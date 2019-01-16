#' Interactions Plot
#'
#' Interactions Plot
#'
#' NOTE: High gain of pair for \code{option="pairs"} can be a result of high gain of down variable (child).
#'      As strong interactions should be considered only these pairs of variables,
#'      where variable on the bottom (child) has higher gain than variable on the top (parent).
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#' @param option if "interactions" then strong interactions will be presented on the plot,
#'            if "pairs" then plot presents all pairs in the model. Default "interactions".
#'
#'
#' @return a ggplot object
#'
#' @import data.table
#' @import ggplot2
#' @import DALEX
#' @import purrr
#'
#' @examples
#'
#' @export


plot.interactionsTable <- function(x, ...) {
  Feature <- sumGain <- Child <- Parent <- breaks <- NULL

  sumGain <- x$sumGain
  breaks <- c(min(sumGain),
              (max(sumGain - min(sumGain)))/4,
              (max(sumGain - min(sumGain)))/2,
              3*(max(sumGain - min(sumGain)))/4,
              max(sumGain))
  x$breaks <- cut(sumGain, breaks = breaks , right = FALSE, dig.lab = 4, include.lowest = TRUE)
  x$Child <- factor(x$Child, levels = unique(x$Child[order(x$sumGain, decreasing = TRUE)]))

  ggplot(data.frame(x), aes(Child, Parent, sumGain)) +
    geom_tile(aes(fill = breaks)) +
    theme_mi2() +
    theme(axis.text.x = element_text(hjust = 1, angle = 90),
          axis.text.y = element_text(hjust = 1, angle = 0)) +
    scale_fill_manual(name = "sumGain",
                      values = c("#ffffff", "#ccccff", "#7f7fff", "#3232ff"),
                      drop = FALSE,
                      breaks = levels(x$breaks),
                      labels = c("very low", "low", "medium", "high")) +
    coord_equal()

}
