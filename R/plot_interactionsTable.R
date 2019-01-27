#' Interactions Plot
#'
#' Interactions Plot
#'
#' NOTE: High gain of pair for \code{option="pairs"} can be a result of high gain of down variable (child).
#'      As strong interactions should be considered only these pairs of variables,
#'      where variable on the bottom (child) has higher gain than variable on the top (parent).
#'
#' @param x a result of `interactionsTable` function
#' @param ... other parameters
#'
#' @return a ggplot object
#'
#' @import data.table
#' @import ggplot2
#' @import DALEX
#' @import purrr
#'
#' @examples
#' #' library("EIX")
#' library("Matrix")
#' library("data.table")
#' library("xgboost")
#'
#' dt_HR <- data.table(HR_data)
#' sm <- sparse.model.matrix(left ~ . - 1,  data = dt_HR)
#'
#' param <- list(objective = "binary:logistic", base_score = 0.5, max_depth = 2)
#' xgb.model <- xgboost( param = param, data = sm, label = dt_HR[, left] == 1, nrounds = 50, verbose = FALSE)
#'
#' inter <- interactionsTable(xgb.model, sm,		option = "interactions")
#' inter
#' plot(inter)
#'
#' inter <- interactionsTable(xgb.model, sm,		option = "pairs")
#' inter
#' plot(inter)
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
