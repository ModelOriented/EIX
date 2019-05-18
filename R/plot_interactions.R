#' Plot importance of interactions or pairs
#'
#' This function plots the importance ranking of interactions and pairs in the model.
#'
#' NOTE: Be careful use of this function with \code{option="pairs"} parameter,
#'       because high gain of pair can be a result of high gain of child variable.
#'      As strong interactions should be considered only these pairs of variables,
#'      where variable on the bottom (child) has higher gain than variable on the top (parent).
#'
#' @param x a result from the \code{interactions} function.
#' @param ... other parameters.
#'
#' @return a ggplot object
#'
#' @import data.table
#' @import ggplot2
#' @importFrom DALEX theme_drwhy
#'
#' @examples
#' library("EIX")
#' library("Matrix")
#' sm <- sparse.model.matrix(left ~ . - 1,  data = HR_data)
#'
#' library("xgboost")
#' param <- list(objective = "binary:logistic", max_depth = 2)
#' xgb_model <- xgboost(sm, params = param, label = HR_data[, left] == 1, nrounds = 25, verbose=0)
#'
#' inter <- interactions(xgb_model, sm,		option = "interactions")
#' inter
#' plot(inter)
#'
#' inter <- interactions(xgb_model, sm,		option = "pairs")
#' inter
#' plot(inter)
#'
#' \donttest{
#'library(lightgbm)
#'train_data <- lgb.Dataset(sm, label =  HR_data[, left] == 1)
#'params <- list(objective = "binary", max_depth = 2)
#'lgb_model <- lgb.train(params, train_data, 25)
#'
#' inter <- interactions(lgb_model, sm,		option = "interactions")
#' inter
#' plot(inter)
#'
#' inter <- interactions(lgb_model, sm,		option = "pairs")
#' inter
#' plot(inter)
#'}
#'
#' @export


plot.interactions <- function(x, ...) {
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
    theme_drwhy() +
    theme(axis.text.x = element_text(hjust = 1, angle = 90),
          axis.text.y = element_text(hjust = 1, angle = 0)) +
    scale_fill_manual(name = "sumGain",
                      values = c("#f4f5ff", "#ccccff", "#7f7fff", "#3232ff"),
                      drop = FALSE,
                      breaks = levels(x$breaks),
                      labels = c("very low", "low", "medium", "high")) +
    coord_equal()

}
