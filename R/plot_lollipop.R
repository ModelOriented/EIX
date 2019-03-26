#' Visualiation of the model
#'
#' The lollipop plots the model with the most important interactions and variables in the roots.
#'
#' @param x a result from the `lollipop` function.
#' @param labels if "topAll" then labels for the most important interactions (vertical label)
#'               and variables in the roots (horizontal label) will be displayed,
#'               if "interactions" then labels for all interactions,
#'               if "roots" then labels for all variables in the root.
#' @param log  TRUE/FALSE logarithmic scale on the plot. Default TRUE.
#' @param ... other parameters.
#'
#' @return a ggplot object
#'
#' @import data.table
#' @import ggplot2
#' @importFrom DALEX theme_mi2
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggrepel geom_text_repel
#'
#' @examples
#' library("EIX")
#' library("Matrix")
#' sm <- sparse.model.matrix(left ~ . - 1,  data = HR_data)
#'
#' library("xgboost")
#' param <- list(objective = "binary:logistic", max_depth = 2)
#' xgb_model <- xgboost(sm, params = param, label = HR_data[, left] == 1, nrounds = 50, verbose=0)
#'
#' lolli <- lollipop(xgb_model, sm)
#' plot(lolli, labels = "topAll", log_scale = TRUE)
#'
#' \dontrun{
#'library(lightgbm)
#'train_data <- lgb.Dataset(sm, label =  HR_data[, left] == 1)
#'params <- list(objective = "binary", max_depth = 3)
#'lgb_model <- lgb.train(params, train_data, 50)
#'
#' lolli <- lollipop(lgb_model, sm)
#' plot(lolli, labels = "topAll", log_scale = TRUE)
#'}
#'
#' @export

plot.lollipop<-function(x, ..., labels = "topAll", log = TRUE){

  Tree <- Quality <- depth <- Feature <- NULL
  nodes <- x[[1]]
  roots <- x[[2]]

  p <- ggplot(data = data.frame(nodes), aes(x = Tree, y = Quality, group = as.factor(depth), label = Feature)) +
    geom_line(data = data.frame(roots), color = "red", size = 1.25, alpha = .5) +
    geom_segment(aes(x = Tree, xend = Tree, y = 0, yend = Quality), size = 1.25) +
    geom_point(aes(shape = as.factor(depth), color = as.factor(depth)), size = 3)

  p <- {
    switch(labels,
           topAll = {
             p + geom_text_repel(data = data.frame(nodes),
                                 aes(label = ifelse((interaction == TRUE) & (Quality > 0.1 * (max(nodes[, Quality]))), Feature, '')),
                                 angle = 90, nudge_y = 0.05, direction  = "x", vjust = 0, segment.size = 0.2) +
                 geom_label_repel(data = data.frame(roots),
                                aes(label = ifelse(Quality > 0.1 * (max(nodes[, Quality] )), Feature, '')))
           },
           interactions = {
             p + geom_text_repel(data = data.frame(nodes),
                                                 aes(label = ifelse(( interaction == TRUE ), Feature, '')),
                                                 angle = 90, nudge_y = 0.05, direction  = "x", vjust = 0, segment.size = 0.2 )
           },
           roots = {
             p + geom_label_repel(data = data.frame(roots), aes(label = Feature))
           })}

  p <- p + theme_mi2() + ylab("Gain") +
    scale_shape_discrete("Depth") +
    scale_colour_discrete("Depth") + if (log) {scale_x_continuous(trans = 'log10')}

  p
}

