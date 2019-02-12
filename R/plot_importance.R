#' Plot importance measures
#'
#' This functions plots selected measures of importance for variables and interactions.
#' It is possible to visualise importance table in two ways: radar plot with six measures
#' and scatter plot with two choosen measures.
#'
#' Available measures:
#'\itemize{
#'\item "sumGain" - sum of Gain value in all nodes, in which given variable occurs,
#'\item "sumCover" - sum of Cover value in all nodes, in which given variable occurs; for LightGBM models: number of observation, which pass through the node,
#'\item "mean5Gain" - mean gain from 5 occurrences of given variable with the highest gain,
#'\item "meanGain" - mean Gain value in all nodes, in which given variable occurs,
#'\item "meanCover" - mean Cover value in all nodes, in which given variable occurs; for LightGBM models: mean number of observation, which pass through the node,
#'\item "freqency" - number of occurrences in the nodes for given variable.
#'}
#'
#' Additionally for plots with single variables:
#'\itemize{
#'\item "meanDepth"  - mean depth weighted by gain,
#'\item "numberOfRoots" - number of occurrences in the root,
#'\item "weightedRoot" - mean number of occurrences in the root, which is weighted by gain.
#'}
#'
#' @param x a result from the `importance` function.
#' @param top number of positions on the plot or NULL for all variable. Default 10.
#' @param radar TRUE/FALSE. If TRUE the plot shows
#'               six measures of variables' or interactions' importance in the model.
#'               If FALSE the plot containing two chosen measures
#'               of variables' or interactions' importance in the model.
#' @param xmeasure measure on the x-axis.Available for `radar=FALSE`. Default "sumCover".
#' @param ymeasure measure on the y-axis. Available for `radar=FALSE`. Default "sumGain".
#' @param ... other parameters.
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @import data.table
#' @import DALEX
#' @import ggrepel
#' @import ggiraphExtra
#'
#' @examples
#' library("EIX")
#' library("Matrix")
#' sm <- sparse.model.matrix(left ~ . - 1,  data = HR_data)
#'
#' library("xgboost")
#' param <- list(objective = "binary:logistic", max_depth = 2)
#' xgb.model <- xgboost(sm, params = param, label = HR_data[, left] == 1, nrounds = 50, verbose=0)
#'
#' imp <- importance(xgb.model, sm, option = "both")
#' imp
#' plot(imp,  top = 10)
#'
#' imp <- importance(xgb.model, sm, option = "variables")
#' imp
#' plot(imp,  top = nrow(imp))
#'
#'  imp <- importance(xgb.model, sm, option = "interactions")
#'  imp
#'  plot(imp,  top = 10)
#'
#'  imp <- importance(xgb.model, sm, option = "variables")
#'  imp
#'  plot(imp, top = nrow(imp), radar = FALSE, xmeasure = "sumCover", ymeasure = "sumGain")
#'
#' @export


plot.importance <- function(x, ...,  top = 10, radar = TRUE,
                                 xmeasure = "sumCover", ymeasure = "sumGain"){

  Feature <- sumGain <- sumCover <- meanGain <- meanCover <-
    mean5Gain <- . <- value <- variable <- NULL

  if (top == "NULL")
    top <- nrow(x)


  if (radar == FALSE) {
    ggplot(data.frame(x[1:top, ]),
           aes_string(x = xmeasure, y = ymeasure, label = "Feature")) +
      geom_point() +
      scale_size() + geom_label_repel() + theme_mi2()

  }else{
    import <- as.data.table(x[1:top, ])
    import <- import[1:top, .(Feature,
                              sumGain = sumGain / max(import[, sumGain]),
                              sumCover = sumCover / max(import[, sumCover]),
                              meanGain = meanGain / max(import[, meanGain]),
                              meanCover = meanCover / max(import[, meanCover]),
                              mean5Gain = mean5Gain / max(import[, mean5Gain]),
                              frequency = frequency / max(import[, frequency]))]

    import$Feature <- factor(import$Feature, levels = import$Feature[order(import$sumGain, decreasing = TRUE)])
    data_to_plot <- melt(import, id = 1, measures = 2:6)

    ggplot(data.frame(data_to_plot),
           aes(x = Feature, y = value, colour = variable, group = variable)) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      theme_mi2() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #axis.text.x = element_text(angle=45),
            legend.position = "bottom",
            #axis.text.y = element_blank(),
            panel.grid.major.y = element_line(colour = "gray68", linetype = "dashed", size = 0.4),
            axis.line = element_blank(),
            plot.margin = margin(40, 40, 40, 40)) +
      labs(fill = "Measures") +
      coord_radar()
  }
}
