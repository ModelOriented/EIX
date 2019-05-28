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
#' @param x a result from the \code{importance} function.
#' @param top number of positions on the plot or NULL for all variable. Default 10.
#' @param radar TRUE/FALSE. If TRUE the plot shows
#'               six measures of variables' or interactions' importance in the model.
#'               If FALSE the plot containing two chosen measures
#'               of variables' or interactions' importance in the model.
#' @param text_start_point place, where the names of the particular feature start. Available for `radar=TRUE`. Range from 0 to 1. Default 0.5.
#' @param text_size size of the text on the plot. Default 3.5.
#' @param xmeasure measure on the x-axis.Available for `radar=FALSE`. Default "sumCover".
#' @param ymeasure measure on the y-axis. Available for `radar=FALSE`. Default "sumGain".
#' @param ... other parameters.
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @import data.table
#' @importFrom DALEX theme_drwhy
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggiraphExtra coord_radar
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
#' imp <- importance(xgb_model, sm, option = "both")
#' imp
#' plot(imp,  top = 10)
#'
#' imp <- importance(xgb_model, sm, option = "variables")
#' imp
#' plot(imp,  top = nrow(imp))
#'
#'  imp <- importance(xgb_model, sm, option = "interactions")
#'  imp
#' plot(imp,  top =  nrow(imp))
#'
#'  imp <- importance(xgb_model, sm, option = "variables")
#'  imp
#' plot(imp, top = NULL, radar = FALSE, xmeasure = "sumCover", ymeasure = "sumGain")
#'
#'\donttest{
#'library(lightgbm)
#'train_data <- lgb.Dataset(sm, label =  HR_data[, left] == 1)
#'params <- list(objective = "binary", max_depth = 2)
#'lgb_model <- lgb.train(params, train_data, 25)
#'
#' imp <- importance(lgb_model, sm, option = "both")
#' imp
#' plot(imp,  top = nrow(imp))
#'
#' imp <- importance(lgb_model, sm, option = "variables")
#' imp
#' plot(imp, top = NULL, radar = FALSE, xmeasure = "sumCover", ymeasure = "sumGain")
#'
#'}
#'
#' @export


plot.importance <- function(x, ...,  top = 10, radar = TRUE, text_start_point = 0.5, text_size=3.5,
                                 xmeasure = "sumCover", ymeasure = "sumGain"){

  Feature <- sumGain <- sumCover <- meanGain <- meanCover <-
    mean5Gain <- . <- value <- variable <- hjust <- NULL

  if (is.null(top))
    top <- nrow(x)


  if (radar == FALSE) {
    ggplot(data.frame(x[1:top, ]),
           aes_string(x = xmeasure, y = ymeasure, label = "Feature")) +
      geom_point() +
      scale_size() + geom_label_repel() + theme_drwhy()

  }else{
    import <- as.data.table(x[1:top, ])
    import <- import[1:top, .(Feature,
                              sumGain = sumGain / max(import[, sumGain]),
                              sumCover = sumCover / max(import[, sumCover]),
                              meanGain = meanGain / max(import[, meanGain]),
                              meanCover = meanCover / max(import[, meanCover]),
                              mean5Gain = mean5Gain / max(import[, mean5Gain]),
                              frequency = frequency / max(import[, frequency]))]
    data<-import[,Feature:= ifelse(nchar(import[,Feature])>20, gsub(":", ": :",import[,Feature]),Feature)]

    import$Feature <- factor(import$Feature, levels = import$Feature[order(import$sumGain, decreasing = TRUE)])

    #angles and hjust of labels
    numberOfBars=nrow(import)
    angle= 90-360*(row(import)[,1]-0.5)/numberOfBars

    import$hjust<-ifelse( angle < -90, 1, 0)
    import$angle<-ifelse(angle < -90, angle+180, angle)

    data_to_plot <- melt(import, id = c(1,8,9), measures = 2:6, value.factor = FALSE)
    data<-data_to_plot[,.(hjust=mean(hjust),angle=mean(angle)), by=Feature]

    ggplot(data.frame(data_to_plot),
           aes(x = Feature, y = value, colour = variable, group = variable)) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      theme_drwhy() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            panel.grid.major.y = element_line(colour = "gray68", linetype = "dashed", size = 0.4),
            axis.line = element_blank(),
            axis.text.x=element_blank(),) +
      labs(fill = "Measures")+
      coord_radar() +
      geom_text(data=data, aes(x=Feature, y= rep(text_start_point,top), label=lapply(strwrap(data[,Feature], width = 10, simplify = FALSE), paste, collapse="\n"), hjust=hjust), color="#371ea3", fontface="bold",alpha=0.6, size=text_size, angle= data$angle, inherit.aes = FALSE )

  }
}
