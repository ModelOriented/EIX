#'waterfallPlot
#'
#'waterfallPlot
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param new_observation a new observation
#' @param opt "interactions", "single"
#'
#' @return a ggplot object
#'
#' @examples
#'
#' @export


waterfallPlot<-function(xgb.model, new_observation, opt="interactions"){

  br<-broken(xgb.model, new_observation, opt)
  plot(br)

}
