#'Importance table
#'
#'Function \code{importaceTable} returns table with importance of single variable and interactions with the following measure:
#'\itemize{
#'\item sumGain
#'\item sumCover
#'\item mean5Gain
#'\item meanGain
#'\item meanCover
#'\item freqency
#'}
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param opt "single", "interactions","mixed". Default "mixed"
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import data.table
#'
#'
#'
#' @export



importanceTable<-function(xgb.model, data,opt="mixed", trees = NULL){

  importance<-NULL

  if(opt=="mixed") {importance<- importanceTableMixed(xgb.model, data, trees)}
  if(opt=="single") {importance<-importanceSingleVariable(xgb.model, data, trees)}
  if(opt=="interactions") {importance<-importanceInteractions(xgb.model, data, trees)}
return(importance)

}
