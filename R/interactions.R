#' Importance of interactions and pairs in the model
#'
#' This function calculates a table with two measures of importance for interactions and pairs in the model.
#'
#' Available measures:
#'\itemize{
#'\item "sumGain" - sum of Gain value in all nodes, in which given variable occurs,
#'\item "freqency" - number of occurrences in the nodes for given variable.
#'}
#'
#' NOTE: Be careful use of this function with \code{option="pairs"} parameter,
#'       because high gain of pair can be a result of high gain of child variable.
#'      As strong interactions should be considered only these pairs of variables,
#'      where variable on the bottom (child) has higher gain than variable on the top (parent).
#'
#' @param xgb_model a xgboost or lightgbm model.
#' @param data a data table with data used to train the model.
#' @param option if "interactions", the table contains interactions,
#'            if "pairs", this table contains all the pairs in the model.
#'            Default "interactions".
#'
#' @return a data table
#'
#' @import data.table
#' @importFrom purrr map
#' @importFrom stats frequency
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
#' inter <- interactions(xgb_model, sm, option = "interactions")
#' inter
#' plot(inter)
#'
#' inter <- interactions(xgb_model, sm, option = "pairs")
#' inter
#' plot(inter)
#'
#' \donttest{
#'library(lightgbm)
#'train_data <- lgb.Dataset(sm, label =  HR_data[, left] == 1)
#'params <- list(objective = "binary", max_depth = 2)
#'lgb_model <- lgb.train(params, train_data, 25)
#'
#' inter <- interactions(lgb_model, sm, option = "interactions")
#' inter
#' plot(inter)
#'
#' inter <- interactions(lgb_model, sm, option = "pairs")
#' inter
#' plot(inter)
#'}
#'
#' @export
#'
#'

interactions <- function(xgb_model, data, option = "interactions"){
  Child <- Parent <- Feature <- sumGain <- . <- NULL

  if (option == "interactions") {
    gainTable <- importanceInteraction(xgb_model, data)[, .(Feature, sumGain, frequency)]
    gainTable <-gainTable[, `:=`(Parent = as.vector(unlist(map(strsplit(gainTable[, Feature], "[:]"), 1))),
                                 Child = as.vector(unlist(map(strsplit(gainTable[, Feature], "[:]"), 2))))]
    gainTable <- gainTable[, -1]
    gainTable <- gainTable[,.(Parent, Child, sumGain, frequency)]
  }
  if (option == "pairs") {
    gainTable <- calculatePairsGainTable(xgb_model, data)
  }
  class(gainTable) <- c("interactions", "data.table")
  return(gainTable)

}

#calculatePairsGainTable containing gains of all variables' pairs occur in the model.
calculatePairsGainTable <- function(xgb_model, data) {
  name_pair <- childsGain <- Parent <- Child <- sumGain <- N <- . <- NULL

  treeList <- calculateGain(xgb_model, data)
  trees <- rbindlist(treeList)

  importanceCount <- data.table(table(trees[, "name_pair"],dnn = "name_pair"))
  importanceGain <- trees[, .(sumGain = sum(childsGain)), by = "name_pair"]
  importance <- merge(importanceCount, importanceGain, by = "name_pair")
  importance <-
  importance[, `:=`(Parent = as.vector(unlist(map(strsplit(importance[, name_pair], "[:]"), 1))),
                     Child = as.vector(unlist(map(strsplit(importance[, name_pair], "[:]"), 2 ))))]
  importance <- importance[, -1]
  setorderv(importance, "sumGain", -1)

  return(importance[,.(Parent, Child, sumGain, frequency = N)])
}
