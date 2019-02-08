#' Importance of interactions and pairs in the model
#'
#' The table with two measures of importance for interactions and pairs in the model.
#'
#' Available measures:
#'\itemize{
#'\item "sumGain" - sum of Gain value in all nodes, in which given variable occurs
#'\item "freqency" - number of occurrences in the nodes for given variable
#'}
#'
#' NOTE: High gain of pair for \code{option="pairs"} can be a result of high gain of down variable (child).
#'      As strong interactions should be considered only these pairs of variables,
#'      where variable on the bottom (child) has higher gain than variable on the top (parent).
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#' @param option if "interactions", the table contains interactions,
#'            if "pairs", this table contains all the pairs in the model.
#'            Default "interactions".
#'
#' @return a data table
#'
#' @import stats
#' @import utils
#' @import data.table
#' @import purrr
#' @import tidyr
#'
#' @examples
#' library("EIX")
#' library("Matrix")
#' library("data.table")
#' library("xgboost")
#'
#' dt_HR <- data.table(HR_data)
#' sm <- sparse.model.matrix(left ~ . - 1,  data = dt_HR)
#'
#' param <- list(objective = "binary:logistic", base_score = 0.5, max_depth = 2)
#' xgb.model <- xgboost(sm, params = param, label = dt_HR[, left] == 1, nrounds = 50, verbose = FALSE)
#'
#' inter <- interactionsTable(xgb.model, sm, option = "interactions")
#' inter
#' plot(inter)
#'
#' inter <- interactionsTable(xgb.model, sm, option = "pairs")
#' inter
#' plot(inter)
#'
#' @export
#'
#'

interactionsTable <- function(xgb.model, data, option = "interactions"){
  Child <- Parent <- Feature <- sumGain <- . <- NULL

  if (option == "interactions") {
    gainTable <- importanceInteraction(xgb.model, data)[, .(Feature, sumGain, frequency)]
    gainTable <-gainTable[, `:=`(Parent = as.vector(unlist(map(strsplit(gainTable[, Feature], "[:]"), 1))),
                                 Child = as.vector(unlist(map(strsplit(gainTable[, Feature], "[:]"), 2))))]
    gainTable <- gainTable[, -1]
    gainTable <- gainTable[,.(Parent, Child, sumGain, frequency)]
  }
  if (option == "pairs") {
    gainTable <- calculatePairsGainTable(xgb.model, data)
  }
  class(gainTable) <- c("interactionsTable", "data.table")
  return(gainTable)

}

#calculatePairsGainTable containing gains of all variables' pairs occur in the model.
calculatePairsGainTable <- function(xgb.model, data) {
  name_pair <- childsGain <- Parent <- Child <- sumGain <- N <- . <- NULL

  treeList <- calculateGain(xgb.model, data)
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
