#'
#'interactionsTable
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#' @param option "interactions", "pairs"
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
#'
#' @export
#'
#'
interactionsTable <- function(xgb.model, data, option = "interactions"){


  Child <- Parent <- NULL

  if (option == "interactions") {
    gainTable <- importanceInteraction(xgb.model, data)[, Feature, sumGain]
    gainTable <-gainTable[, `:=`(Parent = as.vector(unlist(map(strsplit(gainTable[, Feature], "[:]"), 1))),
                                 Child = as.vector(unlist(map(strsplit(gainTable[, Feature], "[:]"), 2))))]
    gainTable <- gainTable[, -2]
  }
  if (option == "pairs") {
    gainTable <- calculatePairsGainTable(xgb.model, data)
  }

  class(gainTable) <- c("interactionsTable", "data.table")
  return(gainTable)

}

#calculatePairsGainTable containing gains of all variables' pairs occur in the model.
calculatePairsGainTable <- function(xgb.model, data) {
  name_pair <- childsGain <- . <- NULL

  treeList <- calculateGain(xgb.model, data)
  trees <- rbindlist(treeList)

  importance <- trees[, .(sumGain = sum(childsGain)), by = name_pair]
  importance <- na.omit(importance)
  importance <-
  importance[, `:=`(Parent = as.vector(unlist(map(strsplit(importance[, name_pair], "[:]"), 1))),
                      Child = as.vector(unlist(map(strsplit(importance[, name_pair], "[:]"), 2 ))))]
  importance <- importance[, -1]
  setorderv(importance, "sumGain", -1)

  return(importance[])

}
