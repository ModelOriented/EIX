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
#' library("EIX")
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
#'
#'

interactionsTable <- function(xgb.model, data, option = "interactions"){
  Child <- Parent <- Feature <- sumGain <- NULL

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
