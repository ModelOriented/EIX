#' Table of occurancess number
#'
#' Table containing occurancess number of variables' pairs in the model.
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#'
#' @return
#'
#' @import data.table
#' @import stats
#' @import utils
#' @import tidyr
#' @import purrr
#'
#' @examples
#'
#' @export

countPairs <- function(xgb.model, data) {

  V1 <- down <- N <- NULL

  treeList <- calculateGain(xgb.model, data)
  trees <- rbindlist(treeList)

  importance <- data.table(table(trees[, "name_pair"]))
  importance <- na.omit(importance)
  importance <- importance[, `:=`(up = as.vector(unlist(map(strsplit(as.character(importance[, V1]), "[:]"), 1))),
                                  down = as.vector(unlist(map(strsplit(importance[, V1], "[:]"), 2))))]
  importance <- importance[, -1]
  importance <- spread(importance, down, N)
  importance[is.na(importance)] <- '.'

  return(importance[])

}



