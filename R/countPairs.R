#' Table of occurancess number
#'
#' Table containing occurancess number of variables' pairs in the model.
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#'
#' @return a data table
#'
#' @import data.table
#' @import stats
#' @import utils
#' @import tidyr
#' @import purrr
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
#' countPairs(xgb.model, sm)
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



