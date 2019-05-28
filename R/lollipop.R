#' Tables needed for lollipop plot
#'
#' This function calculates two tables needed to generate lollipop plot, which visualise the model.
#' The first table contains information about all nodes in the trees forming a model.
#' It includes gain value, depth and ID of each nodes.
#' The second table contains similarly information about roots in the trees.
#'
#' @param xgb_model a xgboost or lightgbm model.
#' @param data a data table with data used to train the model.
#'
#' @return an object of the lollipop class
#'
#' @import data.table
#'
#' @examples
#' library("EIX")
#' library("Matrix")
#' sm <- sparse.model.matrix(left ~ . - 1,  data = HR_data)
#'
#' library("xgboost")
#' param <- list(objective = "binary:logistic", max_depth = 2)
#' xgb_model <- xgboost(sm, params = param, label = HR_data[, left] == 1, nrounds = 25, verbose = 0)
#'
#' lolli <- lollipop(xgb_model, sm)
#' plot(lolli, labels = "topAll", log_scale = TRUE)
#'
#'\donttest{
#'library(lightgbm)
#'train_data <- lgb.Dataset(sm, label =  HR_data[, left] == 1)
#'params <- list(objective = "binary", max_depth = 2)
#'lgb_model <- lgb.train(params, train_data, 25)
#'
#' lolli <- lollipop(lgb_model, sm)
#' plot(lolli, labels = "topAll", log_scale = TRUE)
#'
#'}
#'
#' @export

lollipop <- function(xgb_model, data){

  Feature <- Quality <- Node <- Tree <- ID <- depth <-
    interaction <- . <- parentsName <- name_pair <- NULL

  trees = rbindlist(calculateGain(xgb_model, data))
  roots <- trees[Node == 0, .(Quality, Feature, Tree, ID, depth)]
  nodes <- trees[Feature != "Leaf", .(Quality,
                                      Feature,
                                      Node,
                                      Tree,
                                      ID,
                                      interaction,
                                      depth,
                                      parentsName,
                                      name_pair)]
  nodes <- nodes[interaction == TRUE, Feature := name_pair]

  lollipop <- list(nodes, roots)
  class(lollipop) <- c("lollipop", "list")

  return(lollipop)
}
