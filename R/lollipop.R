#' Tables needed for lollipop plot
#'
#' This function calculates two tables needed to generate lollipop plot, which visualise the model.
#' The first table contains information about all nodes in the trees forming a model.
#' It includes gain value, depth and ID of each nodes.
#' The second table contains similarly information about roots in the trees.
#'
#' @param xgb.model a xgboost or lightgbm model.
#' @param data a data table with data used to train the model.
#'
#' @return an object of the lollipop class
#'
#' @import data.table
#' @import ggplot2
#' @import DALEX
#' @import ggrepel
#'
#' @examples
#' library("EIX")
#' library("Matrix")
#' sm <- sparse.model.matrix(left ~ . - 1,  data = HR_data)
#'
#' library("xgboost")
#' param <- list(objective = "binary:logistic", max_depth = 2)
#' xgb.model <- xgboost(sm, params = param, label = HR_data[, left] == 1, nrounds = 50, verbose=0)
#'
#' lolli <- lollipop(xgb.model, sm)
#' plot(lolli, labels = "topAll", log_scale = TRUE)
#'
#' @export

lollipop <- function(xgb.model, data){

  Feature <- Quality <- Node <- Tree <- ID <- depth <-
    interaction <- . <- parentsName <- name_pair <- NULL

  trees = rbindlist(calculateGain(xgb.model, data))
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
