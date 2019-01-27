#' Lollipop table
#'
#' The table needed to generate lollipop plot, which visualise the model.
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#'
#' @return a data table
#'
#' @import data.table
#' @import ggplot2
#' @import DALEX
#' @import ggrepel
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
#' lolli <- EIX_lollipop(xgb.model, sm)
#' plot(lolli, labels = "topAll", log_scale = TRUE)
#'
#' @export

EIX_lollipop <- function(xgb.model, data){

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
