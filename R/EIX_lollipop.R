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
#'
#' @export

EIX_lollipop <- function(xgb.model, data){

  Feature <- Quality <- Node <- Tree <- ID <- depth <- interaction <- . <- NULL

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
