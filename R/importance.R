#' Importance of variables and interactions in the model
#'
#' This functions calculates a table with selected measures of importance
#' for variables and interactions.
#'
#' Available measures:
#'\itemize{
#'\item "sumGain" - sum of Gain value in all nodes, in which given variable occurs,
#'\item "sumCover" - sum of Cover value in all nodes, in which given variable occurs; for LightGBM models: number of observation, which pass through the node,
#'\item "mean5Gain" - mean gain from 5 occurrences of given variable with the highest gain,
#'\item "meanGain" - mean Gain value in all nodes, in which given variable occurs,
#'\item "meanCover" - mean Cover value in all nodes, in which given variable occurs; for LightGBM models: mean number of observation, which pass through the node,
#'\item "freqency" - number of occurrences in the nodes for given variable.
#'}
#'
#' Additionally for table with single variables:
#'\itemize{
#'\item "meanDepth"  - mean depth weighted by gain,
#'\item "numberOfRoots" - number of occurrences in the root,
#'\item "weightedRoot" - mean number of occurrences in the root, which is weighted by gain.
#'}
#'
#' @param xgb_model a xgboost or lightgbm model.
#' @param data a data table with data used to train the model.
#' @param option if "variables" then table includes only single variables,
#'            if "interactions", then only interactions
#'            if "both", then both single variable and interactions.
#'            Default "both".
#' @param digits number of significant digits that shall be returned. Will be passed to the signif() functions.
#'
#' @return a data table
#'
#' @import data.table
#' @importFrom stats frequency
#' @importFrom stats weighted.mean
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
#' imp <- importance(xgb_model, sm, option = "both")
#' imp
#' plot(imp,  top = 10)
#'
#' imp <- importance(xgb_model, sm, option = "variables")
#' imp
#' plot(imp,  top = nrow(imp))
#'
#'  imp <- importance(xgb_model, sm, option = "interactions")
#'  imp
#' plot(imp,  top =  nrow(imp))
#'
#'  imp <- importance(xgb_model, sm, option = "variables")
#'  imp
#' plot(imp, top = NULL, radar = FALSE, xmeasure = "sumCover", ymeasure = "sumGain")
#'
#'\donttest{
#'library(lightgbm)
#'train_data <- lgb.Dataset(sm, label =  HR_data[, left] == 1)
#'params <- list(objective = "binary", max_depth = 2)
#'lgb_model <- lgb.train(params, train_data, 25)
#'
#' imp <- importance(lgb_model, sm, option = "both")
#' imp
#' plot(imp,  top = nrow(imp))
#'
#' imp <- importance(lgb_model, sm, option = "variables")
#' imp
#' plot(imp, top = NULL, radar = FALSE, xmeasure = "sumCover", ymeasure = "sumGain")
#'
#'}
#'
#' @export

importance <- function(xgb_model, data, option = "both", digits = 4){
  importance <- NULL

  if (option == "both") {
    importance <- importanceTableMixed(xgb_model, data)
  }
  if (option == "variables") {
    importance <- importanceSingleVariable(xgb_model, data)
  }
  if (option == "interactions") {
    importance <- importanceInteraction(xgb_model, data)
  }

  importance <- cbind(importance[, 1], signif(importance[, -1], digits = digits))
  #importance <- unlist(importance)

  class(importance) <- c("importance", "data.table")

  return(importance[])

}

importanceTableMixed <- function(xgb_model, data){
  parentsGain <- childsGain <- name_pair <- Cover <- Feature <-
    Gain <- indx <- . <- Quality <- NULL

  trees <- noLeavesGainTable(xgb_model, data)

  #single variables
  importanceSingle <-
    trees[(interaction == FALSE) | (is.na(interaction)), .(Feature, Gain = Quality, Cover)]

  #interactions
  interactions <- trees[interaction == TRUE]
  importanceInter <- interactions[, .(Feature = name_pair, Gain = childsGain, Cover)]
  importance <- rbind(importanceSingle, importanceInter)

  importance4 <-
    merge(importance[, .(sumGain = sum(Gain),
                         sumCover = sum(Cover),
                         meanGain = mean(Gain),
                         meanCover = mean(Cover),
                         frequency = .N), by = Feature],
          mean5gain(importance), by = "Feature")

  setorderv(importance4, "sumGain", -1)

  return(importance4[])

}


importanceInteraction <- function(xgb_model, data) {
  parentsGain <- childsGain <- name_pair <- Cover <- . <- Feature <- Gain <- indx <- NULL

  trees <- noLeavesGainTable(xgb_model, data)
  trees <- trees[interaction == TRUE]
  tress <- trees[, `:=`(Feature = name_pair, Gain = childsGain)]
  tress <- trees[, .(Feature, Gain, Cover)]
  importance <- merge(trees[, .(sumGain = sum(Gain),
                                sumCover = sum(Cover),
                                meanGain = mean(Gain),
                                meanCover = mean(Cover),
                                frequency = .N), by = Feature],
                      mean5gain(trees), by = "Feature")

  setorderv(importance, "sumGain", -1)

  return(importance[])
}


importanceSingleVariable <- function(xgb_model, data) {
  Feature <- Gain <- Quality <- Cover <- indx <- . <- NULL

  trees <- noLeavesGainTable(xgb_model, data)
  trees[, Gain := Quality]

  importance1 <- merge(countRoots(trees),calculateWeightedDepth(trees), by = "Feature", all = TRUE)[, -"sumGain"]

  trees <- trees[, .(Feature, Gain, Cover)]

  importance2 <- merge(trees[,.(sumGain=sum(Gain),
                                sumCover=sum(Cover),
                                meanGain=mean(Gain),
                                meanCover=mean(Cover),
                                frequency=.N),,by=Feature],
                       mean5gain(trees), by="Feature")
  importance <- merge(importance1, importance2, by = "Feature")[, -"count"]

  setorderv(importance, "sumGain", -1)
  importance[is.na(importance)] <- 0

  return(importance[])

}

#Table with number of roots and weighedRoot
#counts how many times each variable is in the root of the tree and calculates the weighedRoot-number of occurrences in root weighed by Gain.
countRoots <- function(trees) {
  Node <- Quality <- Feature <- sumGain <- . <-
    weightedRoot <- numberOfRoots <- NULL

  roots <- trees[Node == 0, ]
  roots <- roots[, .(sumGain = sum(Quality), numberOfRoots = .N), by = Feature]
  sumGains <- sum(roots[, sumGain])
  roots <- roots[, weightedRoot := round(roots[, sumGain] * roots[, numberOfRoots] / sumGains, 4)]

  return(roots[])

}

#Mean form 5 nodes with the highests gain
mean5gain <- function(trees) {
  indx <- Gain <- . <- Feature <- NULL

  setorder(setDT(trees), Feature,-Gain)[, indx := seq_len(.N), by = Feature]
  importanceTop <- trees[indx <= 5]
  importance <- importanceTop[, .(mean5Gain = mean(Gain)), by = Feature]

  return(importance[])
}

#calculates depth mean for every variable weighted by Gain
calculateWeightedDepth <- function(trees) {
  Feature <- depth <- Quality <- . <- NULL

  trees <- trees[, .(meanDepth = weighted.mean(depth, Quality), count = .N), by = Feature]

  return(trees[])
}


noLeavesGainTable <- function(xgb_model, data) {
  parentsName <- Feature <- Tree <- name_pair <- parentsGain <- childsGain <-
    . <- Cover <- parentsCover <- interaction <- Node <- Quality <- depth <- NULL

  treeList <- calculateGain(xgb_model, data)
  trees <- rbindlist(treeList)
  trees <- trees[Feature != "Leaf", .(Tree, Node, name_pair, parentsGain, childsGain, Cover,
                                      parentsCover, Feature, Quality, parentsName, interaction, depth)]

  return(trees[])
}
