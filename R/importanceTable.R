#' Importance table
#'
#' Importance table
#'
#' Available measures:
#'\itemize{
#'\item "sumGain" - sum of Gain value in all nodes, in which given variable occurs
#'\item "sumCover" - sum of Cover value in all nodes, in which given variable occurs; for LightGBM models: number of observation, which pass through the node
#'\item "mean5Gain" - mean gain from 5 occurrences of given variable with the highest gain
#'\item "meanGain" - mean Gain value in all nodes, in which given variable occurs
#'\item "meanCover" - mean Cover value in all nodes, in which given variable occurs; for LightGBM models: mean number of observation, which pass through the node
#'\item "freqency" - number of occurrences in the nodes for given variable
#'}
#'
#' Additionally for table with single variables:
#'\itemize{
#'\item "meanDepth"  - mean depth weighted by gain
#'\item "numberOfRoots" - number of occurrences in the root
#'\item "weightedRoot" - mean number of occurrences in the root, which is weighted by gain
#'}
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#' @param option if "variables" then table includes only single variable,
#'            if "interactions", then only interactions
#'            if "both", then both single variable and interactons.
#'            Default "both".
#'
#' @return a data table
#' @import data.table
#'
#' @examples
#'
#' @export



importanceTable <- function(xgb.model, data, option = "both"){
  importance <- NULL

  if (option == "both") {
    importance <- importanceTableMixed(xgb.model, data)
  }
  if (option == "variables") {
    importance <- importanceSingleVariable(xgb.model, data)
  }
  if (option == "interactions") {
    importance <- importanceInteraction(xgb.model, data)
  }

  importance <- cbind(importance[, 1], signif(importance[, -1], digits = 4))

  class(importance) <- c("importanceTable", "data.table")

  return(importance[])

}

importanceTableMixed <- function(xgb.model, data){
  parentsGain <- childsGain <- name_pair <- Cover <- Feature <- Gain <- indx <- . <- NULL

  trees <- noLeavesGainTable(xgb.model, data)

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


importanceInteraction <- function(xgb.model, data) {
  parentsGain <- childsGain <- name_pair <- Cover <- . <- Feature <- Gain <- indx <- NULL

  trees <- noLeavesGainTable(xgb.model, data)
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


importanceSingleVariable <- function(xgb.model, data) {
  Feature <- Gain <- Quality <- Cover <- indx <- . <- NULL

  trees <- noLeavesGainTable(xgb.model, data)
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
  Node <- Quality <- Feature <- sumGain <- . <- weightedRoot <- NULL

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


noLeavesGainTable <- function(xgb.model, data) {
  parentsName <- Feature <- Tree <- name_pair <- parentsGain <- childsGain <- . <- Cover <- parentsCover <- interaction <- NULL

  treeList <- calculateGain(xgb.model, data)
  trees <- rbindlist(treeList)
  trees <- trees[Feature != "Leaf", .(Tree, Node, name_pair, parentsGain, childsGain, Cover,
                                      parentsCover, Feature, Quality, parentsName, interaction, depth)]

  return(trees[])
}
