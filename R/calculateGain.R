#' calculateGain
#'
#' List of trees with pairs of variable and other needed fields
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#'
#' @return a list
#'
#' @import data.table
#' @import stats
#' @import utils
#' @import xgboost
#'

calculateGain <- function(xgb.model, data) {

  leaf <- Feature <- Yes <- No <- ID <- parentsGain <- Quality <- parentsCover <-
    Cover <- name_pair <- childsGain <- depth <- parentsName <- NULL

  trees = tableOfTrees(model = xgb.model, data)
  trees[, leaf := Feature == "Leaf"]
  trees$depth <- 0
  treeList = split(trees, as.factor(trees$Tree))

  for (tree in treeList) {
    num_nodes = nrow(tree)
    non_leaf_rows = rev(which(tree[, leaf] == F))
    for (r in non_leaf_rows) {
      left = tree[r, Yes]
      right = tree[r, No]
      if (tree[ID == left, leaf] == F) {

        newDepth <- tree[r , depth] + 1
        tree[ID == left,`:=`(parentsGain = tree[r, Quality],
                             parentsCover = tree[r, Cover],
                             name_pair = paste(tree[r, Feature], tree[ID == left, Feature], sep = ":"),
                             childsGain = Quality,
                             depth = newDepth,
                             parentsName = tree[r, Feature])]
        tree[ID == left, interaction := ((parentsGain < childsGain) & (Feature != parentsName))]
      }

      if (tree[ID == right, leaf]==F) {

        newDepth <- tree[r , depth] + 1
        tree[ID == right, `:=`(parentsGain = tree[r, Quality],
                               parentsCover = tree[r, Cover],
                               name_pair = paste(tree[r, Feature], tree[ID == right, Feature], sep = ":"),
                               childsGain = Quality,
                               depth = newDepth,
                               parentsName = tree[r, Feature])]
        tree[ID == right, interaction := ((parentsGain < childsGain) & (Feature != parentsName))]
      }
    }
  }

  return(treeList)
}
