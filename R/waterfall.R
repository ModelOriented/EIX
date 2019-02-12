#' Explain prediction of a single observation
#'
#' This function calculates a table with influence of variables and interactions
#' on the prediction of a given observation.
#'
#' Function contains code or pieces of code
#' from \code{breakDown} code created by Przemysław Biecek
#' and \code{xgboostExplainer} code created by David Foster.
#'
#' @param xgb.model a xgboost model.
#' @param new_observation a new observation.
#' @param option  if "variables", the plot includes only single variables,
#'            if "interactions", then only interactions.
#'            Default "interaction".
#' @param baseline a number or a character "Intercept" (for model intercept).
#'                The baseline for the plot, where the rectangles should start.
#'                Default 0.
#'
#' @return an object of the broken class
#'
#' @import data.table
#' @import xgboost
#' @import breakDown
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
#' new_observation <- sm[9,]
#' wf <- waterfall(xgb.model, new_observation,  option = "interactions")
#' wf
#' plot(wf)
#'
#' @export

waterfall <- function(xgb.model, new_observation, option = "interactions", baseline = 0){
  #uses pieces of breakDown code created by Przemysław Biecek
  Feature <- NULL

  col_names <- colnames(new_observation)
  trees = xgb.model.dt.tree(col_names, model = xgb.model)

  if (option == "interactions") {
    tree_list = getStatsForTreesInter(trees, type = "binary", base_score = .5)
  }
  if (option == "variables") {
    tree_list = getStatsForTrees(trees, type = "binary", base_score = .5)
  }

  explainer = buildExplainerFromTreeList(tree_list,names(table(rbindlist(tree_list)[,Feature])))
  new_observation_DM <- slice(xgb.DMatrix(t(new_observation)), as.integer(1))

  breakdown = explainPredictions(xgb.model, explainer, new_observation_DM)
  df<-data.frame(
    variable = colnames(breakdown), #,"=", sapply(new_observation_data[colnames(breakdown)], as.character)),
    contribution = as.numeric(breakdown),
    variable_name = colnames(breakdown),
    variable_value = NA#sapply(new_observation_data[colnames(breakdown)], as.character)
  )[as.numeric(breakdown) != 0, ]
  df <- df[order(abs(df[, 2]), decreasing = TRUE), ]
  df <- as.data.frame(df)

  df_intercept <- df[which(df[, 3] == "intercept"), ]
  df <- df[-which(df[, 3] == "intercept"), ]

  broken_sorted <- rbind(df_intercept, df)
  breakDown:::create.broken(broken_sorted, baseline)
}

getStatsForTreesInter = function(trees,type = "binary",  base_score = 0.5) {
 #code uses xgboostExplainer code created by David Foster
  leaf <- Feature <- H <- Cover <- Yes <- No <- ID <-
  weight <- Quality <- previous_weight <- G <- parentsCover <-
  name_pair <- childsGain <- uplift_weight <- parentsGain <-
  parentsName <- NULL

  treeList = copy(trees)
  treeList[, leaf := Feature == 'Leaf']
  treeList[, H := Cover]

  non.leaves = which(treeList[, leaf] == F)



  j = 0
  for (i in rev(non.leaves)) {
    left = treeList[i, Yes]
    right = treeList[i, No]
    treeList[i, H := treeList[ID == left, H] + treeList[ID == right, H]]
    j = j + 1
  }


  if (type == 'regression') {
    base_weight = base_score
  } else{
    base_weight = log(base_score / (1 - base_score))
  }

  treeList[leaf == T, weight := base_weight + Quality]
  treeList[, previous_weight := base_weight]
  treeList[1, previous_weight := 0]
  treeList[leaf == T, G := -weight * H]
  treeList = split(treeList, as.factor(treeList$Tree))

  num_treeList = length(treeList)
  treenums =  as.character(0:(num_treeList - 1))

  for (tree in treeList) {
    num_nodes = nrow(tree)
    non_leaf_rows = rev(which(tree[, leaf] == F))
    for (r in non_leaf_rows) {
      left = tree[r, Yes]
      right = tree[r, No]
      leftG = tree[ID == left, G]
      rightG = tree[ID == right, G]

      tree[r, G := leftG + rightG]
      w = tree[r, -G / H]

      tree[r, weight := w]
      tree[ID == left, previous_weight := w]
      tree[ID == right, previous_weight := w]

      if (tree[ID == left, leaf] == F) {
        tree[ID == left, `:=`(parentsGain = tree[r, Quality], parentsName = tree[r, Feature])]
        tree[ID == left, parentsCover := tree[r, Cover]]
        name_pair4 = paste(tree[r, Feature], tree[ID == left, Feature], sep = ":")
        tree[ID == left, name_pair := name_pair4]
        tree[ID == left, childsGain := Quality]

      }
      if (tree[ID == right, leaf] == F) {
        tree[ID == right, `:=`(parentsGain = tree[r, Quality], parentsName = tree[r, Feature])]
        tree[ID == right, parentsCover := tree[r, Cover]]
        name_pair1 = paste(tree[r, Feature], tree[ID == right, Feature], sep = ":")
        tree[ID == right, name_pair := name_pair1]
        tree[ID == right, childsGain := Quality]
      }
    }

    tree[, uplift_weight := weight - previous_weight]
    tree[, interaction := ((parentsGain < childsGain) &
                             (Feature != parentsName))]
    tree[interaction == TRUE, Feature := name_pair]
  }

  return (treeList)
}


getStatsForTrees = function(trees, nodes.train,type = "binary", base_score = 0.5) {
  #code comes from xgboostExplainer package created by David Foster
  leaf <- Feature <- H <- Cover <- Yes <- No <- ID <-
    weight <- Quality <- previous_weight <- G <- uplift_weight <- NULL

  #Accepts data table of tree (the output of xgb.model.dt.tree)
  #Returns a list of tree, with the stats filled in

  tree_list = copy(trees)
  tree_list[, leaf := Feature == 'Leaf']
  tree_list[, H := Cover]

  non.leaves = which(tree_list[, leaf] == F)


  # The default cover (H) seems to lose precision so this loop recalculates it for each node of each tree
  cat('\n\nRecalculating the cover for each non-leaf... \n')
  pb <- txtProgressBar(style = 3)
  j = 0
  for (i in rev(non.leaves)) {
    left = tree_list[i, Yes]
    right = tree_list[i, No]
    tree_list[i, H := tree_list[ID == left, H] + tree_list[ID == right, H]]
    j = j + 1
    setTxtProgressBar(pb, j / length(non.leaves))
  }


  if (type == 'regression') {
    base_weight = base_score
  } else{
    base_weight = log(base_score / (1 - base_score))
  }

  tree_list[leaf == T, weight := base_weight + Quality]

  tree_list[, previous_weight := base_weight]
  tree_list[1, previous_weight := 0]

  tree_list[leaf == T, G := -weight * H]

  tree_list = split(tree_list, as.factor(tree_list$Tree))
  num_tree_list = length(tree_list)
  treenums =  as.character(0:(num_tree_list - 1))

  for (tree in tree_list) {
    num_nodes = nrow(tree)
    non_leaf_rows = rev(which(tree[, leaf] == F))
    for (r in non_leaf_rows) {
      left = tree[r, Yes]
      right = tree[r, No]
      leftG = tree[ID == left, G]
      rightG = tree[ID == right, G]

      tree[r, G := leftG + rightG]
      w = tree[r, -G / H]

      tree[r, weight := w]
      tree[ID == left, previous_weight := w]
      tree[ID == right, previous_weight := w]
    }

    tree[, uplift_weight := weight - previous_weight]
  }

  return (tree_list)
}


buildExplainerFromTreeList = function(tree_list, col_names) {

  #code comes from xgboostExplainer package created by David Foster

  ####accepts a list of trees and column names
  ####outputs a data table, of the impact of each variable + intercept, for each leaf

  tree_list_breakdown <- vector("list", length(col_names)  + 3)
  names(tree_list_breakdown) = c(col_names, 'intercept', 'leaf', 'tree')

  num_trees = length(tree_list)



  for (x in 1:num_trees) {
    tree = tree_list[[x]]
    tree_breakdown = getTreeBreakdown(tree, col_names)
    tree_breakdown$tree = x - 1
    tree_list_breakdown = rbindlist(append(list(tree_list_breakdown), list(tree_breakdown)))
  }

  return (tree_list_breakdown)

}
getTreeBreakdown = function(tree, col_names) {

  #code comes from xgboostExplainer package created by David Foster
  Node <- NULL
  ####accepts a tree (data table), and column names
  ####outputs a data table, of the impact of each variable + intercept, for each leaf



  tree_breakdown <- vector("list", length(col_names)  + 2)
  names(tree_breakdown) = c(col_names, 'intercept', 'leaf')

  leaves = tree[leaf == T, Node]

  for (leaf in leaves) {
    leaf_breakdown = getLeafBreakdown(tree, leaf, col_names)
    leaf_breakdown$leaf = leaf
    tree_breakdown = rbindlist(append(list(tree_breakdown), list(leaf_breakdown)))
  }

  return (tree_breakdown)
}



getLeafBreakdown = function(tree, leaf, col_names) {
  #code comes from xgboostExplainer package created by David Foster

  Node <- Feature <- . <- uplift_weight <- NULL

  ####accepts a tree, the leaf id to breakdown and column names
  ####outputs a list of the impact of each variable + intercept

  impacts = as.list(rep(0, length(col_names)))
  names(impacts) = col_names

  path = findPath(tree, leaf)
  reduced_tree = tree[Node %in% path, .(Feature, uplift_weight)]

  impacts$intercept = reduced_tree[1, uplift_weight]
  reduced_tree[, uplift_weight := shift(uplift_weight, type = 'lead')]

  tmp = reduced_tree[, .(sum = sum(uplift_weight)), by = Feature]
  tmp = tmp[-nrow(tmp)]
  impacts[tmp[, Feature]] = tmp[, sum]

  return (impacts)
}


findPath = function(tree, currentnode, path = c()) {
  #code comes from xgboostExplainer package created by David Foster
  Node <- Yes <- No <- ID <- NULL
  #accepts a tree data table, and the node to reach
  #path is used in the recursive function - do not set this

  while (currentnode > 0) {
    path = c(path, currentnode)
    currentlabel = tree[Node == currentnode, ID]
    currentnode = c(tree[Yes == currentlabel, Node], tree[No == currentlabel, Node])
  }
  return (sort(c(path, 0)))

}



explainPredictions = function(xgb.model, explainer , data) {

  #code comes from xgboostExplainer package created by David Foster
  tree <- NULL
  #Accepts data table of the breakdown for each leaf of each tree and the node matrix
  #Returns the breakdown for each prediction as a data table

  nodes = predict(xgb.model, data, predleaf = TRUE)

  colnames = names(explainer)[1:(ncol(explainer) - 2)]

  preds_breakdown = data.table(matrix(0, nrow = nrow(nodes), ncol = length(colnames)))
  setnames(preds_breakdown, colnames)

  num_trees = ncol(nodes)


  for (x in 1:num_trees) {
    nodes_for_tree = nodes[, x]
    tree_breakdown = explainer[tree == x - 1]

    preds_breakdown_for_tree = tree_breakdown[match(nodes_for_tree, tree_breakdown$leaf), ]
    preds_breakdown = preds_breakdown + preds_breakdown_for_tree[, colnames, with =
                                                                   FALSE]
  }

  return (preds_breakdown)

}
