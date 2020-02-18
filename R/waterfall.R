#' Explain prediction of a single observation
#'
#' This function calculates a table with influence of variables and interactions
#' on the prediction of a given observation. It supports only xgboost models.
#'
#' The function contains code or pieces of code
#' from \code{breakDown} code created by Przemysław Biecek
#' and \code{xgboostExplainer} code created by David Foster.
#'
#' @param xgb_model a xgboost model.
#' @param new_observation a new observation.
#' @param data row from the original dataset with the new observation to explain (not one-hot-encoded).
#'           The param above has to be set to merge categorical features.
#'           If you dont wont to merge categorical features, set this parameter the same as \code{new_observation}.
#' @param type the learning task of the model. Available tasks: "binary" for binary classification  or "regression" for linear regression.
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
#' @import iBreakDown
#' @importFrom xgboost slice
#' @importFrom xgboost xgb.DMatrix
#'
#' @examples
#'
#' \donttest{
#' library("EIX")
#' library("Matrix")
#' sm <- sparse.model.matrix(left ~ . - 1,  data = HR_data)
#'
#' library("xgboost")
#' param <- list(objective = "binary:logistic", max_depth = 2)
#' xgb_model <- xgboost(sm, params = param, label = HR_data[, left] == 1, nrounds = 25, verbose=0)
#'
#' data <- HR_data[9,-7]
#' new_observation <- sm[9,]
#'
#' wf <- waterfall(xgb_model, new_observation, data,  option = "interactions")
#' wf
#'
#' plot(wf)
#' }
#'
#' @export
#'
waterfall <- function(xgb_model, new_observation, data, type = "binary", option = "interactions", baseline = 0){
  #uses pieces of breakDown code created by Przemysław Biecek
  Feature <- intercept <- NULL

  col_names <- colnames(new_observation)
  trees = xgb.model.dt.tree(col_names, model = xgb_model)

  if (option == "interactions") {
    tree_list = getStatsForTreesInter(trees, type, base_score = .5)
  }
  if (option == "variables") {
    tree_list = getStatsForTrees(trees, type, base_score = .5)
  }

  explainer = buildExplainerFromTreeList(tree_list,names(table(rbindlist(tree_list)[,Feature])))
  new_observation_DM <- slice(xgb.DMatrix(t(new_observation)), as.integer(1))

  breakdown = explainPredictions(xgb_model, explainer, new_observation_DM)


  #merge categorical features
  df <- as.data.frame(breakdown)
  for (i in colnames(data)) {

    indexVariable <- grepl(i, colnames(df)) & !(grepl("\\:", colnames(df))) #for single variables
    if (length(which(indexVariable)) > 1) {
      df[, i] = sum(df[, indexVariable])        #create new column
      new_observation[i] <- as.data.frame(data)[, i]     #varianle_value for categorical  variable
      ix <- which(names(new_observation) %in% names(df[, indexVariable]))       #!delete categorical  features from new observation
      new_observation <- new_observation[-ix]
      df <- df[, !(indexVariable)]                              #delete categorical  features from breakdown
    }

    if (length(which(indexVariable))  == 1) {     #for categorical  features with single occurenses and the rest features
      new_observation[i] <- as.data.frame(data)[, i]
      if (i != colnames(df)[indexVariable]) {
        df[, i] <- df[, indexVariable]
        df <- df[, !(indexVariable)]
      }
    }
    indexInter <- grepl(i, colnames(df)) & (grepl("\\:", colnames(df))) # for interactions
    if (length(which(indexInter)) > 1) {

      indexInter1 <- grepl(i, colnames(df)) & (grepl("\\:", colnames(df))) &
        (unlist(lapply(gregexpr(pattern = i, colnames(df)), `[[`, 1)) < unlist(lapply(gregexpr(pattern = ":", colnames(df)), `[[`, 1)))
      child <- map(strsplit(colnames(df[, indexInter1, drop = FALSE]), "[:]"), 2)
      colnames(df)[indexInter1] <- paste(i, child, sep = ":")

      indexInter2 <- grepl(i, colnames(df)) & (grepl("\\:", colnames(df)))&
        (!(unlist(lapply(gregexpr(pattern = i, colnames(df)), `[[`, 1)) < unlist(lapply(gregexpr(pattern = ":", colnames(df)), `[[`, 1))))
      parent <- map(strsplit(colnames(df[, indexInter2, drop = FALSE]), "[:]"), 1)
      colnames(df)[indexInter2] <- paste(parent, i, sep = ":")
    }
  }
  prefixes = unique(gsub("\\.+[1-9]", "", colnames(df[, grepl("\\:", colnames(df))])))
  interactions <- sapply(prefixes, function(x)sum(df[, startsWith(colnames(df), x)]))
  single <- as.vector(t(df[, !grepl("\\:", colnames(df))]))
  names(single) <- colnames(df[, !grepl("\\:", colnames(df))])
  breakdown <- c(interactions, single)

  #variable_value including interaction
  ilabels <- grep(names(breakdown), pattern = ":", value = TRUE)
  for (interact in ilabels) {
    vars <- strsplit(interact, split = ":")[[1]]
    new_observation[interact] <- paste0(new_observation[vars],collapse = ":")
  }

  breakdown <- data.table(t(breakdown))
  df_intercept <- breakdown[,intercept]
  breakdown <- breakdown[,`:=`(intercept = NULL, Leaf = NULL)]

  df <- data.frame(
    variable = paste(colnames(breakdown),  "=",
                     sapply(new_observation[colnames(breakdown)], as.character)),
    contribution = as.numeric(breakdown),
    variable_name = colnames(breakdown),
    variable_value = sapply(new_observation[colnames(breakdown)], as.character)
  )[as.numeric(breakdown) != 0, ]
  df <- df[order(abs(df[, 2]), decreasing = TRUE), ]
  broken_sorted <- as.data.frame(df)

  if (tolower(baseline) == "intercept"){
    baseline <- df_intercept
  }else{
    broken_sorted <- rbind(
      data.frame(variable = "intercept",
                 contribution = df_intercept - baseline,
                 variable_name = "intercept",
                 variable_value = 1),
      broken_sorted)
  }


  create.broken(broken_sorted, baseline)
}

create.broken <- function(broken_intercept, baseline = 0) {
  #code was copied from breakDown package created by Przemysław Biecek (with small modyfications to can use plot from iBreakDown package)
  broken_cumm <- data.frame(broken_intercept,
                            cumulative = cumsum(as.numeric(broken_intercept$contribution)),
                            sign = factor(sign(as.numeric(broken_intercept$contribution)), levels = c(-1, 0, 1)),
                            position = length(broken_intercept$variable) - seq_along(broken_intercept$variable) + 2,
                            label = rep("xgboost", nrow(broken_intercept)))
  broken_cumm <- rbind(broken_cumm,
                       data.frame(variable = "prediction",
                                  contribution = sum(broken_cumm$contribution),
                                  variable_name = "",
                                  variable_value = "",
                                  cumulative = sum(broken_cumm$contribution),
                                  sign = "X",
                                  position = 1,
                                  label = "xgboost"))
  attr(broken_cumm, "baseline") <- baseline
  class(broken_cumm) <- c("break_down", "data.frame")
  broken_cumm
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
  #code was copied from xgboostExplainer package created by David Foster
  leaf <- Feature <- H <- Cover <- Yes <- No <- ID <-
    weight <- Quality <- previous_weight <- G <- uplift_weight <- NULL

  #Accepts data table of tree (the output of xgb.model.dt.tree)
  #Returns a list of tree, with the stats filled in

  tree_list = copy(trees)
  tree_list[, leaf := Feature == 'Leaf']
  tree_list[, H := Cover]

  non.leaves = which(tree_list[, leaf] == F)


  # The default cover (H) seems to lose precision so this loop recalculates it for each node of each tree
  # cat('\n\nRecalculating the cover for each non-leaf... \n')
  # pb <- txtProgressBar(style = 3)
  # j = 0
  for (i in rev(non.leaves)) {
    left = tree_list[i, Yes]
    right = tree_list[i, No]
    tree_list[i, H := tree_list[ID == left, H] + tree_list[ID == right, H]]
    # j = j + 1
    # setTxtProgressBar(pb, j / length(non.leaves))
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

  #code was copied from xgboostExplainer package created by David Foster

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

  #code was copied from xgboostExplainer package created by David Foster
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
  #code was copied from xgboostExplainer package created by David Foster

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
  #code  was copied from xgboostExplainer package created by David Foster
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

#' @importFrom stats predict

explainPredictions = function(xgb_model, explainer , data) {

  #code was copied from xgboostExplainer package created by David Foster
  tree <- NULL
  #Accepts data table of the breakdown for each leaf of each tree and the node matrix
  #Returns the breakdown for each prediction as a data table

  nodes = predict(xgb_model, data, predleaf = TRUE)

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

