#'tableOfTrees
#'
#'tableOfTrees
#'
#' @param model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#'
#' @return a data table
#'
#' @import data.table
#' @import xgboost
# @import lightgbm


tableOfTrees <- function(model, data){
  count <- split_feature <- leaf_count <- internal_count <-
  split_index <- tree_index <- leaf_index <- threshold <-
  leaf_value <- split_gain <- flag <- node_parent <- leaf_parent<-
  Node <- Feature <- . <- Cover <- Yes <- No <- ID <-
  Tree<- Quality <- Missing <- Split <- NULL


  if(class(model)[1] == "xgb.Booster") {
    return(xgb.model.dt.tree(colnames(data), model)[])
  }
  if (class(model)[1] == "lgb.Booster") {
    lgb.trees <- lgb.model.dt.tree(model)

    lgb.trees <- lgb.trees[, count := ifelse(is.na(split_feature), leaf_count, internal_count)]

    lgb.trees <- lgb.trees[, max := max(split_index, na.rm = TRUE), by = tree_index]

    #UWAGA: nie jest tu istotne rodzaj nierówności, interesuje nas, że ktoś jest rodzicem, a nie, czy idzie w prawo i w lewo, dlatego losowe przypisanie Yes, No

    trees <- lgb.trees[, `:=`(Tree = tree_index,
                              Node = ifelse(is.na(split_index), max + leaf_index + 1, split_index),
                              Feature = ifelse(is.na(split_feature), "Leaf", split_feature),
                              Split = threshold, Missing = NA, Quality = ifelse(is.na(split_feature), leaf_value, split_gain),
                              Cover = count)]
    trees[, `:=`(ID = paste(Tree, Node, sep = "-"))]

    trees[, flag := FALSE]
    treeList = split(trees, as.factor(trees$Tree))

    for (tree in treeList) {
      num_nodes = nrow(tree)
      for (i in 1:num_nodes) {
        if (is.na(tree[i, node_parent]) == FALSE) {
          if (tree[Node == tree[i, node_parent] , flag] == FALSE) {
            tree[Node == tree[i, node_parent] , Yes := paste(tree[i, Tree], tree[i, Node], sep = "-")]
            tree[Node == tree[i, node_parent] , flag := TRUE]
          } else{
            tree[Node == tree[i, node_parent] , No := paste(tree[i, Tree], tree[i, Node], sep = "-")]
          }
        }
        if (is.na(tree[i, leaf_parent]) == FALSE) {
          if (tree[Node == tree[i, leaf_parent] , flag] == FALSE) {
            tree[Node == tree[i, leaf_parent] , Yes := paste(tree[i, Tree], tree[i, Node], sep = "-")]
            tree[Node == tree[i, leaf_parent] , flag := TRUE]
          } else{
            tree[Node == tree[i, leaf_parent] , No := paste(tree[i, Tree], tree[i, Node], sep = "-")]
          }
        }
      }
    }
    trees <- rbindlist(treeList)
    trees[, .(Tree, Node, ID, Feature, Split,  Yes, No, Missing,   Quality, Cover)]
    return(trees[, .(Tree, Node, ID, Feature, Split,  Yes, No, Missing,   Quality, Cover)][])
  }
  if (class(model)[1] != "xgb.Booster" || "lgb.Booster") {
    return(cat( "You should choose one of two available models: xgboost, lightgbm \n" ))
  }
}
