#'caculateGain
#'
#'Function \code{calculateGain} returns list of trees with pairs of variable and their gain.
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import data.table
#'@import stats
#'@import utils
#'@import xgboost
#'
#'@export
#'
#'



calculateGain<- function(xgb.model,data, trees = NULL) {

  leaf<-Feature<-Yes<-No<-ID<-parentsGain<-Quality<-parentsCover<-Cover<-name_pair<-childsGain<-NULL

  trees = xgb.model.dt.tree(colnames(data), model = xgb.model, trees = trees)
  trees[,leaf := Feature == "Leaf"]
  trees$depth<-0
  treeList = split(trees,as.factor(trees$Tree))

  for (tree in treeList){
    num_nodes = nrow(tree)
    non_leaf_rows = rev(which(tree[,leaf]==F))
    for (r in non_leaf_rows){
      left = tree[r,Yes]
      right = tree[r,No]
      if (tree[ID==left,leaf]==F){
         newDepth<-tree[r ,depth] + 1
        tree[ID==left,`:=`(parentsGain=tree[r,Quality],parentsCover=tree[r,Cover],name_pair=paste(tree[r,Feature],tree[ID==left,Feature], sep = ":"),childsGain=Quality,depth= newDepth,parentsName= tree[r,Feature])]
        tree[ID==left,interaction:=((parentsGain<childsGain) & (Feature!=parentsName))]
        # tree[ID==left,parentsGain:=tree[r,Quality]]
        # tree[ID==left,parentsCover:=tree[r,Cover]]
        # name_pair4=paste(tree[r,Feature],tree[ID==left,Feature], sep = ":")
        # tree[ID==left, name_pair:=name_pair4]
        # tree[ID==left,childsGain:=Quality]
        # newDepth<-tree[r ,depth] + 1
        # tree[ID==left, depth:= newDepth]
        # tree[ID==left, parentsName:= tree[r,Feature]]

      }
      if (tree[ID==right,leaf]==F){
        newDepth<-tree[r ,depth] + 1
        tree[ID==right,`:=`(parentsGain=tree[r,Quality],parentsCover=tree[r,Cover],name_pair=paste(tree[r,Feature],tree[ID==right,Feature], sep = ":"),childsGain=Quality,depth= newDepth,parentsName= tree[r,Feature])]
        tree[ID==right,interaction:=((parentsGain<childsGain) & (Feature!=parentsName))]
        # tree[ID==right,parentsGain:=tree[r,Quality]]
        # tree[ID==right,parentsCover:=tree[r,Cover]]
        # name_pair1= paste(tree[r,Feature],tree[ID==right,Feature], sep = ":")
        # tree[ID==right, name_pair:=name_pair1]
        # tree[ID==right,childsGain:=Quality]
        # newDepth<-tree[r ,depth] + 1
        # tree[ID==right, depth:= newDepth]
        # tree[ID==right, parentsName:= tree[r,Feature]]
      }

    }
  }

  return(treeList)
}
