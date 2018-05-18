#'calculateDepth
#'
#'This function returns list of trees with column \code{depth}.
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import data.table
#'@import xgboost
#'
#'

calculateDepth<-function(xgb.model,data, trees = NULL){

Yes<-No<-depth<-ID<-NULL

data=xgb.model.dt.tree(colnames(data),xgb.model,trees)
data$depth<-0
treeList = split(data,as.factor(data$Tree))

i=1
for (tree in treeList){
for(i in 1:nrow(tree)){
  left = tree[i,Yes]
  right = tree[i,No]
  newDepth<-tree[i ,depth] + 1
  tree[ID==left, depth:= newDepth]
  tree[ID==right, depth:= newDepth]
}
}

return(treeList)
}

