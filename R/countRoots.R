#'Table with number of roots
#'
#'Function \code{countRoots} counts how many times each variable is in the root of the tree and calculates the weighedRoot-number of occurrences in root weighed by Gain.
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
#'


countRoots<- function(xgb.model,data, trees = NULL) {

  Node<-Quality<-Feature<-sumGain<-.<-weightedRoot<-NULL

  trees=xgb.model.dt.tree(colnames(data),xgb.model,trees)

  roots<-trees[Node==0,]
  roots<-roots[, .(sumGain=sum(Quality),countRoots=.N),by=Feature]
  sumGains<-sum(roots[,sumGain])
  roots<-roots[, weightedRoot:=round(roots[,sumGain]*roots[,countRoots]/sumGains,4)]

  return(roots[])
  }

