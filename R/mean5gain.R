#'Mean form 5 nodes with the highests gain
#'
#'
#' @param trees table
#'
#' @import data.table
#'
#'
#' @export

mean5gain<-function(trees){

  indx<-Gain<-.<-Feature<-NULL

  setorder(setDT(trees), Feature, -Gain)[, indx := seq_len(.N), by = Feature]
  importanceTop<-trees[indx <= 5]
  importance<-importanceTop[,.(mean5Gain=mean(Gain)), by=Feature]

  return(importance[])
}
