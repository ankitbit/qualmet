

#' @export
run_tpratio<-function(data){
  tpr_list<-numeric(9)
  tpr_list[1]<-find_tpratio(data, edge.betweenness.community(data))
  tpr_list[2]<-find_tpratio(data, fastgreedy.community(data))
  tpr_list[3]<-find_tpratio(data, label.propagation.community(data))
  tpr_list[4]<-find_tpratio(data, leading.eigenvector.community(data))
  tpr_list[5]<-find_tpratio(data, multilevel.community(data))
  tpr_list[6]<-find_tpratio(data, optimal.community(data))
  tpr_list[7]<-find_tpratio(data, spinglass.community(data))
  tpr_list[8]<-find_tpratio(data, walktrap.community(data))
  tpr_list[9]<-find_tpratio(data, infomap.community(data))
  return(tpr_list)
}
