
#' @export
run_conductance<-function(data){
  conductance_list<-numeric(9)

  conductance_list[1]<-find_conductance(data, edge.betweenness.community(data))
  conductance_list[2]<-find_conductance(data, fastgreedy.community(data))
  conductance_list[3]<-find_conductance(data, label.propagation.community(data))
  conductance_list[4]<-find_conductance(data, leading.eigenvector.community(data))
  conductance_list[5]<-find_conductance(data, multilevel.community(data))
  conductance_list[6]<-find_conductance(data, optimal.community(data))
  conductance_list[7]<-find_conductance(data, spinglass.community(data))
  conductance_list[8]<-find_conductance(data, walktrap.community(data))
  conductance_list[9]<-find_conductance(data, infomap.community(data))
  return(conductance_list)
}

