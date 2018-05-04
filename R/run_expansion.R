
#' @export
run_expansion<-function(data){
  expansion_list<-numeric(9)
  expansion_list[1]<-find_expansion(data, edge.betweenness.community(data))
  expansion_list[2]<-find_expansion(data, fastgreedy.community(data))
  expansion_list[3]<-find_expansion(data, label.propagation.community(data))
  expansion_list[4]<-find_expansion(data, leading.eigenvector.community(data))
  expansion_list[5]<-find_expansion(data, multilevel.community(data))
  expansion_list[6]<-find_expansion(data, optimal.community(data))
  expansion_list[7]<-find_expansion(data, spinglass.community(data))
  expansion_list[8]<-find_expansion(data, walktrap.community(data))
  expansion_list[9]<-find_expansion(data, infomap.community(data))
  return(expansion_list)
}
