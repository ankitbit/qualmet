
#' @export
run_modularity<-function(data){
  #
  modularity_list<-numeric(9)

  #Computing Mdularities
  modularity_list[1]<-modularity(edge.betweenness.community(data))
  modularity_list[2]<-modularity(fastgreedy.community(data))
  modularity_list[3]<-modularity(label.propagation.community(data))
  modularity_list[4]<-modularity(leading.eigenvector.community(data))
  modularity_list[5]<-modularity(multilevel.community(data))
  modularity_list[6]<-modularity(optimal.community(data))
  modularity_list[7]<-modularity(spinglass.community(data))
  modularity_list[8]<-modularity(walktrap.community(data))
  modularity_list[9]<-modularity(infomap.community(data))

  return(modularity_list)
}
