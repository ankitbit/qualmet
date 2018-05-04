

#library(igraph)
#library(igraphdata)
#karate <- graph.famous("Zachary")
#data("karate")
#data("Koenigsberg")
#data("UKfaculty")
#data("macaque")

#Generic function that takes any graph data as input and returns the modularity vector
#The modularity vector contains the information of value of modularity for each of the community object

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


#Generic driver function for computing expansion
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


#Generic driver function for computing conductance
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

find_tpratio<-function(data, community_object){
  num_nodes<-compute_number_of_nodes_in_cluster(community_object)
  num_tpr<-frac_nodes_community(data, community_object)
  sum<-0
  for(i in 1:length(community_object)){
    sum<-sum+(num_nodes[i]/vcount(data))*(num_tpr[[i]]/num_nodes[i])
  }
  return(sum)
}



find_conductance<-function(data, community_object){
  num_internal_edges<-num_internal_edge_each_cluster(data, community_object)
  num_nodes<-compute_number_of_nodes_in_cluster(community_object)
  out_going_edges<-compute_outgoing_nodes_from_clusters(data, community_object)
  sum<-0
  for(i in 1:length(community_object)){
    sum<-sum + ((num_nodes[i])/vcount(data))*(out_going_edges[i]/(2*num_internal_edges[i]+
                                                                    out_going_edges[i]))
  }
  return(sum)
}
  # Function for computing the number of elements (nodes) in each Cluster for an object returned after
  # applying a particular Community Detection Algorithm



#This generic function creates a list with elements of this list as index of nodes in different
#clusters for each of the clusters that our community algorithm has generated

compute_community_index<-function(data, community_object){
  community_index<-list()
  temp<-1
  for(i in compute_number_of_nodes_in_cluster(community_object)){
    if(temp<length(unique(membership(community_object))))
      temp_vec<-(numeric(sum(membership(community_object)==temp)))

    j<-1
    for(k in 1:vcount(data)) {
      #vcount is used for computing number of nodes of the graph under consideration
      if((membership(community_object)==temp)[k] ==T){
        temp_vec[j]<-k
        j<-j+1
      }

    }
    community_index[temp]<-list(temp_vec)
    temp<-temp+1;
  }
  return(community_index)

}






#stores the information about elements in each cluster for each of the different clusters
#This function computes how many nodes are outgoing from each of the clusters for a given object
#returned by some community detection algorithm
compute_outgoing_nodes_from_clusters<-function(data, community_object){

  out_going_cluster_nodes<-numeric(length(community_object))

  community_index<-compute_community_index(data, community_object)
  edge_community_object<-edge(data)

  for(temp in 1:length(community_index)){

    for(i in community_index[[temp]]){
      for(j in 1:vcount(data)){
        if(edge_community_object[[1]][i][j]==1){
          #if node i and node j have an edge between them
          if(!j%in%community_index[[temp]]){
            #if the node j is not a member of the particular cluster under consideration
            out_going_cluster_nodes[temp]<-out_going_cluster_nodes[temp]+1
          }
        }
      }

    }
  }
  return(out_going_cluster_nodes)

}

# This function computes the number of nodes in each of the clusters
compute_number_of_nodes_in_cluster<-function(community_object){
  #computing the number of Cluserts ( #C ) using "length(unique(membership(data)))"
  #Computing the number of nodes in each cluster (N_C)
  N_C<-numeric(length(unique(membership(community_object))))

  #Iterating over the number of clusters
  for(i in 1:length(unique(membership(community_object)))){
    N_C[i]<-sum(membership(community_object)==i)
  }
  return(N_C)
}


num_nodes_each_community<-function(community_object){
  num<-numeric(length(community_object))
  for(i in 1:length(community_object)){
    num[i]<-length(community_object[[i]])
  }
  return(num)
}





# Generic function for computing the "Expansion" for a particular community object
# We're using the criteria of weighed average for computing "Expansion" for the entire community object
# The reason behind this is "Expansion" is a metric computed for each of the clusters
# But we want the value of "Expansion" for the whole community object consisting of many clusters
find_expansion<-function(data,community_object){

  out_going_edges<-compute_outgoing_nodes_from_clusters(data, community_object)
  num_nodes<-compute_number_of_nodes_in_cluster(community_object)
  sum<-0
  for(i in 1:length(community_object)){
    sum<-sum + ((num_nodes[i])/vcount(data))*(out_going_edges[i]/num_nodes[i])
  }
 return(sum)
}


# Computes the number of edges internal to a cluster for each of the clusters created by a particular
# Community Detection Algorithm
num_internal_edge_each_cluster<-function(data, community_object){

  num_edge<-numeric(length(community_object))

  edge_community_object<-edge(data)

  for(temp in 1:length(community_object)){

    for(i in community_object[[temp]]){

      for(j in 1:vcount(data)){
        if(edge_community_object[[1]][i][j]==1){
          # That is, if the node i and node j have an edge between them
          if(j%in%community_object[[temp]]){
            #That is, if node j is a member of the particular cluster "temp" under consideration
            num_edge[temp]<-num_edge[temp]+1
          }
        }
      }
    }
  }

  return(num_edge)
}


#Generic function that returns the number of nodes in a community that form triads
frac_nodes_community<-function(data, community_object){
  sum_vec<-list()
  for(index in 1:length(community_object))
  {
    sum_triad<-0
    sub_g<-induced_subgraph(data, membership(community_object)==index)

    uniq_vert<-sort(unique(triangles(sub_g)))
    if(length(uniq_vert)>0){

      some_list<-list()
      for(i in 1:length(uniq_vert)){
        some_list[i]<-list(uniq_vert[i])
      }
      for(i in some_list){
        if(some_list[i]%in%community_object[[index]]){
          sum_triad<-sum_triad+1
        }
      }

    }else{
      sum_triad<-0
    }
    #print(sum_triad)
    sum_vec[index]<-list(sum_triad)

  }

  return((sum_vec))
}




#This is the main function that we run for each dataset to create a table as desired in Task 1
quality_metric_table<-function(data){

  expan<-run_expansion(data)
  cond<-run_conductance(data)
  mod<-run_modularity(data)
  tpr<-run_tpratio(data)
  tab<-as.data.frame(cbind(tpr, expan, cond, mod))
  names(tab)<-c("TPT","Expansion", "Conductance", "Modularity")
  row.names(tab)<-c("edge.betweenness.community", "fastgreedy.community", "label.propagation.community",
                    "leading.eigenvector.community", "multilevel.community",
                    "optimal.community", "spinglass.community", "walktrap.community",
                    "infomap.community")
  return(tab)
}

#results_karate<-quality_metric_table(karate)
#results_UKfaculty<-quality_metric_table(UKfaculty_undir)
#macaque_undirected<-as.undirected(macaque)
#results_macaque<-quality_metric_table(macaque_undirected)

#results_USairports<-quality_metric_table(USairports)

##Task 2 and Discussion (b)
#setwd("C:/Users/Ankit/Documents/CSN-5/data")
#wiki<-read.graph("wikipedia.gml", format="gml")
#wiki_undirected<-as.undirected(wiki)
#Applying the fastgreedy community detection algorithm on Wikipedia dataset
#wiki_fc<-fastgreedy.community(wiki_undirected)
#plot( wiki_undirected, vertex.color=membership(wiki_fc))

#length(unique(membership(wiki_fc)))
#vert_wiki<-num_nodes_each_community(wiki_fc)

#plot(subgraph(wiki_undirected, membership(wiki_fc)==41), vertex.color=membership(wiki_fc))
#plot(subgraph(wiki_undirected, membership(wiki_fc)==40), vertex.color=membership(wiki_fc))
#plot(subgraph(wiki, membership(wiki_fc)==300), vertex.color=membership(wiki_fc))


#Discussions (a)
#m_fc<-fastgreedy.community(macaque_undirected)
#m_wc<-walktrap.community(macaque_undirected)
#m_fc_tpratio<-find_tpratio(macaque_undirected, fastgreedy.community(macaque_undirected))
#m_wc_tpratio<-find_tpratio(macaque_undirected, walktrap.community(macaque_undirected))
#The third as well as the fourth line returns 0.111111
#This has been explained in the discussion section

