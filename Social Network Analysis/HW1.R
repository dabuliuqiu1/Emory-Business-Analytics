library(igraph)
library(data.table)
library(CINNA)
library(corrplot)
library(plyr)

classroom = read.csv("/Users/huangzm/Desktop/Social Network/HW1/social_and_task_network.csv", header=TRUE)
classroom

rownames(classroom) = NULL
class_nodes = unique(classroom["ego"])


# Q1.---------------------------------------------------------------------------

# SOCIAL NETWORK
social_links = classroom[,c(1:3)]
colnames(social_links)[1] = "from"
colnames(social_links)[2] = "to"
colnames(social_links)[3] = "weight"
social_links = social_links[which(social_links$weight > 0),]
rownames(social_links) = NULL

social_net = graph.data.frame(social_links, vertices = class_nodes, directed = TRUE)
social_net

E(social_net)
V(social_net)
E(social_net)$weight

plot(social_net)

# didn't include weights=... bc it's automatically calculated
social_in_degree = degree(social_net,mode="in",normalized = T)
social_out_degree = degree(social_net,mode="out",normalized = T)
social_closeness = closeness(social_net,mode = "all")
social_betweeness = betweenness(social_net, directed=F, normalized=T)
social_PageRank = page_rank(social_net,directed = T)$vector

# TASK NETWORK 
task_links = classroom[,c(1:2,4)]
colnames(task_links)[1] = "from"
colnames(task_links)[2] = "to"
colnames(task_links)[3] = "weight"
task_links = task_links[which(task_links$weight > 0),]
rownames(task_links) = NULL

task_net = graph.data.frame(task_links, vertices = class_nodes, directed = TRUE)
plot(task_net)

task_in_degree = degree(task_net,mode="in",normalized = T)
task_out_degree = degree(task_net,mode="out",normalized = T)
task_closeness = closeness(task_net,mode = "all")
task_betweeness = betweenness(task_net, directed=F, normalized=T)
task_PageRank = page_rank(task_net,directed = T)$vector

# CORRELATION 
corr_df = data.frame(social_in_degree,social_out_degree,social_closeness,social_betweeness,social_PageRank,task_in_degree,task_out_degree,task_closeness,task_betweeness,task_PageRank)

cor(corr_df)[c(1:5),c(6:10)]
corrplot(cor(corr_df)[c(1:5),c(6:10)]) # colorful number

# Findings:
# 1. betweeness, out-degree and in-degree centrality of the task network is strongly correlated with the corresponding centrality of the social network
# 2. closeness of the two network do not hve strong correlation with any of the measures if the other network



# Q2.GRAPH ---------------------------------------------------------------------

nonzero_edges = classroom[-which(classroom$social_tie==0 & classroom$task_tie==0),]
undirected_full = graph.data.frame(nonzero_edges,vertices = class_nodes,directed = T)
plot(undirected_full,edge.curved=F)

# get only strong pairs
full_links = nonzero_edges
full_links$social_str = ""
full_links$task_str = ""
full_links[full_links$social_tie > mean(full_links$social_tie),"social_str"] = "Strong"
full_links[full_links$task_tie > mean(full_links$task_tie),"task_str"] = "Strong"

ranges = full_links
ranges$color = ""
ranges[-which(ranges$social_str == "" & ranges$task_str==""),"color"] = "red"
ranges[which(ranges$social_str == "" & ranges$task_str==""),"color"] = "grey"

plot.igraph(undirected_full,layout=layout.fruchterman.reingold,vertex.label.color="black",edge.color=ranges$color,vertex.size=10, edge.arrow.size=.2,edge.curved=F)



# Q2.FUNCTION ------------------------------------------------------------------

str_links = full_links[-which(full_links$social_str == "" & full_links$task_str==""),1:2]

colnames(str_links) = c("from","to")
for (i in 1:nrow(str_links)){
  if (str_links[i,"to"] < str_links[i,"from"]){
    # swap if to is higher than from
    str_links[i,"to"] = str_links[i,"to"] + str_links[i,"from"]
    str_links[i,"from"] = str_links[i,"to"] - str_links[i,"from"]
    str_links[i,"to"] = str_links[i,"to"] - str_links[i,"from"]}
}

uniq_str = sort(str_links[!duplicated(str_links),]) # drop duplicate row
violate_ties = c()

for (i in min(uniq_str):max(uniq_str)){
  if (length(which(i == uniq_str)>1)){
    element = which(i == uniq_str) # will return element position instead of row number
    # transform element position to row number
    element[which(element>nrow(uniq_str))] = element[which(element>nrow(uniq_str))] - nrow(uniq_str)
    str_i = uniq_str[element,]
    
    for (j in 1:nrow(str_i)){
      if (str_i[j,2]==i){
        # put all i to first column
        str_i[j,"to"] = str_i[j,"to"] + str_i[j,"from"]
        str_i[j,"from"] = str_i[j,"to"] - str_i[j,"from"]
        str_i[j,"to"] = str_i[j,"to"] - str_i[j,"from"]}
    }
    # get all inviolation pairs
    uniq = unique(str_i[,2])
    if (length(uniq) > 1){
      for (a in 1:(length(uniq)-1)){
        for (b in (a+1):length(uniq)){
          STC = FALSE
          for (k in 1:nrow(uniq_str)){
            if ((uniq_str[k,1] == uniq[a] & uniq_str[k,2] == uniq[b])==TRUE) STC = TRUE
            else if ((uniq_str[k,1] == uniq[b] & uniq_str[k,2] == uniq[a])==TRUE) STC = TRUE}
          
          if (STC == FALSE) {violate_ties = rbind(violate_ties, c(i,uniq[a]), c(i,uniq[b]))}
        }
      }
    }
  }
}

colnames(violate_ties) = c("from","to")
for (j in 1:nrow(violate_ties)){
  if (violate_ties[j,"to"] < violate_ties[j,"from"]){
    violate_ties[j,"to"] = violate_ties[j,"to"] + violate_ties[j,"from"]
    violate_ties[j,"from"] = violate_ties[j,"to"] - violate_ties[j,"from"]
    violate_ties[j,"to"] = violate_ties[j,"to"] - violate_ties[j,"from"]}
}

violate_ties = violate_ties[!duplicated(violate_ties),]
nrow(violate_ties)

# there are 16 ties that violate the Stong Triadic Closure Law



# Q3.EDGE BETWEENESS -----------------------------------------------------------
social_edge = edge_betweenness(social_net, e = E(social_net), directed = T, weights = E(social_net)$weight)

social_str = E(social_net)$weight
social_str[social_str > mean(E(social_net)$weight)] = "Strong"
social_str[social_str <= mean(E(social_net)$weight)] = "Weak"

data.frame(social_edge,social_str)


task_edge = edge_betweenness(task_net, e = E(task_net), directed = T, weights = E(task_net)$weight)
task_str = E(task_net)$weight
task_str[task_str > mean(E(task_net)$weight)] = "Strong"
task_str[task_str <= mean(E(task_net)$weight)] = "Weak"

data.frame(task_edge,task_str)

# a edge of high betweenness tend to be a weak tie. This is probably because in a weighted graph, the shortest path become the path with the minimum weight sum, so it is more likely that more shortest path pass through a weak tie than a strong tie.



# Q4.NON-WALKABLE PAIRS --------------------------------------------------------
nonzero_edges = classroom[-which(classroom$social_tie==0 & classroom$task_tie==0),]
full_network = graph.data.frame(nonzero_edges,vertices = class_nodes,directed = T)

plot.igraph(full_network,layout=layout.fruchterman.reingold,vertex.label.color="black",vertex.size=10, edge.arrow.size=.3,edge.curved=F)

full_matrix = as_adjacency_matrix(full_network)
full_matrix = apply(full_matrix,2,as.numeric)

examine_walk = function(i,j,matrix){
  
  walk = FALSE
  n = 1
  while (walk == FALSE & n < nrow(matrix)){
    if (i==j){
      walk = TRUE
      break}
    if (matrix[i,j] > 0){
      walk = TRUE
      break}
    if (matrix[i,j] == 0) {
      matrix = matrix %*% matrix
      matrix = ceiling(matrix/100)
      n = n+1}
  }
  return(walk)
}

walk_matrix = matrix(nrow = nrow(full_matrix), ncol = nrow(full_matrix))
for (i in 1:nrow(full_matrix)){
  for (j in 1:nrow(full_matrix)){
    walk_matrix[i,j] = examine_walk(i,j,full_matrix)
  }
}

length(walk_matrix[walk_matrix == FALSE]) /2

# check using distances matrix
distances = distances(full_network)
length(distances[distances == "Inf"])
which(walk_matrix == FALSE)/2 == which(distances == "Inf")/2

# there are 59 non-walkable pairs in the full network


# Q5.NETWORK CREATION ----------------------------------------------------------
d_0_matrix = matrix(1,nrow=5,ncol=5) - diag(1, nrow=5,ncol=5)
d_0_network = graph_from_adjacency_matrix(d_0_matrix,mode = "undirected")
centr_degree(d_0_network)$centralization
plot(d_0_network)
centr_clo(d_0_network)$centralization
centr_betw(d_0_network)$centralization

d_1_matrix = matrix(0,nrow=5,ncol=5)
d_1_matrix[1,2:5] = 1
d_1_network = graph_from_adjacency_matrix(d_1_matrix,mode = "undirected")
centr_degree(d_1_network)$centralization
plot(d_1_network)
centr_clo(d_1_network)$centralization
centr_betw(d_1_network)$centralization

# The network with 0 degree centrality will also have 0 centrality for other measures
# The network with 1 degree centrality will also have 1 centrality for other measures