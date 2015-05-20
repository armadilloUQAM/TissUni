#### Function to create subgraph of each color

# requires SDDE and igraph packages
# input: an igraph with colors levels (V(graph)$tax)
	# It can be done with load_network() function from SDDE
# output: list of each subgraph by colors

create_subgraph_by_colors<-function(graph){

	#Get nodes by colors
	color.levels<-as.factor(V(graph)$tax)
	mask.col1<-which(V(graph)$tax == levels(color.levels)[1])
	mask.col2<-which(V(graph)$tax == levels(color.levels)[2])
	node.col1<-V(graph)[mask.col1]
	node.col2<-V(graph)[mask.col2]
	
	#Create the subgraphs
	graph.col1<-induced.subgraph(graph,node.col1,impl="auto")
	graph.col2<-induced.subgraph(graph,node.col2,impl="auto")
	
	return(list(gcol1=graph.col1, gcol2=graph.col2))
}

# À modifier:
#possibilité de généraliser pour plus de couleurs avec tapply()?

#plot(gcol1, vertex.color="gold")
#plot(gcol2, vertex.color="springgreen")