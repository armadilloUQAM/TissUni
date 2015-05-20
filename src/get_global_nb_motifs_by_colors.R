#### Function to get motifs numbers by colors of a graph

# requires SDDE and igraph packages and create_subgraph_by_colors function
# input: an igraph with colors levels (V(graph)$tax) and the motif size
# output: list of different attributes

get_global_nb_motifs_by_colors<-function(graph,size){
	subgraphs<-create_subgraph_by_colors(graph)
	
	if (size ==2){
		total_motifs=E(graph)
		unique_motifs_col1=E(subgraphs$gcol1)
		unique_motifs_col2=E(subgraphs$gcol2)
		nb_unique_motifs=length(unique_motifs_col1)+length(unique_motifs_col2)
		nb_shared_motifs=length(total_motifs)-nb_unique_motifs
	}
	if (size ==3 || 4){
		total_motifs=graph.motifs(graph, size)
		unique_motifs_col1=graph.motifs(subgraphs$gcol1, size)
		unique_motifs_col2=graph.motifs(subgraphs$gcol2, size)
		nb_unique_motifs=graph.motifs.no(subgraphs$gcol1, size)+graph.motifs.no(subgraphs$gcol2, size)
		nb_shared_motifs=graph.motifs.no(graph, size)-nb_unique_motifs
	}
	
	ratio=nb_unique_motifs/nb_shared_motifs
	
	return(list(ratio=ratio, size_motif=size,total_motifs=total_motifs,unique_motifs_col1=unique_motifs_col1,unique_motifs_col2=unique_motifs_col2,nb_shared_motifs=nb_shared_motifs,nb_unique_motifs=nb_unique_motifs))
}

# À modifier:
	#Ne prend que les motifs connectés
	#Option pour échantillonner au lieu de tout prendre
