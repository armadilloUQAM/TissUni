#==============================================================================
#=                Code des simulations et des graphiques de résultats
#==============================================================================

# Packages et fonctions requis
library(SDDE)

source("../src/create_subgraph_by_colors.R")
source("../src/get_nb_motifs_by_colors_and_types.R")
source("../src/get_global_nb_motifs_by_colors.R")
source("../src/random_network2.R")

#Wrapper
unifrac.res.1<-function(graph,size_motif=2, nom_col1, nom_col2) {
	results=get_global_nb_motifs_by_colors(graph=graph, size_motif=size_motif, nom_col1=nom_col1,nom_col2=nom_col2)
	if (results$nb_total_motifs==0) return (1);
	return(results$ratio);
}

#Wrapper
unifrac.res.2<-function(graph,size_motif=2, nom_col1, nom_col2) {
	results=get_global_nb_motifs_by_colors(graph=graph, size_motif=size_motif, nom_col1=nom_col1,nom_col2=nom_col2)
	#Create the full graph	
	gf=graph.full(length(V(graph)), directed=FALSE);
	print(length(E(gf)))
	V(gf)$tax=V(graph)$tax
	#Calculate motif for complete graph
	results2=get_global_nb_motifs_by_colors(graph=gf, size_motif=size_motif, nom_col1=nom_col1,nom_col2=nom_col2)
	#$nb_shared_motifs
    #[1] 4
	nb_mono=results$nb_unique_motifs;
	nb_total=results2$nb_total_motifs;
	#$nb_unique_motifs
	#[1] 0
	cat("ratio mix   :",results$ratio,"\n")
	cat("unshared mix:",results$nb_unique_motifs,"\n")
	cat("total mix   :",results$nb_total_motifs,"\n")
	cat("ratio all   :",results2$ratio,"\n")
	cat("unshared all:",results2$nb_unique_motifs,"\n")
	cat("total all   :",results2$nb_total_motifs,"\n")
	cat("New ratio   :",1-nb_mono/nb_total,"\n")
	#cat(nb_mono/nb_total);
	#print(results);
	#print(results2);
	return(results2);
}


		# total_motifs=graph.motifs(graph, size_motif)
		# unique_motifs_col1=graph.motifs(subgraphs$gcol1, size_motif)
		# unique_motifs_col2=graph.motifs(subgraphs$gcol2, size_motif)
		# nb_unique_motifs=graph.motifs.no(subgraphs$gcol1, size_motif)+graph.motifs.no(subgraphs$gcol2, size_motif)
		# nb_shared_motifs=graph.motifs.no(graph, size_motif)-nb_unique_motifs
		
		# #if (nb_shared_motifs==0){
		# #	ratio=0
		# #} else{
			# ratio=nb_unique_motifs/(graph.motifs.no(graph, size_motif))




# Test old functions
# res1=load_network("test_graph1.txt","test_graph1_taxa.txt")
# result1_2=unifrac.res.1(graph=res1, size_motif=2, "A","B")
# result1_3=unifrac.res.1(graph=res1, size_motif=3, "A","B")
# result1_4=unifrac.res.1(graph=res1, size_motif=4, "A","B")

# result1_2b=unifrac.res.2(graph=res1, size_motif=2, "A","B")
# result1_3b=unifrac.res.2(graph=res1, size_motif=3, "A","B")
# result1_4b=unifrac.res.2(graph=res1, size_motif=4, "A","B")

# res2=load_network("test_graph_G.txt","test_graph_G_taxa.txt")
# result2_2=unifrac.res.1(graph=res2, size_motif=2, "A","B")
# result2_3=unifrac.res.1(graph=res2, size_motif=3, "A","B")
# result2_4=unifrac.res.1(graph=res2, size_motif=4, "A","B")
cat("F\n");
res3=load_network("test_graph_F.txt","test_graph_F_taxa.txt")
cat("2\n");
result2_2b=unifrac.res.2(graph=res3, size_motif=2, "A","B")
cat("3\n");
result2_3b=unifrac.res.2(graph=res3, size_motif=3, "A","B")
cat("4\n");
result2_4b=unifrac.res.2(graph=res3, size_motif=4, "A","B")
cat("B\n");
res3=load_network("test_graph_B.txt","test_graph_B_taxa.txt")
cat("2\n");
result2_2b=unifrac.res.2(graph=res3, size_motif=2, "A","B")
cat("3\n");
result2_3b=unifrac.res.2(graph=res3, size_motif=3, "A","B")
cat("4\n");
result2_4b=unifrac.res.2(graph=res3, size_motif=4, "A","B")
cat("C\n");
res3=load_network("test_graph_C.txt","test_graph_C_taxa.txt")
cat("2\n");
result2_2b=unifrac.res.2(graph=res3, size_motif=2, "A","B")
cat("3\n");
result2_3b=unifrac.res.2(graph=res3, size_motif=3, "A","B")
cat("4\n");
result2_4b=unifrac.res.2(graph=res3, size_motif=4, "A","B")
