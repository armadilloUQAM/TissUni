###code des simulations

#Les différents paramètres à tester
taille<-c(10,30,40,80,100)
prob<-c(10,30,50,80)
connect<-c(10,30,40,80,100) # à modifier

#Création de la liste vide des igraphs et du dataframe vide des informations du graphe vide
nombre=0
nombre_elements<-length(taille)*length(prob)*length(connect)
Number<-numeric(nombre_elements)
Graph_size<-numeric(nombre_elements)
Color_1_nodes_number<-numeric(nombre_elements)
Color_2_nodes_number<-numeric(nombre_elements)
Color_proportion<-numeric(nombre_elements)
Connectivity<-numeric(nombre_elements)

simulation_graphs<-vector("list", nombre_elements)

#Créations des différents graphes pour la simulation
for (i in taille){
	for (j in prob){
		noeud_col2<-(i)*(j/100)
		noeud_col1<-i-noeud_col2
		for (k in connect){
			nombre<-nombre+1
			prc_edges=(k/100)
			simulation_graphs[[nombre]]<-(random_network_modif(noeud_col1, noeud_col2,ngroup=1, prc_edges))$g2
			#connectivite et sous-graphs de couleurs?
			Number[nombre] <- nombre #À vérifier si number est nécessaire
			Graph_size[nombre]<-i
			Color_1_nodes_number[nombre]<-noeud_col1
			Color_2_nodes_number[nombre]<-noeud_col2
			Color_proportion[nombre]<-j
			Connectivity[nombre]<-k
		}
	}
}
sommaire<-data.frame(Number,Graph_size,Color_1_nodes_number,Color_2_nodes_number,Color_proportion, Connectivity, stringsAsFactors=FALSE)

 
#Application de la fonction get_global_nb_motifs_by_colors()

results_unifrac_global_2<-lapply(simulation_graphs,	get_global_nb_motifs_by_colors,size=2)

results_unifrac_global_3<-lapply(simulation_graphs,	get_global_nb_motifs_by_colors,size=3)

results_unifrac_global_4<-lapply(simulation_graphs,	get_global_nb_motifs_by_colors,size=4)

#Application de la fonction get_nb_motifs_by_colors_and_types() pour motifs de taille 3 et 4

df.results_by_motif_types_3<-lapply(results_unifrac_global_3,get_nb_motifs_by_colors_and_types)

df.results_by_motif_types_4<-lapply(results_unifrac_global_4,get_nb_motifs_by_colors_and_types)