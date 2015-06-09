###code des simulations

#Les différents paramètres à tester
taille<-c(10,30,40,80,100)
prob<-c(10,30,50,80)
connect<-c(10,30,50,80,100) # à modifier

#Création de la liste vide des igraphs et du dataframe vide des informations du graphe vide
nombre=0
nombre_elements<-length(taille)*length(prob)*length(connect)
Number<-numeric(nombre_elements)
Graph_size<-numeric(nombre_elements)
Color_1_nodes_number<-numeric(nombre_elements)
Color_2_nodes_number<-numeric(nombre_elements)
Color_proportion<-numeric(nombre_elements)
Connectivity<-numeric(nombre_elements)
edges.nb.by.vertex<-numeric(nombre_elements)
mean.degree<-numeric(nombre_elements)

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
			#  <-adjust_density(graph,i,k)
			Number[nombre] <- nombre #À vérifier si number est nécessaire
			Graph_size[nombre]<-i
			Color_1_nodes_number[nombre]<-noeud_col1
			Color_2_nodes_number[nombre]<-noeud_col2
			Color_proportion[nombre]<-j
			Connectivity[nombre]<-k
			edges.nb.by.vertex[nombre]<-((k/100)*((i*(i-1))/2))/(i/2)
			mean.degree[nombre]<-mean(degree(simulation_graphs[[nombre]]))
		}
	}
}
sommaire<-data.frame(Number,Graph_size,Color_1_nodes_number,Color_2_nodes_number,Color_proportion, Connectivity, edges.nb.by.vertex,mean.degree,stringsAsFactors=FALSE)

 
#Application de la fonction get_global_nb_motifs_by_colors()

results_unifrac_global_2<-lapply(simulation_graphs,	get_global_nb_motifs_by_colors,size=2)

results_unifrac_global_3<-lapply(simulation_graphs,	get_global_nb_motifs_by_colors,size=3)

results_unifrac_global_4<-lapply(simulation_graphs,	get_global_nb_motifs_by_colors,size=4)

#Application de la fonction get_nb_motifs_by_colors_and_types() pour motifs de taille 3 et 4

df.results_by_motif_types_3<-lapply(results_unifrac_global_3,get_nb_motifs_by_colors_and_types)

df.results_by_motif_types_4<-lapply(results_unifrac_global_4,get_nb_motifs_by_colors_and_types)

#Résultats des simulations

#= Unifrac global
y2<-unlist(lapply(results_unifrac_global_2, '[[', 1)) 
y3<-unlist(lapply(results_unifrac_global_3, '[[', 1)) 
y4<-unlist(lapply(results_unifrac_global_4, '[[', 1)) 
xtaille<-sommaire[,2]
xcouleur<-sommaire[,5]

#= Unifrac par types de motif

a3<-lapply(df.results_by_motif_types_3,"[",5)
vector_ratio_iso3= c()
vector_ratio_iso2= c()
for (i in (1:length(a3))){
	vector_ratio_iso3[i] <- as.numeric(as.character((a3[[i]][1,])))
	vector_ratio_iso2[i] <- as.numeric(as.character(a3[[i]][2,]))
 }

a4<-lapply(df.results_by_motif_types_4,"[",5)
vector_ratio_iso4= c()
vector_ratio_iso6= c()
vector_ratio_iso7= c()
vector_ratio_iso8= c()
vector_ratio_iso9= c()
vector_ratio_iso10= c()
for (i in (1:length(a4))){
	vector_ratio_iso4[i] <- as.numeric(as.character((a4[[i]][6,])))
	vector_ratio_iso6[i] <- as.numeric(as.character(a4[[i]][5,]))
	vector_ratio_iso7[i] <- as.numeric(as.character(a4[[i]][4,]))
	vector_ratio_iso8[i] <- as.numeric(as.character(a4[[i]][3,]))
	vector_ratio_iso9[i] <- as.numeric(as.character(a4[[i]][2,]))
	vector_ratio_iso10[i] <- as.numeric(as.character(a4[[i]][1,]))
 }

#= Graphiques

global_2_taille<-plot(xtaille,y2,xlab="taille",ylab="unifrac",main="motif2")
global_3_taille<-plot(xtaille,y3,xlab="taille",ylab="unifrac",main="motif3")
motif_iso3_taille<-plot(xtaille,vector_ratio_iso3,col="blue",xlab="taille",ylab="unifrac",main="motif3_iso3")
motif_iso2_taille<-plot(xtaille,vector_ratio_iso2,col="green",xlab="taille",ylab="unifrac",main="motif3_iso2")


global_4_taille<-plot(xtaille,y4,xlab="taille",ylab="unifrac",main="motif4")
motif_iso4_taille<-plot(xtaille,vector_ratio_iso4,col="red",xlab="taille",ylab="unifrac",main="motif4_iso4")
motif_iso6_taille<-plot(xtaille,vector_ratio_iso6,col="orange",xlab="taille",ylab="unifrac",main="motif4_iso6")
motif_iso7_taille<-plot(xtaille,vector_ratio_iso7,col="gold",xlab="taille",ylab="unifrac",main="motif4_iso7")
motif_iso8_taille<-plot(xtaille,vector_ratio_iso8,col="green",xlab="taille",ylab="unifrac",main="motif4_iso8")
motif_iso9_taille<-plot(xtaille,vector_ratio_iso9,col="pink",xlab="taille",ylab="unifrac",main="motif4_iso9")
motif_iso10_taille<-plot(xtaille,vector_ratio_iso10,col="tan",xlab="taille",ylab="unifrac",main="motif4_iso10")

global_2_prop<-plot(xcouleur,y2,xlab="proportion des couleurs",ylab="unifrac",main="motif2")
global_3_prop<-plot(xcouleur,y3,xlab="prop. couleurs",ylab="unifrac",main="motif3")
motif_iso3_prop<-plot(xcouleur,vector_ratio_iso3,col="blue",xlab="couleur",ylab="unifrac",main="motif3_iso3")
motif_iso2_prop<-plot(xcouleur,vector_ratio_iso2,col="green",xlab="couleur",ylab="unifrac",main="motif3_iso2")

global_4_prop<-plot(xcouleur,y4,xlab="prop. couleurs",ylab="unifrac",main="motif4")
motif_iso4_prop<-plot(xcouleur,vector_ratio_iso4,col="red",xlab="couleur",ylab="unifrac",main="motif4_iso4")
motif_iso6_prop<-plot(xcouleur,vector_ratio_iso6,col="orange",xlab="couleur",ylab="unifrac",main="motif4_iso6")
motif_iso7_prop<-plot(xcouleur,vector_ratio_iso7,col="gold",xlab="couleur",ylab="unifrac",main="motif4_iso7")
motif_iso8_prop<-plot(xcouleur,vector_ratio_iso8,col="green",xlab="couleur",ylab="unifrac",main="motif4_iso8")
motif_iso9_prop<-plot(xcouleur,vector_ratio_iso9,col="pink",xlab="couleur",ylab="unifrac",main="motif4_iso9")
motif_iso10_prop<-plot(xcouleur,vector_ratio_iso10,col="tan",xlab="couleur",ylab="unifrac",main="motif4_iso10")