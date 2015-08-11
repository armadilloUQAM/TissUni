#Commandes pour le UniFrac réseau sur R

library(igraph)
library(SDDE)
source(paste0(getwd(),"/../src/create_subgraph_by_colors.r",sep=""))
source(paste0(getwd(),"/../src/get_global_nb_motifs_by_colors.R",sep=""))
source(paste0(getwd(),"/../src/get_nb_motifs_by_colors_and_types.R",sep=""))

#Exemple de chargement d'un réseau

reseau<-load_network("ex_carre.txt","tax_carre.txt")
V(reseau)$color=V(reseau)$tax
color<-levels(as.factor(V(reseau)$tax))
for (i in 1:length(color)){
  V(reseau)$color=replace(V(reseau)$color,which(V(reseau)$color==color[i]),i+1)
}
plot(reseau,vertex.color=V(reseau)$color)

#Exemple de calcul de UniFrac réseau

motif_global_2<-get_global_nb_motifs_by_colors(reseau,2, "A","B")
motif_global_2

motif_global_3<-get_global_nb_motifs_by_colors(reseau,3, "A","B")
motif_global_3

motif_global_4<-get_global_nb_motifs_by_colors(reseau,4, "A","B")
motif_global_4

types3<-get_nb_motifs_by_colors_and_types(motif_global_3)
types3

types4<-get_nb_motifs_by_colors_and_types(motif_global_4)
types4