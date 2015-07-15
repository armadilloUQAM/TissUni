#================================================
#      Script for unifrac_res calculation
#================================================

#============================
# Load packages and functions
#============================
args<-commandArgs(trailingOnly = TRUE)
options(echo=TRUE)
args

path_to_blast.result_file<-args[1]
path_to_group_file<-args[2]
treshold<-args[3]
filename<-args[4]

path_to_blast.result_file
path_to_group_file
treshold
filename

library(SDDE)

# à modifier le path
source("TissUni/src/create_subgraph_by_colors.r")
source("TissUni/src/get_nb_motifs_by_colors_and_types.R")
source("TissUni/src/get_global_nb_motifs_by_colors.R")

#= function for replacing -2 to NaN in motif_list (get_nb_motifs_by_colors_and_types() result)
replace_NaN<-function(motif_list){
	motif_list=replace(motif_list, motif_list==-2, NaN)
	return(motif_list)
}

#============================
#    Unifrac_res calcul
#============================

reseau<-load_network(path_to_blast.result_file,path_to_group_file)

couleur<-levels(as.factor(V(reseau)$tax))

motif_2g<-vector("list", 0.5*(length(couleur)*(length(couleur)-1)))
motif_3g<-vector("list", 0.5*(length(couleur)*(length(couleur)-1)))
motif_4g<-vector("list", 0.5*(length(couleur)*(length(couleur)-1)))
list_color_match<-vector("list",0.5*(length(couleur)*(length(couleur)-1)))

#====== Unifrac_res global
#= Unifrac_res global avec taille de motif 2
	k=1
	
	for (i in 1:(length(couleur)-1)) {
			cj<-couleur[-(1:i)]
		for (j in cj){
			motif_2g[[k]]<-get_global_nb_motifs_by_colors(reseau,2, couleur[i], j)
			motif_3g[[k]]<-get_global_nb_motifs_by_colors(reseau,3, couleur[i], j)
			motif_4g[[k]]<-get_global_nb_motifs_by_colors(reseau,4, couleur[i], j)
			list_color_match[[k]]<-c(couleur[i],j)
			k=k+1
		}
	}


#===== Unifrac_res par types de motif
#= Unifrac_res par types avec taille de motif 3

m3_types<-vector("list", length(list_color_match))
m4_types<-vector("list", length(list_color_match))

	for (i in 1:length(list_color_match)) {
		m3_types[[i]]<-get_nb_motifs_by_colors_and_types(motif_3g[[i]])
		m4_types[[i]]<-get_nb_motifs_by_colors_and_types(motif_4g[[i]])
	}

	for (i in 1:length(list_color_match)) {
		m3_types[[i]]<-replace_NaN(m3_types[[i]])
		m4_types[[i]]<-replace_NaN(m4_types[[i]])
	}
	
motif.global.filename=paste0(filename,treshold, "_motif_info_global.rdata")
save(motif_2g, motif_3g, motif_4g, list_color_match,m3_types, m4_types, file=motif.global.filename)

filename=paste0("unifrac_res_",treshold,filename)
write(c(path_to_blast.result_file, "\n"), filename)

for (i in 1: length(list_color_match)){
write(c("match:",list_color_match[[i]],"motif 2",motif_2g[[i]]$ratio,"motif 3",motif_3g[[i]]$ratio, "motif 4",motif_4g[[i]]$ratio,"motif 3 isoclass"), filename, sep="\t", append=TRUE)
write.table(m3_types[[i]],filename,append=TRUE, sep="\t")
write("motif 4 isoclass", filename, append=TRUE, sep="\t")
write.table(m4_types[[i]],filename,append=TRUE, sep="\t")
}

sessionInfo()
q()
n

