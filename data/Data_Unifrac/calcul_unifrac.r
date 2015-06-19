# import data result from mothur into phyloseq
library(phyloseq)

mothlist="C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/esophagus/esophagus/esophagus.fn.list"
show_mothur_cutoffs(mothlist)

cutoff = "0.10"
x = import_mothur(mothlist, "C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/esophagus/esophagus/esophagus.good.groups", "C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/esophagus/esophagus/esophagus.tree", cutoff)
x

# unweighted unifrac
unw_unifrac<-distance(x, method = "unifrac", type = "samples")

# weighted unifrac
w_unifrac<-distance(x, method="wunifrac",type="samples")


# import network
library(SDDE)
source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/create_subgraph_by_colors.r")
source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/get_nb_motifs_by_colors_and_types.r")
source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/get_global_nb_motifs_by_colors.r")
source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/random_network2.r")

reseau<-load_network("C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/Blast/network.txt","C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/esophagus/esophagus/esophagus.good.groups")

#====== Unifrac_res global
#= Unifrac_res global avec taille de motif 2
B_C_2g<-get_global_nb_motifs_by_colors(reseau,2, "B","C")
B_D_2g<-get_global_nb_motifs_by_colors(reseau,2, "B","D")
C_D_2g<-get_global_nb_motifs_by_colors(reseau,2, "C","D")

#= Unifrac_res global avec taille de motif 3
B_C_3g<-get_global_nb_motifs_by_colors(reseau,3, "B","C")
B_D_3g<-get_global_nb_motifs_by_colors(reseau,3, "B","D")
C_D_3g<-get_global_nb_motifs_by_colors(reseau,3, "C","D")

#= Unifrac_res global avec taille de motif 4
B_C_4g<-get_global_nb_motifs_by_colors(reseau,4, "B","C")
B_D_4g<-get_global_nb_motifs_by_colors(reseau,4, "B","D")
C_D_4g<-get_global_nb_motifs_by_colors(reseau,4, "C","D")

#===== Unifrac_res par types de motif
#= Unifrac_res par types avec taille de motif 3
B_C_3types<-get_nb_motifs_by_colors_and_types(B_C_3g)
B_D_3types<-get_nb_motifs_by_colors_and_types(B_D_3g)
C_D_3types<-get_nb_motifs_by_colors_and_types(C_D_3g)

B_C_3types<-replace(B_C_3types, B_C_3types==-2, NaN)
B_D_3types<-replace(B_D_3types, B_D_3types==-2, NaN)
C_D_3types<-replace(C_D_3types, C_D_3types==-2, NaN)

#= Unifrac_res par types avec taille de motif 4
B_C_4types<-get_nb_motifs_by_colors_and_types(B_C_4g)
B_D_4types<-get_nb_motifs_by_colors_and_types(B_D_4g)
C_D_4types<-get_nb_motifs_by_colors_and_types(C_D_4g)

B_C_4types<-replace(B_C_4types, B_C_4types==-2, NaN)
B_D_4types<-replace(B_D_4types, B_D_4types==-2, NaN)
C_D_4types<-replace(C_D_4types, C_D_4types==-2, NaN)


# création du df avec les différents résultats

B_C_unw<-unw_unifrac[1]
B_D_unw<-unw_unifrac[2]
C_D_unw<-unw_unifrac[3]

B_C_w<-w_unifrac[1]
B_D_w<-w_unifrac[2]
C_D_w<-w_unifrac[3]


BC=c(B_C_unw, B_C_w, B_C_2g$ratio, B_C_3g$ratio, B_C_4g$ratio,B_C_3types[1,5],B_C_3types[2,5],as.numeric(as.character(B_C_4types[1,5])),as.numeric(as.character(B_C_4types[2,5])),as.numeric(as.character(B_C_4types[3,5])),as.numeric(as.character(B_C_4types[4,5])),as.numeric(as.character(B_C_4types[5,5])),as.numeric(as.character(B_C_4types[6,5])))

BD=c(B_D_unw, B_D_w, B_D_2g$ratio, B_D_3g$ratio, B_D_4g$ratio,B_D_3types[1,5],B_D_3types[2,5],as.numeric(as.character(B_D_4types[1,5])),as.numeric(as.character(B_D_4types[2,5])),as.numeric(as.character(B_D_4types[3,5])),as.numeric(as.character(B_D_4types[4,5])),as.numeric(as.character(B_D_4types[5,5])),as.numeric(as.character(B_D_4types[6,5])))

CD=c(C_D_unw, C_D_w, C_D_2g$ratio, C_D_3g$ratio, C_D_4g$ratio, C_D_3types[1,5] ,C_D_3types[2,5], as.numeric(as.character(C_D_4types[1,5])),as.numeric(as.character(C_D_4types[2,5])),as.numeric(as.character(C_D_4types[3,5])),as.numeric(as.character(C_D_4types[4,5])),as.numeric(as.character(C_D_4types[5,5])),as.numeric(as.character(C_D_4types[6,5])))

results.comp.df<-data.frame(BC,BD,CD,stringsAsFactors=FALSE)

row.names(results.comp.df)=c("unweighted unifrac", "weighted unifrac", "global unifrac_res motif size 2", "global unifrac_res motif size 3", "global unifrac_res motif size 4", "unifrac_res motif size 3 isoclass 3", "unifrac_res motif size 3 isoclass 2", "unifrac_res motif size 4 isoclass 10", "unifrac_res motif size 4 isoclass 9", "unifrac_res motif size 4 isoclass 8", "unifrac_res motif size 4 isoclass 7", "unifrac_res motif size 4 isoclass 6", "unifrac_res motif size 4 isoclass 4")

results.comp.df
