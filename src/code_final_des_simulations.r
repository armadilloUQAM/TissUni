#==============================================================================
#=                Code des simulations et des graphiques de résultats
#==============================================================================

# Packages et fonctions requis

library(SDDE)

source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/create_subgraph_by_colors.r")
source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/get_nb_motifs_by_colors_and_types.r")
source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/get_global_nb_motifs_by_colors.r")
source("C:/Users/Tissicca/Documents/GitHub/TissUni/src/random_network2.r")

#==============================================================================
#=                            Simulation
#==============================================================================
simul_nb=0
simul<-vector("list",100)

for (m in 1:100){
	simul_nb=simul_nb+1
	
	#Les différents paramètres à tester
	taille<-c(10,25,50,100)
	prop<-c(10,25,50)
	connect<-c(10,25,50,75,100)

	#Création de la liste vide des igraphs et du dataframe vide des informations du graphe vide
	nombre=0
	nombre_elements<-length(taille)*length(prop)*length(connect)
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
		for (j in prop){
			noeud_col2<-ceiling(i*(j/100))
			noeud_col1<-i-noeud_col2
			
			for (k in connect){
				nombre<-nombre+1
				nb_edges=ceiling((k/100)*((i*(i-1))/2))
				simulation_graphs[[nombre]]<-random_network2(i,nb_edges,noeud_col2)
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

	simul[[simul_nb]]<-list(unig2=results_unifrac_global_2, unig3=results_unifrac_global_3, unig4=results_unifrac_global_4, unidf3=df.results_by_motif_types_3, unidf4=df.results_by_motif_types_4, sommaire=sommaire,graphslist=simulation_graphs)
}

save(simul,file="simulation100_2.r")

#==============================================================================
#=                  Récupération des résultats des simulations
#==============================================================================

#==== Récupérer les unifrac_res globaux et les listes de df contenant les unifrac_res des différents types de motifs

unifrac2gb<-numeric(6000)
unifrac3gb<-numeric(6000)
unifrac4gb<-numeric(6000)
listdf3<-vector("list", 6000)
listdf4<-vector("list", 6000)

k=1

for (i in 1:100){
	for (j in 1:60){
		unifrac2gb[k]<-simul[[i]][[1]][[j]]$ratio #liste des unifrac_res
		unifrac3gb[k]<-simul[[i]][[2]][[j]]$ratio
		unifrac4gb[k]<-simul[[i]][[3]][[j]]$ratio 
		listdf3[[k]]<-simul[[i]][[4]][[j]] #liste de dataframes
		listdf4[[k]]<-simul[[i]][[5]][[j]] 
		k=k+1
	}
}

#==== Récupérer les unifrac_res des différents types de motifs

isoclass3<-numeric(6000)
isoclass2<-numeric(6000)

for (i in 1:6000){
isoclass3[i]<-as.numeric(as.character(listdf3[[i]][1,5]))
isoclass2[i]<-as.numeric(as.character(listdf3[[i]][2,5]))
}

isoclass4<-numeric(6000)
isoclass6<-numeric(6000)
isoclass7<-numeric(6000)
isoclass8<-numeric(6000)
isoclass9<-numeric(6000)
isoclass10<-numeric(6000)

for (i in 1:6000){
isoclass10[i]<-as.numeric(as.character(listdf4[[i]][1,5]))
isoclass9[i]<-as.numeric(as.character(listdf4[[i]][2,5]))
isoclass8[i]<-as.numeric(as.character(listdf4[[i]][3,5]))
isoclass7[i]<-as.numeric(as.character(listdf4[[i]][4,5]))
isoclass6[i]<-as.numeric(as.character(listdf4[[i]][5,5]))
isoclass4[i]<-as.numeric(as.character(listdf4[[i]][6,5]))
}

#==== Remplacer les valeurs de -2 à NaN
isoclass3<-replace(isoclass3, isoclass3==-2, NaN)
isoclass2<-replace(isoclass2, isoclass2==-2, NaN)
isoclass4<-replace(isoclass4, isoclass4==-2, NaN)
isoclass6<-replace(isoclass6, isoclass6==-2, NaN)
isoclass7<-replace(isoclass7, isoclass7==-2, NaN)
isoclass8<-replace(isoclass8, isoclass8==-2, NaN)
isoclass9<-replace(isoclass9, isoclass9==-2, NaN)
isoclass10<-replace(isoclass10, isoclass10==-2, NaN)

#==== Paramètres des différents graphiques selon l'ordre de la simulation
xsize=rep(c(rep(10,15),rep(25,15),rep(50,15),rep(100,15)),100)
xconnect=rep(c(10,25,50,75,100),1200)
xcol=rep(c(rep(10,5),rep(25,5),rep(50,5)),400)

#==== fonction du calcul de l'écart-type et de la moyenne

calcul_sd_or_mean<-function(stat,result,connectivity,color_proportion){
	res<-c(stat(result[which(xconnect==connectivity & xcol==color_proportion & xsize==10)],na.rm=TRUE),stat(result[which(xconnect==connectivity & xcol==color_proportion & xsize==25)],na.rm=TRUE),stat(result[which(xconnect==connectivity & xcol==color_proportion & xsize==50)],na.rm=TRUE),stat(result[which(xconnect==connectivity & xcol==color_proportion & xsize==100)],na.rm=TRUE))
	return(res)
}

#==== fonction pour faire les graphiques pour l'unifrac_res global

make_graph<-function(result,titre_graph,sd10,sd25,sd50,mean10,mean25,mean50,position_legende){
	plot(xsize[which(xconnect==10 & xcol==10)],result[which(xconnect==10 & xcol==10)],xlab="Size",ylab="Unifrac_res",main=titre_graph, col="lightblue",ylim=c(0,1))

	segments(xsize[which(xconnect==10 & xcol==10)],mean10-sd10,xsize[which(xconnect==10 & xcol==10)], mean10+sd10,col="deepskyblue")
	segments(xsize[which(xconnect==10 & xcol==10)]-1,mean10-sd10,xsize[which(xconnect==10 & xcol==10)]+1, mean10-sd10,col="deepskyblue")
	segments(xsize[which(xconnect==10 & xcol==10)]-1,mean10+sd10,xsize[which(xconnect==10 & xcol==10)]+1, mean10+sd10,col="deepskyblue")

	points(xsize[which(xconnect==10 & xcol==25)], result[which(xconnect==10 & xcol==25)],col="green")
	segments(xsize[which(xconnect==10 & xcol==25)],mean25-sd25,xsize[which(xconnect==10 & xcol==25)], mean25+sd25,col="chartreuse")
	segments(xsize[which(xconnect==10 & xcol==25)]-1,mean25-sd25,xsize[which(xconnect==10 & xcol==25)]+1, mean25-sd25,col="chartreuse")
	segments(xsize[which(xconnect==10 & xcol==25)]-1,mean25+sd25,xsize[which(xconnect==10 & xcol==25)]+1, mean25+sd25,col="chartreuse")

	points(xsize[which(xconnect==10 & xcol==50)], result[which(xconnect==10 & xcol==50)],col="darkorange")
	segments(xsize[which(xconnect==10 & xcol==50)],mean50-sd50,xsize[which(xconnect==10 & xcol==50)], mean50+sd50,col="darkorange")
	segments(xsize[which(xconnect==10 & xcol==50)]-1,mean50-sd50,xsize[which(xconnect==10 & xcol==50)]+1, mean50-sd50,col="darkorange")
	segments(xsize[which(xconnect==10 & xcol==50)]-1,mean50+sd50,xsize[which(xconnect==10 & xcol==50)]+1, mean50+sd50,col="darkorange")

	points(c(10,25,50,100),mean10,type="l",col="deepskyblue")
	points(c(10,25,50,100),mean25,type="l",col="chartreuse")
	points(c(10,25,50,100),mean50,type="l",col="darkorange")

	legend(position_legende,legend=c("10%","25%","50%"),title="Color proportion (%)",fill=c("deepskyblue","green","orange"))
}

#==== fonction pour faire les graphiques pour l'unifrac_res par isoclass

make_graph_isoclass<-function(titre_graphe,position_legende,size_motif,list_isoclass,list_means_by_isoclass,list_sd_by_isoclass){
	#list_means_by_isoclass is list_means_by_isoclass[[isoclass]][means], same for list_sd_by_isoclass
	color<-c("green","deepskyblue","orange","limegreen","blue","darkgoldenrod1","greenyellow","royalblue1","sienna1","olivedrab","cornflowerblue","darkorange1","darkgreen","slategray2","orangered","seagreen","cyan","red4")
	
	line_type=1:6
	points_type=c(1,2,4,0,5,8)
	
	for (i in 1:length(list_isoclass)){
		for(j in 1:3){ #3 proportion de couleurs
			if(i == 1 & j == 1){
				plot(c(10,25,50,100), list_means_by_isoclass[[1]][[1]], xlab="Size", ylab="Unifrac_res", main=titre_graphe, col=color[1],ylim=c(0,1), type="b", lty=line_type[1], pch=1)

				segments(c(10,25,50,100), list_means_by_isoclass[[1]][[1]]-list_sd_by_isoclass[[1]][[1]], c(10,25,50,100), list_means_by_isoclass[[1]][[1]]+list_sd_by_isoclass[[1]][[1]], col=color[1])

				segments(c(10,25,50,100)-1, list_means_by_isoclass[[1]][[1]]-list_sd_by_isoclass[[1]][[1]], c(10,25,50,100)+1, list_means_by_isoclass[[1]][[1]]-list_sd_by_isoclass[[1]][[1]], col=color[1])
				
				segments(c(10,25,50,100)-1, list_means_by_isoclass[[1]][[1]]+list_sd_by_isoclass[[1]][[1]], c(10,25,50,100)+1, list_means_by_isoclass[[1]][[1]]+list_sd_by_isoclass[[1]][[1]], col=color[1])
			}
			else{
				points(c(10,25,50,100), list_means_by_isoclass[[i]][[j]], type="b", lty=line_type[i], col=color[((i-1)*3)+j], pch=points_type[i])

				segments(c(10,25,50,100), list_means_by_isoclass[[i]][[j]]-list_sd_by_isoclass[[i]][[j]], c(10,25,50,100), list_means_by_isoclass[[i]][[j]]+list_sd_by_isoclass[[i]][[j]], col=color[((i-1)*3)+j])

				segments(c(10,25,50,100)-1, list_means_by_isoclass[[i]][[j]]-list_sd_by_isoclass[[i]][[j]],  c(10,25,50,100)+1, list_means_by_isoclass[[i]][[j]]-list_sd_by_isoclass[[i]][[j]], col=color[((i-1)*3)+j])

				segments(c(10,25,50,100)-1, list_means_by_isoclass[[i]][[j]]+list_sd_by_isoclass[[i]][[j]], c(10,25,50,100)+1, list_means_by_isoclass[[i]][[j]]+list_sd_by_isoclass[[i]][[j]], col=color[((i-1)*3)+j])
			}
		}
	}
	
	if(size_motif ==3){
		legend(position_legende,legend=c("10%","25%","50%", "isoclass2","isoclass3"),title="Color proportion (%) and isoclass types",col=c("deepskyblue","green","orange","black","black"), lty=c(1,1,1,1,2),pch=c(rep(NA,3),1,2),cex=0.70)
	}
	if(size_motif ==4){
		legend(position_legende,legend=c("10%","25%","50%","isoclass4","isoclass6","isoclass7", "isoclass8","isoclass9","isoclass10"),title="Color proportion (%) and isoclass types",col=c("deepskyblue","green","orange",rep("black",6)), lty=c(1,1,1,1,2,3,4,5,6), pch=c(rep(NA,3),1,2,4,0,5,8),cex=0.70)
	}
}


#==============================================================================
#=                            Graphiques
#==============================================================================

#==== Télécharger les objets R des résulats de la simulation fait sur T-rex

load("GitHub/TissUni/examples/R_objects_sim/R_objects_sim/list_resul_unifrac_simulation.r")
#Sommaire des caractéristiques de chaque graph (1 sommaire pour chaque ensemble de 60 graphs)
load("GitHub/TissUni/examples/R_objects_sim/R_objects_sim/sommaire_graphs_simulation.r")

unifrac2gb<-unifrac_res$unifrac2gb
unifrac3gb<-unifrac_res$unifrac3gb
unifrac4gb<-unifrac_res$unifrac4gb
isoclass2<-unifrac_res$isoclass2
isoclass3<-unifrac_res$isoclass3
isoclass4<-unifrac_res$isoclass4
isoclass6<-unifrac_res$isoclass6
isoclass7<-unifrac_res$isoclass7
isoclass8<-unifrac_res$isoclass8
isoclass9<-unifrac_res$isoclass9
isoclass10<-unifrac_res$isoclass10

#==== Connectivité 10%

# 1) Motif global de taille 2 
sd_conn_10_col_10<-calcul_sd_or_mean(sd,unifrac2gb,10,10)
sd_conn_10_col_25<-calcul_sd_or_mean(sd,unifrac2gb,10,25)
sd_conn_10_col_50<-calcul_sd_or_mean(sd,unifrac2gb,10,50)

mn_conn_10_col_10<-calcul_sd_or_mean(mean,unifrac2gb,10,10)
mn_conn_10_col_25<-calcul_sd_or_mean(mean,unifrac2gb,10,25)
mn_conn_10_col_50<-calcul_sd_or_mean(mean,unifrac2gb,10,50)

png(file="Figures_simulation2/Graph_motif2_global_conn_10.png")
make_graph(unifrac2gb,"Size 2 motif\n Connectivity 10%",sd_conn_10_col_10,sd_conn_10_col_25,sd_conn_10_col_50,mn_conn_10_col_10,mn_conn_10_col_25,mn_conn_10_col_50,"bottomright")
dev.off()

# 2) Motif global de taille 3
sd_conn_10_col_10_m3<-calcul_sd_or_mean(sd,unifrac3gb,10,10)
sd_conn_10_col_25_m3<-calcul_sd_or_mean(sd,unifrac3gb,10,25)
sd_conn_10_col_50_m3<-calcul_sd_or_mean(sd,unifrac3gb,10,50)

mn_conn_10_col_10_m3<-calcul_sd_or_mean(mean,unifrac3gb,10,10)
mn_conn_10_col_25_m3<-calcul_sd_or_mean(mean,unifrac3gb,10,25)
mn_conn_10_col_50_m3<-calcul_sd_or_mean(mean,unifrac3gb,10,50)

png(file="Figures_simulation2/Graph_motif3_global_conn_10.png")
make_graph(unifrac3gb,"Size 3 motif\n Connectivity 10%",sd_conn_10_col_10_m3,sd_conn_10_col_25_m3,sd_conn_10_col_50_m3,mn_conn_10_col_10_m3,mn_conn_10_col_25_m3,mn_conn_10_col_50_m3,"bottomright")
dev.off()

# 3) Motif global de taille 4  
sd_conn_10_col_10_m4<-calcul_sd_or_mean(sd,unifrac4gb,10,10)
sd_conn_10_col_25_m4<-calcul_sd_or_mean(sd,unifrac4gb,10,25)
sd_conn_10_col_50_m4<-calcul_sd_or_mean(sd,unifrac4gb,10,50)

mn_conn_10_col_10_m4<-calcul_sd_or_mean(mean,unifrac4gb,10,10)
mn_conn_10_col_25_m4<-calcul_sd_or_mean(mean,unifrac4gb,10,25)
mn_conn_10_col_50_m4<-calcul_sd_or_mean(mean,unifrac4gb,10,50)

png(file="Figures_simulation2/Graph_motif4_global_conn_10.png")
make_graph(unifrac4gb,"Size 4 motif\n Connectivity 10%",sd_conn_10_col_10_m4,sd_conn_10_col_25_m4,sd_conn_10_col_50_m4,mn_conn_10_col_10_m4,mn_conn_10_col_25_m4,mn_conn_10_col_50_m4,"topright")
dev.off()

# 4) Isoclass de motif de taille 3

list_isoclass<-c("isoclass2","isoclass3")

isoclass2_mean<-list(calcul_sd_or_mean(mean,isoclass2,10,10),calcul_sd_or_mean(mean,isoclass2,10,25),calcul_sd_or_mean(mean,isoclass2,10,50))
isoclass3_mean<-list(calcul_sd_or_mean(mean,isoclass3,10,10),calcul_sd_or_mean(mean,isoclass3,10,25),calcul_sd_or_mean(mean,isoclass3,10,50))

isoclass2_sd<-list(calcul_sd_or_mean(sd,isoclass2,10,10),calcul_sd_or_mean(sd,isoclass2,10,25),calcul_sd_or_mean(sd,isoclass2,10,50))
isoclass3_sd<-list(calcul_sd_or_mean(sd,isoclass3,10,10),calcul_sd_or_mean(sd,isoclass3,10,25),calcul_sd_or_mean(sd,isoclass3,10,50))

list_means_by_isoclass<-list(isoclass2_mean,isoclass3_mean)
list_sd_by_isoclass<-list(isoclass2_sd,isoclass3_sd)

png(file="Figures_simulation2/Graph_types_motif3_conn_10.png")
make_graph_isoclass("Size 3 motif by isoclass \n Connectivity 10%", "bottomright",3,list_isoclass,list_means_by_isoclass,list_sd_by_isoclass)
dev.off()

# 5) Isoclass de motif de taille 4

list_isoclass_4<-c("isoclass4","isoclass6","isoclass7","isoclass8","isoclass9","isoclass10")

isoclass4_mean<-list(calcul_sd_or_mean(mean,isoclass4,10,10),calcul_sd_or_mean(mean,isoclass4,10,25),calcul_sd_or_mean(mean,isoclass4,10,50))
isoclass6_mean<-list(calcul_sd_or_mean(mean,isoclass6,10,10),calcul_sd_or_mean(mean,isoclass6,10,25),calcul_sd_or_mean(mean,isoclass6,10,50))
isoclass7_mean<-list(calcul_sd_or_mean(mean,isoclass7,10,10),calcul_sd_or_mean(mean,isoclass7,10,25),calcul_sd_or_mean(mean,isoclass7,10,50))
isoclass8_mean<-list(calcul_sd_or_mean(mean,isoclass8,10,10),calcul_sd_or_mean(mean,isoclass8,10,25),calcul_sd_or_mean(mean,isoclass8,10,50))
isoclass9_mean<-list(calcul_sd_or_mean(mean,isoclass9,10,10),calcul_sd_or_mean(mean,isoclass9,10,25),calcul_sd_or_mean(mean,isoclass9,10,50))
isoclass10_mean<-list(calcul_sd_or_mean(mean,isoclass10,10,10),calcul_sd_or_mean(mean,isoclass10,10,25),calcul_sd_or_mean(mean,isoclass10,10,50))

isoclass4_sd<-list(calcul_sd_or_mean(sd,isoclass4,10,10),calcul_sd_or_mean(sd,isoclass4,10,25),calcul_sd_or_mean(sd,isoclass4,10,50))
isoclass6_sd<-list(calcul_sd_or_mean(sd,isoclass6,10,10),calcul_sd_or_mean(sd,isoclass6,10,25),calcul_sd_or_mean(sd,isoclass6,10,50))
isoclass7_sd<-list(calcul_sd_or_mean(sd,isoclass7,10,10),calcul_sd_or_mean(sd,isoclass7,10,25),calcul_sd_or_mean(sd,isoclass7,10,50))
isoclass8_sd<-list(calcul_sd_or_mean(sd,isoclass8,10,10),calcul_sd_or_mean(sd,isoclass8,10,25),calcul_sd_or_mean(sd,isoclass8,10,50))
isoclass9_sd<-list(calcul_sd_or_mean(sd,isoclass9,10,10),calcul_sd_or_mean(sd,isoclass9,10,25),calcul_sd_or_mean(sd,isoclass9,10,50))
isoclass10_sd<-list(calcul_sd_or_mean(sd,isoclass10,10,10),calcul_sd_or_mean(sd,isoclass10,10,25),calcul_sd_or_mean(sd,isoclass10,10,50))
list_means_by_isoclass_4<-list(isoclass4_mean,isoclass6_mean,isoclass7_mean,isoclass8_mean,isoclass9_mean,isoclass10_mean)
list_sd_by_isoclass_4<-list(isoclass4_sd,isoclass6_sd,isoclass7_sd,isoclass8_sd,isoclass9_sd,isoclass10_sd)

png(file="Figures_simulation2/Graph_types_motif4_conn_10.png")
make_graph_isoclass("Size 4 motif by isoclass \n Connectivity 10%", "bottomright",4,list_isoclass_4,list_means_by_isoclass_4,list_sd_by_isoclass_4)
dev.off()

#==== Connectivité 25%

# 1) Motif global de taille 2 
sd_conn_25_col_10<-calcul_sd_or_mean(sd,unifrac2gb,25,10)
sd_conn_25_col_25<-calcul_sd_or_mean(sd,unifrac2gb,25,25)
sd_conn_25_col_50<-calcul_sd_or_mean(sd,unifrac2gb,25,50)

mn_conn_25_col_10<-calcul_sd_or_mean(mean,unifrac2gb,25,10)
mn_conn_25_col_25<-calcul_sd_or_mean(mean,unifrac2gb,25,25)
mn_conn_25_col_50<-calcul_sd_or_mean(mean,unifrac2gb,25,50)

png(file="Figures_simulation2/Graph_motif2_global_conn_25.png")
make_graph(unifrac2gb,"Size 2 motif\n Connectivity 25%",sd_conn_25_col_10,sd_conn_25_col_25,sd_conn_25_col_50,mn_conn_25_col_10,mn_conn_25_col_25,mn_conn_25_col_50,"bottomright")
dev.off()

# 2) Motif global de taille 3
sd_conn_25_col_10_m3<-calcul_sd_or_mean(sd,unifrac3gb,25,10)
sd_conn_25_col_25_m3<-calcul_sd_or_mean(sd,unifrac3gb,25,25)
sd_conn_25_col_50_m3<-calcul_sd_or_mean(sd,unifrac3gb,25,50)

mn_conn_25_col_10_m3<-calcul_sd_or_mean(mean,unifrac3gb,25,10)
mn_conn_25_col_25_m3<-calcul_sd_or_mean(mean,unifrac3gb,25,25)
mn_conn_25_col_50_m3<-calcul_sd_or_mean(mean,unifrac3gb,25,50)

png(file="Figures_simulation2/Graph_motif3_global_conn_25.png")
make_graph(unifrac3gb,"Size 3 motif\n Connectivity 25%",sd_conn_25_col_10_m3,sd_conn_25_col_25_m3,sd_conn_25_col_50_m3,mn_conn_25_col_10_m3,mn_conn_25_col_25_m3,mn_conn_25_col_50_m3,"bottomright")
dev.off()

# 3) Motif global de taille 4  
sd_conn_25_col_10_m4<-calcul_sd_or_mean(sd,unifrac4gb,25,10)
sd_conn_25_col_25_m4<-calcul_sd_or_mean(sd,unifrac4gb,25,25)
sd_conn_25_col_50_m4<-calcul_sd_or_mean(sd,unifrac4gb,25,50)

mn_conn_25_col_10_m4<-calcul_sd_or_mean(mean,unifrac4gb,25,10)
mn_conn_25_col_25_m4<-calcul_sd_or_mean(mean,unifrac4gb,25,25)
mn_conn_25_col_50_m4<-calcul_sd_or_mean(mean,unifrac4gb,25,50)

png(file="Figures_simulation2/Graph_motif4_global_conn_25.png")
make_graph(unifrac4gb,"Size 4 motif\n Connectivity 25%",sd_conn_25_col_10_m4,sd_conn_25_col_25_m4,sd_conn_25_col_50_m4,mn_conn_25_col_10_m4,mn_conn_25_col_25_m4,mn_conn_25_col_50_m4,"topright")
dev.off()

# 4) Isoclass de motif de taille 3

list_isoclass<-c("isoclass2","isoclass3")

isoclass2_mean<-list(calcul_sd_or_mean(mean,isoclass2,25,10),calcul_sd_or_mean(mean,isoclass2,25,25),calcul_sd_or_mean(mean,isoclass2,25,50))
isoclass3_mean<-list(calcul_sd_or_mean(mean,isoclass3,25,10),calcul_sd_or_mean(mean,isoclass3,25,25),calcul_sd_or_mean(mean,isoclass3,25,50))

isoclass2_sd<-list(calcul_sd_or_mean(sd,isoclass2,25,10),calcul_sd_or_mean(sd,isoclass2,25,25),calcul_sd_or_mean(sd,isoclass2,25,50))
isoclass3_sd<-list(calcul_sd_or_mean(sd,isoclass3,25,10),calcul_sd_or_mean(sd,isoclass3,25,25),calcul_sd_or_mean(sd,isoclass3,25,50))

list_means_by_isoclass<-list(isoclass2_mean,isoclass3_mean)
list_sd_by_isoclass<-list(isoclass2_sd,isoclass3_sd)

png(file="Figures_simulation2/Graph_types_motif3_conn_25.png")
make_graph_isoclass("Size 3 motif by isoclass \n Connectivity 25%", "bottomright",3,list_isoclass,list_means_by_isoclass,list_sd_by_isoclass)
dev.off()

# 5) Isoclass de motif de taille 4

list_isoclass_4<-c("isoclass4","isoclass6","isoclass7","isoclass8","isoclass9","isoclass10")

isoclass4_mean<-list(calcul_sd_or_mean(mean,isoclass4,25,10),calcul_sd_or_mean(mean,isoclass4,25,25),calcul_sd_or_mean(mean,isoclass4,25,50))
isoclass6_mean<-list(calcul_sd_or_mean(mean,isoclass6,25,10),calcul_sd_or_mean(mean,isoclass6,10,25),calcul_sd_or_mean(mean,isoclass6,25,50))
isoclass7_mean<-list(calcul_sd_or_mean(mean,isoclass7,25,10),calcul_sd_or_mean(mean,isoclass7,25,25),calcul_sd_or_mean(mean,isoclass7,25,50))
isoclass8_mean<-list(calcul_sd_or_mean(mean,isoclass8,25,10),calcul_sd_or_mean(mean,isoclass8,25,25),calcul_sd_or_mean(mean,isoclass8,25,50))
isoclass9_mean<-list(calcul_sd_or_mean(mean,isoclass9,25,10),calcul_sd_or_mean(mean,isoclass9,25,25),calcul_sd_or_mean(mean,isoclass9,25,50))
isoclass10_mean<-list(calcul_sd_or_mean(mean,isoclass10,25,10),calcul_sd_or_mean(mean,isoclass10,25,25),calcul_sd_or_mean(mean,isoclass10,25,50))

isoclass4_sd<-list(calcul_sd_or_mean(sd,isoclass4,25,10),calcul_sd_or_mean(sd,isoclass4,25,25),calcul_sd_or_mean(sd,isoclass4,25,50))
isoclass6_sd<-list(calcul_sd_or_mean(sd,isoclass6,25,10),calcul_sd_or_mean(sd,isoclass6,25,25),calcul_sd_or_mean(sd,isoclass6,25,50))
isoclass7_sd<-list(calcul_sd_or_mean(sd,isoclass7,25,10),calcul_sd_or_mean(sd,isoclass7,25,25),calcul_sd_or_mean(sd,isoclass7,25,50))
isoclass8_sd<-list(calcul_sd_or_mean(sd,isoclass8,25,10),calcul_sd_or_mean(sd,isoclass8,25,25),calcul_sd_or_mean(sd,isoclass8,25,50))
isoclass9_sd<-list(calcul_sd_or_mean(sd,isoclass9,25,10),calcul_sd_or_mean(sd,isoclass9,25,25),calcul_sd_or_mean(sd,isoclass9,25,50))
isoclass10_sd<-list(calcul_sd_or_mean(sd,isoclass10,25,10),calcul_sd_or_mean(sd,isoclass10,25,25),calcul_sd_or_mean(sd,isoclass10,25,50))
list_means_by_isoclass_4<-list(isoclass4_mean,isoclass6_mean,isoclass7_mean,isoclass8_mean,isoclass9_mean,isoclass10_mean)
list_sd_by_isoclass_4<-list(isoclass4_sd,isoclass6_sd,isoclass7_sd,isoclass8_sd,isoclass9_sd,isoclass10_sd)

png(file="Figures_simulation2/Graph_types_motif4_conn_25.png")
make_graph_isoclass("Size 4 motif by isoclass \n Connectivity 25%", "bottomright",4,list_isoclass_4,list_means_by_isoclass_4,list_sd_by_isoclass_4)
dev.off()

#==== Connectivité 50%

# 1) Motif global de taille 2 
sd_conn_50_col_10<-calcul_sd_or_mean(sd,unifrac2gb,50,10)
sd_conn_50_col_25<-calcul_sd_or_mean(sd,unifrac2gb,50,25)
sd_conn_50_col_50<-calcul_sd_or_mean(sd,unifrac2gb,50,50)

mn_conn_50_col_10<-calcul_sd_or_mean(mean,unifrac2gb,50,10)
mn_conn_50_col_25<-calcul_sd_or_mean(mean,unifrac2gb,50,25)
mn_conn_50_col_50<-calcul_sd_or_mean(mean,unifrac2gb,50,50)

png(file="Figures_simulation2/Graph_motif2_global_conn_50.png")
make_graph(unifrac2gb,"Size 2 motif\n Connectivity 50%",sd_conn_50_col_10,sd_conn_50_col_25,sd_conn_50_col_50,mn_conn_50_col_10,mn_conn_50_col_25,mn_conn_50_col_50,"bottomright")
dev.off()

# 2) Motif global de taille 3
sd_conn_50_col_10_m3<-calcul_sd_or_mean(sd,unifrac3gb,50,10)
sd_conn_50_col_25_m3<-calcul_sd_or_mean(sd,unifrac3gb,50,25)
sd_conn_50_col_50_m3<-calcul_sd_or_mean(sd,unifrac3gb,50,50)

mn_conn_50_col_10_m3<-calcul_sd_or_mean(mean,unifrac3gb,50,10)
mn_conn_50_col_25_m3<-calcul_sd_or_mean(mean,unifrac3gb,50,25)
mn_conn_50_col_50_m3<-calcul_sd_or_mean(mean,unifrac3gb,50,50)

png(file="Figures_simulation2/Graph_motif3_global_conn_50.png")
make_graph(unifrac3gb,"Size 3 motif\n Connectivity 50%",sd_conn_50_col_10_m3,sd_conn_50_col_25_m3,sd_conn_50_col_50_m3,mn_conn_50_col_10_m3,mn_conn_50_col_25_m3,mn_conn_50_col_50_m3,"bottomright")
dev.off()

# 3) Motif global de taille 4  
sd_conn_50_col_10_m4<-calcul_sd_or_mean(sd,unifrac4gb,50,10)
sd_conn_50_col_25_m4<-calcul_sd_or_mean(sd,unifrac4gb,50,25)
sd_conn_50_col_50_m4<-calcul_sd_or_mean(sd,unifrac4gb,50,50)

mn_conn_50_col_10_m4<-calcul_sd_or_mean(mean,unifrac4gb,50,10)
mn_conn_50_col_25_m4<-calcul_sd_or_mean(mean,unifrac4gb,50,25)
mn_conn_50_col_50_m4<-calcul_sd_or_mean(mean,unifrac4gb,50,50)

png(file="Figures_simulation2/Graph_motif4_global_conn_50.png")
make_graph(unifrac4gb,"Size 4 motif\n Connectivity 50%",sd_conn_50_col_10_m4,sd_conn_50_col_25_m4,sd_conn_50_col_50_m4,mn_conn_50_col_10_m4,mn_conn_50_col_25_m4,mn_conn_50_col_50_m4,"topright")
dev.off()

# 4) Isoclass de motif de taille 3

list_isoclass<-c("isoclass2","isoclass3")

isoclass2_mean<-list(calcul_sd_or_mean(mean,isoclass2,50,10),calcul_sd_or_mean(mean,isoclass2,50,25),calcul_sd_or_mean(mean,isoclass2,50,50))
isoclass3_mean<-list(calcul_sd_or_mean(mean,isoclass3,50,10),calcul_sd_or_mean(mean,isoclass3,50,25),calcul_sd_or_mean(mean,isoclass3,50,50))

isoclass2_sd<-list(calcul_sd_or_mean(sd,isoclass2,50,10),calcul_sd_or_mean(sd,isoclass2,50,25),calcul_sd_or_mean(sd,isoclass2,50,50))
isoclass3_sd<-list(calcul_sd_or_mean(sd,isoclass3,50,10),calcul_sd_or_mean(sd,isoclass3,50,25),calcul_sd_or_mean(sd,isoclass3,50,50))

list_means_by_isoclass<-list(isoclass2_mean,isoclass3_mean)
list_sd_by_isoclass<-list(isoclass2_sd,isoclass3_sd)

png(file="Figures_simulation2/Graph_types_motif3_conn_50.png")
make_graph_isoclass("Size 3 motif by isoclass \n Connectivity 50%", "bottomright",3,list_isoclass,list_means_by_isoclass,list_sd_by_isoclass)
dev.off()

# 5) Isoclass de motif de taille 4

list_isoclass_4<-c("isoclass4","isoclass6","isoclass7","isoclass8","isoclass9","isoclass10")

isoclass4_mean<-list(calcul_sd_or_mean(mean,isoclass4,50,10),calcul_sd_or_mean(mean,isoclass4,50,25),calcul_sd_or_mean(mean,isoclass4,50,50))
isoclass6_mean<-list(calcul_sd_or_mean(mean,isoclass6,50,10),calcul_sd_or_mean(mean,isoclass6,50,25),calcul_sd_or_mean(mean,isoclass6,50,50))
isoclass7_mean<-list(calcul_sd_or_mean(mean,isoclass7,50,10),calcul_sd_or_mean(mean,isoclass7,50,25),calcul_sd_or_mean(mean,isoclass7,50,50))
isoclass8_mean<-list(calcul_sd_or_mean(mean,isoclass8,50,10),calcul_sd_or_mean(mean,isoclass8,50,25),calcul_sd_or_mean(mean,isoclass8,50,50))
isoclass9_mean<-list(calcul_sd_or_mean(mean,isoclass9,50,10),calcul_sd_or_mean(mean,isoclass9,50,25),calcul_sd_or_mean(mean,isoclass9,50,50))
isoclass10_mean<-list(calcul_sd_or_mean(mean,isoclass10,50,10),calcul_sd_or_mean(mean,isoclass10,50,25),calcul_sd_or_mean(mean,isoclass10,50,50))

isoclass4_sd<-list(calcul_sd_or_mean(sd,isoclass4,50,10),calcul_sd_or_mean(sd,isoclass4,50,25),calcul_sd_or_mean(sd,isoclass4,50,50))
isoclass6_sd<-list(calcul_sd_or_mean(sd,isoclass6,50,10),calcul_sd_or_mean(sd,isoclass6,50,25),calcul_sd_or_mean(sd,isoclass6,50,50))
isoclass7_sd<-list(calcul_sd_or_mean(sd,isoclass7,50,10),calcul_sd_or_mean(sd,isoclass7,50,25),calcul_sd_or_mean(sd,isoclass7,50,50))
isoclass8_sd<-list(calcul_sd_or_mean(sd,isoclass8,50,10),calcul_sd_or_mean(sd,isoclass8,50,25),calcul_sd_or_mean(sd,isoclass8,50,50))
isoclass9_sd<-list(calcul_sd_or_mean(sd,isoclass9,50,10),calcul_sd_or_mean(sd,isoclass9,50,25),calcul_sd_or_mean(sd,isoclass9,50,50))
isoclass10_sd<-list(calcul_sd_or_mean(sd,isoclass10,50,10),calcul_sd_or_mean(sd,isoclass10,50,25),calcul_sd_or_mean(sd,isoclass10,50,50))
list_means_by_isoclass_4<-list(isoclass4_mean,isoclass6_mean,isoclass7_mean,isoclass8_mean,isoclass9_mean,isoclass10_mean)
list_sd_by_isoclass_4<-list(isoclass4_sd,isoclass6_sd,isoclass7_sd,isoclass8_sd,isoclass9_sd,isoclass10_sd)

png(file="Figures_simulation2/Graph_types_motif4_conn_50.png")
make_graph_isoclass("Size 4 motif by isoclass \n Connectivity 50%", "bottomright",4,list_isoclass_4,list_means_by_isoclass_4,list_sd_by_isoclass_4)
dev.off()

#==== Connectivité 75%

# 1) Motif global de taille 2 
sd_conn_75_col_10<-calcul_sd_or_mean(sd,unifrac2gb,75,10)
sd_conn_75_col_25<-calcul_sd_or_mean(sd,unifrac2gb,75,25)
sd_conn_75_col_50<-calcul_sd_or_mean(sd,unifrac2gb,75,50)

mn_conn_75_col_10<-calcul_sd_or_mean(mean,unifrac2gb,75,10)
mn_conn_75_col_25<-calcul_sd_or_mean(mean,unifrac2gb,75,25)
mn_conn_75_col_50<-calcul_sd_or_mean(mean,unifrac2gb,75,50)

png(file="Figures_simulation2/Graph_motif2_global_conn_75.png")
make_graph(unifrac2gb,"Size 2 motif\n Connectivity 75%",sd_conn_75_col_10,sd_conn_75_col_25,sd_conn_75_col_50,mn_conn_75_col_10,mn_conn_75_col_25,mn_conn_75_col_50,"bottomright")
dev.off()

# 2) Motif global de taille 3
sd_conn_75_col_10_m3<-calcul_sd_or_mean(sd,unifrac3gb,75,10)
sd_conn_75_col_25_m3<-calcul_sd_or_mean(sd,unifrac3gb,75,25)
sd_conn_75_col_50_m3<-calcul_sd_or_mean(sd,unifrac3gb,75,50)

mn_conn_75_col_10_m3<-calcul_sd_or_mean(mean,unifrac3gb,75,10)
mn_conn_75_col_25_m3<-calcul_sd_or_mean(mean,unifrac3gb,75,25)
mn_conn_75_col_50_m3<-calcul_sd_or_mean(mean,unifrac3gb,75,50)

png(file="Figures_simulation2/Graph_motif3_global_conn_75.png")
make_graph(unifrac3gb,"Size 3 motif\n Connectivity 75%",sd_conn_75_col_10_m3,sd_conn_75_col_25_m3,sd_conn_75_col_50_m3,mn_conn_75_col_10_m3,mn_conn_75_col_25_m3,mn_conn_75_col_50_m3,"bottomright")
dev.off()

# 3) Motif global de taille 4  
sd_conn_75_col_10_m4<-calcul_sd_or_mean(sd,unifrac4gb,75,10)
sd_conn_75_col_25_m4<-calcul_sd_or_mean(sd,unifrac4gb,75,25)
sd_conn_75_col_50_m4<-calcul_sd_or_mean(sd,unifrac4gb,75,50)

mn_conn_75_col_10_m4<-calcul_sd_or_mean(mean,unifrac4gb,75,10)
mn_conn_75_col_25_m4<-calcul_sd_or_mean(mean,unifrac4gb,75,25)
mn_conn_75_col_50_m4<-calcul_sd_or_mean(mean,unifrac4gb,75,50)

png(file="Figures_simulation2/Graph_motif4_global_conn_75.png")
make_graph(unifrac4gb,"Size 4 motif\n Connectivity 75%",sd_conn_75_col_10_m4,sd_conn_75_col_25_m4,sd_conn_75_col_50_m4,mn_conn_75_col_10_m4,mn_conn_75_col_25_m4,mn_conn_75_col_50_m4,"topright")
dev.off()

# 4) Isoclass de motif de taille 3

list_isoclass<-c("isoclass2","isoclass3")

isoclass2_mean<-list(calcul_sd_or_mean(mean,isoclass2,75,10),calcul_sd_or_mean(mean,isoclass2,75,25),calcul_sd_or_mean(mean,isoclass2,75,50))
isoclass3_mean<-list(calcul_sd_or_mean(mean,isoclass3,75,10),calcul_sd_or_mean(mean,isoclass3,75,25),calcul_sd_or_mean(mean,isoclass3,75,50))

isoclass2_sd<-list(calcul_sd_or_mean(sd,isoclass2,75,10),calcul_sd_or_mean(sd,isoclass2,75,25),calcul_sd_or_mean(sd,isoclass2,75,50))
isoclass3_sd<-list(calcul_sd_or_mean(sd,isoclass3,75,10),calcul_sd_or_mean(sd,isoclass3,75,25),calcul_sd_or_mean(sd,isoclass3,75,50))

list_means_by_isoclass<-list(isoclass2_mean,isoclass3_mean)
list_sd_by_isoclass<-list(isoclass2_sd,isoclass3_sd)

png(file="Figures_simulation2/Graph_types_motif3_conn_75.png")
make_graph_isoclass("Size 3 motif by isoclass \n Connectivity 75%", "bottomright",3,list_isoclass,list_means_by_isoclass,list_sd_by_isoclass)
dev.off()

# 5) Isoclass de motif de taille 4

list_isoclass_4<-c("isoclass4","isoclass6","isoclass7","isoclass8","isoclass9","isoclass10")

isoclass4_mean<-list(calcul_sd_or_mean(mean,isoclass4,75,10),calcul_sd_or_mean(mean,isoclass4,75,25),calcul_sd_or_mean(mean,isoclass4,75,50))
isoclass6_mean<-list(calcul_sd_or_mean(mean,isoclass6,75,10),calcul_sd_or_mean(mean,isoclass6,75,25),calcul_sd_or_mean(mean,isoclass6,75,50))
isoclass7_mean<-list(calcul_sd_or_mean(mean,isoclass7,75,10),calcul_sd_or_mean(mean,isoclass7,75,25),calcul_sd_or_mean(mean,isoclass7,75,50))
isoclass8_mean<-list(calcul_sd_or_mean(mean,isoclass8,75,10),calcul_sd_or_mean(mean,isoclass8,75,25),calcul_sd_or_mean(mean,isoclass8,75,50))
isoclass9_mean<-list(calcul_sd_or_mean(mean,isoclass9,75,10),calcul_sd_or_mean(mean,isoclass9,75,25),calcul_sd_or_mean(mean,isoclass9,75,50))
isoclass10_mean<-list(calcul_sd_or_mean(mean,isoclass10,75,10),calcul_sd_or_mean(mean,isoclass10,75,25),calcul_sd_or_mean(mean,isoclass10,75,50))

isoclass4_sd<-list(calcul_sd_or_mean(sd,isoclass4,75,10),calcul_sd_or_mean(sd,isoclass4,75,25),calcul_sd_or_mean(sd,isoclass4,75,50))
isoclass6_sd<-list(calcul_sd_or_mean(sd,isoclass6,75,10),calcul_sd_or_mean(sd,isoclass6,75,25),calcul_sd_or_mean(sd,isoclass6,75,50))
isoclass7_sd<-list(calcul_sd_or_mean(sd,isoclass7,75,10),calcul_sd_or_mean(sd,isoclass7,75,25),calcul_sd_or_mean(sd,isoclass7,75,50))
isoclass8_sd<-list(calcul_sd_or_mean(sd,isoclass8,75,10),calcul_sd_or_mean(sd,isoclass8,75,25),calcul_sd_or_mean(sd,isoclass8,75,50))
isoclass9_sd<-list(calcul_sd_or_mean(sd,isoclass9,75,10),calcul_sd_or_mean(sd,isoclass9,75,25),calcul_sd_or_mean(sd,isoclass9,75,50))
isoclass10_sd<-list(calcul_sd_or_mean(sd,isoclass10,75,10),calcul_sd_or_mean(sd,isoclass10,75,25),calcul_sd_or_mean(sd,isoclass10,75,50))
list_means_by_isoclass_4<-list(isoclass4_mean,isoclass6_mean,isoclass7_mean,isoclass8_mean,isoclass9_mean,isoclass10_mean)
list_sd_by_isoclass_4<-list(isoclass4_sd,isoclass6_sd,isoclass7_sd,isoclass8_sd,isoclass9_sd,isoclass10_sd)

png(file="Figures_simulation2/Graph_types_motif4_conn_75.png")
make_graph_isoclass("Size 4 motif by isoclass \n Connectivity 75%", "bottomright",4,list_isoclass_4,list_means_by_isoclass_4,list_sd_by_isoclass_4)
dev.off()

#=========== Connectivité 100%

# 1) Motif global de taille 2 
sd_conn_100_col_10<-calcul_sd_or_mean(sd,unifrac2gb,100,10)
sd_conn_100_col_25<-calcul_sd_or_mean(sd,unifrac2gb,100,25)
sd_conn_100_col_50<-calcul_sd_or_mean(sd,unifrac2gb,100,50)

mn_conn_100_col_10<-calcul_sd_or_mean(mean,unifrac2gb,100,10)
mn_conn_100_col_25<-calcul_sd_or_mean(mean,unifrac2gb,100,25)
mn_conn_100_col_50<-calcul_sd_or_mean(mean,unifrac2gb,100,50)

png(file="Figures_simulation2/Graph_motif2_global_conn_100.png")
make_graph(unifrac2gb,"Size 2 motif\n Connectivity 100%",sd_conn_100_col_10,sd_conn_100_col_25,sd_conn_100_col_50,mn_conn_100_col_10,mn_conn_100_col_25,mn_conn_100_col_50,"bottomright")
dev.off()

# 2) Motif global de taille 3
sd_conn_100_col_10_m3<-calcul_sd_or_mean(sd,unifrac3gb,100,10)
sd_conn_100_col_25_m3<-calcul_sd_or_mean(sd,unifrac3gb,100,25)
sd_conn_100_col_50_m3<-calcul_sd_or_mean(sd,unifrac3gb,100,50)

mn_conn_100_col_10_m3<-calcul_sd_or_mean(mean,unifrac3gb,100,10)
mn_conn_100_col_25_m3<-calcul_sd_or_mean(mean,unifrac3gb,100,25)
mn_conn_100_col_50_m3<-calcul_sd_or_mean(mean,unifrac3gb,100,50)

png(file="Figures_simulation2/Graph_motif3_global_conn_100.png")
make_graph(unifrac3gb,"Size 3 motif\n Connectivity 100%",sd_conn_100_col_10_m3,sd_conn_100_col_25_m3,sd_conn_100_col_50_m3,mn_conn_100_col_10_m3,mn_conn_100_col_25_m3,mn_conn_100_col_50_m3,"bottomright")
dev.off()

# 3) Motif global de taille 4  
sd_conn_100_col_10_m4<-calcul_sd_or_mean(sd,unifrac4gb,100,10)
sd_conn_100_col_25_m4<-calcul_sd_or_mean(sd,unifrac4gb,100,25)
sd_conn_100_col_50_m4<-calcul_sd_or_mean(sd,unifrac4gb,100,50)

mn_conn_100_col_10_m4<-calcul_sd_or_mean(mean,unifrac4gb,100,10)
mn_conn_100_col_25_m4<-calcul_sd_or_mean(mean,unifrac4gb,100,25)
mn_conn_100_col_50_m4<-calcul_sd_or_mean(mean,unifrac4gb,100,50)

png(file="Figures_simulation2/Graph_motif4_global_conn_100.png")
make_graph(unifrac4gb,"Size 4 motif\n Connectivity 100%",sd_conn_100_col_10_m4,sd_conn_100_col_25_m4,sd_conn_100_col_50_m4,mn_conn_100_col_10_m4,mn_conn_100_col_25_m4,mn_conn_100_col_50_m4,"topright")
dev.off()

# 4) Isoclass de motif de taille 3

list_isoclass<-c("isoclass2","isoclass3")

isoclass2_mean<-list(calcul_sd_or_mean(mean,isoclass2,100,10),calcul_sd_or_mean(mean,isoclass2,100,25),calcul_sd_or_mean(mean,isoclass2,100,50))
isoclass3_mean<-list(calcul_sd_or_mean(mean,isoclass3,100,10),calcul_sd_or_mean(mean,isoclass3,100,25),calcul_sd_or_mean(mean,isoclass3,100,50))

isoclass2_sd<-list(calcul_sd_or_mean(sd,isoclass2,100,10),calcul_sd_or_mean(sd,isoclass2,100,25),calcul_sd_or_mean(sd,isoclass2,100,50))
isoclass3_sd<-list(calcul_sd_or_mean(sd,isoclass3,100,10),calcul_sd_or_mean(sd,isoclass3,100,25),calcul_sd_or_mean(sd,isoclass3,100,50))

list_means_by_isoclass<-list(isoclass2_mean,isoclass3_mean)
list_sd_by_isoclass<-list(isoclass2_sd,isoclass3_sd)

png(file="Figures_simulation2/Graph_types_motif3_conn_100.png")
make_graph_isoclass("Size 3 motif by isoclass \n Connectivity 100%", "bottomright",3,list_isoclass,list_means_by_isoclass,list_sd_by_isoclass)
dev.off()

# 5) Isoclass de motif de taille 4

list_isoclass_4<-c("isoclass4","isoclass6","isoclass7","isoclass8","isoclass9","isoclass10")

isoclass4_mean<-list(calcul_sd_or_mean(mean,isoclass4,100,10),calcul_sd_or_mean(mean,isoclass4,100,25),calcul_sd_or_mean(mean,isoclass4,100,50))
isoclass6_mean<-list(calcul_sd_or_mean(mean,isoclass6,100,10),calcul_sd_or_mean(mean,isoclass6,100,25),calcul_sd_or_mean(mean,isoclass6,100,50))
isoclass7_mean<-list(calcul_sd_or_mean(mean,isoclass7,100,10),calcul_sd_or_mean(mean,isoclass7,100,25),calcul_sd_or_mean(mean,isoclass7,100,50))
isoclass8_mean<-list(calcul_sd_or_mean(mean,isoclass8,100,10),calcul_sd_or_mean(mean,isoclass8,100,25),calcul_sd_or_mean(mean,isoclass8,100,50))
isoclass9_mean<-list(calcul_sd_or_mean(mean,isoclass9,100,10),calcul_sd_or_mean(mean,isoclass9,100,25),calcul_sd_or_mean(mean,isoclass9,100,50))
isoclass10_mean<-list(calcul_sd_or_mean(mean,isoclass10,100,10),calcul_sd_or_mean(mean,isoclass10,100,25),calcul_sd_or_mean(mean,isoclass10,100,50))

isoclass4_sd<-list(calcul_sd_or_mean(sd,isoclass4,100,10),calcul_sd_or_mean(sd,isoclass4,100,25),calcul_sd_or_mean(sd,isoclass4,100,50))
isoclass6_sd<-list(calcul_sd_or_mean(sd,isoclass6,100,10),calcul_sd_or_mean(sd,isoclass6,100,25),calcul_sd_or_mean(sd,isoclass6,100,50))
isoclass7_sd<-list(calcul_sd_or_mean(sd,isoclass7,100,10),calcul_sd_or_mean(sd,isoclass7,100,25),calcul_sd_or_mean(sd,isoclass7,100,50))
isoclass8_sd<-list(calcul_sd_or_mean(sd,isoclass8,100,10),calcul_sd_or_mean(sd,isoclass8,100,25),calcul_sd_or_mean(sd,isoclass8,100,50))
isoclass9_sd<-list(calcul_sd_or_mean(sd,isoclass9,100,10),calcul_sd_or_mean(sd,isoclass9,100,25),calcul_sd_or_mean(sd,isoclass9,100,50))
isoclass10_sd<-list(calcul_sd_or_mean(sd,isoclass10,100,10),calcul_sd_or_mean(sd,isoclass10,100,25),calcul_sd_or_mean(sd,isoclass10,100,50))
list_means_by_isoclass_4<-list(isoclass4_mean,isoclass6_mean,isoclass7_mean,isoclass8_mean,isoclass9_mean,isoclass10_mean)
list_sd_by_isoclass_4<-list(isoclass4_sd,isoclass6_sd,isoclass7_sd,isoclass8_sd,isoclass9_sd,isoclass10_sd)

# Only isoclass10 has values other than NaN

png(file="Figures_simulation2/Graph_types_motif4_conn_100.png")
make_graph_isoclass("Size 4 motif by isoclass \n Connectivity 100%", "bottomright",4,list_isoclass_4,list_means_by_isoclass_4,list_sd_by_isoclass_4)
dev.off()