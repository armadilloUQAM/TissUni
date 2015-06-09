#### Function to get motifs numbers by type

# input: output of get_global_nb_motifs_by_colors
		#It is a list of the following attributes:
			#ratio
			#size_motif 
			#total_motifs 
			#unique_motifs_col1 
			#unique_motifs_col2 
			#nb_shared_motifs
			#nb_unique_motifs

# output: dataframe of attributes by isoclass type (defined by igraph)

get_nb_motifs_by_colors_and_types<-function(motifs.list){
	if(motifs.list$size_motif ==3){
		isoclass3=return_infos(motifs.list,4)
		isoclass2=return_infos(motifs.list,3)
		rnames=c("isoclass 3", "isoclass 2")
		results<-data.frame(rbind(isoclass3,isoclass2),row.names=rnames)
	}
		if(motifs.list$size_motif ==4){
		isoclass10=return_infos(motifs.list,11)
		isoclass9=return_infos(motifs.list,10)
		isoclass8=return_infos(motifs.list,9)
		isoclass7=return_infos(motifs.list,8)
		isoclass6=return_infos(motifs.list,7)
		isoclass4=return_infos(motifs.list,5)
		rnames=c("isoclass 10", "isoclass 9","isoclass 8","isoclass 7","isoclass 6","isoclass 4")
		results<-data.frame(rbind(isoclass10, isoclass9,isoclass8,isoclass7,isoclass6,isoclass4),row.names=rnames)
	}
	colnames(results)<-c("col1","col2","shared","total","ratio")
	return(results)
}

return_infos<-function(motifs.list, position){
	total=motifs.list$total_motifs[position]
	if (total !=0){
		unique_col1=motifs.list$unique_motifs_col1[position]
		unique_col2=motifs.list$unique_motifs_col2[position]
		shared=total-(unique_col1+unique_col2)
		ratio=(unique_col1+unique_col2)/total
	}
	else{
		unique_col1=0
		unique_col2=0
		shared=0
		ratio="-2"
	}
	return(c(unique_col1,unique_col2,shared,total,ratio))
}
