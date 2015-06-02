#random_network modifié pour avoir la connectivité en pourcentage

random_network_modif<-function(original_node=25, additional_node=5, ngroup=1,prc_conn=0, type='erdos') {
	#require(igraph);	
	total_node_in_g2=original_node+additional_node;
	if (type=='erdos') {
		g2 <- erdos.renyi.game(total_node_in_g2, prc_conn*(total_node_in_g2*(total_node_in_g2-1)/2), type="gnm"); #total de edge permis
	} #else {
		#g2<-barabasi.game(total_node_in_g2, m=vertex_ratio, power=1.2, #zero.appeal=1.3, directed=FALSE);
#	}	
	
	V(g2)$name=paste("x",c(1:total_node_in_g2),sep="");
	E(g2)$weight=1.0;	
	to_remove=sample(1:total_node_in_g2,(total_node_in_g2-original_node), replace = FALSE);
	V(g2)$tax='1';
	i=1;
	for (vn in to_remove) {		
		if (i<=ngroup) {
			V(g2)[vn]$tax=paste('',i+1,sep=""); #ensure that we have at least a vertex of each groups
		} else {
			V(g2)[vn]$tax=paste('',1+sample(1:ngroup,1),sep="");
		}
		i=i+1;
	}
	g1 <- delete.vertices(g2,to_remove);
	V(g1)$tax=paste('',1,sep="");
	#info_network(g1,g2);
	return (list("g1"=g1,"g2"=g2, "total_nodes"=length(V(g2)), "total_edges"=length(E(g2)), "total_original_nodes"=length(V(g1))));
}
