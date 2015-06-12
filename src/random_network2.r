random_network2<-function(total_node, total_edge, total_colored) {
	adj=array(0,c(total_node, total_node));
	avail=numeric();
	
	for(i in 1:total_node){
		for(j in 1:total_node){
			if(j>i & j!=i){
				avail=c(avail,(((j-1)*total_node)+i));
			}
		}
	}
  
  #print(avail);
  
	set.seed(sample(1:100000,1));
	#place edge
	if (total_edge>0.5*((total_node*total_node)-total_node)) total_edge=0.5*((total_node*total_node)-total_node);
	if (total_edge!=0) 
	for (i in 1:total_edge) {
		if (length(avail)==1) {
			pos=as.numeric(avail);
		} else {
		pos=sample(avail, 1);
		avail=avail[! avail %in% pos];
		}
	#print(avail)
	#pos= y * (total_node) + x
	#if (pos<=total_node) {
	#   x=pos;
	#   y=1;
	#   adj[x,y]=1;
	#} else {
   #print(pos)
   y = ceiling(pos / (total_node)); 
		x= pos-((y-1)*total_node); 
    #print(x)
    #print(y)
    adj[x,y]=1;
#}
	}
	#print(adj);
	g2 <- graph.adjacency(adj, mode="undirected")
	V(g2)$name=paste("x",c(1:total_node),sep="");
	to_remove=sample(1:total_node,total_colored, replace = FALSE);
	i=1;
	V(g2)$tax=paste('',1,sep="");
	for (vn in to_remove) {	
		V(g2)[vn]$tax=paste('',2,sep="");
	}
	return(g2)
}

