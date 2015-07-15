#============================
# Load packages and functions
#============================
args<-commandArgs(trailingOnly = TRUE)
options(echo=TRUE)
args

filename<-args[1]

getwd()
results.df<-read.table(filename,header=T)

AB<-results.df[which(results.df[,2]=="BD"),]
BC<-results.df[which(results.df[,2]=="BC"),]
AC<-results.df[which(results.df[,2]=="CD"),]
AB<-rbind(AB[2:30,],AB[1,])
BC<-rbind(BC[2:30,],BC[1,])
AC<-rbind(AC[2:30,],AC[1,])
x_ordre=AB[,1]

ABmotif2<-AB[,3]
ABmotif3<-AB[,4]
ABmotif4<-AB[,5]

BCmotif2<-BC[,3]
BCmotif3<-BC[,4]
BCmotif4<-BC[,5]

ACmotif2<-AC[,3]
ACmotif3<-AC[,4]
ACmotif4<-AC[,5]



png(filename="eso_recovery_motif2a4.png");

plot(x_ordre, ABmotif2, xlab="Blast similarity (%)",ylab="Unifrac_res",main="Global unifrac_res measures\n for esophagus Dataset", col="deepskyblue3",ylim=c(0,1),type="l", lty=1)

points(x_ordre, BCmotif2, type="l", lty=1, col="orangered2")
points(x_ordre, ACmotif2, type="l", lty=1, col="green")

points(x_ordre, ABmotif3, type="l", lty=2, col="deepskyblue3")
points(x_ordre, BCmotif3, type="l", lty=2, col="orangered2")
points(x_ordre, ACmotif3, type="l", lty=2, col="green")

points(x_ordre, ABmotif4, type="l", lty=3, col="deepskyblue3")
points(x_ordre, BCmotif4, type="l", lty=3, col="orangered2")
points(x_ordre, ACmotif4, type="l", lty=3, col="green")

		legend("topright",legend=c("BD","BC","CD", "motif size 2","motif size 3","motif size 4"),title="Color match and motif size",col=c("deepskyblue3","orangered2","green","black","black","black"), lty=c(1,1,1,1,2,3))
		
dev.off()

ABisoclass3<-AB[,6]
ABisoclass2<-AB[,7]
BCisoclass3<-BC[,6]
BCisoclass2<-BC[,7]
ACisoclass3<-AC[,6]
ACisoclass2<-AC[,7]

png(filename="eso_recovery_motif3_isoclass.png");

plot(x_ordre, ABisoclass3, xlab="Blast similarity (%)",ylab="Unifrac_res",main="Motif size 3 isoclass unifrac_res measures\n esophagus Dataset", col="deepskyblue3",ylim=c(0,1),type="l", lty=1)

points(x_ordre, BCisoclass3, type="l", lty=1, col="orangered2")
points(x_ordre, ACisoclass3, type="l", lty=1, col="green")

points(x_ordre, ABisoclass2, type="l", lty=2, col="deepskyblue3")
points(x_ordre, BCisoclass2, type="l", lty=2, col="orangered2")
points(x_ordre, ACisoclass2, type="l", lty=2, col="green")

		legend("topright",legend=c("BD","BC","CD", "isoclass 3","isoclass 2"),title="Color match and motif size",col=c("deepskyblue3","orangered2","green","black","black"), lty=c(1,1,1,1,2))
		
dev.off()
	
add_isoclass_to_graph<-function(x,y, typel,ltyn,color){
	points(x_ordre, y, type=typel, lty=ltyn, col=color)
}


color<-c("orangered2","darkgoldenrod1","green","sienna1","olivedrab","cornflowerblue","darkorange1","darkgreen","slategray2","orangered","seagreen","cyan","red4")
png(filename="eso_recovery_motif4_isoclass.png");
plot(x_ordre, AB[,8], xlab="Blast similarity (%)",ylab="Unifrac_res",main="Motif size 4 isoclass unifrac_res measures \n for esophagus Dataset", col="deepskyblue3",ylim=c(0,1),type="l", lty=1)
add_isoclass_to_graph(x_ordre,BC[,8],"l",1,"orangered2")
add_isoclass_to_graph(x_ordre,AC[,8],"l",1,"green")
add_isoclass_to_graph(x_ordre,AB[,9],"l",2,"deepskyblue3")
add_isoclass_to_graph(x_ordre,BC[,9],"l",2,"orangered2")
add_isoclass_to_graph(x_ordre,AC[,9],"l",2,"green")
add_isoclass_to_graph(x_ordre,AB[,10],"l",3,"deepskyblue3")
add_isoclass_to_graph(x_ordre,BC[,10],"l",3,"orangered2")
add_isoclass_to_graph(x_ordre,AC[,10],"l",3,"green")
add_isoclass_to_graph(x_ordre,AB[,11],"l",4,"deepskyblue3")
add_isoclass_to_graph(x_ordre,BC[,11],"l",4,"orangered2")
add_isoclass_to_graph(x_ordre,AC[,11],"l",4,"green")
add_isoclass_to_graph(x_ordre,AB[,12],"l",5,"deepskyblue3")
add_isoclass_to_graph(x_ordre,BC[,12],"l",5,"orangered2")
add_isoclass_to_graph(x_ordre,AC[,12],"l",5,"green")
add_isoclass_to_graph(x_ordre,AB[,13],"l",6,"deepskyblue3")
add_isoclass_to_graph(x_ordre,BC[,13],"l",6,"orangered2")
add_isoclass_to_graph(x_ordre,AC[,13],"l",6,"green")
legend("topright",legend=c("BD","BC","CD", "isoclass 10","isoclass 9","isoclass 8","isoclass 7","isoclass 6","isoclass 4"),title="Color match and motif size",col=c("deepskyblue3","orangered2","green","black","black","black","black","black","black"), lty=c(1,1,1,1,2,3,4,5,6))
dev.off()

sessionInfo()
