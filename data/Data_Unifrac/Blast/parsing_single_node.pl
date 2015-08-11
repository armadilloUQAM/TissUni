#reseau_ab_97<-load_network("C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/Blast/network_blast.results_ab.txt_97.txt","C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/AbRecovery/AbRecovery/abrecovery.groups")

#degree(reseau_ab)

# à modifier le path
my @seq_list;
my %noeud_present;

open(IN_groups, "C:/Users/Tissicca/Documents/GitHub/TissUni/data/Data_Unifrac/AbRecovery/AbRecovery/abrecovery.groups");

while(my $ligne=<IN_groups>){
	chomp($ligne);
	my @tab=split('\t', $ligne);
	push(@seq_list, @tab[0]);
}

#foreach my $key (sort(keys(%edges))){
#		my @couple=split("_",$key);
#		print "$key: ".$edges{$key}[0].$edges{$key}[1].$edges{$key}[2].$edges{$key}[3]."\n";
#}

#for(my $i=0; $i<scalar(@seq_list); $i++){
#		print "@seq_list[$i]\n"; 
#}
#print scalar(@seq_list);

if (!exists $noeud_present{$qseid}{
	$noeud_present{$qseid}=1;
}
if (!exists $noeud_present{$sseqid}){
	$noeud_present{$ssequid}=1;
}

open(OUT_sing, ">singlenodes.txt");
for(my $i=0; $i<scalar(@seq_list); $i++){
	if(!exists $noeud_present{$seq_list[$i]}){
		print OUT_sing "$seq_list[$i] \n";
	}
}
close(OUT_sing);