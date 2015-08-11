#!/usr/bin/perl

#================================================
#=     Parsing blast result in tabular output
#=         and creating network file
#=
#================================================

use strict;
use warnings;

if (scalar @ARGV !=3){
	die("usage : perl $0 blast.result.txt similarity_threshold group.file\n");
}

#= Déclaration des variables
my $filename = $ARGV[0];
my $treshold= $ARGV[1];
my $groupfile= $ARGV[2];
my %edges;
my @seq_list;
my %noeud_present;
my $newfile="network_".$filename."_".$treshold.".txt";
my $singleton=$filename."_".$treshold."_singleton.txt";

#= Validation des paramètres en ligne de commande
unless ( -e $filename){
	die ("$0: ERREUR: fichier manquant: $filename");
}

if ($treshold =~ /\D/){
	die("$0: ERREUR: entrer un nombre");
}

unless ( -e $groupfile){
	die ("$0: ERREUR: fichier manquant: $groupfile");
}

#= Parcours du fichier
	#ordre des informations: qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore

open(IN_blast_result, $filename) || die($!);
my $n=0;
while(my $ligne = <IN_blast_result>){
		chomp($ligne);
		my @tab_ligne=split('\t', $ligne);
		my $qseid= $tab_ligne[0];
		my $sseqid= $tab_ligne[1];
		my $pident= $tab_ligne[2];
		my $evalue= $tab_ligne[10];
		my $pair=$qseid."_".$sseqid;
		
		if ($pident> $treshold || $pident == $treshold){
			if(!exists($edges{$qseid."_".$sseqid}) && !exists($edges{$sseqid."_".$qseid}) && $qseid ne $sseqid){
				$edges{$pair}=[$qseid, $sseqid, $pident, $evalue];
			}
		}
}

#foreach my $key (sort(keys(%edges))){
#		my @couple=split("_",$key);
#		print "$key: ".$edges{$key}[0].$edges{$key}[1].$edges{$key}[2].$edges{$key}[3]."\n";
#}

#= Création du fichier de réseau et du fichier taxa correspondant
#file with edges in tabular format(node1, node2, edge weight)
#file with taxa information for each node

open(OUT_net, ">$newfile");
foreach my $key (keys %edges){
	print OUT_net "$edges{$key}[0]	$edges{$key}[1]\n";
}
close OUT_net;

#open(OUT_tax, ">tax.txt");
#foreach my $key (keys %tax){
#	print OUT_tax "$key	$tax{$key}\n";
#}
#close OUT_tax;

#= Création du fichier des noeuds n'ayant pas de lien

				if (!exists $noeud_present{$qseid}){
					$noeud_present{$qseid}=1;
				}
				
				if (!exists $noeud_present{$sseqid}){
					$noeud_present{$sseqid}=1;
				}
				
open(IN_groups, $groupfile) || die($!);

while(my $ligne=<IN_groups>){
	chomp($ligne);
	my @tab=split('\t', $ligne);
	push(@seq_list, $tab[0]);
	#print $seq_list[0]."\n";
}
close(IN_groups);

open(OUT_sing, ">$singleton") || die($!);
my $o=0;
for(my $i=0; $i<scalar(@seq_list); $i++){
	if(!exists $noeud_present{$seq_list[$i]}){
		#print OUT_sing "$seq_list[$i] \n";
		print "$seq_list[$i] \n";
	}
}
close(OUT_sing);

#append code to the beginning of R code file
#path_to_blast.result_file=;
#path_to_group_file=;
# log file et output de r