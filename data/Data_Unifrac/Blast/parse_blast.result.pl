#!/usr/bin/perl

#================================================
#=     Parsing blast result in tabular output
#=         and creating network file
#=
#================================================

use strict;
use warnings;

if (scalar @ARGV !=3){
	die("usage : perl $0 blast.result.txt evalue_or_pident similarity_threshold \n");
}

#= Déclaration des variables
my $filename = $ARGV[0];
my $criteria= $ARGV[1];
my $treshold= $ARGV[2];
my %edges;
my %tax;

#= Validation des paramètres en ligne de commande
unless ( -e $filename){
	die ("$0: ERREUR: fichier manquant: $filename");
}

if ($criteria ne "evalue" && $criteria ne "pident"){
	die("$0: ERREUR: entrer: \"evalue\" ou \"pident\"");
}

if ($treshold =~ /\D/){
	die("$0: ERREUR: entrer un nombre");
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
		my $value;
		
		if($criteria eq "pident"){
			$value=$pident;
		}
		else{
			$value=$evalue;
		}
		
		if(!exists($tax{$qseid})){
			$n++;
			$tax{$qseid}=$qseid;
		}
		if(!exists($tax{$sseqid})){
			$n++;
			$tax{$sseqid}=$sseqid;
		}
		if ($value> $treshold || $value == $treshold){
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

open(OUT_net, ">network.txt");
foreach my $key (keys %edges){
	print OUT_net "$edges{$key}[0]	$edges{$key}[1]\n";
}
close OUT_net;

#open(OUT_tax, ">tax.txt");
#foreach my $key (keys %tax){
#	print OUT_tax "$key	$tax{$key}\n";
#}
#close OUT_tax;