#!/bin/usr/perl

# code pour parcourir les fichiers de résultats de unifrac_analysis.pl et créer les graphiques en R

use warnings;
use strict;

my %results;

my $dir= "abrecovery_results";

opendir (DIR, $dir) or die "Can't open $dir: $!";
my @files = sort grep (/unifrac_res/,  readdir(DIR));

foreach my $file (@files){
	#print "$file\n";
	open(RFILE, "$dir/$file") or die "$file = $!";
	
	my %color_match;
	my $blast;
	($blast) = ($file =~ m/([0-9]+)/);	
	my $id="ab_".$blast;

	#print "$id\n";
	while(my $ligne =<RFILE>){
		chomp($ligne);
		
		if($ligne =~/^match/){
			my %values;
			my $ligne2=<RFILE>;
			chomp($ligne2);
			my @tabligne2=split('\t', $ligne2);
			my $color1= $tabligne2[0];
			
			my $ligne3=<RFILE>;
			chomp($ligne3);
			my @tabligne3=split('\t', $ligne3);
			my $color2= $tabligne3[0];
			
			my $ligne_suite;
			#print"new\n";
			
			while($ligne_suite =<RFILE>){
				chomp($ligne_suite);
				
				if($ligne_suite =~ /motif 2/){
					#print "$ligne_suite\n";
					my $ligne_val=<RFILE>;
					chomp($ligne_val);
					#print "$ligne_val\n";
					$values{"motif2"}=$ligne_val;
				}
				
				if($ligne_suite =~ /motif 3$/){
					#print "$ligne_suite\n";
					my $ligne_val3=<RFILE>;
					chomp($ligne_val3);
					#print "$ligne_val3\n";
					$values{"motif3"}=$ligne_val3;
				}
				
				if($ligne_suite =~ /motif 4$/){
					#print "$ligne_suite\n";
					my $ligne_val4=<RFILE>;
					chomp($ligne_val4);
					#print "$ligne_val4\n";
					$values{"motif4"}=$ligne_val4;
				}
				
				if($ligne_suite =~ /motif 3 isoclass/){
					#print "motif 3 isoclass\n";
					my $ligne_c=<RFILE>;
					my @isoclass=("isoclass3","isoclass2");
					for (my $i=0; $i<scalar(@isoclass); $i++){
						$ligne_c=<RFILE>;
						chomp($ligne_c);
						my @ligne_tab=split('\t', $ligne_c);
						my $isoclassval =$ligne_tab[5];
						#print"$isoclassval\n";
						$values{$isoclass[$i]}=$isoclassval;
					}
				}
				
				if($ligne_suite =~ /motif 4 isoclass/){
					my @isoclass=("isoclass10","isoclass9","isoclass8","isoclass7","isoclass6","isoclass4");
					my $ligne_c=<RFILE>;
					#print"motif 4 isoclass\n";
					for (my $i=0; $i<scalar(@isoclass); $i++){
						$ligne_c=<RFILE>;
						chomp($ligne_c);
						my @ligne_tab=split('\t', $ligne_c);
						my $isoclassval =$ligne_tab[5];
						#print"$isoclassval\n";
						$values{$isoclass[$i]}=$isoclassval;
					}
					last;
				}
				
			}
			
			$color_match{"$color1"."$color2"}=\%values;
			
			#for my $key (sort keys(%color_match)){
				#print"$key: $color_match{$key}\n";
					#for my $k(sort keys(%{$color_match{$key}})){
						#print "$k: $values{$k}\n";
					#}
			#}
		}
		
	}
	$results{$id}=\%color_match;
	#for my $elt(sort keys(%results)){
		#print "$elt: \n";
				#for my $key (sort keys(%{$results{$elt}})){
				#print"$key:\n";
					#for my $k(sort keys(%{${$results{$elt}}{$key}})){
						#print "$k: ${${$results{$elt}}{$key}}{$k}\n";
					#}
			#}
	#}
}

my @AB;
my @BC;
my @AC;
my @order;

	for my $elt(sort keys(%results)){
		my $deg;
		($deg) = ($elt =~ m/([0-9]+)/);	
		push(@order, $deg);
		for my $key (sort keys(%{$results{$elt}})){
				if($key eq "AB" || $key eq "BA"){
					push(@AB,\%{${$results{$elt}}{$key}});
					#for my $k(sort keys(%{${$results{$elt}}{$key}})){
							#print "$k: ${${$results{$elt}}{$key}}{$k}\n";
					#}
				}
				if($key eq "BC" || $key eq "CB"){
					push(@BC,\%{${$results{$elt}}{$key}});
				}
				if($key eq "AC" || $key eq "CA"){
					push(@AC,\%{${$results{$elt}}{$key}});
				}
		}
	}

my $filename="all.results_ab.txt";	
open(OUT_ALL, ">$filename") or die($!);
print OUT_ALL "similarity_degree\tcolor_match\tmotif2\tmotif3\tmotif4\tisoclass3\tisoclass2\tisoclass10\tisoclass9\tisoclass8\tisoclass7\tisoclass6\tisoclass4\n";
for(my $i=0; $i<scalar(@order); $i++){
		my %AB_h=%{$AB[$i]};
		print OUT_ALL "$order[$i]\tAB\t$AB_h{motif2}\t$AB_h{motif3}\t$AB_h{motif4}\t$AB_h{isoclass3}\t$AB_h{isoclass2}\t$AB_h{isoclass10}\t$AB_h{isoclass9}\t$AB_h{isoclass8}\t$AB_h{isoclass7}\t$AB_h{isoclass6}\t$AB_h{isoclass4}\n";
		my %BC_h=%{$BC[$i]};
		print OUT_ALL "$order[$i]\tBC\t$BC_h{motif2}\t$BC_h{motif3}\t$BC_h{motif4}\t$BC_h{isoclass3}\t$BC_h{isoclass2}\t$BC_h{isoclass10}\t$BC_h{isoclass9}\t$BC_h{isoclass8}\t$BC_h{isoclass7}\t$BC_h{isoclass6}\t$BC_h{isoclass4}\n";
		my %AC_h=%{$AC[$i]};
		print OUT_ALL "$order[$i]\tAC\t$AC_h{motif2}\t$AC_h{motif3}\t$AC_h{motif4}\t$AC_h{isoclass3}\t$AC_h{isoclass2}\t$AC_h{isoclass10}\t$AC_h{isoclass9}\t$AC_h{isoclass8}\t$AC_h{isoclass7}\t$AC_h{isoclass6}\t$AC_h{isoclass4}\n";
}

close OUT_ALL;

#R code
my $cmd ="R CMD BATCH --no-save \"--args $filename\" make.graph.r log_make.graph.r";
`$cmd`;

my %results2;

my $dir2= "esophagus_results";

opendir (DIR2, $dir2) or die "Can't open $dir2: $!";
my @files2 = sort grep (/unifrac_res/,  readdir(DIR2));

foreach my $file2 (@files2){
	#print "$file\n";
	open(RFILE, "$dir2/$file2") or die "$file2 = $!";
	
	my %color_match;
	my $blast;
	($blast) = ($file2 =~ m/([0-9]+)/);	
	my $id="esophagus_".$blast;

	#print "$id\n";
	while(my $ligne =<RFILE>){
		chomp($ligne);
		
		if($ligne =~/^match/){
			my %values;
			my $ligne2=<RFILE>;
			chomp($ligne2);
			my @tabligne2=split('\t', $ligne2);
			my $color1= $tabligne2[0];
			
			my $ligne3=<RFILE>;
			chomp($ligne3);
			my @tabligne3=split('\t', $ligne3);
			my $color2= $tabligne3[0];
			
			my $ligne_suite;
			#print"new\n";
			
			while($ligne_suite =<RFILE>){
				chomp($ligne_suite);
				
				if($ligne_suite =~ /motif 2/){
					#print "$ligne_suite\n";
					my $ligne_val=<RFILE>;
					chomp($ligne_val);
					#print "$ligne_val\n";
					$values{"motif2"}=$ligne_val;
				}
				
				if($ligne_suite =~ /motif 3$/){
					#print "$ligne_suite\n";
					my $ligne_val3=<RFILE>;
					chomp($ligne_val3);
					#print "$ligne_val3\n";
					$values{"motif3"}=$ligne_val3;
				}
				
				if($ligne_suite =~ /motif 4$/){
					#print "$ligne_suite\n";
					my $ligne_val4=<RFILE>;
					chomp($ligne_val4);
					#print "$ligne_val4\n";
					$values{"motif4"}=$ligne_val4;
				}
				
				if($ligne_suite =~ /motif 3 isoclass/){
					#print "motif 3 isoclass\n";
					my $ligne_c=<RFILE>;
					my @isoclass=("isoclass3","isoclass2");
					for (my $i=0; $i<scalar(@isoclass); $i++){
						$ligne_c=<RFILE>;
						chomp($ligne_c);
						my @ligne_tab=split('\t', $ligne_c);
						my $isoclassval =$ligne_tab[5];
						#print"$isoclassval\n";
						$values{$isoclass[$i]}=$isoclassval;
					}
				}
				
				if($ligne_suite =~ /motif 4 isoclass/){
					my @isoclass=("isoclass10","isoclass9","isoclass8","isoclass7","isoclass6","isoclass4");
					my $ligne_c=<RFILE>;
					#print"motif 4 isoclass\n";
					for (my $i=0; $i<scalar(@isoclass); $i++){
						$ligne_c=<RFILE>;
						chomp($ligne_c);
						my @ligne_tab=split('\t', $ligne_c);
						my $isoclassval =$ligne_tab[5];
						#print"$isoclassval\n";
						$values{$isoclass[$i]}=$isoclassval;
					}
					last;
				}
				
			}
			
			$color_match{"$color1"."$color2"}=\%values;
			
			#for my $key (sort keys(%color_match)){
				#print"$key: $color_match{$key}\n";
					#for my $k(sort keys(%{$color_match{$key}})){
						#print "$k: $values{$k}\n";
					#}
			#}
		}
		
	}
	$results2{$id}=\%color_match;
	#for my $elt(sort keys(%results2)){
		#print "$elt: \n";
				#for my $key (sort keys(%{$results2{$elt}})){
				#print"$key:\n";
					#for my $k(sort keys(%{${$results2{$elt}}{$key}})){
						#print "$k: ${${$results2{$elt}}{$key}}{$k}\n";
					#}
			#}
	#}
}

my @BD2;
my @BC2;
my @CD2;
my @order2;

	for my $elt(sort keys(%results2)){
		my $deg;
		($deg) = ($elt =~ m/([0-9]+)/);	
		push(@order2, $deg);
		for my $key (sort keys(%{$results2{$elt}})){
				if($key eq "BD" || $key eq "DB"){
					push(@BD2,\%{${$results2{$elt}}{$key}});
					#for my $k(sort keys(%{${$results2{$elt}}{$key}})){
							#print "$k: ${${$results2{$elt}}{$key}}{$k}\n";
					#}
				}
				if($key eq "BC" || $key eq "CB"){
					push(@BC2,\%{${$results2{$elt}}{$key}});
				}
				if($key eq "CD" || $key eq "CD"){
					push(@CD2,\%{${$results2{$elt}}{$key}});
				}
		}
	}

my $filename2="all.results_esophagus.txt";	
open(OUT_ALL, ">$filename2") or die($!);
print OUT_ALL "similarity_degree\tcolor_match\tmotif2\tmotif3\tmotif4\tisoclass3\tisoclass2\tisoclass10\tisoclass9\tisoclass8\tisoclass7\tisoclass6\tisoclass4\n";
for(my $i=0; $i<scalar(@order2); $i++){
		my %AB_h=%{$BD2[$i]};
		print OUT_ALL "$order2[$i]\tBD\t$AB_h{motif2}\t$AB_h{motif3}\t$AB_h{motif4}\t$AB_h{isoclass3}\t$AB_h{isoclass2}\t$AB_h{isoclass10}\t$AB_h{isoclass9}\t$AB_h{isoclass8}\t$AB_h{isoclass7}\t$AB_h{isoclass6}\t$AB_h{isoclass4}\n";
		my %BC_h=%{$BC2[$i]};
		print OUT_ALL "$order2[$i]\tBC\t$BC_h{motif2}\t$BC_h{motif3}\t$BC_h{motif4}\t$BC_h{isoclass3}\t$BC_h{isoclass2}\t$BC_h{isoclass10}\t$BC_h{isoclass9}\t$BC_h{isoclass8}\t$BC_h{isoclass7}\t$BC_h{isoclass6}\t$BC_h{isoclass4}\n";
		my %AC_h=%{$CD2[$i]};
		print OUT_ALL "$order2[$i]\tCD\t$AC_h{motif2}\t$AC_h{motif3}\t$AC_h{motif4}\t$AC_h{isoclass3}\t$AC_h{isoclass2}\t$AC_h{isoclass10}\t$AC_h{isoclass9}\t$AC_h{isoclass8}\t$AC_h{isoclass7}\t$AC_h{isoclass6}\t$AC_h{isoclass4}\n";
}

close OUT_ALL;
my $cmd2 ="R CMD BATCH --no-save \"--args $filename2\" make.graph.eso.r log_eso_make.graph.r";
`$cmd2`;


