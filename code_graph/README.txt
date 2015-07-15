-  Lignes de commande avec mothur:

unifrac.unweighted(tree=abrecovery.paup.nj, group=abrecovery.groups, groups=all) 
unifrac.weighted(tree=abrecovery.paup.nj, group=abrecovery.groups)

unifrac.unweighted(tree=esophagus.tree, group=esophagus.good.groups, groups=all) 
unifrac.weighted(tree=esophagus.tree, group=esophagus.good.groups)

2- Fichiers: unifrac_analysis.pl, 
                    code_general_unifrac.r, 
                    parse_result_and_create_graph_no.pl, 
                    make.graph.r, 
                    make.graph.eso.r,

    Lignes de commande: 
    
    Pour l'analyse de l'unifrac_reseau pour un % en particulier:
    perl unifrac_analysis.pl fichier_blast %identité fichier.group
    
    Pour parser les fichiers de résultats et générer le fichier commun du style    "data.frame" et créer les graphiques:
   mkdir esophagus_results
   mkdir abrecovery_results
   mv *blast.results2* esophagus_results/
   mv *blast.results_ab* abrecovery_results/

   perl parse_result_and_create_graph_no.pl
