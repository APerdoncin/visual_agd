##################################################
# REALISER ET VISUALISER UNE ACM
# (Anton Perdoncin, EHESS, ERC Lubartworld)
##################################################


# REALISATION DE L'ACM ----

# Sélection des variables de l'analyse ----

d_acm <- interim %>% 
  select("age", "genre", "diplome", "nationalite", "gsp", "sociopro", "menage", 
         "position", "choix", "horaires", "heures_plus", 
         "public_prive", "secteur") %>% 
  mutate_all(factor) %>%  # car la fonction MCA ne mange que du "factor"
  drop_na() # enlever toutes les NA (57 individus) ; autres solutions possibles : ACM spécifique (speMCA dans le package GDAtools)

# Visualisation rapide du jeu de données ----

set.seed(999) # fixer l'aléa

d_acm %>%
  sample_n(50) %>% # échantillon aléatoire de 50 lignes
  flextable() %>% 
  autofit() %>% 
  set_caption("Extrait du jeu de données")

# Réalisation de l'analyse ----

res_acm <- MCA(d_acm, quali.sup = 5:7)

# première visualisation des résultats ----

# On peut prendre un peu de temps pour regarder les premières sorties statistiques "par défaut" pour se rendre compte de ce que l'on vient de faire... et du fait que ces graphiques ne sont pas publiables en l'état.

# Autre possibilité : le package factoextra : http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization

# Encore une autre possibilité : le package explor (Julien Barnier)
# https://cran.r-project.org/web/packages/explor/vignettes/introduction_fr.html
# https://quanti.hypotheses.org/1171

explor(res_acm)

# DESCRIPTION STATISTIQUE DES AXES ----

variances <- as.data.frame(res_acm$eig) %>%
  rownames_to_column() %>% # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
  slice(1:10) %>% # conserver que les infos des 10 premiers axes
  mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
  select(-rowname) %>%# on enlève cette colonne dont on n'a plus besoin
  rename(`Valeurs propres` = eigenvalue) %>%
  rename(`% de variance` = `percentage of variance`) %>% # on renomme les autres colonnes
  rename(`% cumulé de variance` = `cumulative percentage of variance`) %>% 
  mutate(Axes = fct_relevel(Axes, paste("Axe", 1:10))) %>% # pour que l'ordre de 1 à 10 soit bien respecté dans les graphiques
  select(Axes, `Valeurs propres`, `% de variance`, `% cumulé de variance`)

variances %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 2) %>% 
  autofit() %>% 
  set_caption("Valeurs propres et variances le long des axes")

# pour exporter ce tableau, plusieurs solutions possibles, notamment : 
write_csv2(variances, "sorties/tab-variances.csv")


# Diagrammes des sauts de variance ----

ggplot(variances, aes(x = Axes)) + # initialisation du graphique et de l'axe horizontal
  geom_bar(aes(y = `% de variance`),   # on indique le type de graphique (barplot) et la variable à représenter sur l'axe vertical
           stat = "identity", 
           fill = "red") + # parce que j'aime bien le rouge
  xlab("") + # on enlève le label de l'axe des x, pas très utile
  ylab("% de variance") + # on renomme proprement le label de l'axe des y
  theme_minimal() # un des thèmes possibles dans ggplot, que j'aime bien car il est... minimaliste !

# % cumulé de variance

ggplot(variances, aes(x = Axes)) +
  geom_bar(aes(y = `% cumulé de variance`), 
           stat = "identity", 
           fill = "blue") + 
  xlab("") +
  ylab("% cumulé de variance") +
  theme_minimal()

# Seuil de contribution ----

seuil <- 100 / nrow(res_acm$var$coord)

# Ce seuil est bien évidemment arbitraire, on ne discutera pas ici de ce point. Le seuil ici retenu est la contribution moyenne (1 / nombre de modalités actives). Le principe est simple : une modalité est dite "contributive" si elle "pèse" plus lourd sur un axe que son poids moyen dans l'ensemble du nuage d'origine. 


# MISE EN FORME DES RESULTATS STATISTIQUES ----

# L'objectif est de parvenir à créer un grand tableau qui contienne autant de lignes que de modalités actives et/ou supplémentaires. Chacune de ces modalités est décrite par sa fréquence dans le jeu de données, et par les indicateurs usuels de l'AGD pour les premiers axes factoriels (contribution, coordonnée, cosinus carré, v.test).

# Ce tableau peut être exporté dans un tableur afin de servir de support à l'interprétation des axes. Rappelons que l'interprétation statistique des axes doit impérativement précéder l'interprétation graphique, faute de quoi on s'expose à commettre de grosses erreurs d'interprétation. 

# Ce tableau peut aussi, en étant légèrement mis en forme, être utilisé dans une annexe de mémoire, de thèse, ou dans une publication. Pour la raison évoquée dans le paragraphe précédent, cela devrait même être obligatoire... 


# Fréquences des modalités actives et supplémentaires ----

# Première étape : récupérer les fréquences de chaque modalité. ce faisant (grâce à la fonction map), on récupère aussi la liste de toutes les modalités du jeu de données, ainsi que les variables auxquelles elles appartiennent. Ce sera très utile plus bas pour dessiner des symboles différents par variable... 

frequences <- d_acm %>% 
  pivot_longer(everything(),
               names_to = "variables", 
               values_to = "modalites") %>%  # compter toutes les occurences des couples variable/modalite
  count(variables, modalites) %>% # compter le nombre de couples "variable/modalité" unique (donc le nombre d'individus par modalité du jeu de données)
  group_by(variables) %>% 
  mutate(pourcentage = round(100 * n / nrow(d_acm), 1)) %>% # calculer des pourcentages pour chaque groupe de variable
  ungroup() %>% 
  select(variables, modalites, n, pourcentage)  # sélectionner les variables dans un ordre plus lisible

frequences %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 1) %>% 
  autofit() %>% 
  set_caption("Tableau de fréquence de l'ensemble des variables du jeu de données")

write_csv2(frequences, "sorties/tab-frequences.csv")


# Modalités actives ----

# Deuxième étape : récupérer les indicateurs statistiques des modalités actives.

# Coordonnées (modalités actives) ----

coordonnees <- as_tibble(res_acm$var$coord,
                         rownames = "modalites") %>%  # récupérer les coordonnées des modalités actives
  mutate_if(is.numeric, round, digits = 2) %>%  # arrondir à 2 décimales les variables numériques (c'est bien suffisant)
  rename_all(tolower) %>% # tout en minuscules
  rename_all(~ str_replace(., " ", "")) %>% # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1" 
  rename_if(is.numeric, ~ str_c(., "coord", sep = "_")) # ajouter le suffixe _coord à chaque nom de variable (sauf la variable modalites). On obtient ainsi par exemple "dim1_coord"

# Contributions (modalités actives) ----
contributions <- as_tibble(res_acm$var$contrib,
                           rownames = "modalites") %>%  # récupérer les coordonnées des modalités actives
  mutate_if(is.numeric, round, digits = 2) %>%  # arrondir à 2 décimales les variables numériques (c'est bien suffisant)
  rename_all(tolower) %>% # tout en minuscules
  rename_all(~ str_replace(., " ", "")) %>% # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1" 
  rename_if(is.numeric, ~ str_c(., "contrib", sep = "_")) # idem sauf qu'ici on obtient "dim1_contrib"

# Cosinus carrés (modalités actives) ----
cos2 <- as_tibble(res_acm$var$cos2,
                   rownames = "modalites") %>%  # récupérer les coordonnées des modalités actives
  mutate_if(is.numeric, round, digits = 2) %>%  # arrondir à 2 décimales les variables numériques (c'est bien suffisant)
  rename_all(tolower) %>% # tout en minuscules
  rename_all(~ str_replace(., " ", "")) %>% # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1" 
  rename_if(is.numeric, ~ str_c(., "cos2", sep = "_")) # idem sauf qu'ici on obtient "dim1_cos2"


# vtest (modalités actives) ----
vtest <- as_tibble(res_acm$var$v.test,
                   rownames = "modalites") %>%  # récupérer les coordonnées des modalités actives
  mutate_if(is.numeric, round, digits = 2) %>%  # arrondir à 2 décimales les variables numériques (c'est bien suffisant)
  rename_all(tolower) %>% # tout en minuscules
  rename_all(~ str_replace(., " ", "")) %>% # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1" 
  rename_if(is.numeric, ~ str_c(., "vtest", sep = "_")) # idem sauf qu'ici on obtient "dim1_vtest"

# Assemblage des résultats statistiques (modalités actives) ----

resultats_actives <- frequences %>% 
  right_join(coordonnees) %>% 
  right_join(contributions) %>% 
  right_join(cos2) %>% 
  right_join(vtest) %>% # fusionner les jeux de données ; la clé de fusion (implicite) est la variable "modalites", qui est commune à tous. 
  mutate(type = "Variable active") %>% # ajout d'une colonne contenant la chaîne de caractères "Variable active" (pour pouvoir distinguer plus tard avec les variables supplémentaires)
  select(type, variables, modalites, n, pourcentage,
         contains("dim1"), contains("dim2"),
         contains("dim3"), contains("dim4")) %>% # conserver et réorganiser les variables pertinentes axe par axe
  mutate(variables = fct_recode(variables, # renommer les noms des variables actives afin qu'elles soient lisibles et "publiables"
                                "Age" = "age",
                                "Genre" = "genre", 
                                "Diplôme" = "diplome", 
                                "Nationalité" = "nationalite",
                                "Position dans l'emploi" = "position", 
                                "Choix du contrat" = "choix", 
                                "Horaire de travail" = "horaires", 
                                "Souhaite travailler plus" = "heures_plus", 
                                "Public/Privé" = "public_prive", 
                                "Secteur d'activité" = "secteur"))


resultats_actives %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 1) %>% 
  autofit() %>% 
  set_caption("Résultats statistiques variables activess")


write_csv2(resultats_actives, path = "sorties/tab-resultats_actives.csv")


# Modalités supplémentaires ----

# Coordonnées (modalités supplémentaires) ----

coordonnees_sup <- as_tibble(res_acm$quali.sup$coord,
                         rownames = "modalites") %>%  # la démarche est la même que supra, mais avec le sous-objet quali.sup qui stocke les informations sur les variables qualitatives supplémentaires
  mutate_if(is.numeric, round, digits = 2) %>%  
  rename_all(tolower) %>%
  rename_all(~ str_replace(., " ", "")) %>%
  rename_if(is.numeric, ~ str_c(., "coord", sep = "_"))

# Cosinus carrés (modalités actives) ----
cos2_sup <- as_tibble(res_acm$quali.sup$cos2,
                  rownames = "modalites") %>%  
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename_all(tolower) %>% 
  rename_all(~ str_replace(., " ", "")) %>%
  rename_if(is.numeric, ~ str_c(., "cos2", sep = "_")) 

# vtest (modalités actives) ----
vtest_sup <- as_tibble(res_acm$quali.sup$v.test,
                   rownames = "modalites") %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename_all(tolower) %>%
  rename_all(~ str_replace(., " ", "")) %>%
  rename_if(is.numeric, ~ str_c(., "vtest", sep = "_")) 

# Assemblage des résultats statistiques (modalités actives) ----

resultats_sup <- frequences %>% 
  right_join(coordonnees_sup) %>% 
  right_join(cos2_sup) %>% 
  right_join(vtest_sup) %>% 
  mutate(type = "Variable supplémentaire") %>% # comme supra pour le tableau des résultats des modalités actives : on distingue ici le type de variable.
  select(type, variables, modalites, n, pourcentage,
         contains("dim1"), contains("dim2"),
         contains("dim3"), contains("dim4")) %>% 
  mutate(variables = fct_recode(variables,
                                "Groupe socio-professionnel" = "gsp",
                                "Catégorie socio-professionnelle" = "sociopro", 
                                "Type de ménage" = "menage"))

resultats_sup %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 1) %>% 
  autofit() %>% 
  set_caption("Résultats statistiques variables supplémentaires")


write_csv2(resultats_sup, path = "sorties/tab-resultats_sup.csv")

# Assemblage du tableau complet de résultats (variables actives et supplémentaires) ----

# On colle les lignes des tableaux contenant tous les résultats statistiques des variables actives et supplémentaires. On a donc dans un seul et même objet toutes les informations nécessaires pour 1/ interpréter correctement les axes et 2/ faire de beaux graphiques !

resultats_complet <- bind_rows(resultats_actives, resultats_sup) %>% 
  write_csv2(resultats_actives, path = "sorties/tab-resultats_complet.csv")


# VISUALISATIONS GRAPHIQUES ----

# Nuage des modalités actives ----

# Nuage des modalités actives simple ----

resultats_actives %>% 
  filter(dim1_contrib > seuil |
           dim2_contrib > seuil) %>% # on part du tableau de résultat des modalités actives et on filtre uniquement celles dont la contribution dépasse le seuil pour l'un ou l'autre des deux axes (| est l'opérateur logique OU).
  
  ggplot(aes(x = dim1_coord, y = dim2_coord, # initialisation du graphique
             label = modalites, # les labels des points sont les modalités
             shape = variables)) +  # les formes des points dépendent des variables : à chaque variable son symbole
  
  geom_point() + # tracer les points
  coord_fixed() + # pour que les échelles des axes soient identiques
  geom_text_repel(segment.alpha = 0.5) + # tracer les labels, en utilisant cette fonction du package ggrepel qui permet de s'assurer qu'il n'y a pas de chevauchement. segment.alpha : transparence du petit tiret qui relie si besoin le libellé au point
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne horizontale y = 0
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne verticale x = 0
  
  xlab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
  ylab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) + # idem pour l'axe vertical
  
  scale_shape_manual(name = "", values = 0:20) + # sélection des types de symboles 
  
  guides(shape=guide_legend(title = "")) + # paramètres de la légende : pas de titre
  
  theme_minimal() + # mise en forme globale du graphique ; theme_minimal est la plus "sobre" mais d'autres sont possibles... 
  
  theme(legend.position="bottom") # pour que la légende se place sous le graphique.

# Nuage des modalités actives : points proportionnels aux effectifs ----

resultats_actives %>% 
  filter(dim1_contrib > seuil |
           dim2_contrib > seuil) %>% 
  
  ggplot(aes(x = dim1_coord, y = dim2_coord, 
             label = modalites,
             shape = variables, 
             size = n)) + # il suffit d'ajouter ce paramètre : la taille des points s'ajuste à la variable n (effectifs)
  
  geom_point() +
  coord_fixed() + # pour que les échelles des axes soient identiques
  geom_text_repel(size = 3, segment.alpha = 0.5) +
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  ylab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  
  scale_shape_manual(name = "", values = 0:20) +
  guides(shape=guide_legend(title = ""), size = FALSE) + # paramètres de la légende : pour ne pas que les échelles de taille s'affichent dans la légende
  
  theme_minimal() +
  theme(legend.position="bottom")

# On pourrait multiplier les types de graphiques différents, en modifiant juste le paramètre "size"
# points proportionnels aux contributions sur l'axe 1 ou l'axe 2, ou à la moyenne des contributions sur les deux axes
# points proportionnels aux cos2, etc.

# On pourrait aussi colorer les symboles et les labels en fonction des effectifs, en utilisant des nuances d'une couleur :

resultats_actives %>% 
  filter(dim1_contrib > seuil |
           dim2_contrib > seuil) %>% 
  
  ggplot(aes(x = dim1_coord, y = dim2_coord, 
             label = modalites,
             shape = variables, 
             color = n)) + # il suffit de remplacer size par color
  
  geom_point() +
  coord_fixed() + # pour que les échelles des axes soient identiques
  geom_text_repel(size = 3, segment.alpha = 0.5) +
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  ylab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  
  scale_shape_manual(name = "", values = 0:20) +
  scale_color_gradient(name = "", 
                       low = "#FFCC33", high = "#FF3300" # un exemple de modification de couleur du gradient
  ) +
  guides(shape=guide_legend(title = "")) + 
  
  theme_minimal() +
  theme(legend.position="bottom")


# Nuage des modalités actives et supplémentaires ----

resultats_complet %>% 
  filter(dim1_contrib > seuil |
           dim2_contrib > seuil |
           is.na(dim2_contrib) & dim1_coord > 0.29 |
           is.na(dim2_contrib) & dim1_coord < -0.31) %>% # on part du tableau complet et on sélectionne, en plus des modalités actives qui passent le seuil, les modalités supplémentaires dont les coordonnées s'écartent de +/- 0.3 du barycentre (c'est parfaitement arbitraire...).
  
  ggplot(aes(x = dim1_coord, y = dim2_coord, 
             label = modalites,
             shape = variables,
             colour = type, # on distingue par des couleurs différentes les variables actives et supplémentaires
             size = n)) + 
  
  geom_point() +
  coord_fixed() + # pour que les échelles des axes soient identiques
  geom_text_repel(size = 3, segment.alpha = 0.5) +
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Axe 1 (", round(variances[1, 2], 1), " %)")) +
  ylab(paste0("Axe 2 (", round(variances[2, 2], 1), " %)")) +
  
  scale_shape_manual(name="", values = 0:20) +
  scale_color_manual(values = c("black", "darkgrey")) + # paramètres de couleur ; tout est possible ici... à vous de tester les lignes ci-dessous par exemple :
  # scale_color_brewer(palette = "Set1") +
  # scale_color_grey() +
  # scale_color_brewer(palette = "Accent")
  
  guides(shape = guide_legend(title="Nom des variables (actives et supplémentaires)", # titre de la légende des noms de variable
                              title.position = "top"), 
         colour = guide_legend(title = "Type de variable", # titre de la légende distinguant actives et supplémentaires
                               title.position = "top",
                               nrow = 2),
         size = FALSE) + # toujours pas de légende pour les tailles de point
  
  theme_minimal() +
  theme(legend.position="bottom")


# Nuage des individus ----


indiv12 <- as_tibble(res_acm$ind$coord[,1:2]) # récupérer les coordonnées des individus sur les axes.

ggplot(indiv12, aes(x = `Dim 1`, y = `Dim 2`)) + # initialisation du graphique 
  
  geom_point(alpha = 0.6, # alpha : permet une certaine transparence des points, pour mieux voir les superpositions. On pourrait aussi changer la taille des points avec l'argument size = 1 (par exemple).
             colour = "#E41A1C") + # couleur hexadécimale tout à fait arbitraire... les goûts et les couleurs...  
  coord_fixed() + # pour que les échelles des axes soient identiques
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  ylab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  
  guides(colour = FALSE) + # pas de légende pour le paramètre de couluer
  
  theme_minimal()


# Habillage du nuage des individus : points habillés selon le genre ----

indiv12_genre <- d_acm %>% 
  select(genre) %>% 
  bind_cols(indiv12) # pour habiller le nuage de points, on a besoin d'adjoindre aux coordonnées l'information avec laquelle on veut l'habiller. Ici le genre. On s'autorise cette manipulation uniquement car l'ordre des lignes n'a pas été modifié... 

ggplot(indiv12_genre, aes(x = `Dim 1`, y = `Dim 2`, 
                          colour = genre)) + # ce paramètre permet de colorier les points en fonction de la variable "genre"
  
  geom_point(alpha = 0.6) +
  coord_fixed() + # pour que les échelles des axes soient identiques
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  ylab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  
  scale_color_brewer(palette = "Set2") + # idem ici : tout est possible, à partir de RColorBrewer notamment. Voir display.brewer.all()
  
  guides(colour = guide_legend(title="")) + # pour enlever le titre de la légende
  
  theme_minimal()


# Habillage du nuage des individus : points habillés et ellipses de confiance ----

ggplot(indiv12_genre, aes(x = `Dim 1`, y = `Dim 2`, 
                          colour = genre)) + 
  
  geom_point(alpha = 0.6) +
  coord_fixed() + # pour que les échelles des axes soient identiques
  
  stat_ellipse() + # unique ajout par rapport au graphique précédent, permettant de tracer les ellipses
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  ylab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  
  scale_color_brewer(palette = "Set2") +
  
  guides(colour = guide_legend(title="")) +
  
  theme_minimal()




