##################################################
# SELECTIONNER ET RECODER LES VARIABLES
# (Anton Perdoncin, EHESS, ERC Lubartworld)
##################################################
  
# VARIABLE SOCIOPRO et DEMO ---- 

# Age de PI ----

d <- d %>% 
  mutate(age = case_when(
    age5 == 15 ~ "15-29a",
    age5 == 30 ~ "30-39a",
    age5 == 40 ~ "40-49a",
    age5 %in% c(50, 60) ~ "+50a"))

# Sexe de PI ----
  
d <- d %>% 
  mutate(genre = case_when(
    sexe == 1 ~ "Hommes",
    sexe == 2 ~ "Femmes"))

# Diplome de PI ----

d <- d %>% 
  mutate(diplome = case_when(
    dip11 == 10 | dip11 == 11 ~ "L3 et sup",
    dip11 == 30 | dip11 == 31 | dip11 == 33 ~ "L2 ou équiv",
    dip11 == 41 ~ "Bac général",
    dip11 == 42 ~ "Bac techno/pro",
    dip11 == 50 ~ "CAP/BEP",
    dip11 == 60 | dip11 == 70 ~ "Brevet collèges ou CEP",
    dip11 == 71 ~ "Sans diplôme"))

# Nationalité de PI ----

d <- d %>% 
  mutate(nationalite = case_when(
    nfrred == 1 ~ "Français de naissance",
    nfrred == 2 ~ "Français naturalisés",
    nfrred == 3 ~ "Etrangers"))

# Groupe sociopro de PI ----

# PCS niveau 1

d <- d %>% 
  mutate(gsp = case_when(
    csp %in% c(31:38) ~ "CPIS",
    csp %in% c(42:48) ~ "Professions intermédiaires",
    csp %in% c(52:56) ~ "Employé-e-s",
    csp %in% c(62:69) ~ "Ouvriers"))

# CS ad hoc

d <- d %>% 
  mutate(sociopro = case_when(
    csp %in% c(33:45, 48) ~ "Cadres et autres professions intermédiaires",
    csp == 46 ~ "Professions intermédiaires du privé",
    csp == 47 ~ "Technicien-ne-s",
    csp %in% c(52:53) ~ "Employés du public",
    csp == 54 ~ "Employés admin du privé",
    csp == 55 ~ "Employés commerciaux du privé",
    csp == 56 ~ "Employés services aux particuliers",
    csp == 62 ~ "Ouvriers qual industrie",
    csp == 63 ~ "Ouvriers qual artisanat",
    csp == 64 ~ "Chauffeurs",
    csp == 65 ~ "Ouvriers qual manut, magasin, transport",
    csp == 67 ~ "Ouvriers non qual industrie",
    csp %in% c(68:69) ~ "Ouvriers non qual artisanat et agric"))

# Type de ménage ----

d <- d %>% 
  mutate(menage = case_when( # renommer et regrouper la variable type de ménage
    typmen7 == 1 ~ "Personne seule",
    typmen7 == 2 ~ "Famille monoparentale",
    typmen7 == 3 ~ "Couple sans enfant",
    typmen7 == 4 ~ "Couple avec enfant(s)",
    typmen7 %in% c(5:9) ~ "Autres ménages"))

# Vérification des recodages ----

freq(d$age)
freq(d$genre)
freq(d$diplome) # 24 NA
freq(d$nationalite)
freq(d$gsp) # 13 NA
freq(d$sociopro) # 13 NA
freq(d$menage)

# VARIABLE EMPLOI / TRAVAIL

# Posiion hiérarchique et professionnelle dans l'emploi ---- 

d <- d %>% 
  mutate(position = case_when(
    qprc == 1 ~ "Manoeuvres ou OS", 
    qprc == 2 ~ "Ouvriers qual, techniciens d'atelier", 
    qprc == 3 ~ "Techniciens", 
    qprc == 4 ~ "Employés", 
    qprc %in% c(5:6)  ~ "Agents maîtrise, ingénieurs, cadres", 
    qprc %in% c(7:9)  ~ "Autres, nr"))

# Choix du contrat de travail ---- 

d <- d %>% 
  mutate(choix = case_when(
    rdet == 1 ~ "Contrat choisi",
    rdet == 2 ~ "Contrat subi"))

# Quotité et type d'horaires de travail hebdomadaire ----

d <- d %>% 
  mutate(horaires = case_when(
    duhab %in% c(1:3) ~ "Temps partiel",
    duhab %in% c(4:5) ~ "Temps complet -35h",
    duhab == 6 ~ "Temps complet 35-39h",
    duhab == 7 ~ "Temps complet +40h",
    duhab == 9 ~ "Pas d'horaire habituel")) %>% 
  mutate(heures_plus = case_when(
    stplc == 1 ~ "Souhaite plus d'heures",
    stplc == 2 ~ "Souhaite pas plus d'heures"))

# Vérification des recodages ----

freq(d$position) # 20 NA
freq(d$choix)
freq(d$horaires)
freq(d$heures_plus) # 3 NA


# VARIABLES EMPLOYEURS  ----

# Public / Privé ----

d <- d %>% 
  mutate(public_prive = case_when(
    chpub == "1" ~ "Employeur privé",
    chpub %in% c(2:5) ~ "Employeur public"))

# Secteur d'activité ----

# nomenclature NAF : intérimaires reclassés dans le secteur de l'entreprise utilisatrice

d <- d %>% 
  mutate(secteur = case_when(
    nafg017un == "C1" ~ "Fabrication alimentaire et tabacs",
    nafg017un == "C3" ~ "Fab élec, info, machines",
    nafg017un == "C4" ~ "Fab matériel transp",
    nafg017un == "C5" ~ "Fab autres prod indus",
    nafg017un == "DE" ~ "Energie, eau, déchets, dépol",
    nafg017un == "FZ" ~ "Construction",
    nafg017un == "GZ" ~ "Commerce",
    nafg017un == "HZ" ~ "Transports et entreposage",
    nafg017un == "IZ" ~ "Héberg et restauration",
    nafg017un == "MN" ~ "Activ admin scientifqs et technqs",
    nafg017un == "OQ" ~ "Admin publiques",
    TRUE ~ "Autres secteurs"))

# Vérification des recodages ----

freq(d$nafg017un)
freq(d$secteur)


# DEFINITION DE LA SOUS-POPULATION ----

interim <- d %>% 
filter(acteu == 1, #  on ne conserve que les actifs occupés
       statut == 21, # et les intérimaires
       chpub != 7) # et on enlève les salariés de particuliers employeurs
  


# EXPORT DONNEES RECODEES ----

interim_extr <- interim %>% 
  select(
    age, genre, diplome, nationalite, gsp, sociopro, menage,
    position, choix, horaires, heures_plus,
    nafg017un, secteur) %>% 
  write_csv2("data_prod/ee18_extraits_recode.csv")
