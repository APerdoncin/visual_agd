##################################################
# Importer les données depuis le site de l'Insee
# (Anton Perdoncin, EHESS, ERC Lubartworld)
##################################################

# Définir les liens de téléchargement et d'extraction des données

url <- "https://www.insee.fr/fr/statistiques/fichier/4191029/fd_eec18_csv.zip"
path_zip <- "data_brut/zip"
path_unzip <- "data_brut"
destfile <- "archive.zip"

# Télécharger le fichier zip

### Enlever le commentaire de la ligne ci-dessous si vous souhaitez télécharger les données par vous-même
# curl_download(url, destfile = paste(path_zip, destfile, sep = "/"))

# Dé-ziper dans le dossier dat

unzip(zipfile = "data_brut/zip/archive.zip", 
      exdir = path_unzip)


# Importer le fichier csv

d <- read_csv2("data_brut/FD_csv_EEC18.csv") %>% 
  rename_all(tolower) # pour que les noms des variables soient minuscule

