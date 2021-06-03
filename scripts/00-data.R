# importer les données depuis le site de l'Insee

url <- "url-to-your-zip"
path_zip <- "your-downloaded-zip-local-path"
path_unzip <- "path-where-to-save-unzip-files"
destfile <- "archive.zip"

# download zip
curl::curl_download(url, destfile = paste(path_zip, destfile, sep = "/"))

#unzip
unzip(destfile, exdir = path)

# list all files
files <- list.files(path = path_unzip)

# apply map_df() to iterate read_csv over files
data <- map_df(paste(path_unzip, files, sep = "/"),
               read_csv,
               ## additional params to read_csv here
)

