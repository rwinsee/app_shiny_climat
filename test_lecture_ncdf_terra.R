library(terra)

url_nc <- "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_19900101-19991231.nc"
url_nc <- "https://github.com/justinesommerlatt/Hackathon-Meteo-France/blob/main/Isotherme0_data/anomaly_maps/freezing_days_anomaly_2020_2040.nc"
r <- rast(url_nc)
r
plot(r)

library(httr)
library(jsonlite)
library(terra)

# 1. Lister le contenu du dossier Tropical_data via l'API GitHub
api_url <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Tropical_data"

res <- GET(api_url)
stop_for_status(res)

files_meta <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

# 2. Garder seulement les .nc et récupérer les URL de téléchargement
nc_urls <- files_meta$download_url[grepl("\\.nc$", files_meta$name)]
nc_urls
