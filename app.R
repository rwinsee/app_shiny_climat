# Définit la liste des packages R nécessaires pour faire tourner l'application
liste_packages <- c(
  "shiny",        # Package pour créer l'application web
  "bslib",        # Package pour gérer le thème Bootstrap (apparence)
  "leaflet",      # Package pour afficher des cartes interactives
  "httr",         # Package pour appeler des API HTTP (ici : GitHub)
  "jsonlite",     # Package pour lire / écrire du JSON
  "terra",        # Package pour manipuler des données spatiales (raster, etc.)
  "raster",       # Ancien package raster, utile pour leaflet::addRasterImage
  "ncdf4",        # Package pour lire les fichiers NetCDF
  "shinyWidgets"  # Widgets supplémentaires pour Shiny (boutons, sliders, etc.)
)

# Parcourt la liste des packages :
# - si un package n'est pas installé, installe le package
# - charge ensuite chaque package dans la session R
for (pkg in liste_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)    # Installe le package manquant
  }
  library(pkg, character.only = TRUE)  # Charge le package
}

# Thème global de l'application Shiny
# Définit les couleurs, la police, etc. pour toute l'interface
app_theme <- bs_theme(
  version = 5,                      # Utilise Bootstrap 5
  base_font = font_google("Inter"), # Définit la police principale (Google Fonts)
  bg = "#2e2e2e",                   # Couleur de fond principale (gris foncé)
  fg = "#eaeaea",                   # Couleur du texte principal (gris clair)
  primary = "#9ae3c4",              # Couleur principale (boutons, éléments actifs)
  success = "#9ae3c4",              # Couleur "succès" (messages OK)
  info = "#8ecae6"                  # Couleur "info" (messages informatifs)
)

# Petite fonction utilitaire pour appeler une URL en évitant de faire planter l'application
# safe_get() :
# - appelle l'URL avec httr::GET
# - en cas d'erreur (API GitHub indisponible, problème réseau...), renvoie NULL
#   au lieu de provoquer une erreur bloquante
safe_get <- function(url) {
  res <- try(httr::GET(url), silent = TRUE)  # Tente l'appel à l'URL
  if (inherits(res, "try-error") || httr::http_error(res)) {
    warning("⚠️ Impossible d’accéder à ", url, " (GitHub API). Passage en mode fallback.")
    return(NULL)  # Renvoie NULL si l'appel échoue
  }
  res  # Renvoie la réponse si tout se passe bien
}

# ===========================
#  Nuits tropicales (NetCDF)
# ===========================

# URL de l'API GitHub qui liste les fichiers du dossier Tropical_data
api_url_trop <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Tropical_data"

# Appelle l'API de manière robuste (avec safe_get)
res_trop <- safe_get(api_url_trop)

if (!is.null(res_trop)) {
  # Si l'API répond correctement :
  #  - récupère le JSON
  #  - transforme le JSON en data.frame R
  files_meta_trop <- jsonlite::fromJSON(
    httr::content(res_trop, as = "text", encoding = "UTF-8")
  )
  
  # Sélectionne uniquement les fichiers qui se terminent par ".nc"
  # et récupère leur URL de téléchargement (download_url)
  tropical_nc_urls <- files_meta_trop$download_url[grepl("\\.nc$", files_meta_trop$name)]
  
  # Donne comme noms de vecteur les noms de fichiers (pratique pour les logs)
  names(tropical_nc_urls) <- files_meta_trop$name[grepl("\\.nc$", files_meta_trop$name)]
  
} else {
  # Si l’API GitHub ne répond pas dans le pod (par exemple en production),
  # met en place un "plan B" (fallback) en construisant un vecteur d'URLs à la main.
  # Objectif : garder un vecteur de longueur 11, compatible avec :
  #   - la fonction choose_nc_url() (indices 1, 5, 8, 11)
  #   - le mode "décennies" dans l'onglet Animations
  
  # Vecteur de base avec 4 fichiers représentatifs :
  #  - un fichier pour le climat 1990
  #  - un pour le climat 2020
  #  - un pour le climat 2050
  #  - un pour le climat 2080
  trop_base <- c(
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_19900101-19991231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_20200101-20291231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_20500101-20591231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_20800101-20891231.nc"
  )
  
  # Remplit un vecteur de 11 cases en approximant les décades :
  #  - indices  1 à  4 ≈ climat 1990
  #  - indices  5 à  7 ≈ climat 2020
  #  - indices  8 à 10 ≈ climat 2050
  #  - indice  11       ≈ climat 2080
  tropical_nc_urls <- rep(trop_base[1], 11)
  tropical_nc_urls[5]  <- trop_base[2]
  tropical_nc_urls[8]  <- trop_base[3]
  tropical_nc_urls[11] <- trop_base[4]
  
  # Donne comme noms de vecteur les noms de fichiers (partie après le dernier '/')
  names(tropical_nc_urls) <- basename(tropical_nc_urls)
}

# ======================================
#  Jours de gel (freezing days, NetCDF)
# ======================================

# URL de l'API GitHub qui liste les fichiers du dossier Isotherme0_data
api_url_froid <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Isotherme0_data"

# Appelle l'API pour ce dossier
res_froid <- safe_get(api_url_froid)

if (!is.null(res_froid)) {
  # Si l'API répond correctement, lit le JSON et construit un data.frame
  files_meta_froid <- jsonlite::fromJSON(
    httr::content(res_froid, as = "text", encoding = "UTF-8")
  )
  
  # Récupère les URLs de téléchargement pour les fichiers .nc
  freezing_nc_urls <- files_meta_froid$download_url[grepl("\\.nc$", files_meta_froid$name)]
  
  # Utilise les noms de fichiers comme noms du vecteur (pour les logs)
  names(freezing_nc_urls) <- files_meta_froid$name[grepl("\\.nc$", files_meta_froid$name)]
  
} else {
  # Fallback complet : définit à la main 11 fichiers, un par "décennie" approximative
  freezing_nc_urls <- c(
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_19900101-19991231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20000101-20091231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20100101-20141231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20150101-20191231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20200101-20291231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20300101-20391231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20400101-20491231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20500101-20591231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20600101-20691231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20700101-20791231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Isotherme0_data/freezing_days_per_year_20800101-20891231.nc"
  )
  # Associe les noms des fichiers au vecteur
  names(freezing_nc_urls) <- basename(freezing_nc_urls)
}

# =====================================
#  Grille Alpes (Alpes_grid.nc)
#  -> Sert à récupérer les coordonnées
# =====================================

# URL du fichier NetCDF contenant la grille (lon/lat) de la zone Alpes
grid_url <- "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Alpes_grid.nc"

# Définit le chemin local (dans le répertoire temporaire R) pour stocker la grille
grid_dest <- file.path(tempdir(), "Alpes_grid.nc")

# Télécharge le fichier de grille si nécessaire (si pas déjà présent en local)
if (!file.exists(grid_dest)) {
  download.file(grid_url, grid_dest, mode = "wb")
}

# Ouvre le fichier NetCDF de la grille avec ncdf4
nc_grid <- ncdf4::nc_open(grid_dest)

# Récupère la liste des noms de variables présentes dans le NetCDF
var_grid_names <- names(nc_grid$var)

# Essaie de détecter les variables de longitude et latitude
# en cherchant "lon" et "lat" dans les noms de variables (sans tenir compte de la casse)
lon_var_name <- var_grid_names[grepl("lon", var_grid_names, ignore.case = TRUE)][1]
lat_var_name <- var_grid_names[grepl("lat", var_grid_names, ignore.case = TRUE)][1]

# Si aucune variable lon/lat n'est trouvée, ferme le fichier et stoppe le script
if (is.na(lon_var_name) || is.na(lat_var_name)) {
  ncdf4::nc_close(nc_grid)
  stop("Impossible d'identifier lon/lat dans Alpes_grid.nc")
}

# Lit les valeurs de longitude et de latitude dans le NetCDF
lon_grid <- ncdf4::ncvar_get(nc_grid, lon_var_name)
lat_grid <- ncdf4::ncvar_get(nc_grid, lat_var_name)

# Ferme le fichier NetCDF
ncdf4::nc_close(nc_grid)

# Calcule la boîte englobante (bounding box) de la grille des Alpes :
# xmin, xmax, ymin, ymax = min/max des longitudes et latitudes
alpes_bbox <- c(
  xmin = min(lon_grid, na.rm = TRUE),
  xmax = max(lon_grid, na.rm = TRUE),
  ymin = min(lat_grid, na.rm = TRUE),
  ymax = max(lat_grid, na.rm = TRUE)
)

# Affiche la bbox dans la console, utile pour vérifier la zone
print(alpes_bbox)

# Fonction utilitaire qui choisit l’URL NetCDF adaptée à un type d’indicateur
# ("tropical" ou "freezing") et à une période climatique ("1981–2010", etc.)
choose_nc_url <- function(type = c("tropical", "freezing"), periode) {
  # Restreint l’argument `type` aux valeurs autorisées et renvoie une valeur unique
  type <- match.arg(type)
  
  # Sélectionne le vecteur d’URLs à utiliser selon le type d’indicateur
  vec <- if (type == "tropical") tropical_nc_urls else freezing_nc_urls
  
  # Associe chaque période climatique à un indice dans le vecteur d’URLs
  idx <- switch(
    periode,
    "1981–2010" = 1,   # Période historique -> premier fichier
    "2011–2040" = 5,   # Premier horizon de projection -> indice 5
    "2041–2070" = 8,   # Deuxième horizon -> indice 8
    "2071–2100" = 11,  # Troisième horizon -> indice 11
    1                  # Valeur par défaut si période inconnue
  )
  
  # on clip au cas où il y aurait moins de fichiers que prévu
  # (garantit un indice compris entre 1 et length(vec))
  idx <- max(1, min(idx, length(vec)))
  
  # Récupère l’URL NetCDF correspondant à l’indice calculé
  url <- vec[idx]
  
  # Écrit une ligne de log lisible dans la console
  message("→ Période sélectionnée : ", periode,
          " | type : ", type,
          " | fichier : ", names(vec)[idx])
  
  # Renvoie l’URL choisie
  url
}

# Palette couleur perso pour les jours de gel (freezing)
# Déclare une fonction qui génère un dégradé bleu correspondant :
#  - bleu clair (#B3DFFF) -> bleu moyen (#2EA8FF) -> bleu foncé (#004475)
pal_freezing_fn <- colorRampPalette(c("#B3DFFF", "#2EA8FF", "#004475"))


###############################################################################
# ui.R

# Déclare l’interface utilisateur principale sous forme de barre de navigation
ui <- navbarPage(
  # Définit le titre affiché dans la barre de navigation
  title = "Explor'Alpes",
  # Applique le thème global défini plus haut (couleurs, polices, etc.)
  theme = app_theme,
  # Donne un identifiant à la navbar (utile si besoin de la manipuler côté serveur)
  id = "navbar",
  # Autorise la navbar à se replier sur petits écrans (mode mobile)
  collapsible = TRUE,
  
  # Injecte du CSS personnalisé dans l’en-tête de la page HTML
  header = tags$head(
    tags$style(HTML("
      /* Applique un fond sombre et une police claire sur tout le layout */
      body, .container-fluid, .navbar, .tab-content {
        background-color: #2e2e2e !important;
        color: #eaeaea !important;
      }

      /* Force la couleur blanche pour tous les titres, labels, textes et liens de la navbar */
      h1, h2, h3, h4, h5, h6, label, p, .navbar-brand, .nav-link {
        color: #ffffff !important;
      }

      /* Définit le style des blocs de résumé en haut de page (KPI, texte d’intro, etc.) */
      .top-block {
        background-color: #3a3a3a;
        border-radius: 16px;
        padding: 16px 24px;
        margin-bottom: 16px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
        min-height: 25vh; /* ~1/4 d'écran */
        display: flex;
        flex-direction: column;
        justify-content: center;
      }

      /* Définit le style de chaque petite boîte de métrique (KPI) */
      .metric-box {
        background-color: #2e2e2e;
        border-radius: 12px;
        padding: 12px 16px;
        margin-top: 8px;
      }

      /* Style du titre des KPI (texte petit, en majuscule, un peu transparent) */
      .metric-title {
        font-size: 0.85rem;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        opacity: 0.8;
      }

      /* Style de la valeur des KPI (gros texte, en gras) */
      .metric-value {
        font-size: 1.4rem;
        font-weight: 600;
      }

      /* Style de la colonne de filtres à gauche (fond, bords arrondis, ombre) */
      .sidebar-climat {
        background-color: #3a3a3a;
        border-radius: 16px;
        padding: 16px 16px 8px 16px;
        height: 100%;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
      }

      /* Réduit un peu l’espace entre les éléments de formulaire dans la sidebar */
      .sidebar-climat .form-group {
        margin-bottom: 10px;
      }

      /* Style du conteneur Leaflet (carte) : bords arrondis + ombre */
      .leaflet-container {
        border-radius: 16px !important;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
      }

      /* Style des champs de formulaire (inputs, select) en mode sombre */
      .form-control, .selectize-input {
        background-color: #2b2b2b !important;
        color: #ffffff !important;
        border-radius: 10px !important;
        border: 1px solid #555 !important;
      }

      /* Style des boutons primaires (couleur verte du thème) */
      .btn-primary {
        background-color: #9ae3c4 !important;
        border-color: #9ae3c4 !important;
        color: #2b2b2b !important;
        border-radius: 10px !important;
      }

      /* Style au survol des boutons primaires (vert un peu plus foncé) */
      .btn-primary:hover {
        background-color: #7ed6b0 !important;
        border-color: #7ed6b0 !important;
        color: #222 !important;
      }
      
      /* Classe dédiée au bouton de mise à jour des données pour le rendre bien visible */
      .update-btn {
        width: 100%;
        font-weight: 700;
        margin-top: 12px;
        margin-bottom: 8px;
        text-transform: uppercase;
        letter-spacing: 0.05em;
      }

    "))
  ),
  
  # Onglet principal de l’application : tableau de bord général
  tabPanel(
    # Titre de l’onglet dans la barre de navigation
    "Tableau de bord",
    
    # Mise en page fluide (adaptée à la largeur de l’écran)
    fluidPage(
      
      # Première ligne : bloc de présentation + indicateurs (KPI)
      fluidRow(
        column(
          width = 12,   # colonne pleine largeur
          div(
            # Utilise la classe CSS "top-block" définie dans le header pour le style
            class = "top-block",
            
            # Titre principal du tableau de bord
            h2("Évolution des jours sous 0°C et des nuits tropicales"),
            
            # Paragraphe d’introduction expliquant l’objectif de la visualisation
            p("Visualiser les indicateurs climatiques pour la région alpine (hors Var) afin de mieux comprendre l’évolution des températures hivernales et estivales."),
            
            # Paragraphe complémentaire, style plus discret (police plus petite, opacité réduite)
            p(
              style = "opacity:0.8;font-size:0.9rem;",
              "Projet Explor'Alpes, développé dans le cadre du Hackathon Météo-France 2025 « Le climat en données »."
            ),
            
            # Saut de ligne pour aérer le bloc avant les KPI
            br(),
            
            # Ligne contenant les trois indicateurs clés (KPI)
            fluidRow(
              # KPI 1 : Jours sous 0°C
              column(
                4,
                div(
                  class = "metric-box",  # utilise le style de boîte KPI défini en CSS
                  div(
                    class = "metric-title",
                    "Jours sous 0°C (hiver)"   # libellé du KPI
                  ),
                  div(
                    class = "metric-value",
                    textOutput("kpi_jours_zero")  # valeur du KPI, calculée côté serveur
                  )
                )
              ),
              
              # KPI 2 : Nuits tropicales
              column(
                4,
                div(
                  class = "metric-box",
                  div(
                    class = "metric-title",
                    "Nuits tropicales"
                  ),
                  div(
                    class = "metric-value",
                    textOutput("kpi_nuits_tropicales")  # valeur dynamique, liée aux données chargées
                  )
                )
              ),
              
              # KPI 3 : Période analysée
              column(
                4,
                div(
                  class = "metric-box",
                  div(
                    class = "metric-title",
                    "Période analysée"
                  ),
                  div(
                    class = "metric-value",
                    textOutput("kpi_periode")  # affiche la période sélectionnée dans les filtres
                  )
                )
              )
            )
          )
        )
      ),
      
      # Deuxième ligne du tableau de bord :
      # à gauche : panneau de filtres
      # à droite : carte Leaflet
      fluidRow(
        column(
          width = 3,  # colonne de gauche, 3/12 de la largeur totale
          div(
            class = "sidebar-climat",  # applique le style de panneau latéral défini en CSS
            
            # Titre du bloc de filtres
            h4("Filtres"),
            
            # Liste déroulante pour choisir le scénario climatique
            selectInput(
              "scenario",                      # nom de l’input (accessible côté serveur : input$scenario)
              "Scénario climatique",           # label affiché dans l’interface
              choices = c("Historique", "RCP 4.5", "RCP 8.5"),  # options proposées
              selected = "Historique"          # valeur sélectionnée par défaut
            ),
            
            # Liste déroulante pour choisir la période climatique
            selectInput(
              "periode",
              "Période",
              choices = c(
                "1981–2010",
                "2011–2040",
                "2041–2070",
                "2071–2100"
              ),
              selected = "1981–2010"           # période utilisée par défaut au chargement
            ),
            
            # Groupe de cases à cocher pour choisir les indicateurs affichés sur la carte
            checkboxGroupInput(
              "indicateurs",
              "Indicateurs à afficher",
              choices = c(
                # chaque élément : "label affiché" = "valeur envoyée à Shiny"
                "Jours avec Tmoy < 0°C"   = "jours_zero",
                "Nuits avec Tmin ≥ 20°C"  = "nuits_tropicales",
                "Isotherme 0°C (altitude)" = "iso_zero"
              ),
              # par défaut, affiche les jours de gel et les nuits tropicales
              selected = c("jours_zero", "nuits_tropicales")
            ),
            
            # Liste déroulante pour préciser la saison étudiée
            selectInput(
              "saison",
              "Saison",
              choices = c("Hiver (DJF)", "Année complète", "Été (JJA)"),
              selected = "Hiver (DJF)"          # par défaut, travaille sur l’hiver
            ),
            
            # Bouton principal d’action pour mettre à jour la carte
            # après modification des filtres
            actionBttn(
              inputId = "btn_maj_carte",        # identifiant de l’action (observeEvent côté serveur)
              label   = "Mettre à jour les données",
              icon    = icon("sync"),           # icône de rafraîchissement
              style   = "fill",                 # style plein (bouton bien visible)
              color   = "success",              # couleur type “succès” (vert)
              size    = "lg"                    # bouton de grande taille
            ),
            
            # Saut de ligne pour aérer sous le bouton principal
            br(),
            
            # Zone dynamique pour le bouton de téléchargement :
            # le serveur décide d’afficher ou non ce bouton selon
            # que des données ont été chargées (output$dl_data_ui)
            uiOutput("dl_data_ui")
          )
        ),
        
        # Colonne de droite : carte interactive Leaflet
        column(
          width = 9,  # 9/12 de la largeur pour laisser un maximum de place à la carte
          leafletOutput("map_climat", height = "70vh")  # “70vh” = 70% de la hauteur de la fenêtre
        )
      )))
      ,

      # Onglet : Animations ----
      # Crée un onglet "Animations" dans la barre de navigation principale
      tabPanel(
        "Animations",
        fluidPage(
          # Affiche un titre pour la page d’animations
          h3("Animations jours de gel et nuits tropicales"),
          br(),
          
          # Ligne 0 : ligne de crête
          # Première ligne de contenu : animations de la ligne de crête
          fluidRow(
            column(
              width = 12,
              
              # Ligne 0 : ligne de crête
              # Deuxième définition de la même ligne, dédiée à l’animation de la crête
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "top-block",  # réutilise le style de bloc du tableau de bord
                    h4("Profil de crêtes avec altitudes de l'isotherme 0°C"),
                    div(
                      style = "
          background-color:#1e1e1e;
          border-radius:16px;
          text-align:center;
          padding:15px;
          overflow-x:auto;             /* autorise le scroll horizontal si besoin */
        ",
                      # Insère un GIF animé hébergé sur GitHub
                      # représentant le profil de crête et l’altitude de l’isotherme 0°C
                      tags$img(
                        src = "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/crete_animations/crete_animation.gif",
                        style = "
            transform: scale(1.5);      /* zoom lisible mais modéré */
            transform-origin: center;
            width: 1400px;              /* largeur fixe pour bien remplir le bloc */
            max-width: 100%;
            height: auto;               /* ajuste la hauteur automatiquement */
            object-fit: contain;
            border-radius: 12px;
            background-color: #1e1e1e;
            display: inline-block;
          "
                      )
                    )
                  )
                )
              )
              
              
              
              
            )
          ),
          
          # Ligne 1 : jours de gel
          # Deuxième bloc : deux colonnes côte à côte pour les jours de gel
          fluidRow(
            column(
              width = 6,
              div(
                class = "top-block",
                h4("Jours de gel – évolution annuelle"),
                # GIF montrant l’évolution des jours de gel année par année
                tags$img(
                  src = "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/isotherme_animations/freezing_days_evolution.gif",
                  style = "width: 100%; max-height: 500px; object-fit: contain;"
                )
              )
            ),
            column(
              width = 6,
              div(
                class = "top-block",
                h4("Jours de gel – intervalles de 20 ans"),
                # GIF montrant les jours de gel moyennés par intervalles de 20 ans
                tags$img(
                  src = "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/isotherme_animations/freezing_days_intervals.gif",
                  style = "width: 100%; max-height: 500px; object-fit: contain;"
                )
              )
            )
          ),
          
          br(),
          # Titre séparant la partie "nuits tropicales"
          h3("Animations nuits tropicales"),
          br(),
          
          # Ligne 2 : nuits tropicales
          # Troisième bloc : deux colonnes côte à côte pour les nuits tropicales
          fluidRow(
            column(
              width = 6,
              div(
                class = "top-block",
                h4("Nuits tropicales – évolution annuelle"),
                # GIF montrant l’évolution annuelle des nuits tropicales
                tags$img(
                  src = "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/tropical_animations/tropical_days_evolution.gif",
                  style = "width: 100%; max-height: 500px; object-fit: contain;"
                )
              )
            ),
            column(
              width = 6,
              div(
                class = "top-block",
                h4("Nuits tropicales – intervalles de 20 ans"),
                # GIF montrant les nuits tropicales par intervalles de 20 ans
                tags$img(
                  src = "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/tropical_animations/tropical_days_intervals.gif",
                  style = "width: 100%; max-height: 500px; object-fit: contain;"
                )
              )
            )
          )
        )
      ),
      
      # Onglet : Guide d'utilisation ----
      # Crée un onglet "Guide d'utilisation" dans la barre de navigation principale
      tabPanel(
        "Guide d'utilisation",
        fluidPage(
          # Affiche un titre principal pour cette page
          h3("Guide d'utilisation d'Explor'Alpes"),
          br(),
          
          # Section 1 : explication détaillée de la page "Tableau de bord"
          h4("1. Page « Tableau de bord »"),
          # Texte introductif expliquant le rôle du tableau de bord
          p("Cette page permet d’explorer les cartes de jours de gel et de nuits tropicales pour la région alpine (hors Var)."),
          
          # Liste à puces décrivant les différents contrôles de la page
          tags$ul(
            # Explication du sélecteur de scénario climatique
            tags$li(
              strong("Scénario climatique : "),
              "choisit soit les données historiques, soit un scénario de projection (RCP 4.5 ou RCP 8.5)."
            ),
            # Explication du choix de période climatique
            tags$li(
              strong("Période : "),
              "sélectionne une période climatique (1981–2010, 2011–2040, 2041–2070, 2071–2100)."
            ),
            # Explication des indicateurs cartographiés
            tags$li(
              strong("Indicateurs : "),
              "coche les variables à afficher sur la carte :",
              tags$ul(
                tags$li("« Jours avec Tmoy < 0°C » pour les jours de gel en hiver ;"),
                tags$li("« Nuits avec Tmin ≥ 20°C » pour les nuits tropicales ;"),
                tags$li("« Isotherme 0°C (altitude) » (future extension).")
              )
            ),
            # Explication du filtre saisonnier
            tags$li(
              strong("Saison : "),
              "choisit la saison d’analyse (Hiver DJF, année complète, été JJA)."
            ),
            # Explication du bouton de mise à jour
            tags$li(
              strong("Bouton « Mettre à jour les données » : "),
              "après réglage des filtres, clique pour charger les données NetCDF et rafraîchir la carte."
            ),
            # Explication du fonctionnement de la carte Leaflet
            tags$li(
              strong("Carte : "),
              "affiche une carte leaflet représentant l’intensité de l’indicateur sur la région alpine, avec une légende et un recentrage automatique sur la zone d’étude."
            ),
            # Explication des indicateurs de synthèse (KPI)
            tags$li(
              strong("Indicateurs en haut de page : "),
              "synthétisent le nombre moyen de jours de gel, de nuits tropicales et la période analysée."
            )
          ),
          
          tags$hr(),
          
          # Section 2 : explication de la page "Animations"
          h4("2. Page « Animations »"),
          # Texte introductif expliquant le but des animations
          p("Cette page présente des animations pré-calculées illustrant l’évolution de l’isotherme 0°C, des jours de gel et des nuits tropicales."),
          
          # Liste à puces décrivant chaque type d’animation
          tags$ul(
            # Animation du profil de crête et de l’isotherme 0°C
            tags$li(
              strong("Profil de crêtes avec altitudes de l'isotherme 0°C : "),
              "présente une animation montrant, pour chaque département alpin, l’altitude des principaux sommets et la position moyenne de l’isotherme 0°C sur la période récente. Permet de visualiser la marge de manœuvre entre les reliefs et l’altitude de 0°C."
            ),
            # Animation des jours de gel année par année
            tags$li(
              strong("Jours de gel – évolution annuelle : "),
              "affiche un GIF montrant l’évolution année par année du nombre de jours avec température moyenne inférieure à 0°C."
            ),
            # Animation des jours de gel par intervalles de 20 ans
            tags$li(
              strong("Jours de gel – intervalles de 20 ans : "),
              "affiche un GIF présentant des cartes moyennées par grandes périodes (20 ans) pour visualiser les tendances de fond."
            ),
            # Animation des nuits tropicales année par année
            tags$li(
              strong("Nuits tropicales – évolution annuelle : "),
              "affiche un GIF montrant la progression dans le temps des nuits avec Tmin ≥ 20°C."
            ),
            # Animation des nuits tropicales par intervalles de 20 ans
            tags$li(
              strong("Nuits tropicales – intervalles de 20 ans : "),
              "montre la variation des nuits tropicales par grandes périodes, afin de comparer les régimes climatiques."
            )
          ),
      
      tags$hr(),
      h4("3. Page « À propos »"),
      p("Cette page décrit le contexte du projet, les sources de données, les indicateurs suivis et présente l’équipe ayant contribué au développement de l’outil."),
      tags$ul(
        tags$li("Contexte du hackathon « Climat des données »."),
        tags$li("Description des jeux de données climatiques utilisés."),
        tags$li("Liste des usages visés (sensibilisation, compréhension des impacts, etc.)."),
        tags$li("Présentation de l’équipe projet.")
      ),
      
      tags$hr(),
      h4("Conseils de lecture"),
      tags$ul(
        tags$li("Commencer par le « Tableau de bord » pour explorer un indicateur et une période en particulier."),
        tags$li("Passer ensuite par « Animations » pour visualiser la dynamique temporelle globale."),
        tags$li("Utiliser « À propos » et ce guide pour comprendre le cadre scientifique et les limites de l’outil.")
      )
    )
  ),
  
  # Onglet : À propos ----
  tabPanel(
    "À propos",
    fluidPage(
      h3("Contexte : Hackathon Météo-France 2025"),
      p("Projet réalisé dans le cadre du hackathon ", strong("Météo-France 2025 – Le climat en données"), "."),
      p("Ce défi porte sur la ", strong("visualisation de données climatiques"),
        " pour faciliter leur compréhension et leur appropriation par différents publics : décideurs publics, acteurs territoriaux, citoyens, professionnels, etc. ",
        "Une attention particulière est portée à la représentation lisible de l’incertitude climatique."
      ),
      
      tags$hr(),
      
      h3("Solution proposée : Explor'Alpes"),
      p("Avec Explor'Alpes, nous nous concentrons sur deux signaux forts du changement climatique dans les Alpes :"),
      tags$ul(
        tags$li(
          strong("L’évolution de l’isotherme 0°C"),
          " à l’échelle régionale (jours de gel)."
        ),
        tags$li(
          strong("La fréquence d’apparition des nuits tropicales"),
          " définies comme des nuits où la température ne descend pas sous 20°C."
        )
      ),
      p("L’application vise à vulgariser ces indicateurs via une carte interactive et quelques indicateurs synthétiques (KPIs) afin de rendre les résultats accessibles au plus grand nombre."),
      
      tags$hr(),
      
      h3("Zone d'étude"),
      p("La zone d’étude couvre la région alpine (hors Var), définie par les départements suivants :"),
      tags$ul(
        tags$li("Alpes-de-Haute-Provence (04)"),
        tags$li("Hautes-Alpes (05)"),
        tags$li("Alpes-Maritimes (06)"),
        tags$li("Drôme (26)"),
        tags$li("Isère (38)"),
        tags$li("Savoie (73)"),
        tags$li("Haute-Savoie (74)"),
        tags$li("Vaucluse (84)")
      ),
      
      tags$hr(),
      
      h3("Approche adoptée"),
      h4("1️⃣ Extraction et nettoyage des données"),
      p("Nous utilisons les données issues du jeu ", code("ESMS2-1 ALPX3 2,5 km"), 
        " pour la région alpine. Ce choix de résolution permet une analyse relativement fine des gradients altitudinaux et des contrastes spatiaux."),
      
      h4("2️⃣ Calcul des indicateurs"),
      p("Deux familles d’indicateurs sont mises en avant : l’isotherme zéro (jours de gel) et les nuits tropicales."),
      
      h5("Isotherme 0°C – jours de gel"),
      tags$ul(
        tags$li("Nombre de jours par an où la température moyenne est supérieure à 0°C (Tmean > 0°C), en particulier en hiver (décembre-janvier-février)."),
        tags$li("Calcul de la moyenne, du minimum et du maximum du nombre de jours par tranche temporelle (horizons de projection)."),
        tags$li("Comparaison des indicateurs entre climat historique et climat projeté (écarts de moyenne, minimum, maximum)."),
        tags$li("Interprétation en termes d’“,hiver moyen”, “hiver chaud” et “hiver froid”.")
      ),
      
      h5("Nuits tropicales"),
      p("Une nuit tropicale est définie comme une nuit durant laquelle la température minimale ne descend pas en dessous de 20°C (Tmin ≥ 20°C)."),
      tags$ul(
        tags$li("Nombre de nuits tropicales par an dans les données historiques et de projection."),
        tags$li("Calcul de la moyenne, du minimum et du maximum du nombre de nuits tropicales par plage temporelle."),
        tags$li("Comparaison entre climat historique et différents horizons de projection."),
        tags$li("Interprétation en scénarios de “climat moyen”, “chaud” et “froid”.")
      ),
      
      h4("3️⃣ Visualisation via Explor'Alpes"),
      tags$ul(
        tags$li("Carte interactive permettant de visualiser les indicateurs choisis (jours de gel, nuits tropicales)."),
        tags$li("Indicateurs synthétiques (moyennes régionales) affichés en haut de la page."),
        tags$li("Possibilité de choisir la période (horizons climatiques) et l’indicateur étudié.")
      ),
      
      tags$hr(),
      
      h3("Données utilisées et téléchargement"),
      p("Les données proviennent du dépôt GitHub dédié au hackathon (Météo-France / partenaires). "
      ),
      tags$div(
        style = "margin-top:8px;margin-bottom:16px;",
        tags$a(
          href   = "https://github.com/justinesommerlatt/Hackathon-Meteo-France/tree/main/Tropical_data",
          target = "_blank",
          class  = "btn btn-outline-light btn-sm",
          "📁 Données nuits tropicales (NetCDF)"
        ), " ",
        tags$a(
          href   = "https://github.com/justinesommerlatt/Hackathon-Meteo-France/tree/main/Isotherme0_data",
          target = "_blank",
          class  = "btn btn-outline-light btn-sm",
          "📁 Données jours de gel / isotherme 0°C"
        ), " ",
        tags$a(
          href   = "https://github.com/justinesommerlatt/Hackathon-Meteo-France/blob/main/Alpes_grid.nc",
          target = "_blank",
          class  = "btn btn-outline-light btn-sm",
          "📄 Grille Alpes (Alpes_grid.nc)"
        )
      ),
      p("Ces fichiers NetCDF sont utilisés par l’application pour construire les cartes affichées dans l’onglet “Tableau de bord”."),
      
      tags$hr(),
      
      h3("Code source & déploiement"),
      tags$ul(
        tags$li(
          "🔄 Page de réutilisation sur data.gouv.fr : ",
          a("Explor'Alpes sur data.gouv.fr",
            href   = "https://www.data.gouv.fr/reuses/exploralpes/",
            target = "_blank")
        ),
        tags$li(
          "📦 Dépôt des données climatiques (NetCDF) : ",
          a("github.com/justinesommerlatt/Hackathon-Meteo-France",
            href   = "https://github.com/justinesommerlatt/Hackathon-Meteo-France",
            target = "_blank")
        ),
        tags$li(
          "💻 Code source de l’application Shiny : ",
          a("github.com/rwinsee/app_shiny_climat",
            href   = "https://github.com/rwinsee/app_shiny_climat",
            target = "_blank")
        ),
        tags$li(
          "🧩 Chart Helm / projet de déploiement : ",
          a("github.com/rwinsee/hackathon_defi8_dataviz",
            href   = "https://github.com/rwinsee/hackathon_defi8_dataviz",
            target = "_blank")
        ),

        tags$li(
          "🐳 Image Docker (Docker Hub) : ",
          a("rwinsee/app_shiny_climat",
            href   = "https://hub.docker.com/r/rwinsee/app_shiny_climat",
            target = "_blank")
        ),
        tags$li(
          "☁️ Hébergement : application déployée sur le ",
          strong("SSP Cloud de l’Insee"),
          " via Onyxia (service RStudio pour le développement) et un ",
          strong("chart Helm shiny-app-template"),
          " déployé sur le cluster Kubernetes du SSP Cloud."
        ),
        tags$li(
          "🔗 Lien direct vers l’application : ",
          a("https://hackathon-climat-defi8.lab.sspcloud.fr/",
            href   = "https://hackathon-climat-defi8.lab.sspcloud.fr/",
            target = "_blank")
        )
      ),
      
      tags$hr(),
      
      h3("Mode d'emploi rapide"),
      tags$ol(
        tags$li("Choisir un ", strong("scénario climatique"), " et une ", strong("période"), " dans le panneau de gauche."),
        tags$li("Cocher l’", strong("indicateur à cartographier"), " (jours de gel ou nuits tropicales)."),
        tags$li("Cliquer sur ", strong("« Mettre à jour la carte »"), " pour actualiser la carte et les indicateurs en haut de page."),
        tags$li("Explorer la carte (zoom, déplacement) pour localiser les zones les plus impactées.")
      ),
      
      tags$hr(),
      
      h3("Équipe"),
      tags$ul(
        tags$li("Maëlle ABRAHAM (Consultante adaptation - Carbone 4)"),
        tags$li("Julien AVINÉE (Consultant adaptation - Carbone 4)"),
        tags$li("Madeleine D’ARRENTIERES (Consultante adaptation - Carbone 4)"),
        tags$li("Lucio LURASCHI (Ingénieur logiciel - EDF)"),
        tags$li("Etienne PAUTHENET (Data Scientist - IRD Brest)"),
        tags$li("Sandrine PARADOWSKI (Géomaticienne - DDT 77)"),
        tags$li("Justine SOMMERLATT (Data Scientist - BKW)"),
        tags$li("Romuald WEIDMANN (Développeur R - INSEE)")
      )
    )
  )
)

### server.r
server <- function(input, output, session) {
  # ---- État courant de la carte (ce qui est VRAIMENT affiché) ----
  # Mémorise l’état actuel de la carte : type de données, période, URL et indicateur
  # Permet de savoir exactement quelles données sont affichées au moment d’un téléchargement
  current_data <- reactiveValues(
    type     = NULL,   # "tropical" ou "freezing"
    periode  = NULL,   # texte ex : "1981–2010"
    url      = NULL,   # URL NetCDF utilisée
    indic    = NULL    # "nuits_tropicales" ou "jours_zero"
  )
  
  # ---- KPI réactifs ----
  # Initialise les valeurs des indicateurs de synthèse (KPI) pour les nuits tropicales et les jours de gel
  # Met à jour ces valeurs quand la carte se recharge avec de nouvelles données
  kpi_nuits <- reactiveVal("—")
  kpi_froid <- reactiveVal("—")
  
  # Vecteurs de temps pour l'animation ----
  # Définit les grandes périodes climatiques utilisées dans le mode "période"
  periodes_vec  <- c("1981–2010", "2011–2040", "2041–2070", "2071–2100")
  
  # Définit les intervalles de 10 ans (décennies) utilisés dans le mode "décennie"
  # Sert d’échelle temporelle pour les animations fines
  decennies_vec <- c(
    "1990–1999",
    "2000–2009",
    "2010–2019",
    "2020–2029",
    "2030–2039",
    "2040–2049",
    "2050–2059",
    "2060–2069",
    "2070–2079",
    "2080–2089"
  )  
  
  # Carte animée initiale ----
  # Initialise la carte de l’onglet "Animations" avec un fond OSM centré sur les Alpes
  output$map_anim <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 6.5, lat = 45.5, zoom = 7)
  })
  
  # Slider dynamique selon le mode (périodes / décades) ----
  # Génère dynamiquement le slider temporel selon le mode choisi :
  #  - "periode" : slider sur les périodes climatiques
  #  - "decennie": slider sur les décades
  output$anim_slider_ui <- renderUI({
    if (req(input$anim_mode) == "periode") {
      # Crée un slider pour parcourir les 4 périodes climatiques
      sliderInput(
        "anim_index",
        "Période climatique",
        min = 1,
        max = length(periodes_vec),
        value = 1,
        step = 1,
        ticks = FALSE,
        animate = animationOptions(interval = 2000, loop = TRUE)
      )
    } else {
      # Crée un slider pour parcourir les décades définies dans decennies_vec
      sliderInput(
        "anim_index",
        "Décennie",
        min = 1,
        max = length(decennies_vec),
        value = 1,
        step = 1,
        ticks = FALSE,
        animate = animationOptions(interval = 2000, loop = TRUE)
      )
    }
  })
  
  # Label affiché sous les filtres ----
  # Affiche, sous le slider, le libellé lisible correspondant à la position du slider :
  #  - soit une période climatique
  #  - soit une décennie
  output$anim_periode_label <- renderText({
    req(input$anim_mode, input$anim_index)
    if (input$anim_mode == "periode") {
      periodes_vec[input$anim_index]
    } else {
      decennies_vec[input$anim_index]
    }
  })
  
  # Animation : met à jour la carte selon indicateur + période/décennie ----
  # Déclenche une mise à jour de la carte d’animation à chaque changement de position du slider
  observeEvent(input$anim_index, {
    # Vérifie que le mode (période/décennie), l’indicateur et l’index du slider sont bien définis
    req(input$anim_mode, input$anim_indic, input$anim_index)
    
    # --- choix texte affiché & URL NetCDF ---
    if (input$anim_mode == "periode") {
      # Si le mode choisi est "période", récupère le libellé de la période à partir du vecteur periodes_vec
      periode_label <- periodes_vec[input$anim_index]
      # Récupère le type d’indicateur à afficher : "tropical" (nuits tropicales) ou "freezing" (jours de gel)
      type_sel      <- input$anim_indic
      # Utilise la fonction choose_nc_url() pour sélectionner l’URL NetCDF correspondant à ce type et cette période
      url_sel       <- choose_nc_url(type_sel, periode_label)
      # Construit le titre de la légende en fonction du type d’indicateur
      leg_title     <- if (type_sel == "tropical") {
        paste0("Nuits tropicales / an (", periode_label, ")")
      } else {
        paste0("Jours avec Tmoy < 0°C (", periode_label, ")")
      }
    } else {
      # Si le mode choisi est "décennie", sélectionne directement le i-ème fichier dans le vecteur des URLs
      type_sel <- input$anim_indic
      # Choisit le bon vecteur d’URL selon l’indicateur (tropical ou freezing)
      vec      <- if (type_sel == "tropical") tropical_nc_urls else freezing_nc_urls
      
      # Calcule un indice valide (entre 1 et la longueur du vecteur) en fonction de la position du slider
      idx <- max(1, min(input$anim_index, length(vec)))
      # Sélectionne l’URL correspondante
      url_sel   <- vec[idx]
      # Récupère le libellé de la décade correspondante
      dec_label <- decennies_vec[idx]
      
      # Construit le titre de la légende pour le mode "décennie"
      leg_title <- if (type_sel == "tropical") {
        paste0("Nuits tropicales / an (", dec_label, ")")
      } else {
        paste0("Jours avec Tmoy < 0°C (", dec_label, ")")
      }
    }
    
    # Logs
    # Affiche dans la console quelques informations utiles pour le débogage
    cat("\n=== [ANIM] Mise à jour de la carte ===\n")
    cat("→ Mode temporel :", input$anim_mode, "\n")
    cat("→ Indicateur    :", input$anim_indic, "\n")
    cat("→ URL NetCDF    :", url_sel, "\n")
    
    # Chargement du raster
    # Construit un chemin local temporaire basé sur le nom de fichier de l’URL
    dest <- file.path(tempdir(), basename(url_sel))
    # Télécharge le fichier NetCDF si non encore présent en local
    if (!file.exists(dest)) {
      download.file(url_sel, dest, mode = "wb")
    }
    
    # Charge le fichier NetCDF en raster
    r_raster <- raster::raster(dest)
    # Applique l’emprise (bbox) de la grille Alpes pour recadrer le raster sur la zone d’étude
    raster::extent(r_raster) <- unname(alpes_bbox)
    # Définit le système de coordonnées (WGS84) compatible avec Leaflet
    raster::crs(r_raster)    <- "+proj=longlat +datum=WGS84 +no_defs"
    
    # Récupère toutes les valeurs du raster (une valeur par maille)
    vals <- raster::values(r_raster)
    # Ne garde que les valeurs finies (supprime les NA/Inf)
    vals <- vals[is.finite(vals)]
    # Si aucune valeur valable, affiche une notification d’erreur et arrête le traitement
    if (!length(vals)) {
      showNotification("Pas de valeurs numériques dans le raster chargé (animation).", type = "error")
      return()
    }
    
    # Palette conditionnelle : bleu pour jours de gel, viridis pour nuits tropicales
    # Choisit la palette de couleurs selon le type d’indicateur
    if (type_sel == "freezing") {
      pal <- colorNumeric(
        palette  = pal_freezing_fn(256),  # palette bleue définie plus haut
        domain   = vals,                  # étendue des valeurs du raster
        na.color = "transparent"          # rendu transparent pour les NA
      )
    } else {
      pal <- colorNumeric(
        palette  = "viridis",             # palette viridis standard pour les nuits tropicales
        domain   = vals,
        na.color = "transparent"
      )
    }
    
    # Récupère l’emprise du raster pour recadrer la carte sur la zone affichée
    e <- raster::extent(r_raster)
    
    # Met à jour la carte leaflet existante (output$map_anim) sans la recréer
    leafletProxy("map_anim") |>
      clearMarkers() |>
      clearShapes() |>
      clearControls() |>
      addTiles() |>
      addRasterImage(
        r_raster,
        colors  = pal,
        opacity = 0.8,
        project = TRUE
      ) |>
      addLegend(
        pal    = pal,
        values = vals,
        title  = leg_title
      ) |>
      fitBounds(e@xmin, e@ymin, e@xmax, e@ymax)
  })
  
  # KPIs ----
  # Définit le texte affiché pour le KPI "Jours sous 0°C (hiver)"
  # Récupère simplement la valeur stockée dans la variable réactive kpi_froid()
  output$kpi_jours_zero <- renderText({
    kpi_froid()
  })
  
  # Définit le texte affiché pour le KPI "Nuits tropicales"
  # Utilise la valeur réactive kpi_nuits(), mise à jour lors du chargement des rasters
  output$kpi_nuits_tropicales <- renderText({
    kpi_nuits()
  })
  
  # Définit le texte affiché pour le KPI "Période analysée"
  # Affiche directement la valeur choisie dans le selectInput input$periode
  output$kpi_periode <- renderText({
    input$periode
  })
  
  # Carte initiale ----
  # Initialise la carte Leaflet du tableau de bord
  # Ajoute le fond de carte (addTiles) et centre la vue sur les Alpes
  output$map_climat <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      # centre approximatif Alpes
      setView(lng = 6.5, lat = 45.5, zoom = 7)
  })
  
  # ---- Graphique ligne de crête ----
  # Crée le graphique des altitudes des principaux sommets par département
  output$plot_ligne_crete <- renderPlot({
    # Copie le data frame contenant les informations sur les sommets
    df <- ligne_crete_df
    
    # Ne garde que les lignes pour lesquelles l'altitude du sommet est connue (non NA)
    df <- df[!is.na(df$Altitude_Sommet), , drop = FALSE]
    
    # Si aucune ligne n’est disponible, affiche un message et arrête le dessin
    if (!nrow(df)) {
      plot.new()
      text(0.5, 0.5, "Aucun sommet avec altitude renseignée.", cex = 0.9)
      return()
    }
    
    # Trie les sommets par altitude décroissante pour donner un effet de "ligne de crête"
    ord <- order(df$Altitude_Sommet, decreasing = TRUE)
    df  <- df[ord, ]
    
    # Crée un facteur sur le département pour associer une couleur à chaque département
    dep_fac <- factor(df$Département)
    cols    <- as.numeric(dep_fac)
    
    # Trace un graphique en barres verticales (type = "h") :
    # chaque barre représente l'altitude d’un sommet
    plot(
      df$Altitude_Sommet,
      type = "h",                     # segments verticaux depuis 0
      lwd  = 4,                       # épaisseur des segments
      col  = cols,                    # couleur selon le département
      xaxt = "n",                     # supprime l’axe des x (géré à part)
      xlab = "",
      ylab = "Altitude du sommet (m)",
      main = "Altitudes des principaux sommets par département"
    )
    
    # Prépare les noms de sommets en supprimant d’éventuels espaces inutiles
    noms_sommets <- trimws(df$Nom_Sommet)
    
    # Ajoute les étiquettes sur l’axe des x :
    # une position par sommet, avec le nom en vertical (las = 2)
    axis(
      1,
      at = seq_along(df$Altitude_Sommet),
      labels = noms_sommets,
      las = 2,        # texte vertical
      cex.axis = 0.5  # réduit la taille du texte pour tout faire tenir
    )
    
    # Ajoute une légende indiquant la couleur associée à chaque département
    legend(
      "topright",
      legend = levels(dep_fac),                  # noms des départements
      col    = seq_along(levels(dep_fac)),       # mêmes indices de couleur
      lwd    = 4,                                # épaisseur des segments dans la légende
      cex    = 0.6,                              # taille du texte
      bty    = "n"                               # pas de bordure autour de la légende
    )
  })
  
  # Mise à jour de la carte + logs ----
  # Observe les clics sur le bouton "Mettre à jour les données"
  # À chaque clic, recharge les données NetCDF, met à jour la carte et les KPI
  observeEvent(input$btn_maj_carte, {
    
    # --- LOGS ---
    # Écrit dans la console les paramètres choisis par l'utilisateur
    cat("\n=== [LOG] Bouton 'Mettre à jour' cliqué ===\n")
    cat("→ Période sélectionnée :", input$periode, "\n")
    cat("→ Indicateurs sélectionnés :", paste(input$indicateurs, collapse = ", "), "\n")
    
    # Récupère la liste des indicateurs cochés
    ind <- input$indicateurs
    
    # Choisit le type de données à charger en fonction des cases cochées
    # Priorité aux nuits tropicales si plusieurs cases sont actives
    if ("nuits_tropicales" %in% ind) {
      type_sel  <- "tropical"                     # identifie les données "nuits tropicales"
      url_sel   <- choose_nc_url("tropical", input$periode)  # choisit l’URL NetCDF adaptée
      leg_title <- "Nuits tropicales / an"        # titre de légende
      cible_kpi <- "nuits"                        # indique quel KPI mettre à jour
      
    } else if ("jours_zero" %in% ind) {
      type_sel  <- "freezing"                     # identifie les données "jours de gel"
      url_sel   <- choose_nc_url("freezing", input$periode)
      leg_title <- "Jours avec Tmoy < 0°C"
      cible_kpi <- "froid"
      
    } else {
      # Si aucun indicateur pertinent n'est coché, affiche un message et arrête le traitement
      showNotification("Sélectionner au moins un indicateur (nuits tropicales ou jours de gel).",
                       type = "warning")
      return()
    }
    
    # Loggue dans la console le type de données et l’URL utilisée
    cat("→ Type de données choisi :", type_sel, "\n")
    cat("→ URL NetCDF :", url_sel, "\n")
    cat("→ Fichier NetCDF local :", basename(url_sel), "\n")
    
    # Message dans la console pour préciser le chargement du fichier NetCDF
    message("Chargement du NetCDF (téléchargement local si besoin) : ", url_sel)
    
    # Construit le chemin de stockage local dans un dossier temporaire
    dest <- file.path(tempdir(), basename(url_sel))
    # Télécharge le fichier uniquement s’il n’est pas déjà présent en local
    if (!file.exists(dest)) {
      download.file(url_sel, dest, mode = "wb")
    }
    
    # Charge le raster à partir du fichier NetCDF téléchargé
    r_raster <- raster::raster(dest)
    # Applique l’emprise géographique des Alpes (bbox calculée plus haut)
    raster::extent(r_raster) <- unname(alpes_bbox)
    # Définit le système de coordonnées en WGS84 (compatible Leaflet)
    raster::crs(r_raster)    <- "+proj=longlat +datum=WGS84 +no_defs"
    
    # Extrait toutes les valeurs du raster
    vals <- raster::values(r_raster)
    # Ne garde que les valeurs finies (exclut les NA/Inf)
    vals <- vals[is.finite(vals)]
    # Si aucune valeur exploitable n’est trouvée, affiche un message et arrête la mise à jour
    if (!length(vals)) {
      showNotification("Pas de valeurs numériques dans le raster chargé.", type = "error")
      return()
    }
    
    # Choisit la palette de couleurs en fonction du type d'indicateur
    if (type_sel == "freezing") {
      # Palette bleutée pour les jours de gel
      pal <- colorNumeric(
        palette  = pal_freezing_fn(256),
        domain   = vals,
        na.color = "transparent"
      )
    } else {
      # Palette "viridis" pour les nuits tropicales
      pal <- colorNumeric(
        palette  = "viridis",
        domain   = vals,
        na.color = "transparent"
      )
    }
    
    # Calcule un KPI simple : moyenne des valeurs sur toutes les mailles
    kpi_val <- round(mean(vals, na.rm = TRUE), 1)
    # Met à jour le KPI approprié (nuits ou jours de gel)
    if (cible_kpi == "nuits") {
      kpi_nuits(paste0(kpi_val, " nuits/an (moyenne maille)"))
    } else {
      kpi_froid(paste0(kpi_val, " jours/an (moyenne maille)"))
    }
    
    # Récupère l’emprise du raster pour recadrer la carte
    e <- raster::extent(r_raster)
    
    # Met à jour la carte Leaflet existante (map_climat) sans la recréer
    leafletProxy("map_climat") |>
      clearMarkers() |>
      clearShapes() |>
      clearControls() |>
      addTiles() |>
      addRasterImage(
        r_raster,
        colors  = pal,
        opacity = 0.8,
        project = TRUE
      ) |>
      addLegend(
        pal    = pal,
        values = vals,
        title  = leg_title
      ) |>
      fitBounds(e@xmin, e@ymin, e@xmax, e@ymax)
    
    # Mémorise l'état réellement affiché sur la carte pour le bouton de téléchargement
    current_data$type    <- type_sel
    current_data$periode <- input$periode
    current_data$url     <- url_sel
    current_data$indic   <- if (type_sel == "tropical") "nuits_tropicales" else "jours_zero"
    
    # Loggue en console l’état courant de la carte
    cat("→ [STATE] Carte mise à jour avec type =", current_data$type,
        "| période =", current_data$periode,
        "| url =", current_data$url, "\n")
    
  })
  
  # Bouton de téléchargement visible seulement quand une carte a été chargée
  # Génère dynamiquement (ou non) le bouton de téléchargement dans l’UI
  output$dl_data_ui <- renderUI({
    # Si aucune URL n’a été mémorisée (pas de carte encore chargée), ne montre rien
    if (is.null(current_data$url)) {
      return(NULL)
    }
    
    # Si des données sont disponibles, affiche un bouton de téléchargement stylé
    downloadButton(
      "dl_data",
      "⬇ Télécharger les données affichées",
      class = "btn btn-lg btn-block",
      style = paste(
        "margin-top:12px;",
        "width:100%;",
        "background-color:#9ae3c4;",   # vert clair du thème
        "color:#1a1a1a;",              # texte foncé
        "border:none;",
        "font-weight:600;",
        "text-transform:uppercase;",
        "letter-spacing:0.05em;"
      )
    )
  })
  
  # ---- Téléchargement des données AFFICHÉES ----
  # Définit ce qui se passe quand l'utilisateur clique sur le bouton "dl_data"
  output$dl_data <- downloadHandler(
    # Construction du nom de fichier téléchargé
    filename = function() {
      # Si aucun état n’est enregistré, renvoie un nom générique
      if (is.null(current_data$indic) || is.null(current_data$periode)) {
        return("exploralpes_donnees.nc")
      }
      
      # Identifie le type de données courant pour adapter le nom du fichier
      type <- if (current_data$indic == "nuits_tropicales") {
        "nuits_tropicales"
      } else if (current_data$indic == "jours_zero") {
        "jours_gel"
      } else {
        "donnees"
      }
      
      # Remplace le tiret long par un tiret simple pour un nom de fichier valide
      periode_safe <- gsub("–", "-", current_data$periode)  # 1981–2010 -> 1981-2010
      paste0("exploralpes_", type, "_", periode_safe, ".nc")
    },
    # Logique de génération du contenu du fichier téléchargé
    content = function(file) {
      # 1) Vérifie que la carte a bien été mise à jour au préalable
      if (is.null(current_data$url)) {
        showNotification(
          "Clique d'abord sur « Mettre à jour les données » pour charger une carte avant de télécharger.",
          type = "warning",
          duration = 5,
          closeButton = TRUE
        )
        return(NULL)
      }
      
      # Récupère l’URL du NetCDF effectivement utilisé pour tracer la carte
      url_sel <- current_data$url
      
      # 2) Affiche une notification de début de téléchargement (toast "en cours")
      id_notif <- showNotification(
        ui = "📡 Téléchargement des données affichées en cours…",
        type = "message",
        duration = NULL,
        closeButton = TRUE
      )
      
      message("[DOWNLOAD] Téléchargement des données affichées depuis : ", url_sel)
      
      # Indicateur de succès pour savoir si le téléchargement s'est bien passé
      ok <- TRUE
      tryCatch(
        {
          # Télécharge le fichier NetCDF dans le fichier temporaire fourni par Shiny
          utils::download.file(url_sel, destfile = file, mode = "wb")
        },
        error = function(e) {
          # En cas d'erreur, change le drapeau de succès et affiche une notification d'erreur
          ok <<- FALSE
          message("[DOWNLOAD] ERREUR : ", e$message)
          showNotification(
            ui = paste("❌ Erreur lors du téléchargement :", e$message),
            type = "error",
            duration = 8,
            closeButton = TRUE
          )
        }
      )
      
      # 3) Supprime la notification "en cours" une fois l’opération terminée
      removeNotification(id_notif)
      
      # 4) Affiche une notification de succès si tout s'est bien déroulé
      if (ok) {
        showNotification(
          ui = "✅ Données prêtes. Le téléchargement devrait démarrer dans votre navigateur.",
          type = "message",
          duration = 5,
          closeButton = TRUE
        )
      }
    }
  )
  
  
}

shinyApp(ui = ui, server = server)
