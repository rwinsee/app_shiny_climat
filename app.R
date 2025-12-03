liste_packages <- c(
  "shiny",
  "bslib",
  "leaflet",
  "httr",
  "jsonlite",
  "terra",
  "raster",
  "ncdf4",
  "shinyWidgets"
)

for (pkg in liste_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Thème global ----
app_theme <- bs_theme(
  version = 5,
  base_font = font_google("Inter"),
  bg = "#2e2e2e",
  fg = "#eaeaea",
  primary = "#9ae3c4",
  success = "#9ae3c4",
  info = "#8ecae6"
)

# # ---- Récupérer les fichiers NetCDF depuis GitHub ----
# # ---- Nuits tropicales ----
# api_url_trop <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Tropical_data"
# 
# res_trop <- GET(api_url_trop)
# stop_for_status(res_trop)
# 
# files_meta_trop <- fromJSON(content(res_trop, as = "text", encoding = "UTF-8"))
# 
# tropical_nc_urls <- files_meta_trop$download_url[grepl("\\.nc$", files_meta_trop$name)]
# names(tropical_nc_urls) <- files_meta_trop$name[grepl("\\.nc$", files_meta_trop$name)]
# 
# 
# # ---- Jours de gel (freezing days) ----
# api_url_froid <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Isotherme0_data"
# 
# res_froid <- GET(api_url_froid)
# stop_for_status(res_froid)
# 
# files_meta_froid <- fromJSON(content(res_froid, as = "text", encoding = "UTF-8"))
# 
# freezing_nc_urls <- files_meta_froid$download_url[grepl("\\.nc$", files_meta_froid$name)]
# names(freezing_nc_urls) <- files_meta_froid$name[grepl("\\.nc$", files_meta_froid$name)]
# ---- Récupérer les fichiers NetCDF depuis GitHub ----

# Petite fonction "sécure" pour éviter que l'app plante si GitHub renvoie 403/500/etc.
safe_get <- function(url) {
  res <- try(httr::GET(url), silent = TRUE)
  if (inherits(res, "try-error") || httr::http_error(res)) {
    warning("⚠️ Impossible d’accéder à ", url, " (GitHub API). Passage en mode fallback.")
    return(NULL)
  }
  res
}

# ---- Nuits tropicales ----
api_url_trop <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Tropical_data"

res_trop <- safe_get(api_url_trop)

if (!is.null(res_trop)) {
  files_meta_trop <- jsonlite::fromJSON(
    httr::content(res_trop, as = "text", encoding = "UTF-8")
  )
  
  tropical_nc_urls <- files_meta_trop$download_url[grepl("\\.nc$", files_meta_trop$name)]
  names(tropical_nc_urls) <- files_meta_trop$name[grepl("\\.nc$", files_meta_trop$name)]
  
} else {
  # Fallback si l’API GitHub ne répond pas dans le pod :
  # on force un vecteur de longueur 11 pour être compatible avec
  #   - choose_nc_url() (indices 1,5,8,11)
  #   - le mode décades (animation)
  
  trop_base <- c(
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_19900101-19991231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_20200101-20291231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_20500101-20591231.nc",
    "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Tropical_data/tropical_days_per_year_20800101-20891231.nc"
  )
  
  # On remplit 11 cases en approximant les décades :
  #  1–4  ~ climat 1990
  #  5–7  ~ climat 2020
  #  8–10 ~ climat 2050
  #  11   ~ climat 2080
  tropical_nc_urls <- rep(trop_base[1], 11)
  tropical_nc_urls[5]  <- trop_base[2]
  tropical_nc_urls[8]  <- trop_base[3]
  tropical_nc_urls[11] <- trop_base[4]
  
  names(tropical_nc_urls) <- basename(tropical_nc_urls)
}

# ---- Jours de gel (freezing days) ----
api_url_froid <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Isotherme0_data"

res_froid <- safe_get(api_url_froid)

if (!is.null(res_froid)) {
  files_meta_froid <- jsonlite::fromJSON(
    httr::content(res_froid, as = "text", encoding = "UTF-8")
  )
  
  freezing_nc_urls <- files_meta_froid$download_url[grepl("\\.nc$", files_meta_froid$name)]
  names(freezing_nc_urls) <- files_meta_froid$name[grepl("\\.nc$", files_meta_froid$name)]
  
} else {
  # Fallback complet : 11 fichiers, un par "décennie" (approx)
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
  names(freezing_nc_urls) <- basename(freezing_nc_urls)
}

# ---- Grille Alpes pour la géolocalisation ----
grid_url <- "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/Alpes_grid.nc"

# on télécharge dans un fichier temporaire (ou data/Alpes_grid.nc si préféré)
grid_dest <- file.path(tempdir(), "Alpes_grid.nc")
if (!file.exists(grid_dest)) {
  download.file(grid_url, grid_dest, mode = "wb")
}

# on ouvre le NetCDF pour récupérer lon/lat
nc_grid <- ncdf4::nc_open(grid_dest)
var_grid_names <- names(nc_grid$var)

# on essaie de trouver les variables lon/lat dans le fichier de grille
lon_var_name <- var_grid_names[grepl("lon", var_grid_names, ignore.case = TRUE)][1]
lat_var_name <- var_grid_names[grepl("lat", var_grid_names, ignore.case = TRUE)][1]

if (is.na(lon_var_name) || is.na(lat_var_name)) {
  ncdf4::nc_close(nc_grid)
  stop("Impossible d'identifier lon/lat dans Alpes_grid.nc")
}

lon_grid <- ncdf4::ncvar_get(nc_grid, lon_var_name)
lat_grid <- ncdf4::ncvar_get(nc_grid, lat_var_name)
ncdf4::nc_close(nc_grid)

# bbox de la grille Alpes
alpes_bbox <- c(
  xmin = min(lon_grid, na.rm = TRUE),
  xmax = max(lon_grid, na.rm = TRUE),
  ymin = min(lat_grid, na.rm = TRUE),
  ymax = max(lat_grid, na.rm = TRUE)
)

print(alpes_bbox)

choose_nc_url <- function(type = c("tropical", "freezing"), periode) {
  type <- match.arg(type)
  vec <- if (type == "tropical") tropical_nc_urls else freezing_nc_urls
  
  idx <- switch(
    periode,
    "1981–2010" = 1,
    "2011–2040" = 5,
    "2041–2070" = 8,
    "2071–2100" = 11,
    1  # valeur par défaut
  )
  
  # on clip au cas où il y aurait moins de fichiers que prévu
  idx <- max(1, min(idx, length(vec)))
  
  url <- vec[idx]
  message("→ Période sélectionnée : ", periode,
          " | type : ", type,
          " | fichier : ", names(vec)[idx])
  url
}

# Palette couleur perso pour les jours de gel (freezing)
pal_freezing_fn <- colorRampPalette(c("#B3DFFF", "#2EA8FF", "#004475"))

###

# ui.R

ui <- navbarPage(
  title = "Explor'Alpes",
  theme = app_theme,
  id = "navbar",
  collapsible = TRUE,
  
  # CSS custom pour la mise en page
  header = tags$head(
    tags$style(HTML("
      body, .container-fluid, .navbar, .tab-content {
        background-color: #2e2e2e !important;
        color: #eaeaea !important;
      }

      h1, h2, h3, h4, h5, h6, label, p, .navbar-brand, .nav-link {
        color: #ffffff !important;
      }

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

      .metric-box {
        background-color: #2e2e2e;
        border-radius: 12px;
        padding: 12px 16px;
        margin-top: 8px;
      }

      .metric-title {
        font-size: 0.85rem;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        opacity: 0.8;
      }

      .metric-value {
        font-size: 1.4rem;
        font-weight: 600;
      }

      .sidebar-climat {
        background-color: #3a3a3a;
        border-radius: 16px;
        padding: 16px 16px 8px 16px;
        height: 100%;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
      }

      .sidebar-climat .form-group {
        margin-bottom: 10px;
      }

      .leaflet-container {
        border-radius: 16px !important;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
      }

      .form-control, .selectize-input {
        background-color: #2b2b2b !important;
        color: #ffffff !important;
        border-radius: 10px !important;
        border: 1px solid #555 !important;
      }

      .btn-primary {
        background-color: #9ae3c4 !important;
        border-color: #9ae3c4 !important;
        color: #2b2b2b !important;
        border-radius: 10px !important;
      }

      .btn-primary:hover {
        background-color: #7ed6b0 !important;
        border-color: #7ed6b0 !important;
        color: #222 !important;
      }
    "))
  ),
  
  # Onglet principal : Dashboard ----
  tabPanel(
    "Tableau de bord",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            class = "top-block",
            h2("Évolution des jours sous 0°C et des nuits tropicales"),
            p("Visualiser les indicateurs climatiques pour la région alpine (hors Var) afin de mieux comprendre l’évolution des températures hivernales et estivales."),
            br(),
            fluidRow(
              column(
                4,
                div(
                  class = "metric-box",
                  div(class = "metric-title", "Jours sous 0°C (hiver)"),
                  div(class = "metric-value", textOutput("kpi_jours_zero"))
                )
              ),
              column(
                4,
                div(
                  class = "metric-box",
                  div(class = "metric-title", "Nuits tropicales"),
                  div(class = "metric-value", textOutput("kpi_nuits_tropicales"))
                )
              ),
              column(
                4,
                div(
                  class = "metric-box",
                  div(class = "metric-title", "Période analysée"),
                  div(class = "metric-value", textOutput("kpi_periode"))
                )
              )
            )
          )
        )
      ),
     
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-climat",
            h4("Filtres"),
            selectInput(
              "scenario",
              "Scénario climatique",
              choices = c("Historique", "RCP 4.5", "RCP 8.5"),
              selected = "Historique"
            ),
            selectInput(
              "periode",
              "Période",
              choices = c(
                "1981–2010",
                "2011–2040",
                "2041–2070",
                "2071–2100"
              ),
              selected = "1981–2010"
            ),
            checkboxGroupInput(
              "indicateurs",
              "Indicateurs à afficher",
              choices = c(
                "Jours avec Tmoy < 0°C" = "jours_zero",
                "Nuits avec Tmin ≥ 20°C" = "nuits_tropicales",
                "Isotherme 0°C (altitude)" = "iso_zero"
              ),
              selected = c("jours_zero", "nuits_tropicales")
            ),
            selectInput(
              "saison",
              "Saison",
              choices = c("Hiver (DJF)", "Année complète", "Été (JJA)"),
              selected = "Hiver (DJF)"
            ),
            # actionButton("btn_maj_carte", "Mettre à jour la carte")
            actionBttn(
              inputId = "btn_maj_carte",
              label = "Mettre à jour les données",
              icon = icon("sync"),
              style = "fill",
              color = "success",
              size = "lg"
            )
            )
        ),
        column(
          width = 9,
          leafletOutput("map_climat", height = "70vh")
        )
      )
    )
  ),

  # Onglet : Animations ----
  tabPanel(
    "Animations",
    fluidPage(
      h3("Animations jours de gel et nuits tropicales"),
      br(),
      # Ligne 0 : ligne de crête
      fluidRow(
        column(
          width = 12,
  #         div(
  #           class = "top-block",
  #           h4("Profil de crêtes avec altitudes de l'isotherme 0°C"),
  #           tags$img(
  #             src = "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/crete_animations/crete_animation.gif",
  #             style = "
  #   width: 110%;
  #   max-height: 1000px;
  #   object-fit: cover;
  #   border-radius: 10px;
  #   background-color: #222;
  # "            )
  #         )
  # Ligne 0 : ligne de crête
  fluidRow(
    column(
      width = 12,
      div(
        class = "top-block",
        h4("Profil de crêtes avec altitudes de l'isotherme 0°C"),
        div(
          style = "
          background-color:#1e1e1e;
          border-radius:16px;
          text-align:center;
          padding:15px;
          overflow-x:auto;             /* autorise le scroll horizontal si besoin */
        ",
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
      fluidRow(
        column(
          width = 6,
          div(
            class = "top-block",
            h4("Jours de gel – évolution annuelle"),
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
            tags$img(
              src = "https://raw.githubusercontent.com/justinesommerlatt/Hackathon-Meteo-France/main/isotherme_animations/freezing_days_intervals.gif",
              style = "width: 100%; max-height: 500px; object-fit: contain;"
            )
          )
        )
      ),
      
      br(),
      h3("Animations nuits tropicales"),
      br(),
      
      # Ligne 2 : nuits tropicales
      fluidRow(
        column(
          width = 6,
          div(
            class = "top-block",
            h4("Nuits tropicales – évolution annuelle"),
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
  tabPanel(
    "Guide d'utilisation",
    fluidPage(
      h3("Guide d'utilisation d'Explor'Alpes"),
      br(),
      
      h4("1. Page « Tableau de bord »"),
      p("Cette page permet d’explorer les cartes de jours de gel et de nuits tropicales pour la région alpine (hors Var)."),
      tags$ul(
        tags$li(
          strong("Scénario climatique : "),
          "choisissez soit les données historiques, soit un scénario de projection (RCP 4.5 ou RCP 8.5)."
        ),
        tags$li(
          strong("Période : "),
          "sélectionnez une période climatique (1981–2010, 2011–2040, 2041–2070, 2071–2100)."
        ),
        tags$li(
          strong("Indicateurs : "),
          "cochez les variables à afficher sur la carte :",
          tags$ul(
            tags$li("« Jours avec Tmoy < 0°C » pour les jours de gel en hiver ;"),
            tags$li("« Nuits avec Tmin ≥ 20°C » pour les nuits tropicales ;"),
            tags$li("« Isotherme 0°C (altitude) » (future extension).")
          )
        ),
        tags$li(
          strong("Saison : "),
          "choisissez la saison d’analyse (Hiver DJF, année complète, été JJA)."
        ),
        tags$li(
          strong("Bouton « Mettre à jour les données » : "),
          "après avoir réglé les filtres, cliquez pour charger les données NetCDF et rafraîchir la carte."
        ),
        tags$li(
          strong("Carte : "),
          "une carte leaflet affiche l’intensité de l’indicateur sur la région alpine, avec une légende et un recentrage automatique sur la zone d’étude."
        ),
        tags$li(
          strong("Indicateurs en haut de page : "),
          "les KPI synthétisent le nombre moyen de jours de gel, de nuits tropicales et la période analysée."
        )
      ),
      
      tags$hr(),
      h4("2. Page « Animations »"),
      p("Cette page présente des animations pré-calculées illustrant l’évolution de l’isotherme 0°C, des jours de gel et des nuits tropicales."),
      tags$ul(
        tags$li(
          strong("Profil de crêtes avec altitudes de l'isotherme 0°C : "),
          "animation montrant, pour chaque département alpin, l’altitude des principaux sommets et la position moyenne de l’isotherme 0°C sur la période récente. Elle permet de visualiser la marge de manœuvre entre les reliefs et l’altitude de 0°C."
        ),
        tags$li(
          strong("Jours de gel – évolution annuelle : "),
          "GIF montrant l’évolution année par année du nombre de jours avec température moyenne inférieure à 0°C."
        ),
        tags$li(
          strong("Jours de gel – intervalles de 20 ans : "),
          "GIF montrant des cartes moyennées par grandes périodes (20 ans) pour visualiser les tendances de fond."
        ),
        tags$li(
          strong("Nuits tropicales – évolution annuelle : "),
          "GIF montrant la progression dans le temps des nuits avec Tmin ≥ 20°C."
        ),
        tags$li(
          strong("Nuits tropicales – intervalles de 20 ans : "),
          "variation des nuits tropicales par grandes périodes, pour comparer les régimes climatiques."
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
      h3("Objectif du projet"),
      p("Informer et sensibiliser en vulgarisant les données liées à l’évolution des températures dans la région alpine (hors Var)."),
      p("L’application permet de présenter l’évolution de l’isotherme zéro en hiver, ainsi que l’émergence de nuits tropicales (température minimale supérieure à 20°C)."),
      
      tags$hr(),
      
      h3("Approche adoptée"),
      h4("1️⃣ Données"),
      p("Les données historiques et les projections climatiques (par exemple issues du jeu ESMS2-1 ALPX3, résolution 2,5 km) sont exploitées pour décrire l’évolution du climat local."),
      h4("2️⃣ Indicateurs"),
      tags$ul(
        tags$li("Nombre de jours avec température moyenne sous 0°C en hiver."),
        tags$li("Nombre de jours avec température minimale supérieure à 20°C (nuits tropicales)."),
        tags$li("Évolution de l’altitude de l’isotherme 0°C.")
      ),
      h4("3️⃣ Visualisation"),
      p("Un tableau de bord interactif permet d’explorer ces indicateurs à l’aide de cartes et de graphiques, avec des messages clés destinés à faciliter la compréhension des enjeux."),
      
      tags$hr(),
      
      h3("Usages visés"),
      tags$ul(
        tags$li("Sensibilisation aux risques de santé publique liés aux fortes chaleurs estivales."),
        tags$li("Compréhension de la diminution des jours de gel et de ses impacts (neige, glaciers, écosystèmes, activités économiques)."),
        tags$li("Mise en évidence des disparités spatiales au sein de la région alpine.")
      ),
      
      tags$hr(),
      
      h3("Équipe"),
      tags$ul(
        tags$li("Julien AVINÉE (Consultant adaptation - Carbone 4)"),
        tags$li("Madeleine D’ARRENTIERES (Consultante adaptation - Carbone 4)"),
        tags$li("Maëlle ABRAHAM (Consultante adaptation - Carbone 4)"),
        tags$li("Etienne PAUTHENET (Data Scientist - IRD Brest)"),
        tags$li("Lucio LURASCHI (Ingénieur logiciel - EDF)"),
        tags$li("Sandrine PARADOWSKI (Géomaticienne - DDT 77)"),
        tags$li("Romuald WEIDMANN (Développeur R - INSEE)"),
        tags$li("Justine SOMMERLATT (Data Scientist - BKW)")
      ),
      
      tags$hr(),
      
      h3("Contexte"),
      p("Application développée dans le cadre du hackathon « Climat des données », défi datavisualisation sur les indicateurs climatiques alpins."),
      p("Projet collaboratif associant profils data science, climatologie et développement Shiny pour produire un outil pédagogique et réutilisable.")
    )
  )
)

### server.r
server <- function(input, output, session) {
  
  # ---- KPI réactifs ----
  kpi_nuits <- reactiveVal("—")
  kpi_froid <- reactiveVal("—")
  
  # Vecteurs de temps pour l'animation ----
  periodes_vec  <- c("1981–2010", "2011–2040", "2041–2070", "2071–2100")
  # À adapter si on a moins/plus de fichiers, mais l'idée est :
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
  # Carte animée initiale ----
  output$map_anim <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 6.5, lat = 45.5, zoom = 7)
  })
  
  # Slider dynamique selon le mode (périodes / décades) ----
  output$anim_slider_ui <- renderUI({
    if (req(input$anim_mode) == "periode") {
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
  output$anim_periode_label <- renderText({
    req(input$anim_mode, input$anim_index)
    if (input$anim_mode == "periode") {
      periodes_vec[input$anim_index]
    } else {
      decennies_vec[input$anim_index]
    }
  })
  
  # Animation : met à jour la carte selon indicateur + période/décennie ----
  observeEvent(input$anim_index, {
    req(input$anim_mode, input$anim_indic, input$anim_index)
    
    # --- choix texte affiché & URL NetCDF ---
    if (input$anim_mode == "periode") {
      # Mode périodes climatiques : on réutilise choose_nc_url()
      periode_label <- periodes_vec[input$anim_index]
      type_sel      <- input$anim_indic  # "tropical" ou "freezing"
      url_sel       <- choose_nc_url(type_sel, periode_label)
      leg_title     <- if (type_sel == "tropical") {
        paste0("Nuits tropicales / an (", periode_label, ")")
      } else {
        paste0("Jours avec Tmoy < 0°C (", periode_label, ")")
      }
    } else {
      # Mode décades : on prend directement le i-ème fichier
      type_sel <- input$anim_indic
      vec      <- if (type_sel == "tropical") tropical_nc_urls else freezing_nc_urls
      
      idx <- max(1, min(input$anim_index, length(vec)))
      url_sel   <- vec[idx]
      dec_label <- decennies_vec[idx]
      
      leg_title <- if (type_sel == "tropical") {
        paste0("Nuits tropicales / an (", dec_label, ")")
      } else {
        paste0("Jours avec Tmoy < 0°C (", dec_label, ")")
      }
    }
    
    # Logs
    cat("\n=== [ANIM] Mise à jour de la carte ===\n")
    cat("→ Mode temporel :", input$anim_mode, "\n")
    cat("→ Indicateur    :", input$anim_indic, "\n")
    cat("→ URL NetCDF    :", url_sel, "\n")
    
    # Chargement du raster
    dest <- file.path(tempdir(), basename(url_sel))
    if (!file.exists(dest)) {
      download.file(url_sel, dest, mode = "wb")
    }
    
    r_raster <- raster::raster(dest)
    raster::extent(r_raster) <- unname(alpes_bbox)
    raster::crs(r_raster)    <- "+proj=longlat +datum=WGS84 +no_defs"
    
    vals <- raster::values(r_raster)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) {
      showNotification("Pas de valeurs numériques dans le raster chargé (animation).", type = "error")
      return()
    }
    
    # Palette conditionnelle : bleu pour jours de gel, viridis pour nuits tropicales
    if (type_sel == "freezing") {
      pal <- colorNumeric(
        palette  = pal_freezing_fn(256),
        domain   = vals,
        na.color = "transparent"
      )
    } else {
      pal <- colorNumeric(
        palette  = "viridis",
        domain   = vals,
        na.color = "transparent"
      )
    }
    
    
    e <- raster::extent(r_raster)
    
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
  
  # ---- KPI réactifs ----
  # kpi_nuits <- reactiveVal("—")
  # kpi_froid <- reactiveVal("—")
  
  # KPIs (placeholders à remplacer par de vrais calculs) ----
  output$kpi_jours_zero <- renderText({
    kpi_froid()
  })
  
  output$kpi_nuits_tropicales <- renderText({
    kpi_nuits()
  })
  
  output$kpi_periode <- renderText({
    input$periode
  })
  
  # Carte initiale ----
  output$map_climat <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      # centre approximatif Alpes
      setView(lng = 6.5, lat = 45.5, zoom = 7)
  })
  # ---- Graphique ligne de crête ----
  # ---- Graphique ligne de crête ----
  output$plot_ligne_crete <- renderPlot({
    df <- ligne_crete_df
    
    # On garde les lignes où l'altitude du sommet est renseignée
    df <- df[!is.na(df$Altitude_Sommet), , drop = FALSE]
    
    if (!nrow(df)) {
      plot.new()
      text(0.5, 0.5, "Aucun sommet avec altitude renseignée.", cex = 0.9)
      return()
    }
    
    # On ordonne par altitude décroissante pour un effet "ligne de crête"
    ord <- order(df$Altitude_Sommet, decreasing = TRUE)
    df  <- df[ord, ]
    
    # Couleurs par département
    dep_fac <- factor(df$Département)
    cols    <- as.numeric(dep_fac)
    
    # Plot en "barres verticales" (type 'h' = segments depuis 0)
    plot(
      df$Altitude_Sommet,
      type = "h",
      lwd  = 4,
      col  = cols,
      xaxt = "n",
      xlab = "",
      ylab = "Altitude du sommet (m)",
      main = "Altitudes des principaux sommets par département"
    )
    
    # Étiquettes en x = noms de sommets (raccourcis)
    noms_sommets <- trimws(df$Nom_Sommet)
    axis(
      1,
      at = seq_along(df$Altitude_Sommet),
      labels = noms_sommets,
      las = 2,
      cex.axis = 0.5
    )
    
    # Légende des départements
    legend(
      "topright",
      legend = levels(dep_fac),
      col    = seq_along(levels(dep_fac)),
      lwd    = 4,
      cex    = 0.6,
      bty    = "n"
    )
  })
  
  
  # Mise à jour de la carte + logs ----
  observeEvent(input$btn_maj_carte, {
    
    # --- LOGS ---
    cat("\n=== [LOG] Bouton 'Mettre à jour' cliqué ===\n")
    cat("→ Période sélectionnée :", input$periode, "\n")
    cat("→ Indicateurs sélectionnés :", paste(input$indicateurs, collapse = ", "), "\n")
    
    ind <- input$indicateurs
    
    # On choisit quel type de données charger
    if ("nuits_tropicales" %in% ind) {
      type_sel  <- "tropical"
      url_sel   <- choose_nc_url("tropical", input$periode)
      leg_title <- "Nuits tropicales / an"
      cible_kpi <- "nuits"
      
    } else if ("jours_zero" %in% ind) {
      type_sel  <- "freezing"
      url_sel   <- choose_nc_url("freezing", input$periode)
      leg_title <- "Jours avec Tmoy < 0°C"
      cible_kpi <- "froid"
      
    } else {
      showNotification("Sélectionner au moins un indicateur (nuits tropicales ou jours de gel).",
                       type = "warning")
      return()
    }
    
    cat("→ Type de données choisi :", type_sel, "\n")
    cat("→ URL NetCDF :", url_sel, "\n")
    cat("→ Fichier NetCDF local :", basename(url_sel), "\n")
    
    message("Chargement du NetCDF (téléchargement local si besoin) : ", url_sel)
    
    dest <- file.path(tempdir(), basename(url_sel))
    if (!file.exists(dest)) {
      download.file(url_sel, dest, mode = "wb")
    }
    
    r_raster <- raster::raster(dest)
    raster::extent(r_raster) <- unname(alpes_bbox)
    raster::crs(r_raster)    <- "+proj=longlat +datum=WGS84 +no_defs"
    
    vals <- raster::values(r_raster)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) {
      showNotification("Pas de valeurs numériques dans le raster chargé.", type = "error")
      return()
    }
    
    if (type_sel == "freezing") {
      pal <- colorNumeric(
        palette  = pal_freezing_fn(256),
        domain   = vals,
        na.color = "transparent"
      )
    } else {
      pal <- colorNumeric(
        palette  = "viridis",
        domain   = vals,
        na.color = "transparent"
      )
    }
    
    kpi_val <- round(mean(vals, na.rm = TRUE), 1)
    if (cible_kpi == "nuits") {
      kpi_nuits(paste0(kpi_val, " nuits/an (moyenne maille)"))
    } else {
      kpi_froid(paste0(kpi_val, " jours/an (moyenne maille)"))
    }
    
    e <- raster::extent(r_raster)
    
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
  })
  
  
}

shinyApp(ui = ui, server = server)