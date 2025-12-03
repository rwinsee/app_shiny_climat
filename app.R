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

# ---- Récupérer les fichiers NetCDF depuis GitHub ----
# ---- Nuits tropicales ----
api_url_trop <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Tropical_data"

res_trop <- GET(api_url_trop)
stop_for_status(res_trop)

files_meta_trop <- fromJSON(content(res_trop, as = "text", encoding = "UTF-8"))

tropical_nc_urls <- files_meta_trop$download_url[grepl("\\.nc$", files_meta_trop$name)]
names(tropical_nc_urls) <- files_meta_trop$name[grepl("\\.nc$", files_meta_trop$name)]


# ---- Jours de gel (freezing days) ----
api_url_froid <- "https://api.github.com/repos/justinesommerlatt/Hackathon-Meteo-France/contents/Isotherme0_data"

res_froid <- GET(api_url_froid)
stop_for_status(res_froid)

files_meta_froid <- fromJSON(content(res_froid, as = "text", encoding = "UTF-8"))

freezing_nc_urls <- files_meta_froid$download_url[grepl("\\.nc$", files_meta_froid$name)]
names(freezing_nc_urls) <- files_meta_froid$name[grepl("\\.nc$", files_meta_froid$name)]

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
  
  # Onglet : À propos ----
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
        tags$li("Sandrine PARADOWSKI (Géomaticienne - DDT 77 )"),
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
    
    pal <- colorNumeric("viridis", domain = vals, na.color = "transparent")
    
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