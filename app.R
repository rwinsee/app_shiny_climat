# Packages ----
liste_packages <- c(
  "shiny",
  "bslib",
  "leaflet"
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

# à TODO : ici on chargera les données climatiques (scénarios, mailles, etc.)
# ex :
# data_zero <- readRDS("data/data_zero.rds")
# data_tropicales <- readRDS("data/data_tropicales.rds")

###

# ui.R

ui <- navbarPage(
  title = "Climat des données – Jours sous 0°C",
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
            actionButton("btn_maj_carte", "Mettre à jour la carte")
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
  
  # KPIs (placeholders à remplacer par de vrais calculs) ----
  output$kpi_jours_zero <- renderText({
    "—"   # ex : round(mean(data_zero$nb_jours_zero), 1)
  })
  
  output$kpi_nuits_tropicales <- renderText({
    "—"   # ex : round(mean(data_tropicales$nb_nuits_trop), 1)
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
  
  # Mise à jour de la carte selon les filtres (placeholder) ----
  observeEvent(input$btn_maj_carte, {
    # Ici tu ajouteras tes polygones / tuiles / cercles selon les indicateurs
    leafletProxy("map_climat") |>
      clearMarkers() |>
      # ex : addPolygons(...) ou addRasterImage(...)
      addPopups(
        lng = 6.5, lat = 45.5,
        popup = paste(
          "Scénario :", input$scenario, "<br>",
          "Période :", input$periode, "<br>",
          "Saison :", input$saison
        )
      )
  })
}

shinyApp(ui = ui, server = server)