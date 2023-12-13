library("shiny")
library("shinythemes")
library("bslib")

# ui
ui <- navbarPage(
  title = strong("Von Beats zur Beliebtheit"),
  theme = bs_theme(bootswatch = "vapor"),
  header = tags$head(
    tags$style(HTML("
            .navbar {
                height: 70px;
            }
            .navbar-brand, .navbar-nav li a {
                line-height: 70px;
                font-size: 20px;
            }
        "))
  ),
  tabPanel(title = "Home", 
           h4(style="text-align: center;",
              strong("Semesterarbeit Modul Predictive Analytics HS23")),
           br(),
           uiOutput("titelbild"),
           br(),
           p(style="text-align: center;","Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya")
  ),
  tabPanel(title = "Bedienungsanleitung",
           h4(strong("Beschreibung der App")),
           p(style="text-align: justify; font-size = 25px",
             "Beschreibung der App: Navigation (wo ist was zu finden);Datenbeschreibung; Modelleistungen (was wurde gemessen);
             Modellanwendung (was kann die App); über uns Kontaktangaben"),
           hr()),
  tabPanel(title = "Datenbeschreibung",
           h4(strong("Datenbschreibung")),
           p(style="text-align: justify; font-size = 25px",
             "Beschreibung aller zur Verfügung stehender Prädiktoren...."),
           hr()),
  tabPanel(title = "Modelleistungen",
           h4(strong("Ergebnisse zur Leistung der einzelnen Modelle")),
           selectInput("datensatzAuswahl1", "Wählen Sie einen Datensatz:", 
                       choices = c("spotify_songs_cleaned_with_trans", "spotify_songs_cleaned_with_trans_optima", "spotify_songs_cleaned_without_trans")),
           
           selectInput("modellAuswahl", "Wählen Sie ein Modell:", 
                       choices = c("Multiple lineare Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
           uiOutput("modellGueteOptionen"),
           uiOutput("modellGueteErgebnis"),
           br(),br(),
           mainPanel(
             uiOutput("dynamischeModellGuete")
             # imageOutput("observedPredicted"),
             # uiOutput("summaryOutput"),
             # imageOutput("plotTree"),
             # uiOutput("results")
             
           ),
           
           hr()
  ),
  tabPanel(title = "Modellanwendung",
           h4(strong("Modellvorhersage anhand neuer Beobachtung")),
           selectInput("datensatzAuswahl2", "Wählen Sie einen Datensatz:", 
                       choices = c("spotify_songs_cleaned_with_trans", "spotify_songs_cleaned_with_trans_optima", "spotify_songs_cleaned_without_trans")),
           selectInput("modellBestimmung", "Wählen Sie ein Modell:", 
                       choices = c("Multiple lineare Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
           p("Für jeden Prädiktor kann der Wert aus der neuen Beobachtung eingetragen
                    werden und danach mittels em(Vorhersage machen)
                    die Vorhersage für die neue Beobachtung berechnet werden."),
           uiOutput("vorhersageBereich"),
           uiOutput("dynamischeInputs"),
           hr()
  )
  ,
  tabPanel(title = "Über uns",
           h4(strong("Das Team von \"Von Beats zur Beliebtheit\"")),
           tableOutput("team"),
           #p(style="text-align: center;","Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya"),
           uiOutput("teambild")
           )
  
)