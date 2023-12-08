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
           p(style="text-align: center;",
             strong("Semesterarbeit Modul Predictive Analytics HS23")),
           imageOutput("titelbild"),
           hr(),
           p(style="text-align: center;","Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya")
  ),
  tabPanel(title = "Bedienungsanleitung",
           h4(strong("Beschreibung...")),
           p(style="text-align: justify; font-size = 25px",
             "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
                        sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
                        sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.
                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
                        Lorem ipsum dolor sit amet,
                        consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
                        sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.
                        Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
           hr()),
  tabPanel(title = "Modelleistungen",
           h4(strong("Güte der einzelnen Modelle")),
           selectInput("datensatzAuswahl1", "Wählen Sie einen Datensatz:", 
                       choices = c("spotify_songs_cleaned_with_trans", "spotify_songs_cleaned_with_trans_optima", "spotify_songs_cleaned_without_trans")),
           
           selectInput("modellAuswahl", "Wählen Sie ein Modell:", 
                       choices = c("Multivariate Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
           uiOutput("modellGueteOptionen"),
           mainPanel(
             uiOutput("modellGueteErgebnis"),
             #imageOutput("observedPredicted"),
             uiOutput("summaryOutput"),
             #imageOutput("plotTree"),
             uiOutput("results")
             
           ),
           
           hr()
  ),
  tabPanel(title = "Modellanwendung",
           h4(strong("Modellvorhersage anhand neuer Beobachtung")),
           selectInput("datensatzAuswahl2", "Wählen Sie einen Datensatz:", 
                       choices = c("spotify_songs_cleaned_with_trans", "spotify_songs_cleaned_with_trans_optima", "spotify_songs_cleaned_without_trans")),
           selectInput("modellBestimmung", "Wählen Sie ein Modell:", 
                       choices = c("Multivariate Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
           p("Für jeden Prädiktor kann der Wert aus der neuen Beobachtung eingetragen
                    werden und danach mittels em(Vorhersage machen)
                    die Vorhersage für die neue Beobachtung berechnet werden."),
           uiOutput("vorhersageBereich"),
           uiOutput("dynamischeInputs"),
           hr()
  )
  ,
  tabPanel(title = "Über uns",
           "Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya")
)