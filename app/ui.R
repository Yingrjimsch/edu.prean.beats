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
           p(style="text-align: justify; font-size = 25px",
             "Herzlich willkommen auf unserer Predictive Analytics Website, Ihrem umfassenden Werkzeug für datengetriebene Entscheidungen und zukunftsweisende Vorhersagen bezüglich Spotify Hits. Unsere Plattform bietet eine Vielzahl von Funktionen, um Ihnen Einblicke in unsere Semeseterarbeit zu verschaffen."),
           hr(),
           h4(strong("Datenbeschreibung")),
           p(style="text-align: justify; font-size = 25px",
             "Unter der Menüoption \"Datenbeschreibung\" erhalten Sie einen detaillierten Überblick über die Prädiktoren, die in unseren Modellen verwendet werden. Diese Transparenz ermöglicht es Ihnen hoffentlich, die Daten besser zu verstehen."),
           hr(),
           h4(strong("Modellleistungen")),
           p(style="text-align: justify; font-size = 25px",
             "In der Sektion \"Modellleistungen\" präsentieren wir Ihnen visuell ansprechende Plots zwischen Observed vs Predicted, Summary und Results der verschiedenen Modelle, die wir anbieten. Von Regressionsbäumen über Bagged Regressionsbäume bis hin zu K-nearest Neighbours und Multiple linear regression – wir ermöglichen Ihnen einen direkten Vergleich der Leistungsfähigkeit dieser Modelle. Unsere intuitiven Grafiken geben Ihnen die Möglichkeit, die Stärken und Schwächen der Modelle auf einen Blick zu erfassen."),
           hr(),
           h4(strong("Modellanwendung")),
           p(style="text-align: justify; font-size = 25px",
             "Die \"Modellanwendung\" ist der Ort, an dem Sie die Kontrolle übernehmen können. Hier können Sie die verschiedenen Prädiktoren sowohl individuell anpassen als auch optimal transformieren und sehen sofort, wie sich diese Änderungen auf die Vorhersage auswirken. Unser interaktives Interface erlaubt es Ihnen, Szenarien zu simulieren und die Auswirkungen verschiedener Entscheidungen auf Ihre Zielvariablen zu testen. Dieser Abschnitt ist Ihr Werkzeug, um die Erkenntnisse aus den Modellen in die Praxis umzusetzen."),
           hr(),
           p(strong("Bitte beachten Sie, dass der detaillierte Bericht einen tieferen Einblick in Ihre Semesterarbeit gewährt, indem er spezifische Aspekte und Zusammenhänge aufschlüsselt. Trotz der nützlichen Einblicke sollten Sie sich bewusst sein, dass die präsentierten Regressionsmodelle aufgrund ihrer Komplexität und begrenzten Daten möglicherweise nicht die höchste Genauigkeit aufweisen."))),
  tabPanel(title = "Datenbeschreibung",
           h4(strong("Datenbeschreibung")),
           p(HTML("Die Prädiktoren für die App <strong>Von Beats zur Beliebtheit</strong> basieren auf einem Datensatz von
            <a href='https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023' target='_blank'>Kaggle</a>. 
             Während der Datenaufbereitung wurden einige Prädiktoren entfernt, modifiziert oder neu hinzugefügt.<br>
             Um eine bessere Annäherung an die Normalverteilung zu erreichen und die Modellleistung bei einigen Methoden optimieren zu können,
             wurden zusätzlich bestimmte Prädiktoren auch transformiert verwendet. Um den Vergleich diesbezüglich zu erhalten kann
             in den Abschnitten Modellleistung und Modellanwendung die Prädiktoren mit und ohne Transformation verwendet. <br>
             Insgesamt sind 21 Prädiktoren verbleiben, welche für das Training der Modelle und zur Vorhersage der Anzahl an Streams verwendet. <br><br>
             Im Folgenden finden Sie eine Liste dieser Prädiktoren mit einer kurzen Erläuterung zu ihrer Bedeutung.")),
           tags$ul(
             tags$li(strong("listeners_cum:"), "Monatliche Anzahl Streams pro Interpret:in des Songs. Sind an einem Song mehrere Interpreten beteiligt, wird der mean aus den Werten verwendet"),
             tags$li(strong("artist_count:"), "Anzahl der am Song beteiligten Interpreten:innen"),
             tags$li(strong("released_weekday:"), "Wochentag der Veröffentlichung des Songs"),
             tags$li(strong("years_since_release:"), "Anzahl Jahre seit der Veröffentlichung des Songs bis 2023"),
             tags$li(strong("released_month:"), "Monat der Veröffentlichung des Songs"),
             tags$li(strong("in_spotify_playlists:"), "Anzahl der Spotify-Playlists, in denen der Song enthalten ist. Wurde auch transformiert mittels Log verwendet"),
             tags$li(strong("in_spotify_charts:"), "Rangierung des Songs in den Spotify-Charts"),
             tags$li(strong("in_apple_charts:"), "Anzahl der Apple Music-Playlists, in denen der Titel enthalten ist"),
             tags$li(strong("in_deezer_playlists:"), "Anzahl der Deezer-Playlists, in denen der Song enthalten ist"),
             tags$li(strong("in_deezer_charts:"), "Rangierung des Songs in den Deezer-Charts"),
             tags$li(strong("in_shazam_charts:"), "Rangierung des Songs in den Shazam-Charts"),
             tags$li(strong("bpm:"), "Beats per Minute, ein Mass für das Songtempo. Wurde auch transformiert mittels Log verwendet"),
             tags$li(strong("key:"), "Tonart des Songs"),
             tags$li(strong("mode:"), "Modus des Songs (Dur oder Moll)"),
             tags$li(strong("danceability_.:"), "Prozentsatz, der angibt, wie geeignet das Lied zum Tanzen ist"),
             tags$li(strong("valence_.:"), "Positivität des musikalischen Inhalts des Songs"),
             tags$li(strong("energy_.:"), "Energieniveau des Songs. Wurde auch transformiert mittels BoxCox verwendet"),
             tags$li(strong("acousticness_.:"), "Anteil des akustischen Klangs im Song"),
             tags$li(strong("liveness_.:"), "Vorhandensein von Live-Performance-Elementen"),
             tags$li(strong("speechiness_.:"), "Anzahl der gesprochenen Worte im Song"),
           )),
  tabPanel(title = "Modelleistungen",
           h4(strong("Ergebnisse zur Leistung der einzelnen Modelle")),
           selectInput("datensatzAuswahl", "Wählen Sie einen Datensatz:", 
                       choices = c("Daten mit Transformationen", "Daten ohne Transformationen")),
           selectInput("modellAuswahl", "Wählen Sie ein Modell:", 
                       choices = c("Multiple lineare Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
           uiOutput("modellGueteOptionen"),
           uiOutput("modellGueteErgebnis"),
           br(),br(),
           mainPanel(
             uiOutput("dynamischeModellGuete")
           ),
           hr()
  ),
  tabPanel(title = "Modellanwendung",
           h4(strong("Modellvorhersage anhand neuer Beobachtung")),
           selectInput("modellBestimmung", "Wählen Sie ein Modell:", 
                       choices = c("Multiple lineare Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
           p("Für jeden Prädiktor kann der Wert aus der neuen Beobachtung eingetragen
                    werden und danach mittels", strong("Vorhersage ohne Transformationen"),
                    "die Vorhersage für die neue Beobachtung berechnet werden."),
           uiOutput("vorhersageBereich"),
           mainPanel(uiOutput("dynamischeInputs")),
           hr()
  )
  ,
  tabPanel(title = "Über uns",
           h4(strong("Das Team von \"Von Beats zur Beliebtheit\"")),
           tableOutput("team"),
           uiOutput("teambild")
           )
  
)