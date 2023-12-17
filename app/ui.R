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
           h2(style="text-align: center;",
              strong(HTML("&#x1F39C;"),HTML("&#x1F39D;"), "Von Beats zur Beliebtheit", HTML("&#x1F39D;"), HTML("&#x1F39C;")),),
           p(style="text-align: center;","Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya"),
           br(),
           div(style = "text-align: center;",
           uiOutput("titelbild"),
           a("data-science-in-the-music-industry", href = "https://godatadrive.com/blog/data-science-in-the-music-industry", target = "_blank"),
           br(),
           )
  ),
  tabPanel(title = "Bedienungsanleitung",
           h2(strong("Herzlich Willkommen", HTML("&#x1F39D;"), HTML("&#x1F39C;"), HTML("&#x1F39D;"))),
           p(style="text-align: justify; font-size = 25px",
             "Wir, das Team von ", strong("Von Beats zur Beliebtheit"), " begrüsst Sie auf unserer Predictive Analytics Website Ihrem Werkzeug für datengetriebene Entscheidungen und zukunftsweisende Vorhersagen bezüglich Spotify Songs und Beliebtheit.
              Unsere Plattform bietet eine Vielzahl von Funktionen, um Ihnen Einblicke in unsere Semeseterarbeit zu verschaffen."),
           hr(),
           h4(strong("Datenbeschreibung")),
           p(style="text-align: justify; font-size = 25px",
             "Unter der Menüoption ", strong("Datenbeschreibung"), "erhalten Sie einen detaillierten Überblick über die Prädiktoren, die in unseren Modellen verwendet werden.
              Diese Transparenz ermöglicht es Ihnen die einzelnen Prädiktoren besser verstehen zu können."),
           hr(),
           h4(strong("Modellleistungen")),
           p(style="text-align: justify; font-size = 25px",
             "Unter ", strong("Modellleistungen"), "präsentieren wir Ihnen visuell ansprechende Plots zwischen Observed vs Predicted, Summary und Results der verschiedenen Methoden, welche wir in dieser Semesterarbeit verwendet haben.
              (Multiple linear Regression, k-nearest Neighbours, Regressionsbaum und Bagged-Regressionsbaum).
              Diese ermöglichen Ihnen einen direkten Vergleich der Leistungsfähigkeit der angewendeten Methoden. Unsere intuitiven Grafiken geben Ihnen die Möglichkeit, die Stärken und Schwächen der Modelle auf einen Blick erfassen zu können."),
           hr(),
           h4(strong("Modellanwendung")),
           p(style="text-align: justify; font-size = 25px",
             "Die ", strong("Modellanwendung"), "ist der Ort, an dem Sie die Kontrolle übernehmen können. Hier können Sie die verschiedenen Prädiktoren sowohl individuell anpassen als auch optimal transformieren und sehen sofort, wie sich diese Änderungen auf die Vorhersage auswirken.
              Zu beachten ist dabei jedoch, dass Vorhersagen nur mittels der Methoden", strong("Multiple lineare Regression"), "und", strong("k-nearest Neighbours"), "möglich sind.
              Auf die Thematik, weshalb die baumbasierten Methoden",  strong("Regressionsbaum"), "und", strong("Bagged-Regressionsbaum"), "wird in der Semesterarbeit vertieft eingegangen.
              Unser interaktives Interface erlaubt es Ihnen, Szenarien zu simulieren und die Auswirkungen verschiedener Entscheidungen auf Ihre Zielvariablen zu testen.
              Dieser Abschnitt ist Ihr Werkzeug, um die Erkenntnisse aus den Modellen in die Praxis umzusetzen."),
           hr(),
           h5(strong("Hinweis:")),
           p("Bitte beachten Sie, dass der detaillierte Bericht einen tieferen Einblick in Ihre Semesterarbeit gewährt,
             indem er spezifische Aspekte und Zusammenhänge aufschlüsselt.
             Trotz der nützlichen Einblicke sollten Sie sich bewusst sein, dass die präsentierten Regressionsmodelle aufgrund ihrer Komplexität und begrenzten Daten möglicherweise nicht die höchste Genauigkeit aufweisen.")),
  tabPanel(title = "Datenbeschreibung",
           h4(strong("Datenbeschreibung")),
           p(HTML("Die Prädiktoren für die App <strong>Von Beats zur Beliebtheit</strong> basieren auf einem Datensatz von
            <a href='https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023' target='_blank'>Kaggle</a>. 
             Während der Datenaufbereitung wurden einige Prädiktoren entfernt, modifiziert oder neu hinzugefügt.<br>
             Um eine bessere Annäherung an die Normalverteilung zu erreichen und die Modellleistung bei einigen Methoden optimieren zu können,
             wurden zusätzlich bestimmte Prädiktoren auch transformiert verwendet. <br>
             In den Menüoptionen <strong>Modellleistung</strong> und <strong>Modellanwendung </strong> wird Ihnen der Vergleich der Prädiktoren mit und ohne Transformation ermöglicht. <br>
             Insgesamt sind schlussendlich 21 Prädiktoren vorhanden, welche für das Training der Modelle und zur Vorhersage der Anzahl an Streams verwendet werden. <br><br>
             Im Folgenden finden Sie eine Liste dieser Prädiktoren mit einer kurzen Erläuterung zu ihrer Bedeutung:")),
           tags$ul(
             tags$li(
               strong("listeners_cum:"), 
               div(style = "margin-left: 20px;",
                     "Monatliche Anzahl Streams pro Interpret:in des Songs. Sind an einem Song mehrere Interpreten beteiligt, wird der mean aus den Werten verwendet")
                   ),
             tags$li(
               strong("artist_count:"),
               div(style = "margin-left: 20px;",
                   "Anzahl der am Song beteiligten Interpreten:innen")
             ),
             tags$li(
               strong("released_weekday:"), 
               div(style = "margin-left: 20px;",
               "Wochentag der Veröffentlichung des Songs")
              ),
             tags$li(
               strong("years_since_release:"),
               div(style = "margin-left: 20px;",
               "Anzahl Jahre seit der Veröffentlichung des Songs bis 2023")
              ),
             tags$li(
               strong("released_month:"),
               div(style = "margin-left: 20px;",
               "Monat der Veröffentlichung des Songs")
              ),
             tags$li(
               strong("in_spotify_playlists:"),
               div(style = "margin-left: 20px;",
                "Anzahl der Spotify-Playlists, in denen der Song enthalten ist.", em("Wurde auch transformiert mittels Log verwendet"))
              ),
             tags$li(
               strong("in_spotify_charts:"), 
               div(style = "margin-left: 20px;",
               "Rangierung des Songs in den Spotify-Charts")
              ),
             tags$li(
               strong("in_apple_charts:"),
               div(style = "margin-left: 20px;",
               "Anzahl der Apple Music-Playlists, in denen der Titel enthalten ist")
              ),
             tags$li(
               strong("in_deezer_playlists:"), 
               div(style = "margin-left: 20px;",
               "Anzahl der Deezer-Playlists, in denen der Song enthalten ist")
              ),
             tags$li(
               strong("in_deezer_charts:"),
               div(style = "margin-left: 20px;",
               "Rangierung des Songs in den Deezer-Charts")
              ),
             tags$li(
               strong("in_shazam_charts:"), 
               div(style = "margin-left: 20px;",
               "Rangierung des Songs in den Shazam-Charts")
              ),
             tags$li(
               strong("bpm:"),
               div(style = "margin-left: 20px;",
               "Beats per Minute, ein Mass für das Songtempo.", em("Wurde auch transformiert mittels Log verwendet"))
              ),
             tags$li(
               strong("key:"), 
               div(style = "margin-left: 20px;",
               "Tonart des Songs")
              ),
             tags$li(
               strong("mode:"), 
               div(style = "margin-left: 20px;",
               "Modus des Songs (Dur oder Moll)")
              ),
             tags$li(
               strong("danceability_.:"),
               div(style = "margin-left: 20px;",
               "Prozentsatz, der angibt, wie geeignet das Lied zum Tanzen ist")
              ),
             tags$li(
               strong("valence_.:"),
               div(style = "margin-left: 20px;",
               "Positivität des musikalischen Inhalts des Songs")
              ),
             tags$li(
               strong("energy_.:"),
               div(style = "margin-left: 20px;",
               "Energieniveau des Songs.", em("Wurde auch transformiert mittels BoxCox verwendet"))
              ),
             tags$li(
               strong("acousticness_.:"), 
               div(style = "margin-left: 20px;",
               "Anteil des akustischen Klangs im Song")
              ),
             tags$li(
               strong("liveness_.:"), 
               div(style = "margin-left: 20px;",
               "Vorhandensein von Live-Performance-Elementen")
              ),
             tags$li(
               strong("speechiness_.:"),
               "Anzahl der gesprochenen Worte im Song")
             ),
           ),
  tabPanel(title = "Modelleistungen",
           h4(strong("Ergebnisse zur Leistung der einzelnen Modelle")),
           selectInput("modellAuswahl", "Wählen Sie ein Modell:", 
                       choices = c("Multiple lineare Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
           selectInput("datensatzAuswahl", "Wählen Sie einen Datensatz:", 
                       choices = c("Daten mit Transformationen", "Daten ohne Transformationen")),
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
                       #choices = c("Multiple lineare Regression", "k-Nearest Neighbors", "Regressionsbaum", "Bagged-Regressionsbaum")),
                       choices = c("Multiple lineare Regression", "k-Nearest Neighbors")),
           p("Für jeden Prädiktor kann der Wert aus der neuen Beobachtung eingetragen
                    werden und danach mittels", strong("Vorhersage ohne Transformationen"), "oder ", strong("Vorhersage mit Transformationen"),
                    "die Vorhersage für die neue Beobachtung berechnet werden."),
           uiOutput("vorhersageBereich"),
           mainPanel(uiOutput("dynamischeInputs")),
           hr()
  )
  ,
  tabPanel(title = "Über uns",
           h4(strong("Das Team von \"Von Beats zur Beliebtheit\"")),
           tableOutput("team"),
           uiOutput("teambild"),
           a("data-science-in-the-music-industry", href = "https://godatadrive.com/blog/data-science-in-the-music-industry", target = "_blank")
           )
  
)