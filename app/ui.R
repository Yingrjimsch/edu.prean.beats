library("shiny")
library("shinythemes")
library("bslib")

# ui
ui <- navbarPage(
  hr(),
  theme = bs_theme(bootswatch = "vapor"),
  tags$head(
    tags$style(HTML("
            .navbar {
                height: 70px;
            }
            .navbar-brand, .navbar-nav li a {
                line-height: 70px;
            }
        "))
  ),
  title = strong("Von Beats zur Beliebtheit"),
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
  tabPanel(title = "Modellanwendung",
           uiOutput("dynamischeInputs"),
           mainPanel(
             
           )
  )
  ,
  tabPanel(title = "Über uns",
           "Annaheim, Fabian C. | Nobel, Gabriel | von Wartburg Rebekka | Waldburger, Safiyya")
)