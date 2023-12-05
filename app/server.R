library("shiny")


load("../data/spotify_songs_cleaned_with_trans.RData")
load("../data/spotify_songs_cleaned_with_trans_optima.RData")
load("../data/spotify_songs_cleaned_without_trans.RData")



# server
server <- function(input, output) {
  output$dynamischeInputs <- renderUI({
    spotify_songs_cleaned_without_trans <- spotify_songs_cleaned_without_trans[-which(names(spotify_songs_cleaned_without_trans) == "streams")]
    inputList <- lapply(names(spotify_songs_cleaned_without_trans), function(var) {
      if(is.numeric(spotify_songs_cleaned_without_trans[[var]])) {
        sliderInput(inputId = var, 
                    label = paste(var),
                    min = min(spotify_songs_cleaned_without_trans[[var]], na.rm = TRUE), 
                    max = max(spotify_songs_cleaned_without_trans[[var]], na.rm = TRUE), 
                    value = mean(spotify_songs_cleaned_without_trans[[var]], na.rm = TRUE))
      } else if(is.factor(spotify_songs_cleaned_without_trans[[var]])) {
        selectInput(inputId = var, 
                    label = paste(var),
                    choices = levels(spotify_songs_cleaned_without_trans[[var]]))
      }
    })
    do.call(tagList, inputList)
  })
  
  
  output$titelbild <- renderImage({
    list(src = "WWW/titelbild.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = FALSE)
  
}