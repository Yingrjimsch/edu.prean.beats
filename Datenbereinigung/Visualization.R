######################### Laden der Libraries ##########################
library("RColorBrewer")


######################### Lesen der RData Datei ##########################

load("spotify_songs_cleaned_with_trans.RData")
load("spotify_songs_cleaned_with_trans_optima.RData")
load("spotify_songs_cleaned_without_trans.RData")

str(spotify_songs_cleaned_with_trans)
str(spotify_songs_cleaned_with_trans_optima)
str(spotify_songs_cleaned_without_trans)

# definieren der Farbe der Balken im Balkendiagramm
#color <- brewer.pal(11, "Spectral") 
color <- brewer.pal(11, "PRGn")
#color <- brewer.pal(10, "Paired")


create_plots <- function(dataframe, dependent_variable) {
  num_predictors <- ncol(dataframe)
  
  for (i in 1:num_predictors) {
    predictor <- names(dataframe)[i]
    
    if (predictor == dependent_variable) {
      next
    }
    # Plots für kategorielle Prädiktoren
    if (is.factor(dataframe[[predictor]])) {
      
      par(mfrow=c(1, 2))
      
      # Balkendiagramm
      barplot(table(dataframe[[predictor]]), main=paste("Balkendiagramm für", predictor), col = color)
      
      # Boxplot für jede Kategorie
      boxplot(dataframe[[dependent_variable]] ~ dataframe[[predictor]], main=paste("Boxplot für", predictor), col = color, horizontal=TRUE)
      
    } else {
      # Plots für kontinuierliche Prädiktoren
      par(mfrow=c(2, 2))
      
      # Histogramm
      hist(dataframe[[predictor]], main=paste("Histogramm: ", predictor), col = color, xlab=predictor)
      
      # Dichteplot
      plot(density(na.omit(dataframe[[predictor]])), main=paste("Dichteplot für", predictor), col = "red", xlab=predictor)
      
      # QQ-Plot
      qqnorm(dataframe[[predictor]], main=paste("QQ-Plot: ", predictor))
      qqline(dataframe[[predictor]], col = "red")
      
      # Boxplot
      boxplot(dataframe[[predictor]], main=paste("Boxplot: ", predictor), col = color, horizontal=TRUE)
      
    }
    # Residuenplot für kontinuierliche Prädiktoren
    if (!is.factor(dataframe[[predictor]])) {
      modell <- lm(formula(paste(dependent_variable, "~", predictor)), data = dataframe)
      plot(modell$fitted.values, resid(modell),
           xlab = "Vorhergesagte Werte",
           ylab = "Residuen",
           main = paste("Residuenplot für", predictor),
           col = "darkgreen",
           pch = 19)
      abline(h = 0, col = "red")
    }
  }
}


create_plots(spotify_songs_cleaned_with_trans, "streams") #-> auskommentieren, wenn Plots gewünscht!
create_plots(spotify_songs_cleaned_with_trans_optima, "streams") #-> auskommentieren, wenn Plots gewünscht!
create_plots(spotify_songs_cleaned_without_trans, "streams") #-> auskommentieren, wenn Plots gewünscht!

