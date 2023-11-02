

################################# Datenbereinigung ###################################


spotify_songs <- read.csv("spotify-2023.csv")
summary(spotify_songs) # Summary des Datasets
str(spotify_songs) # Struktur des Datasets

#Beispiele
#sort(unique(spotify_songs$speechiness_.), na.last=TRUE) # sortiert alle unterschiedlichen Werte und hängt NaN's am Schluss an

sapply(spotify_songs, function(x) sum(is.nan(x))) # gibt Spaltenweise Anzahl NaN's zurück
sapply(spotify_songs, function(x) sum(is.na(x))) # gibt Spaltenweise Anzahl Na's zurück


#### 1. Inkonsistente, Fehlerhafte, Missing Daten dokumentieren ####

# track_name          : chr -> drop, da aufwändig um numerisch zu verwenden und encoding von ausländischen Artisten "verhauen" 
# artist.s._name      : chr -> ersetzen mit: streams/artist/Monat (zuerst muss encoding gefixt werden)
# artist_count        : int -> belassen, keine missings und bereits numerisch
# released_year       : int -> umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon
# released_month      : int -> belasssen bereits numerisch und keine missings
# released_day        : int -> vorerst belassen -> Einführung Prädiktor "weekday"
# in_spotify_playlists: int -> belassen  
# in_spotify_charts   : int -> vorerst belassen (Recherche, was es genau ist)
# streams             : chr -> ZIELVARIABLE! numerisch konvertieren
# in_apple_playlists  : int -> belassen und verwenden
# in_apple_charts     : int -> belassen und verwenden
# in_deezer_playlists : chr -> numerisch konvertieren und verwenden 
# in_deezer_charts    : int -> belassen und verwenden 
# in_shazam_charts    : chr -> numerisch konvertieren und verwenden; missings 
# bpm                 : int -> verwenden und belassen
# key                 : chr -> numerisch konvertieren (encoden) und verwenden
# mode                : chr -> numerisch konvertieren (encoden) und verwenden 
# danceability_.      : int -> verwenden und belassen
# valence_.           : int -> Recherche was es ist. ggf. verwenden und belassen 
# energy_.            : int -> verwenden und belassen
# acousticness_.      : int -> verwenden und belassen 
# instrumentalness_.  : int -> evtl. verwerfen, da praktisch alle Werte = 0
# liveness_.          : int -> verwenden und belassen 
# speechiness_.       : int -> verwenden und belassen


#### 2. Inkonsistente, Fehlerhafte, Missing Daten bereinigen ####

## track_name -> drop, da aufwändig um numerisch zu verwenden und encoding von ausländischen Artisten "verhauen"
spotify_songs$track_name <- NULL # erst entfernen, wenn artist.s._name cleaned ist


## artist.s._name -> vorerst behalten und versuchen zu bereinigung (encoding fixen)
sum(is.na(spotify_songs$artist.s._name)) # missings erkennen -> 0
Encoding(spotify_songs$artist.s._name) # viele "unknowns" -> encoding kann nicht angepasst werden -> manuelle Korrektur

# Manuelle Korrektur
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Mï¿½ï¿½ne", "Måneskin")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Michael Bublï¿", "Michael Bublé")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Quevedo, La Pantera, Juseph, Cruz Cafunï¿½ï¿½, Bï¿½ï¿½jo, Abhir Hathi", "Quevedo, La Pantera, Juseph, Cruz Cafuné, Bejo, Abhir Hathi")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Rauw Alejandro, ROSALï¿½", "Rauw Alejandro, Rosalía")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Rï¿½ï", "Rema")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Rï¿½ï¿½ma, Selena G", "Rema, Selena G")  
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Rich The Kid, Matuï¿", "Rich The Kid, Matuê")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "ROSALï¿½", "Rosalía")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Schï¿½ï¿½rze, DJ R", "Schürze, DJ Robin")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Sebastian Yatra, Manuel Turizo, Beï¿½ï", "Sebastian Yatra, Manuel Turizo, Beéle")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Semicenk, Doï¿½ï¿½u ", "Semicenk, Doğu Swag")                                
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "The Weeknd, ROSALï¿½", "The Weeknd, Rosalía")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Tiï¿½ï¿", "Tiësto")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Tiï¿½ï¿½sto, Ava", "Tiësto, Ava")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Tiï¿½ï¿½sto, Kar", "Tiësto, Karol G")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Tiï¿½ï¿½sto, Tate M", "Tiësto, Tate McRae")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Wisin & Yandel, ROSALï¿½", "Wisin & Yandel, Rosalía")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Xamï¿½ï¿½, Gustah, Neo B", NA) # -> droppen
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Zï¿½ï¿½ Fe", "Zé Felipe")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Zï¿½ï¿½ Neto & Crist", "Zé Neto e Cristiano")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Matuï¿½ï¿½, Wiu, ", "Matuê, Teto, WIU")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Marï¿½ï¿½lia Mendonï¿½ï¿½a, Maiara &", "Marília Mendonça, Maiara & Maraisa")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Marï¿½ï¿½lia Mendonï¿½ï¿½a, Hugo & G", "Marília Mendonça, Hugo & Guilherme")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Marï¿½ï¿½lia Mendonï¿½ï¿½a, George Henrique &", "Marília Mendonça, George Henrique & Rodrigo")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Marï¿½ï¿½lia Mendo", "Marília Mendonça")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Luï¿½ï¿½sa Sonza, MC Frog, Dj Gabriel do Borel, Davi K", "Luísa Sonza, MC Frog, Dj Gabriel do Borel, Davi K")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Luciano, Aitch, Bï¿½", "Luciano, Aitch, BIA")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Kendrick Lamar, Beyoncï¿", "Kendrick Lamar, Beyoncé")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Justin Quiles, Lenny Tavï¿½ï¿½rez, BL", "Justin Quiles, Lenny Tavarez, BL")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Junior H, Eden Muï¿½ï", "Junior H, Eden Muñoz")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Josï¿½ï¿½ Felic", "José Feliciano")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Jordan Fisher, Josh Levi, Finneas O'Connell, 4*TOWN (From Disney and Pixarï¿½ï¿½ï¿½s Turning Red), Topher Ngo, Grayson Vill", "Jordan Fisher, Josh Levi, Finneas O'Connell, 4*TOWN (From Disney and Pixar's Turning Red), Topher Ngo, Grayson Vill")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Jasiel Nuï¿½ï¿½ez, Peso P", "Jasiel Nuñez, Peso P")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Eden Muï¿½ï", "Eden Muñoz")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Dj LK da Escï¿½ï¿½cia, Tchakabum, mc jhenny, M", "Dj LK da Escócia,Tchakabum, MC Ryan SP, mc jhenny")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Bomba Estï¿½ï¿½reo, Bad B", "Bomba Estéreo, Bad Bunny")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Bad Bunny, The Marï¿½ï", "Bad Bunny, The Marías")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Arcangel, De La Ghetto, Justin Quiles, Lenny Tavï¿½ï¿½rez, Sech, Dalex, Dimelo Flow, Rich Music", "Arcangel, De La Ghetto, Justin Quiles, Lenny Tavárez, Sech, Dalex, Dimelo Flow, Rich Music")
spotify_songs$artist.s._name <- replace(spotify_songs$artist.s._name, spotify_songs$artist.s._name == "Beyoncï¿", "Beyoncé")

#Falls nach cleaning neues csv benötigt wird:

# write.csv(spotify_songs, "spotify-2023_cleaned.csv", row.names = FALSE)

## artist_count -> belassen, keine missings und bereits numerisch
sum(is.na(spotify_songs$artist_count)) # missings erkennen -> 0


# released_day -> vorerst belassen (falls möglich weekend eruieren)
# ???? Ansatz unten: Released Datum aus year, month und day zusammensetzen; weekday eruieren und numerisch konvertieren
spotify_songs$released_date <- as.Date(paste(spotify_songs$released_year, spotify_songs$released_month, spotify_songs$released_day, sep="-"), format="%Y-%m-%d")
spotify_songs$weekday <- weekdays(spotify_songs$released_date)
day_mapping <- c("Montag" = 1, "Dienstag" = 2, "Mittwoch" = 3, "Donnerstag" = 4, "Freitag" = 5, "Samstag" = 6, "Sonntag" = 7)
spotify_songs$weekday <- as.numeric(day_mapping[spotify_songs$weekday])
sum(is.na(spotify_songs$artist_count)) # missings erkennen -> 0

spotify_songs$released_date <- NULL # -> zusammengesetztes release_date wieder entfernen


## released_year -> umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon
spotify_songs$released_year <- 2023 - spotify_songs$released_year
sum(is.na(spotify_songs$artist_count)) # missings erkennen -> 0

## released_month -> belasssen bereits numerisch und keine missings
sum(is.na(spotify_songs$released_month)) # missings erkennen -> 0

## in_spotify_playlists -> belassen
sum(is.na(spotify_songs$in_spotify_playlists)) # missings erkennen -> 0

# in_spotify_charts -> vorerst belassen (Recherche, was es genau ist)
sum(is.na(spotify_songs$in_spotify_charts)) # missings erkennen -> 0

## streams -> ZIELVARIABLE! numerisch konvertieren
spotify_songs$streams <- as.numeric(spotify_songs$streams)
sum(is.na(spotify_songs$streams)) # missings erkennen -> 1
spotify_songs <-spotify_songs[-575,] # wenn Index bekannt so löschen
sum(is.na(spotify_songs$streams)) # missings erkennen -> 0

## in_apple_playlists -> belassen und verwenden
sum(is.na(spotify_songs$in_apple_playlists)) # missings erkennen -> 0

## in_apple_charts -> belassen und verwenden
sum(is.na(spotify_songs$in_apple_charts)) # missings erkennen -> 0

## in_deezer_playlists -> numerisch konvertieren und verwenden
sum(is.na(spotify_songs$in_deezer_playlists)) # missings erkennen -> 0
spotify_songs$in_deezer_playlists # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs$in_deezer_playlists <- gsub(",", "", spotify_songs$in_deezer_playlists) # ersetzt "," durch ""
spotify_songs$in_deezer_playlists <- as.numeric(spotify_songs$in_deezer_playlists)
spotify_songs$in_deezer_playlists[is.na(spotify_songs$in_deezer_playlists)] # keine NA's mehr

## in_deezer_charts -> belassen und verwenden
sum(is.na(spotify_songs$in_deezer_charts)) # missings erkennen -> 0

## in_shazam_charts -> numerisch konvertieren und verwenden; missings
spotify_songs$in_shazam_charts # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs$in_shazam_charts <- gsub(",", "", spotify_songs$in_shazam_charts)
spotify_songs$in_shazam_charts <- as.numeric(spotify_songs$in_shazam_charts)
spotify_songs$in_shazam_charts[is.na(spotify_songs$in_shazam_charts)] # viele NA's
sum(is.na(spotify_songs$in_shazam_charts))  # missings erkennen -> 50
# Wie NA's handeln??? unten Variante mit median
spotify_songs$in_shazam_charts[is.na(spotify_songs$in_shazam_charts)] <- median(spotify_songs$in_shazam_charts, na.rm = TRUE) 
spotify_songs$in_shazam_charts <- round(spotify_songs$in_shazam_charts)

## bpm -> verwenden und belassen
sum(is.na(spotify_songs$bpm))  # missings erkennen -> 0

## key -> numerisch konvertieren (encoden) und verwenden
spotify_songs$key # -> enthält leere Strings ""
sum(is.na(spotify_songs$key))  # missings erkennen -> 0
key_value <- as.character(names(sort(table(spotify_songs$key), decreasing=TRUE)[1])) # ermittelt häfigst verwendeter Key
spotify_songs$key[spotify_songs$key == ""] <- key_value # -> setzt den ermittelten key für den leeren String ein

factor_column_key <- as.factor(spotify_songs$key) # faktorisiert den key
levels(factor_column_key) # 1 - 11 : "A"  "A#" "B"  "C#" "D"  "D#" "E"  "F"  "F#" "G"  "G#"
spotify_songs$key <- as.numeric(as.factor(spotify_songs$key))


## mode -> numerisch konvertieren (encoden) und verwenden
spotify_songs$mode
sum(is.na(spotify_songs$mode))  # missings erkennen -> 0
factor_column_mode <- as.factor(spotify_songs$mode)
levels(factor_column_mode) # 1 = "Major"; 2 = "Minor"
spotify_songs$mode <- as.numeric(as.factor(spotify_songs$mode))


## danceability_. -> verwenden und belassen
spotify_songs$danceability_.
sum(is.na(spotify_songs$danceability_.))  # missings erkennen -> 0


## valence_. -> Recherche was es ist. ggf. verwenden und belassen
spotify_songs$valence_.
sum(is.na(spotify_songs$valence_.))  # missings erkennen -> 0

## energy_. -> verwenden und belassen
spotify_songs$energy_.
sum(is.na(spotify_songs$energy_.))  # missings erkennen -> 0

## acousticness_. -> verwenden und belassen
spotify_songs$acousticness_.
sum(is.na(spotify_songs$acousticness_.))  # missings erkennen -> 0

## instrumentalness_.-> evtl. verwerfen, da praktisch alle Werte = 0
spotify_songs$instrumentalness_.
sum(is.na(spotify_songs$instrumentalness_.))  # missings erkennen -> 0

## liveness_. -> verwenden und belassen
spotify_songs$liveness_.
sum(is.na(spotify_songs$liveness_.))  # missings erkennen -> 0

## speechiness_. -> verwenden und belassen
spotify_songs$speechiness_.
sum(is.na(spotify_songs$speechiness_.))  # missings erkennen -> 0

#### 3 Alle Werte "messbar" machen ######

# integriert in 2.

#### 4 Datentypen in R auf Korrektheit prüfen #####



###### Plausibilisierung #######

#### 1 Verteilung, Symmetrie, Ausreisser pro Prädiktor plotten und beschreiben #### 

hist(spotify_songs$streams, col = 'purple', xlab = "Streams per Year", ylab = "Count", main = "Verteilung der Streams per Year")
hist(sqrt(spotify_songs$streams), col = 'green', xlab = "Streams per Year", ylab = "Count", main = "Verteilung der Streams per Year")

# Streudiagramm released_year / streams
plot(x = sqrt(spotify_songs$released_year), y = log(spotify_songs$streams))
abline(lm(log(streams) ~ sqrt(released_year), spotify_songs), col = "red")


spotify_songs$artist.s._name <- NULL # ACHTUNG: sobald artist.s._name numerisch löschen!!

# Erstellt Plots für jeden Prädiktor (Histogramm, Dichteplot, QQ -Plot und BoxPlot)
create_plots <- function(predictor, spotify_songs) {
  par(mfrow=c(2, 2))
  
  # Histogramm
  hist(spotify_songs[[predictor]], main=paste("Histogramm für", predictor), col = "purple", xlab=predictor)
  
  # Dichteplot
  plot(density(spotify_songs[[predictor]], na.rm=TRUE), main=paste("Dichteplot für", predictor), col = "red",  xlab=predictor)
  
  # QQ-Plot
  qqnorm(spotify_songs[[predictor]], main=paste("QQ-Plot für", predictor))
  qqline(spotify_songs[[predictor]], col = "red")
  
  # Boxplot
  boxplot(spotify_songs[[predictor]], main=paste("Boxplot für", predictor), col = "blue", horizontal=TRUE)
}

# Anpassung der Ränder, wenn Screen zu klein
par(mar=c(3,3,2,2)) 

# Anwenden der Funktion auf jeden Prädiktor
lapply(names(spotify_songs), create_plots, spotify_songs)

#### 2 Alle Werte, wo notwendig transformieren #####

# Transformierungen anhand Plots in Betracht ziehen (Achtung durch Transformierungen kann die Interpretierbarkeit schwieriger werden)

#### 3 Fehlende Variablen beschreiben #####

# Hat es fehlende Werte? 