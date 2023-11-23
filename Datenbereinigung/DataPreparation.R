
######################################################################################

#                                     Datenbereinigung

######################################################################################

#################

# Vorbereitungen

#################


# Laden der notwendigen Libraries
library(MASS)

# Lesen des Rohdatensatzes (csv von kaggle)
spotify_songs <- read.csv("./data/spotify-2023.csv", encoding="latin1")

# Kopie des Rohdatensatzes zwecks möglichen Manipulationen am Datensatz
spotify_songs_man <- spotify_songs 

# Erzeugen von leeren Dataframes, welche mit den optimierten Prädiktoren befüllt werden
spotify_songs_cleaned_with_trans <- data.frame() # leeres Dataframe, in welchem die "bereinigten" Prädiktoren (teilweise mit Transformation) abgelegt werden (bereit für Modellbildung)
spotify_songs_cleaned_with_trans_optima <- data.frame() # # leeres Dataframe, in welchem die "bereinigten" Prädiktoren (mit bestmöglicher Transformation) abgelegt werden (bereit für Modellbildung)
spotify_songs_cleaned_without_trans <- data.frame() # leeres Dataframe, in welchem die "bereinigten" Prädiktoren (ohne Transformation) abgelegt werden (bereit für Modellbildung)

#  Darstellen der Ausgangslage des Datasets
summary(spotify_songs_man) # Summary des Datasets
str(spotify_songs_man) # Struktur des Datasets

# Aufzeigen von Auffälligkeiten in den Rohdaten
sapply(spotify_songs_man, function(x) sum(is.nan(x))) # gibt Spaltenweise Anzahl NaN's zurück -> keine
sapply(spotify_songs_man, function(x) sum(is.na(x))) # gibt Spaltenweise Anzahl Na's zurück -> keine
sapply(spotify_songs_man, function(x) sum(x == "")) # gibt Spaltenweise Anzahl leerer Strings zurück zurück -> key = 95; in_shazam_charts = 50
sapply(spotify_songs_man, function(x) grep("ï¿½", x)) # gibt Spaltenweise Indices der Einträge mit fehlerhaftem Encoding zurück zurück -> track_name = 58; artist.s._name = 40
sapply(spotify_songs_man, function(x) sum(x == -Inf)) # gibt Spaltenweise Anzahl der -Inf Werte zurück -> keine
sum(duplicated(spotify_songs_man)) #Gesamtanzahl duplizierter Werte zurück -> keine

# track_name          : chr -> drop, da aufwändig um numerisch zu verwenden und encoding von ausländischen Artisten "verhauen" 
# artist.s._name      : chr -> ersetzen mit: streams pro Artist pro Monat (mittels Web-Scraping), nachdem encoding korrigiert wurde
# artist_count        : int -> belassen, keine missings und bereits numerisch
# released_year       : int -> umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon
# released_month      : int -> belasssen bereits numerisch und keine missings
# released_day        : int -> Einführung Prädiktor "weekday" 
# in_spotify_playlists: int -> belassen  
# in_spotify_charts   : int -> belassen
# streams             : chr -> ZIELVARIABLE! numerisch konvertieren
# in_apple_playlists  : int -> belassen und verwenden
# in_apple_charts     : int -> belassen und verwenden
# in_deezer_playlists : chr -> numerisch konvertieren und verwenden 
# in_deezer_charts    : int -> belassen und verwenden 
# in_shazam_charts    : chr -> numerisch konvertieren und verwenden; missings 
# bpm                 : int -> verwenden und belassen
# key                 : chr -> numerisch konvertieren (faktorisieren) und verwenden
# mode                : chr -> numerisch konvertieren (faktorisieren) und verwenden 
# danceability_.      : int -> verwenden und belassen
# valence_.           : int -> verwenden und belassen 
# energy_.            : int -> verwenden und belassen
# acousticness_.      : int -> verwenden und belassen 
# instrumentalness_.  : int -> verwerfen, da praktisch alle Werte = 0
# liveness_.          : int -> verwenden und belassen 
# speechiness_.       : int -> verwenden und belassen


######################################

# Cleaning der einzelnen Prädiktoren

######################################

### Prädiktor track_name ###
# drop, da aufwändig um numerisch zu verwenden und encoding von ausländischen Artisten "verhauen"
indices_with_encoding_errors <-grep("ï¿½", spotify_songs_man$track_name)
values_with_encoding_errors <- spotify_songs_man$track_name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (58 Outputs)
spotify_songs_man$track_name <- NULL # erst entfernen, wenn artist.s._name cleaned ist (14.11.2023 cleaned)

###  Prädiktor artist.s._name ###  
# vorerst behalten und bereinigen (encoding fixen)
# mittels web scraping die Streams pro artist pro Monat zu ermitteln; sind in einem track_name 
# mehrere artisten beteiligt -> Mittelwert verwenden

#spotify_songs_man$artist.s._name
sum(is.na(spotify_songs_man$artist.s._name)) # missings erkennen -> 0
Encoding(spotify_songs_man$artist.s._name) # viele "unknowns" -> encoding kann nicht angepasst werden -> manuelle Korrektur

indices_with_encoding_errors <-grep("ï¿½", spotify_songs_man$artist.s._name)
values_with_encoding_errors <- spotify_songs_man$artist.s._name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (40 Outputs)

# Manuelle Korrektur aller encoding Fehler in artist.s._name
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Mï¿½ï¿½ne", "Måneskin")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Michael Bublï¿", "Michael Bublé")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Quevedo, La Pantera, Juseph, Cruz Cafunï¿½ï¿½, Bï¿½ï¿½jo, Abhir Hathi", "Quevedo, La Pantera, Juseph, Cruz Cafuné, Bejo, Abhir Hathi")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Rauw Alejandro, ROSALï¿½", "Rauw Alejandro, Rosalía")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Rï¿½ï", "Rema")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Rï¿½ï¿½ma, Selena G", "Rema, Selena G")  
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Rich The Kid, Matuï¿", "Rich The Kid, Matuê")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "ROSALï¿½", "Rosalía")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Schï¿½ï¿½rze, DJ R", "Schürze, DJ Robin")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Sebastian Yatra, Manuel Turizo, Beï¿½ï", "Sebastian Yatra, Manuel Turizo, Beéle")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Semicenk, Doï¿½ï¿½u ", "Semicenk, Doğu Swag")                                
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "The Weeknd, ROSALï¿½", "The Weeknd, Rosalía")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Tiï¿½ï¿", "Tiësto")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Tiï¿½ï¿½sto, Ava", "Tiësto, Ava")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Tiï¿½ï¿½sto, Kar", "Tiësto, Karol G")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Tiï¿½ï¿½sto, Tate M", "Tiësto, Tate McRae")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Wisin & Yandel, ROSALï¿½", "Wisin & Yandel, Rosalía")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Xamï¿½ï¿½, Gustah, Neo B", NA) # -> droppen
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Zï¿½ï¿½ Fe", "Zé Felipe")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Zï¿½ï¿½ Neto & Crist", "Zé Neto e Cristiano")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Matuï¿½ï¿½, Wiu, ", "Matuê, Teto, WIU")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Marï¿½ï¿½lia Mendonï¿½ï¿½a, Maiara &", "Marília Mendonça, Maiara & Maraisa")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Marï¿½ï¿½lia Mendonï¿½ï¿½a, Hugo & G", "Marília Mendonça, Hugo & Guilherme")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Marï¿½ï¿½lia Mendonï¿½ï¿½a, George Henrique &", "Marília Mendonça, George Henrique & Rodrigo")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Marï¿½ï¿½lia Mendo", "Marília Mendonça")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Luï¿½ï¿½sa Sonza, MC Frog, Dj Gabriel do Borel, Davi K", "Luísa Sonza, MC Frog, Dj Gabriel do Borel, Davi K")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Luciano, Aitch, Bï¿½", "Luciano, Aitch, BIA")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Kendrick Lamar, Beyoncï¿", "Kendrick Lamar, Beyoncé")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Justin Quiles, Lenny Tavï¿½ï¿½rez, BL", "Justin Quiles, Lenny Tavarez, BL")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Junior H, Eden Muï¿½ï", "Junior H, Eden Muñoz")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Josï¿½ï¿½ Felic", "José Feliciano")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Jordan Fisher, Josh Levi, Finneas O'Connell, 4*TOWN (From Disney and Pixarï¿½ï¿½ï¿½s Turning Red), Topher Ngo, Grayson Vill", "Jordan Fisher, Josh Levi, Finneas O'Connell, 4*TOWN (From Disney and Pixar's Turning Red), Topher Ngo, Grayson Vill")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Jasiel Nuï¿½ï¿½ez, Peso P", "Jasiel Nuñez, Peso P")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Eden Muï¿½ï", "Eden Muñoz")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Dj LK da Escï¿½ï¿½cia, Tchakabum, mc jhenny, M", "Dj LK da Escócia,Tchakabum, MC Ryan SP, mc jhenny")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Bomba Estï¿½ï¿½reo, Bad B", "Bomba Estéreo, Bad Bunny")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Bad Bunny, The Marï¿½ï", "Bad Bunny, The Marías")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Arcangel, De La Ghetto, Justin Quiles, Lenny Tavï¿½ï¿½rez, Sech, Dalex, Dimelo Flow, Rich Music", "Arcangel, De La Ghetto, Justin Quiles, Lenny Tavárez, Sech, Dalex, Dimelo Flow, Rich Music")
spotify_songs_man$artist.s._name <- replace(spotify_songs_man$artist.s._name, spotify_songs_man$artist.s._name == "Beyoncï¿", "Beyoncé")

# Kontrolle, ob alle encoding Fehler in artist.s._name korrigiert sind
indices_with_encoding_errors <-grep("ï¿½", spotify_songs_man$artist.s._name)
values_with_encoding_errors <- spotify_songs_man$artist.s._name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (0 outputs verbleibend)

### Prädiktor artist_name ###
# ersetzen mit: streams/artist/Monat (zuerst muss encoding gefixt werden) mittels Web-Scraping
listeners <- read.csv("./data/cumulative_listeners.csv") # von spotify extrahierte Daten (nicht im ursprünglichen Datensatz enthalten)
spotify_songs_man$listeners_cum <- listeners$cl # Streams pro Monat pro Interpret

sum(is.na(spotify_songs_man$listeners_cum)) # missings erkennen -> 0
# Eruieren möglicher Probleme des Prädiktors
hist(spotify_songs_man$listeners_cum) # leicht rechtsschief verteilt
hist(log(spotify_songs_man$listeners_cum)) # linksschief verteilt
hist(sqrt(spotify_songs_man$listeners_cum)) # sinnvolle Transformation, da normalverteilt im Mittelpunkt von ~ 5500
hist(1/ spotify_songs_man$listeners_cum)
boxcox(lm(spotify_songs_man$listeners_cum ~ 1)) # optimales Lambda bei 0.4
hist((spotify_songs_man$listeners_cum^(0.4) -1) / 0.4) # ebenfalls sinnvolle Transformation, da normalverteilt im Mittelpunkt von ~2500

# residuenanalyse listeners_cum 
streams <- as.numeric(spotify_songs_man$streams)
streams <- streams[-575]
listeners_cum <- spotify_songs_man$listeners_cum
listeners_cum <- listeners_cum[-575]

model_listeners_cum<- lm(log(streams) ~ sqrt(listeners_cum)) # optimaler Residuenplot bei Transformation mit sqrt()
summary(model_listeners_cum)
# plot(model_listeners_cum) # Plots der Resiudenanalyse

# Hinzufügen von listeners_cum den cleaned Dataframes (zweimal mit sqrt() und einmal OHNE Transformation)
spotify_songs_cleaned_with_trans <- data.frame(listeners_cum_sqrt = sqrt(spotify_songs_man$listeners_cum)) 
spotify_songs_cleaned_with_trans_optima <- data.frame(listeners_cum = spotify_songs_man$listeners_cum)
spotify_songs_cleaned_without_trans <- data.frame(listeners_cum = spotify_songs_man$listeners_cum)

### Prädiktor artist_count ###
# belassen, keine missings und bereits numerisch
# spotify_songs_man$artist_count
sum(is.na(spotify_songs_man$artist_count)) # missings erkennen -> 0
hist(spotify_songs_man$artist_count) # rechtsschief
hist(log(spotify_songs_man$artist_count)) # keine Veränderung
hist(sqrt(spotify_songs_man$artist_count)) # keine Veränderung
hist(1/ spotify_songs_man$artist_count) # linksschief
boxcox(lm(spotify_songs_man$artist_count ~ 1)) # optimales Lambda bei - 1.5
hist((spotify_songs_man$artist_count^(-1.5) -1) / -1.5) # immer noch rechtsschief

# residuenanalyse artist_count
artist_counts <- spotify_songs_man$artist_count
artist_counts <- artist_counts[-575]

model_artist_count <- lm(log(streams) ~ log(artist_counts))
summary(model_artist_count)
#plot(model_artist_count) # Normalenplot sieht gut aus, Residualplot aufällig -> keine optimale Transformation eruiert

# Transformation nicht möglich -> evtl. verwerfen oder als kategorieller Prädiktor verwenden
# Hinzufügen von artist_count den cleaned Dataframes  (einmal faktorisiert, zweimal numerisch)
spotify_songs_cleaned_with_trans["artist_count"] <- factor(spotify_songs_man$artist_count)
spotify_songs_cleaned_with_trans_optima["artist_count"] <- spotify_songs_man$artist_count
spotify_songs_cleaned_without_trans["artist_count"] <- spotify_songs_man$artist_count

barplot(table(spotify_songs_cleaned_with_trans$artist_count), main="Barplot artist_count", xlab="artist_count", ylab="Frequency") # rechtsschief -> Transformation?

### Prädiktor released_date ###
# aus released_year, released_month und released_day zusammensetzen; released_weekday extrahieren und faktorisieren
spotify_songs_man$released_date <- as.Date(paste(spotify_songs_man$released_year, spotify_songs_man$released_month, spotify_songs_man$released_day, sep="-"), format="%Y-%m-%d")
spotify_songs_man$released_weekday <- weekdays(spotify_songs_man$released_date)
spotify_songs_man$released_weekday <- factor(spotify_songs_man$released_weekday)
# spotify_songs_man$released_weekday
sum(is.na(spotify_songs_man$released_weekday)) # missings erkennen -> 0
barplot(table(spotify_songs_man$released_weekday), main="Barplot weekday", xlab="weekday", ylab="Frequency")

# Hinzufügen von released_weekday den cleaned Dataframes (faktorisiert)
spotify_songs_cleaned_with_trans["released_weekday"] <- spotify_songs_man$released_weekday
spotify_songs_cleaned_with_trans_optima["released_weekday"] <- spotify_songs_man$released_weekday
spotify_songs_cleaned_without_trans["released_weekday"]<- spotify_songs_man$released_weekday

### Prädiktor released_year ###
# umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon (neuer Prädiktor years_since_release)
spotify_songs_man$years_since_release <- 2023 - spotify_songs_man$released_year
sum(is.na(spotify_songs_man$years_since_release)) # missings erkennen -> 0
# spotify_songs_man$years_since_release
hist(spotify_songs_man$years_since_release) # rechtsschief
hist(log(spotify_songs_man$years_since_release)) # erzeugt - Inf Werte, da 0 Werte enthalten
spotify_songs_man$years_since_release[spotify_songs_man$years_since_release == 0] <- 0.0001 # Korrektur der -Inf Werte mittels Setzen von kleinem Wert anstelle der 0 Werte
hist(log(spotify_songs_man$years_since_release)) # nicht normalverteilt
hist(sqrt(spotify_songs_man$years_since_release)) # rechtsschief
hist(1/(spotify_songs_man$years_since_release)) # keine Veränderung 

# optimale Transformation mittels boxcox ermitteln
boxcox(lm((spotify_songs_man$years_since_release) ~ 1)) # optimales lambda ~ 0.3 
hist((spotify_songs_man$years_since_release^(0.2) -1) / 0.2) # nicht normalverteilt

# Residuenanalyse years_since_release
years_since_release <- spotify_songs_man$years_since_release
years_since_release <- years_since_release[-575]
years_since_release <- (years_since_release^(0.2) -1) / 0.2

model_years_since_release <- lm(log(streams) ~ years_since_release)
summary(model_years_since_release)
# plot(model_years_since_release) # Normalplot sieht soweit gut aus; keine optimale Transformation möglich

# Hinzufügen von years_since_release den cleaned Dataframes (einmal Boxcox mit Lambda 0.2 und zweimal ohne Transformation)
spotify_songs_cleaned_with_trans["years_since_release_boxcox"] <- (spotify_songs_man$years_since_release^(0.2) -1) / 0.2
spotify_songs_cleaned_with_trans_optima["years_since_release"] <- spotify_songs_man$years_since_release
spotify_songs_cleaned_without_trans["years_since_release"] <- spotify_songs_man$years_since_release

### Prädiktor released_month ### 
# belasssen bereits numerisch und keine missings
# spotify_songs_man$released_month
sum(is.na(spotify_songs_man$released_month)) # missings erkennen -> 0
spotify_songs_man$released_month <-factor(spotify_songs_man$released_month, levels = 1:12, labels = month.name)

# Hinzufügen von released_month den beiden cleaned Dataframes (faktorisiert)
spotify_songs_cleaned_with_trans["released_month"] <- spotify_songs_man$released_month
spotify_songs_cleaned_with_trans_optima["released_month"] <- spotify_songs_man$released_month
spotify_songs_cleaned_without_trans["released_month"] <- spotify_songs_man$released_month
barplot(table(spotify_songs_cleaned_with_trans$released_month), main="Barplot released_month", xlab="Month", ylab="Frequency")

### Prädiktor in_spotify_playlists ###
# belassen, bereits numerisch
# spotify_songs_man$in_spotify_playlists
sum(is.na(spotify_songs_man$in_spotify_playlists)) # missings erkennen -> 0
hist(spotify_songs_man$in_spotify_playlists) # rechtsschief
hist(log(spotify_songs$in_spotify_playlists)) # normalverteilt und keine -Inf Werte
hist(sqrt(spotify_songs$in_spotify_playlists)) # rechtsschief
hist(1/spotify_songs$in_spotify_playlists) # rechtsschief
boxcox(lm((spotify_songs_man$in_spotify_playlists) ~ 1)) #optimales lambda ~ 0 -> log Transformation

# Residuenanalyse in_spotify_playlists
in_spotify_playlist <- spotify_songs_man$in_spotify_playlists
in_spotify_playlist <- in_spotify_playlist[-575]

model_in_spotify_playlists <- lm(log(streams) ~ log(in_spotify_playlist))
summary(model_in_spotify_playlists)
#plot(model_in_spotify_playlists)

# Hinzufügen von in_spotify_playlists den beiden cleaned Dataframes (einmal MIT log- Transformation einmal OHNE Transformation)
spotify_songs_cleaned_with_trans["in_spotify_playlists_log"] <- log(spotify_songs_man$in_spotify_playlists)
spotify_songs_cleaned_with_trans_optima["in_spotify_playlists_log"] <- log(spotify_songs_man$in_spotify_playlists)
spotify_songs_cleaned_without_trans["in_spotify_playlists"] <- spotify_songs_man$in_spotify_playlists

### Prädiktor in_spotify_charts ###
# belassen, bereits numerisch
#spotify_songs_man$in_spotify_charts # enthält viele 0 Werte
sum(is.na(spotify_songs_man$in_spotify_charts)) # missings erkennen -> 0
hist(spotify_songs_man$in_spotify_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll? -> erzeugt -Inf Werte; besser Transformation mittels sqrt()?
hist(log(spotify_songs_man$in_spotify_charts)) # Transformation mittels log() erzeugt -Inf Werte
spotify_songs_man$in_spotify_charts[spotify_songs_man$in_spotify_charts == 0] <- 0.0001 # Korrektur der -Inf Werte
hist(sqrt(spotify_songs_man$in_spotify_charts)) # rechtsschief
hist(1/(spotify_songs_man$in_spotify_charts)) # nicht normalverteilt
hist(log(spotify_songs_man$in_spotify_charts)) # nicht normalverteilt
boxcox(lm((spotify_songs_man$in_spotify_charts) ~ 1)) #optimales lambda ~ 0 -> log Transformation

# Residuenanalyse in_spotify_charts
in_spotify_chart <- spotify_songs_man$in_spotify_charts
in_spotify_chart <- in_spotify_chart[-575]
in_spotify_chart[in_spotify_chart == 0] <- 0.0001 

model_in_spotify_charts <- lm(log(streams) ~ log(in_spotify_chart))
summary(model_in_spotify_charts)
# plot(model_in_spotify_charts) # keine optimale Transformation erreicht

# Hinzufügen von in_spotify_charts den cleaned Dataframes (dreimal OHNE Transformation)
spotify_songs_cleaned_with_trans["in_spotify_charts"] <- spotify_songs_man$in_spotify_charts
spotify_songs_cleaned_with_trans_optima["in_spotify_charts"] <- spotify_songs_man$in_spotify_charts
spotify_songs_cleaned_without_trans["in_spotify_charts"] <- spotify_songs_man$in_spotify_charts

### Prädiktor in_apple_playlists ### 
# belassen, bereits numerisch
# spotify_songs_man$in_apple_playlists
sum(is.na(spotify_songs_man$in_apple_playlists)) # missings erkennen -> 0
hist(spotify_songs_man$in_apple_playlists) # rechtsschief 
hist(log(spotify_songs_man$in_apple_playlists))# erzeugt -Inf Werte, da 0 Werte enthalten
spotify_songs_man$in_apple_playlists[spotify_songs_man$in_apple_playlists == 0] <- 0.0001 # Korrektur der -Inf Werte
hist(sqrt(spotify_songs_man$in_apple_playlists)) # rechtsschief
hist(1/(spotify_songs_man$in_apple_playlists)) # keine Normalverteilung
hist(log(spotify_songs_man$in_apple_playlists)) # keine Normalverteilung
boxcox(lm((spotify_songs_man$in_apple_playlists) ~ 1)) # optimales lambda ~ 0.3
hist((spotify_songs_man$in_apple_playlists^(0.3) - 1 )/0.3) # keine Normalverteilung

#Residuenanalyse in_apple_playlists
in_apple_playlist <- spotify_songs_man$in_apple_playlists
in_apple_playlist <- in_apple_playlist[-575]
in_apple_playlist[in_apple_playlist == 0] <- 0.0001 
in_apple_playlist <- (in_apple_playlist^(0.3) - 1 )/0.3

model_in_apple_playlists <- lm(log(streams) ~ in_apple_playlist)
summary(model_in_apple_playlists)
# plot(model_in_apple_playlists) # Boxcox Transformation mit Lambda von 0.3 zeigt beste Resiudenanalyse

# Hinzufügen von in_apple_playlists cleaned Dataframes (einmal MIT BoxCox - Transformation und lambda von 0.3 und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["in_apple_playlists_boxcox"] <- (spotify_songs_man$in_apple_playlists^(0.3) - 1)/0.3
spotify_songs_cleaned_with_trans_optima["in_apple_playlists"] <- spotify_songs_man$in_apple_playlists
spotify_songs_cleaned_without_trans["in_apple_playlists"] <- spotify_songs_man$in_apple_playlists

### Prädiktor in_apple_charts ###
# belassen, bereits numerisch
# spotify_songs_man$in_apple_charts # enthält viele 0 Werte
sum(is.na(spotify_songs_man$in_apple_charts)) # missings erkennen -> 0
hist(spotify_songs_man$in_apple_charts) # rechtsschief
hist(log(spotify_songs_man$in_apple_charts)) # erzeugt -Inf Werte, da 0 Werte vorhanden
spotify_songs_man$in_apple_charts[spotify_songs_man$in_apple_charts == 0] <- 0.0001 # Korrektur der -Inf Werte
hist(sqrt(spotify_songs_man$in_apple_charts))# keine Normalverteilung
hist(1/(spotify_songs_man$in_apple_charts)) # keine Normalverteilung
hist(log(spotify_songs_man$in_apple_charts)) # keine Normalverteilung
boxcox(lm((spotify_songs_man$in_apple_charts) ~ 1)) # optimales lambda ~ 0.2 
hist((spotify_songs_man$in_apple_charts^(0.3) - 1 )/0.3) # keine Normalverteilung

# Residuenanalyse in_apple_charts
in_apple_chart <- spotify_songs_man$in_apple_charts
in_apple_chart <- in_apple_chart[-575]
in_apple_chart[in_apple_chart == 0] <- 0.0001 
in_apple_chart <- (in_apple_chart^(0.3) - 1 )/0.3

model_in_apple_charts <- lm(log(streams) ~ in_apple_chart)
summary(model_in_apple_charts)
# plot(model_in_apple_charts) # keine optimale Transformation möglich, bester Residualplot bei BoxCox Transformation mit Lambda 0.3

# Hinzufügen von in_apple_charts den cleaned Dataframes (einmal MIT BoxCox - Transformation und lambda von 0.3 und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["in_apple_charts_boxcox"] <- (spotify_songs_man$in_apple_charts^(0.3) - 1 )/0.3
spotify_songs_cleaned_with_trans_optima["in_apple_charts"] <- spotify_songs_man$in_apple_charts
spotify_songs_cleaned_without_trans["in_apple_charts"] <- spotify_songs_man$in_apple_charts

### Prädiktor in_deezer_playlists ###
# numerisch konvertieren
sum(is.na(spotify_songs_man$in_deezer_playlists)) # missings erkennen -> 0
# spotify_songs_man$in_deezer_playlists # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs_man$in_deezer_playlists <- gsub(",", "", spotify_songs_man$in_deezer_playlists) # ersetzt "," durch ""
spotify_songs_man$in_deezer_playlists <- as.numeric(spotify_songs_man$in_deezer_playlists)
spotify_songs_man$in_deezer_playlists[is.na(spotify_songs_man$in_deezer_playlists)] # keine NA's mehr

hist(spotify_songs_man$in_deezer_playlists) # rechtsschief 
hist(log(spotify_songs_man$in_deezer_playlists)) # erzeugt -Inf Werte, da 0 Werte vorhanden
spotify_songs_man$in_deezer_playlists[spotify_songs_man$in_deezer_playlists == 0] <- 0.0001 # Korrektur der -Inf Werte
hist(log(spotify_songs_man$in_deezer_playlists))# keine Normalvertielung
hist(sqrt(spotify_songs_man$in_deezer_playlists)) # rechtsschief
hist(1/(spotify_songs_man$in_deezer_playlists)) # keine Normalverteilung
boxcox(lm((spotify_songs_man$in_deezer_playlists) ~ 1)) # optimales lambda ~ 0.1 
hist((spotify_songs_man$in_deezer_playlists^(0.1) - 1 )/0.1) # keine optimale Normalverteilung

# Residuenanalyse in_deezer_playlists
in_deezer_playlist <- spotify_songs_man$in_deezer_playlists
in_deezer_playlist <- in_deezer_playlist[-575]
in_deezer_playlist[in_deezer_playlist == 0] <- 0.0001 
in_deezer_playlist <- (in_deezer_playlist^(0.1) - 1 )/0.1

model_in_deezer_playlists <- lm(log(streams) ~ in_deezer_playlist)
summary(model_in_deezer_playlists)
# plot(model_in_deezer_playlists) # keine optimale Transformation möglich; beste Residualanalyse mit BoxCox Transformation und Lambda 0.1

# Hinzufügen von in_deezer_playlists den cleaned Dataframes (einmal MIT BoxCox - Transformation und lambda von 0.1 und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["in_deezer_playlists_boxcox"] <- (spotify_songs_man$in_deezer_playlists^(0.1) - 1 )/0.1
spotify_songs_cleaned_with_trans_optima["in_deezer_playlists"] <- spotify_songs_man$in_deezer_playlists
spotify_songs_cleaned_without_trans["in_deezer_playlists"] <- spotify_songs_man$in_deezer_playlists

### Prädiktor in_deezer_charts ###
# belassen, hat aber viele 0 Werte!!
# spotify_songs_man$in_deezer_charts
sum(is.na(spotify_songs_man$in_deezer_charts)) # missings erkennen -> 0
hist(spotify_songs_man$in_deezer_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs_man$in_deezer_charts)) # erzeugt -Inf Werte, da 0 Werte vorhanden
spotify_songs_man$in_deezer_charts[spotify_songs_man$in_deezer_charts == 0] <- 0.0001 
hist(log(spotify_songs_man$in_deezer_charts)) # keine Normalverteilung
hist(sqrt(spotify_songs_man$in_deezer_charts)) # rechtsschief
hist(1/(spotify_songs_man$in_deezer_charts)) # keine Normalverteilung
boxcox(lm((spotify_songs_man$in_deezer_charts) ~ 1)) # optimales lambda nahe 0 -> log Transformation

# Residuenanalyse in_deezer_charts
in_deezer_chart <- spotify_songs_man$in_deezer_charts
in_deezer_chart <- in_deezer_chart[-575]
in_deezer_chart[in_deezer_chart == 0] <- 0.0001 

model_in_deezer_charts <- lm(log(streams) ~ in_deezer_chart)
summary(model_in_deezer_charts)
# plot(model_in_deezer_charts) # keine optimale Transformation möglich

# Hinzufügen von in_deezer_playlists den cleaned Dataframes (dreimal OHNE Transformation)
spotify_songs_cleaned_with_trans["in_deezer_charts"] <- spotify_songs_man$in_deezer_charts
spotify_songs_cleaned_with_trans_optima["in_deezer_charts"] <- spotify_songs_man$in_deezer_charts
spotify_songs_cleaned_without_trans["in_deezer_charts"] <- spotify_songs_man$in_deezer_charts

### Prädiktor in_shazam_charts
# numerisch konvertieren und verwenden; missings
# spotify_songs_man$in_shazam_charts # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs_man$in_shazam_charts <- gsub(",", "", spotify_songs_man$in_shazam_charts)
spotify_songs_man$in_shazam_charts <- as.numeric(spotify_songs_man$in_shazam_charts)
spotify_songs_man$in_shazam_charts[is.na(spotify_songs_man$in_shazam_charts)] # viele NA's
sum(is.na(spotify_songs_man$in_shazam_charts))  # missings erkennen -> 50
# NA's mittels median der enthaltenen Werte korrigieren
spotify_songs_man$in_shazam_charts[is.na(spotify_songs_man$in_shazam_charts)] <-round(median(spotify_songs_man$in_shazam_charts, na.rm = TRUE))
hist(spotify_songs_man$in_shazam_charts) # rechtsschief 
hist(log(spotify_songs_man$in_shazam_charts)) # erzeugt -Inf Werte, da 0 Werte enthalten
spotify_songs_man$in_shazam_charts[spotify_songs_man$in_shazam_charts == 0] <- 0.0001 
hist(log(spotify_songs_man$in_shazam_charts)) # keine Normalverteilung
hist(sqrt(spotify_songs_man$in_shazam_charts)) # rechtsschief
hist(1/(spotify_songs_man$in_shazam_charts)) # keine Normalverteilung
boxcox(lm((spotify_songs_man$in_shazam_charts) ~ 1)) # optimales lambda nahe 0 -> log Transformation

# Residuenanalyse in_shazam_charts
in_shazam_chart <- spotify_songs_man$in_shazam_charts
in_shazam_chart <- in_shazam_chart[-575]
in_shazam_chart[in_shazam_chart == 0] <- 0.0001 

model_in_shazam_charts <- lm(log(streams) ~ log(in_shazam_chart))
summary(model_in_shazam_charts)
# plot(model_in_shazam_charts) # keine optimale Transformation möglich; beste Residualanalyse mit log Transformation

# Hinzufügen von in_shazam_charts den cleaned Dataframes (einmal mit Log- Transformation und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["in_shazam_charts_log"] <- log(spotify_songs_man$in_shazam_charts)
spotify_songs_cleaned_with_trans_optima["in_shazam_charts"] <- spotify_songs_man$in_shazam_charts
spotify_songs_cleaned_without_trans["in_shazam_charts"] <- spotify_songs_man$in_shazam_charts

### Prädiktor bpm ###
# verwenden und belassen
# spotify_songs_man$bpm
sum(is.na(spotify_songs_man$bpm))  # missings erkennen -> 0
hist(spotify_songs_man$bpm) # nicht normalverteilt
hist(log(spotify_songs_man$bpm)) # Normalverteilung erreicht im Mittelpunkt von 4.8, keine - Inf Werte
hist(sqrt(spotify_songs_man$bpm)) # Normalverteilung erreicht im Mittelpunkt von 4.8
hist(1/spotify_songs_man$bpm) # keine optimale Normalverteilung
boxcox(lm((spotify_songs_man$bpm) ~ 1)) # optimales lambda nahe 0 -> log Transformation

# Residuenanalyse bpm
bpms <- spotify_songs_man$bpm
bpms <- bpms[-575]

model_bpm <- lm(log(streams) ~ log(bpms))
summary(model_bpm)
# plot(model_bpm) # beste Residualanalyse mit Log- Transformation

# Hinzufügen von bpm dencleaned Dataframes (einmal mit sqrt- Transformation einmal mit Log- Transformation und einmal OHNE Transformation)
spotify_songs_cleaned_with_trans["bpm_sqrt"] <- sqrt(spotify_songs_man$bpm)
spotify_songs_cleaned_with_trans_optima["bpm_log"] <- log(spotify_songs_man$bpm)
spotify_songs_cleaned_without_trans["bpm"] <- spotify_songs_man$bpm

### Prädiktor key ### 
# numerisch konvertieren (encoden) und verwenden
# spotify_songs_man$key # -> enthält leere Strings ""
sum(spotify_songs_man$key == "")  # 95 leere Strings -> neues Level "Keine Angabe" hinzufügen
spotify_songs_man$key[spotify_songs_man$key == ""] <- "Keine Angabe" # -> setzt den ermittelten key für den leeren String ein

factor_column_key <- as.factor(spotify_songs_man$key) # faktorisiert den key
levels(factor_column_key) # 1 - 11 : "A"  "A#" "B"  "C#" "D"  "D#" "E"  "F"  "F#" "G"  "G#" "Keine Angabe"
spotify_songs_man$key <- as.factor(spotify_songs_man$key)
barplot(table(spotify_songs_man$key), main="Barplot key", xlab="Key", ylab="Frequency")

# Hinzufügen von key den cleaned Dataframes (faktorisiert)
spotify_songs_cleaned_with_trans["key"] <- spotify_songs_man$key
spotify_songs_cleaned_with_trans_optima["key"] <- spotify_songs_man$key
spotify_songs_cleaned_without_trans["key"] <- spotify_songs_man$key

### Prädiktor mode ###
# numerisch konvertieren (encoden) und verwenden
# spotify_songs$mode
sum(is.na(spotify_songs$mode))  # missings erkennen -> 0

# Entweder mittels "if ... else" für qualitative Prädiktoren 
# spotify_songs_cleaned_with_Trans["mode"] <- data.frame(mode = spotify_songs$mode)
# spotify_songs_cleaned_with_Trans$mode <-ifelse(spotify_songs_cleaned_with_Trans$mode == "Minor", 1, 0)
# barplot(table(spotify_songs_cleaned_with_Trans$mode), main="Barplot mode", xlab="Mode", ylab="Frequency", names.arg=c("Major", "Minor"), col=c("blue", "red"))

# oder mittels as.factor() für Dummy- Codierung
spotify_songs_man$mode <-as.factor(spotify_songs_man$mode)
barplot(table(spotify_songs_man$mode), main="Barplot mode", xlab="Mode", ylab="Frequency", names.arg=c("Major", "Minor"), col=c("blue", "red"))

# Hinzufügen von mode den cleaned Dataframes (faktorisiert)
spotify_songs_cleaned_with_trans["mode"] <- spotify_songs_man$mode
spotify_songs_cleaned_with_trans_optima["mode"] <- spotify_songs_man$mode
spotify_songs_cleaned_without_trans["mode"] <- spotify_songs_man$mode

### Prädiktor danceability_.###
# verwenden und belassen
#spotify_songs_man$danceability_.
sum(is.na(spotify_songs_man$danceability_.))  # missings erkennen -> 0
hist(spotify_songs_man$danceability_.) # keine optimale Normalverteilung
hist(log(spotify_songs_man$danceability_.)) # keine optimale Normalverteilung
hist(sqrt(spotify_songs_man$danceability_.)) # keine optimale Normalverteilung
hist(1/spotify_songs_man$danceability_.) # rechtsschief
boxcox(lm((spotify_songs_man$danceability_.) ~ 1)) # optimales lambda nahe 1.5 
hist((spotify_songs_man$danceability_.^(1.5) - 1 )/1.5) # annährend normalverteilt

# Residuenanalyse danceability_.
danceability_.s <- spotify_songs_man$danceability_.
danceability_.s <- danceability_.s[-575]
danceability_.s <- (danceability_.s^(1.5) - 1 )/1.5

model_danceability_. <- lm(log(streams) ~ danceability_.s)
summary(model_danceability_.)
# plot(model_danceability_.)

# Hinzufügen von danceability_. den cleaned Dataframes (einmal mit BoxCox Transformation und Lambda 1.5 und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["danceability_._boxcox"] <- (spotify_songs_man$danceability_.^(1.5) - 1 )/1.5
spotify_songs_cleaned_with_trans_optima["danceability_."] <- spotify_songs_man$danceability_.
spotify_songs_cleaned_without_trans["danceability_."] <- spotify_songs_man$danceability_.

### Prädiktor valence_. (happiness) ###
# low_valence: sad depressed, angry
# high_valence: happy cheerful, euphoric
# spotify_songs_man$valence_.
sum(is.na(spotify_songs_man$valence_.))  # missings erkennen -> 0
hist(spotify_songs_man$valence_.) # normalverteilt mit Mittelpunkt 50

# Residuenanalyse valence_.
valence_.s <- spotify_songs_man$valence_.
valence_.s <- valence_.s[-575]

model_valence_. <- lm(log(streams) ~ valence_.s)
summary(model_valence_.)
# plot(model_valence_.)

# Hinzufügen von valence_. den cleaned Dataframes (OHNE Transformation)
spotify_songs_cleaned_with_trans["valence_."] <- spotify_songs_man$valence_.
spotify_songs_cleaned_with_trans_optima["valence_."] <- spotify_songs_man$valence_.
spotify_songs_cleaned_without_trans["valence_."] <- spotify_songs_man$valence_.

### Prädiktor energy_. ###
# verwenden, bereits numerisch
# spotify_songs$energy_.
sum(is.na(spotify_songs$energy_.))  # missings erkennen -> 0
hist(spotify_songs$energy_.) # keine optimale Normalverteilung
hist(log(spotify_songs$energy_.)) # rechtsschief
hist(sqrt(spotify_songs$energy_.)) # keine optimale Normalverteilung
hist(1/ spotify_songs$energy_.) # rechtsschief
boxcox(lm((spotify_songs_man$energy_.) ~ 1)) # optimales lambda nahe 1.5 
hist((spotify_songs_man$energy_.^(1.5) - 1 )/1.5) # annährend normalverteilt mit Mittelpunkt um 300

# Residuenanalyse energy_.
energy_.s <- spotify_songs_man$energy_.
energy_.s <- energy_.s[-575]
energy_.s <- (energy_.s^(1.5) - 1 )/1.5

model_energy_. <- lm(log(streams) ~ energy_.s)
summary(model_energy_.)
# plot(model_energy_.) # beste Residualanalyse mit BoxCox Transformation mit Lambda 1.5

# Hinzufügen von energy_. den cleaned Dataframes (einmal mit BoxCox Transformation und Lambda 1.5 und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["energy_."] <- spotify_songs_man$energy_.
spotify_songs_cleaned_with_trans_optima["energy_._boxcox"] <- (spotify_songs_man$energy_.^(1.5) - 1 )/1.5
spotify_songs_cleaned_without_trans["energy_."] <- spotify_songs_man$energy_.

### Prädiktor acousticness_. ### 
# verwenden, bereits numerisch
# spotify_songs_man$acousticness_.
sum(is.na(spotify_songs_man$acousticness_.))  # missings erkennen -> 0
hist(spotify_songs_man$acousticness_.) # rechtsschief
hist(log(spotify_songs_man$acousticness_.)) # erzeugt -Inf Werte
spotify_songs_man$acousticness_.[spotify_songs_man$acousticness_. == 0] <- 0.0001 # Korrektur der -Inf Werte
hist(log(spotify_songs_man$acousticness_.)) # keine Normalverteilung
hist(sqrt(spotify_songs_man$acousticness_.)) # keine Normalverteilung
hist(1/(spotify_songs_man$acousticness_.)) # keine Normalverteilung
boxcox(lm((spotify_songs_man$acousticness_.) ~ 1)) # optimales lambda ~ 0.4
hist((spotify_songs_man$acousticness_.^0.4 - 1) / 0.4) # annähernde Normalverteilung

# Residuenanalyse acousticness_.
acousticness_.s <- spotify_songs_man$acousticness_.
acousticness_.s <- acousticness_.s[-575]
acousticness_.s[acousticness_.s == 0] <- 0.0001 
acousticness_.s <-(acousticness_.s^0.4 - 1) / 0.4

model_acousticness_. <- lm(log(streams) ~ acousticness_.s)
summary(model_acousticness_.)
# plot(model_acousticness_.)

# Hinzufügen von acousticness_. den beiden cleaned Dataframes (einmal MIT BoxCox Transformation mit Lambda 0.4 und einmal OHNE Transformation)
spotify_songs_cleaned_with_trans["acousticness_._boxcox"] <- (spotify_songs_man$acousticness_.^0.4 - 1) / 0.4
spotify_songs_cleaned_with_trans_optima["acousticness_."] <- spotify_songs_man$acousticness_.
spotify_songs_cleaned_without_trans["acousticness_."] <- spotify_songs_man$acousticness_.

### Prädiktor instrumentalness_. ###
# verwerfen, da praktisch alle Werte = 0
#spotify_songs$instrumentalness_.
sum(is.na(spotify_songs$instrumentalness_.))  # missings erkennen -> 0
sum(spotify_songs$instrumentalness_.== 0) # 865 Einträge mit dem Wert 0!!

### Prädiktor liveness_. ###
# verwenden, bereits numerisch
# spotify_songs_man$liveness_.
sum(is.na(spotify_songs_man$liveness_.))  # missings erkennen -> 0
hist(spotify_songs_man$liveness_.) # rechtsschief 
hist(log(spotify_songs_man$liveness_.)) # keine optimale Normalverteilung
hist(sqrt(spotify_songs_man$liveness_.)) # keine optimale Normalverteilung
hist(1/(spotify_songs_man$liveness_.)) # rechtsschief
boxcox(lm((spotify_songs_man$liveness_.) ~ 1)) # optimales lambda ~ -0.3
hist((spotify_songs_man$liveness_.^(-0.3) - 1) / -0.3) # keine optimale Normalverteilung

# Residuenanalyse liveness_.
liveness_.s <- spotify_songs_man$liveness_.
liveness_.s <- liveness_.s[-575]

model_liveness_. <- lm(log(streams) ~ log(liveness_.s))
summary(model_liveness_.)
# plot(model_liveness_.)

# Hinzufügen von liveness_. den cleaned Dataframes (einmal MIT log Transformation und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["liveness_._log"] <- log(spotify_songs_man$liveness_.)
spotify_songs_cleaned_with_trans_optima["liveness_."] <- spotify_songs_man$liveness_.
spotify_songs_cleaned_without_trans["liveness_."] <- spotify_songs_man$liveness_.

### Prädiktor speechiness_. ###
# verwenden, bereits numerisch
# spotify_songs_man$speechiness_.
sum(is.na(spotify_songs_man$speechiness_.))  # missings erkennen -> 0
hist(spotify_songs_man$speechiness_.) # rechtsschief 
hist(log(spotify_songs_man$speechiness_.)) # keine Normalverteilung
hist(sqrt(spotify_songs_man$speechiness_.)) # rechtsschief
hist(1/(spotify_songs_man$speechiness_.))# keine Normalverteilung
boxcox(lm((spotify_songs_man$speechiness_.) ~ 1)) # optimales lambda ~ -0.3
hist((spotify_songs_man$speechiness_.^(-0.3) - 1) / -0.3) # keine Normalverteilung

# Residuenanalyse speechiness_.
speechiness_.s <- spotify_songs_man$speechiness_.
speechiness_.s <- speechiness_.s[-575]
speechiness_.s <-(speechiness_.s^(-0.3) - 1) / -0.3

model_speechiness_. <- lm(log(streams) ~ speechiness_.s)
summary(model_speechiness_.)
# plot(model_speechiness_.) # beste Residualanalyse mit BoxCox Transformation mit Lambda von -0.3

# Hinzufügen von speechiness_. den cleaned Dataframes (einmal MIT BoxCox Transformation mit Lambda -0.3 und zweimal OHNE Transformation)
spotify_songs_cleaned_with_trans["speechiness_._boxcox"] <- (spotify_songs_man$speechiness_.^(-0.3) - 1) / -0.3
spotify_songs_cleaned_with_trans_optima["speechiness_."] <- spotify_songs_man$speechiness_.
spotify_songs_cleaned_without_trans["speechiness_."] <- spotify_songs_man$speechiness_.

### ZIELVARIABLE streams ###
# numerisch konvertieren
spotify_songs_man$streams <- as.numeric(spotify_songs_man$streams)
sum(is.na(spotify_songs_man$streams)) # missings erkennen -> 1

# zuerst streams den cleaned dataframes hinzufügen und dann missing Wert entfernen
spotify_songs_cleaned_with_trans["streams"] <- data.frame(streams = spotify_songs_man$streams) 
spotify_songs_cleaned_with_trans_optima["streams"] <- data.frame(streams = spotify_songs_man$streams) 
spotify_songs_cleaned_without_trans["streams"] <- data.frame(streams = spotify_songs_man$streams) 

spotify_songs_cleaned_with_trans <-spotify_songs_cleaned_with_trans[-575,] # Index mit fehlendem Wert
spotify_songs_cleaned_with_trans_optima <-spotify_songs_cleaned_with_trans_optima[-575,]
spotify_songs_cleaned_without_trans <-spotify_songs_cleaned_without_trans[-575,] 

sum(is.na(spotify_songs_cleaned_with_trans$streams)) # missings erkennen -> 0
hist(spotify_songs_cleaned_with_trans$streams)# stark rechtsschief 
hist(log(spotify_songs_cleaned_with_trans$streams))# annährend normalverteilt
hist(sqrt(spotify_songs_cleaned_with_trans$streams)) # rechtsschief
hist(1/(spotify_songs_cleaned_with_trans$streams))
boxcox(lm((spotify_songs_man$streams) ~ 1)) # optimales lambda ~ 0.1
hist((spotify_songs_man$streams^(0.1) - 1) / 0.1) # keine optimale Normalverteilung

# ZIELVARIABLE streams in den cleaned Dataframes mittels Log transformieren (bei einem ohne Tranformation)
spotify_songs_cleaned_with_trans$streams <- log(spotify_songs_cleaned_with_trans$streams)
spotify_songs_cleaned_with_trans_optima$streams <- log(spotify_songs_cleaned_with_trans_optima$streams)
spotify_songs_cleaned_without_trans$streams <- spotify_songs_cleaned_without_trans$streams


#############################

# Überprüfung und Abschluss

#############################

# Struktur der beiden cleaned Dataframes
str(spotify_songs_cleaned_with_trans)
str(spotify_songs_cleaned_with_trans_optima)
str(spotify_songs_cleaned_without_trans)

sapply(spotify_songs_cleaned_with_trans, function(x) sum(x == -Inf)) # Überprüfung, dass alle -Inf Values bereinigt sind -> keine mehr
sapply(spotify_songs_cleaned_with_trans_optima, function(x) sum(x == -Inf))
sapply(spotify_songs_cleaned_without_trans, function(x) sum(x == -Inf))

# Erster Test mit Regressionsmodell
model <- lm(streams ~ ., data = spotify_songs_cleaned_with_trans_optima)
summary(model)
# plot(model) # Outliers erkennbar

# Entfernen eruierter Outliers
spotify_songs_cleaned_with_trans <- spotify_songs_cleaned_with_trans[-c(124, 394, 426),]
spotify_songs_cleaned_with_trans_optima <- spotify_songs_cleaned_with_trans_optima[-c(124, 394, 426),]
spotify_songs_cleaned_without_trans <- spotify_songs_cleaned_without_trans[-c(124, 394, 426),]

# Test mit Regressionsmodell nach Entfernen offensichtlicher Outliers
model <- lm(streams ~ ., data = spotify_songs_cleaned_with_trans_optima)
summary(model)

################################################################################

#             Datasets für Modellbildung
#       1. RData Files, damit Datentypen, wie factor korrekt übernommen werden
#       2. csv Files, falls explizit csv File benötigt wird

###############################################################################

# RData mit teilweise transformierten Prädiktoren (nicht alle optimal)
save(spotify_songs_cleaned_with_trans, file = "./data/spotify_songs_cleaned_with_trans.RData")
# RData mit teilweise transformierten Prädiktoren (alle optimal)
save(spotify_songs_cleaned_with_trans_optima, file = "./data/spotify_songs_cleaned_with_trans_optima.RData")
# Rdata mit Prädiktoren ohne Transformationen (ausser Zielvariable)
save(spotify_songs_cleaned_without_trans, file = "./data/spotify_songs_cleaned_without_trans.RData")



# csv mit teilweise transformierten Prädiktoren (nicht alle optimal)
write.csv(spotify_songs_cleaned_with_trans, "./data/spotify-2023_cleaned_with_trans.csv", row.names = FALSE)
# csv mit teilweise transformierten Prädiktoren (alle optimal)
write.csv(spotify_songs_cleaned_with_trans_optima, "./data/spotify-2023_cleaned_with_trans_optima.csv", row.names = FALSE)
# csv mit Prädiktoren ohne Transformationen (ausser Zielvariable)
write.csv(spotify_songs_cleaned_without_trans, "./data/spotify-2023_cleaned_without_trans.csv", row.names = FALSE)
