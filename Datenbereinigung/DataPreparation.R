library(MASS)

################################# Datenbereinigung ###################################

spotify_songs <- read.csv("spotify-2023.csv", encoding="latin1") # dataframe mit csv von kaggle
spotify_songs_man <- spotify_songs # kopiertes Dataframe für Manipulationen
spotify_songs_cleaned <- data.frame() # leeres Dataframe, in welchem die "bereinigten" Prädiktoren abgelegt werden (bereit für Modellbildung)

# Ausgangslage des Datasets
summary(spotify_songs_man) # Summary des Datasets
str(spotify_songs_man) # Struktur des Datasets

sapply(spotify_songs_man, function(x) sum(is.nan(x))) # gibt Spaltenweise Anzahl NaN's zurück -> keine
sapply(spotify_songs_man, function(x) sum(is.na(x))) # gibt Spaltenweise Anzahl Na's zurück -> keine
sapply(spotify_songs_man, function(x) sum(x == "")) # gibt Spaltenweise Anzahl leerer Strings zurück zurück -> key = 95; in_shazam_charts = 50
sapply(spotify_songs_man, function(x) grep("ï¿½", x)) # gibt Spaltenweise Indices der Einträge mit fehlerhaftem Encoding zurück zurück -> track_name = 58; artist.s._name = 40
sapply(spotify_songs_man, function(x) sum(x == -Inf)) # gibt Spaltenweise Anzahl der -Inf Werte zurück -> keine

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
indices_with_encoding_errors <-grep("ï¿½", spotify_songs_man$track_name)
values_with_encoding_errors <- spotify_songs_man$track_name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (58 Outputs)
spotify_songs_man$track_name <- NULL # erst entfernen, wenn artist.s._name cleaned ist

## artist.s._name -> vorerst behalten und versuchen zu bereinigung (encoding fixen)
# mittels web scraping versuchen die Streams pro artist pro Monat zu ermitteln; sind in einem track_name 
# mehrere artisten beteiligt -> Mittelwert verwenden
spotify_songs_man$artist.s._name
sum(is.na(spotify_songs_man$artist.s._name)) # missings erkennen -> 0
Encoding(spotify_songs_man$artist.s._name) # viele "unknowns" -> encoding kann nicht angepasst werden -> manuelle Korrektur

indices_with_encoding_errors <-grep("ï¿½", spotify_songs_man$artist.s._name)
values_with_encoding_errors <- spotify_songs_man$artist.s._name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (40 Outputs)

# Manuelle Korrektur
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


indices_with_encoding_errors <-grep("ï¿½", spotify_songs_man$artist.s._name)
values_with_encoding_errors <- spotify_songs_man$artist.s._name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (0 outputs verbleibend)


## artist_count -> belassen, keine missings und bereits numerisch
spotify_songs_man$artist_count
sum(is.na(spotify_songs_man$artist_count)) # missings erkennen -> 0
barplot(table(spotify_songs_man$artist_count), main="Barplot artist_count", xlab="artist_count", ylab="Frequency") # rechtsschief -> Transformation?
hist(log(spotify_songs_man$artist_count)) # keine Veränderung
hist(sqrt(spotify_songs_man$artist_count)) # keine Veränderung
boxcox(lm(spotify_songs_man$artist_count ~ 1)) # optimales Lambda bei - 1.5
hist((spotify_songs_man$artist_count^(-1.5) -1) / -1.5) # immer noch rechtsschief


# residuenanalyse artist_count
streams <- as.numeric(spotify_songs_man$streams)
streams <- streams[-575]
artist_counts <- spotify_songs_man$artist_count
artist_counts <- artist_counts[-575]

model_artist_count <- lm(log(streams) ~ artist_counts)
summary(model_artist_count)
#plot(model_artist_count)


# Transformation nicht möglich -> evtl. verwerfen oder als kategorieller Prädiktor verwenden

# artist_count dem "cleaned dataframe" hinzufügen, falls verwendet wird (kategoriell)!
spotify_songs_cleaned <- data.frame(artist_count = as.factor(spotify_songs_man$artist_count))
barplot(table(spotify_songs_cleaned$artist_count), main="Barplot artist_count", xlab="artist_count", ylab="Frequency") # rechtsschief -> Transformation?

# artist_count dem "cleaned dataframe" hinzufügen, falls verwendet wird (numerisch)!
#spotify_songs_cleaned <- data.frame(artist_count = spotify_songs$artist_count)

# released_day -> z. B. Datum zusammenfügen und den Wochentag (weekday) daraus extrahieren 
# ???? Ansatz unten: Released Datum aus year, month und day zusammensetzen; weekday eruieren und numerisch konvertieren
spotify_songs_man$released_date <- as.Date(paste(spotify_songs$released_year, spotify_songs$released_month, spotify_songs$released_day, sep="-"), format="%Y-%m-%d")
spotify_songs_man$released_weekday <- weekdays(spotify_songs_man$released_date)
spotify_songs_man$released_weekday

# Entweder mittels mapping und numerischer Konvertierung 
# day_mapping <- c("Montag" = 1, "Dienstag" = 2, "Mittwoch" = 3, "Donnerstag" = 4, "Freitag" = 5, "Samstag" = 6, "Sonntag" = 7)
# spotify_songs_cleaned$released_weekday <- as.numeric(day_mapping[spotify_songs_cleaned$released_weekday])
# spotify_songs_cleaned$released_weekday
# sum(is.na(spotify_songs_cleaned$released_weekday)) # missings erkennen -> 0

# oder mittels as.factor()
spotify_songs_cleaned$released_weekday <- factor(spotify_songs_man$released_weekday)
spotify_songs_cleaned$released_weekday
sum(is.na(spotify_songs_cleaned$released_weekday)) # missings erkennen -> 0

# plot
barplot(table(spotify_songs_cleaned$released_weekday), main="Barplot weekday", xlab="weekday", ylab="Frequency")
#spotify_songs_cleaned$released_date <- NULL # -> zusammengesetztes release_date wieder entfernen

## released_year -> umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon (year_since_release)
# wahrscheinlich verwerfen, da keine Transformation erfolgreich ist
spotify_songs_man$years_since_release <- 2023 - spotify_songs_man$released_year
sum(is.na(spotify_songs_man$years_since_release)) # missings erkennen -> 0
spotify_songs_man$years_since_release
hist(spotify_songs_man$years_since_release) # rechtsschief
hist(log(spotify_songs_man$years_since_release)) # erzeugt - Inf Werte
spotify_songs_man$years_since_release[spotify_songs_man$years_since_release == 0] <- 0.0001 # Korrektur der -Inf Werte 
hist(log(spotify_songs_man$years_since_release)) # Korrektur der - Inf Werte
hist(sqrt(spotify_songs_man$years_since_release)) # keine Veränderung
hist(1/(spotify_songs_man$years_since_release)) # keine Veränderung 


# optimale Transformation mittels boxcox ermitteln
boxcox(lm((spotify_songs_man$years_since_release) ~ 1)) # optimales lambda ~ 0.3 
hist((spotify_songs_man$years_since_release^(0.2) -1) / 0.2)

#Residuenanalyse years_since_release
years_since_release <- spotify_songs_man$years_since_release
years_since_release <- years_since_release[-575]
years_since_release <- (years_since_release^(0.2) -1) / 0.2

model_years_since_release <- lm(log(streams) ~ years_since_release)
summary(model_years_since_release)
#plot(model_years_since_release)

# transformierte years_since_release dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["years_since_release"] <- (spotify_songs_man$years_since_release^(0.2) -1) / 0.2

## released_month -> belasssen bereits numerisch und keine missings
spotify_songs_man$released_month
sum(is.na(spotify_songs_man$released_month)) # missings erkennen -> 0
barplot(table(spotify_songs_man$released_month), main="Barplot released_month", xlab="Month", ylab="Frequency")

# released_month faktorisieren und dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["released_month"] <- factor(spotify_songs_man$released_month, levels = 1:12, labels = month.name)
barplot(table(spotify_songs_cleaned$released_month), main="Barplot released_month", xlab="Month", ylab="Frequency")

## in_spotify_playlists -> belassen
spotify_songs_man$in_spotify_playlists
sum(is.na(spotify_songs_man$in_spotify_playlists)) # missings erkennen -> 0
hist(spotify_songs_man$in_spotify_playlists) # rechtsschief -> Transformierung mittels Log() sinnvoll?

hist(log(spotify_songs$in_spotify_playlists))
qqnorm(log(spotify_songs$in_spotify_playlists))
qqline(log(spotify_songs$in_spotify_playlists), col = "red")

#Residuenanalyse in_spotify_playlists
in_spotify_playlist <- spotify_songs_man$in_spotify_playlists
in_spotify_playlist <- in_spotify_playlist[-575]

model_in_spotify_playlists <- lm(log(streams) ~ log(in_spotify_playlist))
summary(model_in_spotify_playlists)
#plot(model_in_spotify_playlists)


# log(in_spotify_playlists) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_spotify_playlists"] <- data.frame(in_spotify_playlists = log(spotify_songs_man$in_spotify_playlists))

# in_spotify_charts -> vorerst belassen (Recherche, was es genau ist)
spotify_songs_man$in_spotify_charts # enthält viele 0 Werte
sum(is.na(spotify_songs_man$in_spotify_charts)) # missings erkennen -> 0
hist(spotify_songs_man$in_spotify_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll? -> erzeugt -Inf Werte; besser Transformation mittels sqrt()?
hist(log(spotify_songs_man$in_spotify_charts)) # Transformation mittels log() erzeugt -Inf Werte
hist(sqrt(spotify_songs_man$in_spotify_charts)) # rechtsschief
hist(1/(spotify_songs_man$in_spotify_charts)) # rechtsschief
spotify_songs_man$in_spotify_charts[spotify_songs_man$in_spotify_charts == 0] <- 0.0001 
hist(log(spotify_songs_man$in_spotify_charts))
boxcox(lm((spotify_songs_man$in_spotify_charts) ~ 1)) #optimales lambda ~ 0 -> log Transformation

#Residuenanalyse in_spotify_charts
in_spotify_chart <- spotify_songs_man$in_spotify_charts
in_spotify_chart <- in_spotify_chart[-575]
in_spotify_chart[in_spotify_chart == 0] <- 0.0001 

model_in_spotify_charts <- lm(log(streams) ~ log(in_spotify_chart))
summary(model_in_spotify_charts)
#plot(model_in_spotify_charts)

# in_spotify_charts dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_spotify_charts"] <- data.frame(in_spotify_charts = log(spotify_songs_man$in_spotify_charts))

## in_apple_playlists -> belassen und verwenden
spotify_songs_man$in_apple_playlists
sum(is.na(spotify_songs_man$in_apple_playlists)) # missings erkennen -> 0
hist(spotify_songs_man$in_apple_playlists) # rechtsschief -> Transformierung mittels Log() sinnvoll? -> erzeugt -Inf Werte; besser Transformation mittels sqrt()?
hist(log(spotify_songs_man$in_apple_playlists))
hist(sqrt(spotify_songs_man$in_apple_playlists))
hist(1/(spotify_songs_man$in_apple_playlists)) 
spotify_songs_man$in_apple_playlists[spotify_songs_man$in_apple_playlists == 0] <- 0.0001 
hist(log(spotify_songs_man$in_apple_playlists))
boxcox(lm((spotify_songs_man$in_apple_playlists) ~ 1)) # optimales lambda ~ 0.2 
hist((spotify_songs_man$in_apple_playlists^(0.3) - 1 )/0.3)

#Residuenanalyse in_apple_playlists
in_apple_playlist <- spotify_songs_man$in_apple_playlists
in_apple_playlist <- in_apple_playlist[-575]
in_apple_playlist[in_apple_playlist == 0] <- 0.0001 
in_apple_playlist <- (in_apple_playlist^(0.3) - 1 )/0.3

model_in_apple_playlists <- lm(log(streams) ~ in_apple_playlist)
summary(model_in_apple_playlists)
#plot(model_in_apple_playlists)

# transformierte in_apple_playlists dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_apple_playlists"] <- data.frame(in_apple_playlists = (spotify_songs_man$in_apple_playlists^(0.3) - 1 )/0.3)

## in_apple_charts -> belassen und verwenden
spotify_songs_man$in_apple_charts # enthält viele 0 Werte
sum(is.na(spotify_songs_man$in_apple_charts)) # missings erkennen -> 0
hist(spotify_songs_man$in_apple_charts)
hist(log(spotify_songs_man$in_apple_charts)) # rechtsschief -> Transformierung mittels Log() sinnvoll? -> erzeugt -Inf Werte; besser Transformation mittels sqrt()?
hist(sqrt(spotify_songs_man$in_apple_charts))# leichte Verbesserung
hist(1/(spotify_songs_man$in_apple_charts)) # rechtsschief
spotify_songs_man$in_apple_charts[spotify_songs_man$in_apple_charts == 0] <- 0.0001 
hist(log(spotify_songs_man$in_apple_charts))
boxcox(lm((spotify_songs_man$in_apple_charts) ~ 1)) # optimales lambda ~ 0.2 
hist((spotify_songs_man$in_apple_charts^(0.3) - 1 )/0.3)

#Residuenanalyse in_apple_charts
in_apple_chart <- spotify_songs_man$in_apple_charts
in_apple_chart <- in_apple_chart[-575]
in_apple_chart[in_apple_chart == 0] <- 0.0001 
in_apple_chart <- (in_apple_chart^(0.3) - 1 )/0.3

model_in_apple_charts <- lm(log(streams) ~ in_apple_chart)
summary(model_in_apple_charts)
#plot(model_in_apple_charts)

# transformierte in_apple_charts dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_apple_charts"] <- data.frame(in_apple_charts = (spotify_songs_man$in_apple_charts^(0.3) - 1 )/0.3)

## in_deezer_playlists -> numerisch konvertieren und verwenden
sum(is.na(spotify_songs_man$in_deezer_playlists)) # missings erkennen -> 0
spotify_songs_man$in_deezer_playlists # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs_man$in_deezer_playlists <- gsub(",", "", spotify_songs_man$in_deezer_playlists) # ersetzt "," durch ""
spotify_songs_man$in_deezer_playlists <- as.numeric(spotify_songs_man$in_deezer_playlists)
spotify_songs_man$in_deezer_playlists[is.na(spotify_songs_man$in_deezer_playlists)] # keine NA's mehr

hist(spotify_songs_man$in_deezer_playlists) # rechtsschief -> Transformierung mittels Log() sinnvoll? -> erzeugt -Inf Werte
hist(log(spotify_songs_man$in_deezer_playlists))
spotify_songs_man$in_deezer_playlists[spotify_songs_man$in_deezer_playlists == 0] <- 0.0001 
hist(log(spotify_songs_man$in_deezer_playlists))
hist(sqrt(spotify_songs_man$in_deezer_playlists))
hist(1/(spotify_songs_man$in_deezer_playlists))
boxcox(lm((spotify_songs_man$in_deezer_playlists) ~ 1)) # optimales lambda ~ 0.1 
hist((spotify_songs_man$in_deezer_playlists^(0.1) - 1 )/0.1)

#Residuenanalyse in_deezer_playlists
in_deezer_playlist <- spotify_songs_man$in_deezer_playlists
in_deezer_playlist <- in_deezer_playlist[-575]
in_deezer_playlist[in_deezer_playlist == 0] <- 0.0001 
in_deezer_playlist <- (in_deezer_playlist^(0.1) - 1 )/0.1

model_in_deezer_playlists <- lm(log(streams) ~ in_deezer_playlist)
summary(model_in_deezer_playlists)
#plot(model_in_deezer_playlists)

# transformierte in_deezer_playlist dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_deezer_playlists"] <- data.frame(in_deezer_playlists = (spotify_songs_man$in_deezer_playlists^(0.1) - 1 )/0.1)

## in_deezer_charts -> vorerst belassen; hat aber viele 0 Werte!!
spotify_songs_man$in_deezer_charts
sum(is.na(spotify_songs_man$in_deezer_charts)) # missings erkennen -> 0
hist(spotify_songs_man$in_deezer_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs_man$in_deezer_charts))
spotify_songs_man$in_deezer_charts[spotify_songs_man$in_deezer_charts == 0] <- 0.0001 
hist(log(spotify_songs_man$in_deezer_charts))
hist(sqrt(spotify_songs_man$in_deezer_charts))
hist(1/(spotify_songs_man$in_deezer_charts))
boxcox(lm((spotify_songs_man$in_deezer_charts) ~ 1)) # optimales lambda nahe 0 -> log Transformation

#Residuenanalyse in_deezer_charts
in_deezer_chart <- spotify_songs_man$in_deezer_charts
in_deezer_chart <- in_deezer_chart[-575]
in_deezer_chart[in_deezer_chart == 0] <- 0.0001 

model_in_deezer_charts <- lm(log(streams) ~ log(in_deezer_chart))
summary(model_in_deezer_charts)
plot(model_in_deezer_charts)

# log(in_deezer_charts) dem "cleaned dataframe" hinzufügen
#spotify_songs_cleaned["in_deezer_charts"] <- data.frame(in_deezer_charts = log(spotify_songs_man$in_deezer_charts))


## in_shazam_charts -> numerisch konvertieren und verwenden; missings
spotify_songs_man$in_shazam_charts # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs_man["in_shazam_charts"] <- data.frame(in_shazam_charts = spotify_songs_man$in_shazam_charts)
spotify_songs_man$in_shazam_charts <- gsub(",", "", spotify_songs_man$in_shazam_charts)
spotify_songs_man$in_shazam_charts <- as.numeric(spotify_songs_man$in_shazam_charts)
spotify_songs_man$in_shazam_charts[is.na(spotify_songs_man$in_shazam_charts)] # viele NA's
sum(is.na(spotify_songs_man$in_shazam_charts))  # missings erkennen -> 50
# Wie NA's handeln??? unten Variante mit median
spotify_songs_man$in_shazam_charts[is.na(spotify_songs_man$in_shazam_charts)] <-round(median(spotify_songs_man$in_shazam_charts, na.rm = TRUE))
hist(spotify_songs_man$in_shazam_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs_man$in_shazam_charts))
spotify_songs_man$in_shazam_charts[spotify_songs_man$in_shazam_charts == 0] <- 0.0001 
hist(log(spotify_songs_man$in_shazam_charts))
hist(sqrt(spotify_songs_man$in_shazam_charts))
hist(1/(spotify_songs_man$in_shazam_charts))
boxcox(lm((spotify_songs_man$in_shazam_charts) ~ 1)) # optimales lambda nahe 0 -> log Transformation

#Residuenanalyse in_shazam_charts
in_shazam_chart <- spotify_songs_man$in_shazam_charts
in_shazam_chart <- in_shazam_chart[-575]
in_shazam_chart[in_shazam_chart == 0] <- 0.0001 

model_in_shazam_chart <- lm(log(streams) ~ log(in_shazam_chart))
summary(model_in_shazam_charts)
#plot(model_in_shazam_charts)

# log(n_shazam_charts) dem "cleaned dataframe" hinzufügen
#spotify_songs_cleaned["in_shazam_charts"] <- data.frame(in_shazam_charts = (spotify_songs_man$in_shazam_charts))

## bpm -> verwenden und belassen
spotify_songs_man$bpm
sum(is.na(spotify_songs_man$bpm))  # missings erkennen -> 0
hist(spotify_songs_man$bpm)
qqnorm(spotify_songs_man$bpm)
qqline(spotify_songs_man$bpm, col = "red")


#Residuenanalyse bpm
bpms <- spotify_songs_man$bpm
bpms <- bpms[-575]

model_bpm <- lm(log(streams) ~ bpms)
summary(model_bpm)
plot(model_bpm)

# bpm dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["bpm"] <- data.frame(bpm = spotify_songs_man$bpm)

## key -> numerisch konvertieren (encoden) und verwenden
spotify_songs$key # -> enthält leere Strings ""
sum(spotify_songs$key == "")  # 95 leere Strings
#key_value <- as.character(names(sort(table(spotify_songs$key), decreasing=TRUE)[1])) # ermittelt häfigst verwendeter Key
# key dem "cleaned dataframe" hinzufügen
spotify_songs_man$key[spotify_songs_man$key == ""] <- "Keine Angabe" # -> setzt den ermittelten key für den leeren String ein

factor_column_key <- as.factor(spotify_songs_man$key) # faktorisiert den key
levels(factor_column_key) # 1 - 11 : "A"  "A#" "B"  "C#" "D"  "D#" "E"  "F"  "F#" "G"  "G#" "Keine Angabe"
spotify_songs_man$key <- as.factor(spotify_songs_man$key)
barplot(table(spotify_songs_man$key), main="Barplot key", xlab="Key", ylab="Frequency")

# key dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["key"] <- data.frame(key = spotify_songs_man$key)

## mode -> numerisch konvertieren (encoden) und verwenden
spotify_songs$mode
sum(is.na(spotify_songs$mode))  # missings erkennen -> 0

# Entweder mittels "if ... else" für qualitative Prädiktoren 
# spotify_songs_cleaned["mode"] <- data.frame(mode = spotify_songs$mode)
# spotify_songs_cleaned$mode <-ifelse(spotify_songs_cleaned$mode == "Minor", 1, 0)
# barplot(table(spotify_songs_cleaned$mode), main="Barplot mode", xlab="Mode", ylab="Frequency", names.arg=c("Major", "Minor"), col=c("blue", "red"))

# oder mittels as.factor() für Dummy- Codierung
spotify_songs_man["mode"] <- data.frame(mode = spotify_songs_man$mode)
spotify_songs_man$mode <-as.factor(spotify_songs_man$mode)
barplot(table(spotify_songs_man$mode), main="Barplot mode", xlab="Mode", ylab="Frequency", names.arg=c("Major", "Minor"), col=c("blue", "red"))

# key dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["mode"] <- data.frame(mode = spotify_songs_man$mode)

## danceability_. -> verwenden und belassen
spotify_songs_man$danceability_.
sum(is.na(spotify_songs_man$danceability_.))  # missings erkennen -> 0
hist(spotify_songs_man$danceability_.)
qqnorm(spotify_songs_man$danceability_.)
qqline(spotify_songs_man$danceability_., col = "red")

#Residuenanalyse danceability_.
danceability_.s <- spotify_songs_man$danceability_.
danceability_.s <- danceability_.s[-575]

model_danceability_. <- lm(log(streams) ~ danceability_.s)
summary(model_danceability_.)
#plot(model_danceability_.)

# danceability_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["danceability_."] <- data.frame(danceability_. = spotify_songs_man$danceability_.)

## valence_. -> Recherche was es ist. ggf. verwenden und belassen -> happiness
# low_valence: sad depressed, angry
# high_valence: happy cheerful, euphoric
spotify_songs_man$valence_.
sum(is.na(spotify_songs_man$valence_.))  # missings erkennen -> 0
hist(spotify_songs_man$valence_.)
qqnorm(spotify_songs_man$valence_.)
qqline(spotify_songs_man$valence_., col = "red")

#Residuenanalyse valence_.
valence_.s <- spotify_songs_man$valence_.
valence_.s <- valence_.s[-575]

model_valence_. <- lm(log(streams) ~ valence_.s)
summary(model_valence_.)
#plot(model_valence_.)

# valence_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["valence_."] <- data.frame(valence_. = spotify_songs_man$valence_.)

## energy_. -> verwenden und belassen
spotify_songs$energy_.
sum(is.na(spotify_songs$energy_.))  # missings erkennen -> 0
hist(spotify_songs$energy_.)
qqnorm(spotify_songs$energy_.)
qqline(spotify_songs$energy_., col = "red")

#Residuenanalyse energy_.
energy_.s <- spotify_songs_man$energy_.
energy_.s <- energy_.s[-575]

model_energy_. <- lm(log(streams) ~ energy_.s)
summary(model_energy_.)
#plot(model_energy_.)

# energy_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["energy_."] <- data.frame(energy_. = spotify_songs$energy_.)

## acousticness_. -> verwenden und belassen
spotify_songs_man$acousticness_.
sum(is.na(spotify_songs_man$acousticness_.))  # missings erkennen -> 0
hist(spotify_songs_man$acousticness_.)
hist(log(spotify_songs_man$acousticness_.))
qqnorm(spotify_songs_man$acousticness_.)
qqline(spotify_songs_man$acousticness_., col = "red")
spotify_songs_man$acousticness_.[spotify_songs_man$acousticness_. == 0] <- 0.0001 
hist(log(spotify_songs_man$acousticness_.))
hist(sqrt(spotify_songs_man$acousticness_.))
hist(1/(spotify_songs_man$acousticness_.))
boxcox(lm((spotify_songs_man$acousticness_.) ~ 1)) # optimales lambda ~ 0.4
hist((spotify_songs_man$acousticness_.^0.4 - 1) / 0.4)

#Residuenanalyse acousticness_.
acousticness_.s <- spotify_songs_man$acousticness_.
acousticness_.s <- acousticness_.s[-575]
acousticness_.s[acousticness_.s == 0] <- 0.0001 
acousticness_.s <-(acousticness_.s^0.4 - 1) / 0.4

model_acousticness_. <- lm(log(streams) ~ acousticness_.s)
summary(model_acousticness_.)
#plot(model_acousticness_.)

# transformierte acousticness_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["acousticness_."] <- data.frame(acousticness_. = (spotify_songs_man$acousticness_.^0.4 - 1) / 0.4)

## instrumentalness_.-> evtl. verwerfen, da praktisch alle Werte = 0
spotify_songs$instrumentalness_.
sum(is.na(spotify_songs$instrumentalness_.))  # missings erkennen -> 0
sum(spotify_songs$instrumentalness_.== 0) # 865 Einträge mit dem Wert 0!!

## liveness_. -> verwenden und belassen
spotify_songs_man$liveness_.
sum(is.na(spotify_songs_man$liveness_.))  # missings erkennen -> 0
hist(spotify_songs_man$liveness_.) # rechtsschief -> Transformierung mittels Log() sinnvoll
hist(log(spotify_songs_man$liveness_.))
hist(sqrt(spotify_songs_man$liveness_.))
hist(1/(spotify_songs_man$liveness_.))
qqnorm(log(spotify_songs_man$liveness_.))
qqline(log(spotify_songs_man$liveness_.), col = "red")
boxcox(lm((spotify_songs_man$liveness_.) ~ 1)) # optimales lambda ~ -0.3
hist((spotify_songs_man$liveness_.^(-0.3) - 1) / -0.3)

#Residuenanalyse liveness_.
liveness_.s <- spotify_songs_man$liveness_.
liveness_.s <- liveness_.s[-575]

model_liveness_. <- lm(log(streams) ~ log(liveness_.s))
summary(model_liveness_.)
#plot(model_liveness_.)

# log(liveness_.) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["liveness_."] <- data.frame(liveness_. = log(spotify_songs_man$liveness_.))

## speechiness_. -> verwenden und belassen
spotify_songs_man$speechiness_.
sum(is.na(spotify_songs_man$speechiness_.))  # missings erkennen -> 0
hist(spotify_songs_man$speechiness_.) # rechtsschief -> Transformierung mittels Log() sinnvoll
hist(log(spotify_songs_man$speechiness_.))
hist(sqrt(spotify_songs_man$speechiness_.))
hist(1/(spotify_songs_man$speechiness_.))
qqnorm(log(spotify_songs_man$speechiness_.))
qqline(log(spotify_songs_man$speechiness_.), col = "red")

boxcox(lm((spotify_songs_man$speechiness_.) ~ 1)) # optimales lambda ~ -0.3
hist((spotify_songs_man$speechiness_.^(-0.3) - 1) / -0.3)

#Residuenanalyse speechiness_.
speechiness_.s <- spotify_songs_man$speechiness_.
speechiness_.s <- speechiness_.s[-575]
speechiness_.s <-(speechiness_.s^(-0.3) - 1) / -0.3

model_speechiness_. <- lm(log(streams) ~ speechiness_.s)
summary(model_speechiness_.)
#plot(model_speechiness_.)

# transformierte speechiness_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["speechiness_."] <- data.frame(speechiness_. = (spotify_songs_man$speechiness_.^(-0.3) - 1) / -0.3)

## streams -> ZIELVARIABLE! numerisch konvertieren
spotify_songs_man$streams <- as.numeric(spotify_songs$streams)
sum(is.na(spotify_songs_man$streams)) # missings erkennen -> 1

# streams dem "cleanded" dataframe hinzufügen
spotify_songs_cleaned["streams"] <- data.frame(streams = spotify_songs_man$streams) 

spotify_songs_cleaned <-spotify_songs_cleaned[-575,] # wenn Index bekannt so löschen
sum(is.na(spotify_songs_cleaned$streams)) # missings erkennen -> 0
hist(spotify_songs_cleaned$streams)# rechtsschief -> Transformierung?
hist(log(spotify_songs_cleaned$streams))

boxcox(lm((spotify_songs_man$streams) ~ 1)) # optimales lambda ~ 0.1
hist((spotify_songs_man$streams^(0.1) - 1) / 0.1)
spotify_songs_cleaned$streams <- log(spotify_songs_cleaned$streams)

str(spotify_songs_cleaned)

sapply(spotify_songs_cleaned, function(x) sum(x == -Inf)) # Überprüfung, dass alle -Inf Values bereinigt sind -> keine mehr
#### 3 Alle Werte "messbar" machen ######

# integriert in 2.

#### 4 Datentypen in R auf Korrektheit prüfen #####


###### Plausibilisierung #######

#### 1 Verteilung, Symmetrie, Ausreisser pro Prädiktor plotten #### 

spotify_songs_cleaned$artist.s._name <- NULL # ACHTUNG: sobald artist.s._name numerisch , diese Zeile nicht mehr verwenden!

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
      barplot(table(dataframe[[predictor]]), main=paste("Balkendiagramm für", predictor), col = "purple")
      
      # Boxplot für jede Kategorie
      boxplot(dataframe[[dependent_variable]] ~ dataframe[[predictor]], main=paste("Boxplot für", predictor), col = "blue", horizontal=TRUE)
      
    } else {
      # Plots für kontinuierliche Prädiktoren
      par(mfrow=c(2, 2))
      
      # Histogramm
      hist(dataframe[[predictor]], main=paste("Histogramm für", predictor), col = "purple", xlab=predictor)
      
      # Dichteplot
      plot(density(na.omit(dataframe[[predictor]])), main=paste("Dichteplot für", predictor), col = "red", xlab=predictor)
      
      # QQ-Plot
      qqnorm(dataframe[[predictor]], main=paste("QQ-Plot für", predictor))
      qqline(dataframe[[predictor]], col = "red")
      
      # Boxplot
      boxplot(dataframe[[predictor]], main=paste("Boxplot für", predictor), col = "blue", horizontal=TRUE)
      
    }
      # Residuenplot für kontinuierliche Prädiktoren
      if (!is.factor(dataframe[[predictor]])) {
        modell <- lm(formula(paste(dependent_variable, "~", predictor)), data = dataframe)
        plot(modell$fitted.values, resid(modell),
             xlab = "Vorhergesagte Werte",
             ylab = "Residuen",
             main = paste("Residuenplot für", predictor),
             col = "darkgreen")
        abline(h = 0, col = "red")
      }
    }
  }

#create_plots(spotify_songs_cleaned, "streams") #-> auskommentieren, wenn Plots gewünscht!

#### 1 Verteilung, Symmetrie, Ausreisser pro Prädiktor beschreiben #### 
## TODO

#### 2 Alle Werte, wo notwendig transformieren #####

# track_name          : dropped
# artist.s._name      : wird verwendet, sobald web scraping fertig
# artist_count        : aktuell noch verwendet mit Faktorisierung; numerische Verwendung nicht sinnvoll, da Rechtsschiefe auch durch Transformationen nicht eliminiert werden kann
# weekday             : neuer Prädiktor aus released_year, released_month und released_day; wird faktorisiert verwendet
# years_since-release : neuer Prädiktor aus released_year; Rechtsschiefe kann auch durch Transformierungen nicht beseitigt werden; wird nicht verwendet
# released_month      : wird faktorisiert verwendet
# released_day        : wird für weekday benötigt; kein eigenständiger Prädiktor
# in_spotify_playlists: wird nach Transformierung mittels log() verwendet
# in_spotify_charts   : keine sinnvolle Transformierung zur Beseitigung der Rechtsschiefe; wird nicht verwendet
# streams             : chr -> ZIELVARIABLE! numerisch konvertieren und aufgrund bestehender Rechtsschiefe boxcox Transformierung mit Lambda = 0.1
# in_apple_playlists  : aufgrund bestehender Rechtsschiefe boxcox Transformierung mit Lambda = 0.3
# in_apple_charts     : aufgrund bestehender Rechtsschiefe boxcox Transformierung mit Lambda = 0.3
# in_deezer_playlists : aufgrund bestehender Rechtsschiefe boxcox Transformierung mit Lambda = 0.1
# in_deezer_charts    : keine sinnvolle Transformierung zur Beseitigung der Rechtsschiefe; wird nicht verwendet
# in_shazam_charts    : keine sinnvolle Transformierung zur Beseitigung der Rechtsschiefe; wird nicht verwendet 
# bpm                 : kann direkt als Prädiktor verwendet werden
# key                 : wird faktorisiert verwendet; fehlende Keys werden als zusätzliches Level "keine Angabe" kodiert
# mode                : wird faktorisiert verwendet
# danceability_.      : kann direkt als Prädiktor verwendet werden
# valence_.           : kann direkt als Prädiktor verwendet werden 
# energy_.            : kann direkt als Prädiktor verwendet werden
# acousticness_.      : aufgrund bestehender Rechtsschiefe boxcox Transformierung mit Lambda = 0.4
# instrumentalness_.  : zu viele 0 Werte; wird nicht verwendet
# liveness_.          : aufgrund bestehender Rechtsschiefe boxcox Transformierung mit Lambda = -0.3
# speechiness_.       : aufgrund bestehender Rechtsschiefe boxcox Transformierung mit Lambda = -0.3



#### 3 Fehlende Variablen beschreiben #####

# Hat es fehlende Werte? 
## TODO

# Wenn Dataset bereit für Modellbildung:

write.csv(spotify_songs_cleaned, "spotify-2023_cleaned.csv", row.names = FALSE)


# Tests
# 1.
model <- lm(streams ~ ., data = spotify_songs_cleaned)
summary(model)
plot(model)

str(spotify_songs_cleaned)

spotify_songs_cleaned <- spotify_songs_cleaned[-c(124, 145, 394, 403, 426, 620),]

model <- lm(streams ~ ., data = spotify_songs_cleaned)
summary(model)
plot(model)

str(spotify_songs_cleaned)

drop1(object = model)
model_backward <- update(object = model, formula = . ~. - key)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - artist_count)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - released_weekday)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - speechiness_.)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - in_deezer_playlists)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - liveness_.)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - valence_.)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - mode)
drop1(object = model_backward)

model_backward <- update(object = model_backward, formula = . ~. - energy_.)
drop1(object = model_backward)

summary(model_backward)
plot(model_backward)

library(car)
vif(model_backward)
1/vif(model_backward)

pairs(~ in_apple_charts + in_apple_playlists + streams, upper.panel = NULL)


# 2.
model <- lm(streams ~ in_spotify_playlists  + years_since_release + in_spotify_charts, data = spotify_songs_cleaned)
summary(model)
plot(model)

str(spotify_songs_cleaned)


subset_cor_lists <- subset(spotify_songs_cleaned, select = c(in_spotify_playlists, in_apple_playlists, in_apple_charts,
                                                       in_deezer_playlists))
corr_tab_lists <- cor(subset_cor_lists)
corr_tab_lists

subset_cor_modes <- subset(spotify_songs_cleaned, select = c(danceability_., valence_., energy_.,
                                                             acousticness_., liveness_., speechiness_.))
corr_tab_modes <- cor(subset_cor_modes)
corr_tab_modes


library(leaps)

model_selection <- regsubsets(x = streams ~. -released_weekday, data = spotify_songs_cleaned)
summary(model_selection)
plot(model_selection)

