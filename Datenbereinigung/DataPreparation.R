

################################# Datenbereinigung ###################################


spotify_songs <- read.csv("spotify-2023.csv", encoding="latin1")
summary(spotify_songs) # Summary des Datasets
str(spotify_songs) # Struktur des Datasets

sort(unique(spotify_songs$speechiness_.), na.last=TRUE) # sortiert alle unterschiedlichen Werte und hängt NaN's am Schluss an
sum(is.na(spotify_songs$artist.s._name)) # missings erkennen


#### 1. Inkonsistente, Fehlerhafte, Missing Daten dokumentieren ####

# track_name -> -> drop, da aufwändig um numerisch zu verwenden und encoding von ausländischen Artisten "verhauen"
# artist.s._name -> vorerst behalten und versuchen zu bereinigung (encoding fixen)
# artist_count -> belassen, keine missings und bereits numersisch
# released_year -> umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon
# released_month -> belasssen bereits numerisch und keine missings
# released_day -> vorerst belassen (falls möglich weekend eruieren)
# in_spotify_playlists -> belassen
# in_apple_charts -> vorerst belassen (Recherche, was es genau ist)
# streams -> ZIELVARIABLE! numerisch konvertieren
# in_apple_playlists -> belassen und verwenden
# in_apple_charts -> belassen und verwenden
# in_deezer_playlists -> numerisch konvertieren und verwenden
# in_deezer_charts -> belassen und verwenden
# in_shazam_charts -> numerisch konvertieren und verwenden; missings
# bpm -> verwenden und belassen
# key -> numerisch konvertieren (encoden) und verwenden
# mode -> numerisch konvertieren (encoden) und verwenden
# danceability_. -> verwenden und belassen
# valence_. -> Recherche was es ist. ggf. verwenden und belassen
# energy_. -> verwenden und belassen
# acousticness_. -> verwenden und belassen
# instrumentalness_.-> evtl. verwerfen, da praktisch alle Werte = 0
# liveness_. -> verwenden und belassen
# speechiness_. -> verwenden und belassen

#### 2. Inkonsistente, Fehlerhafte, Missing Daten bereinigen ####
#### Encoding fixen (z.B. Trackname)

# track_name -> drop, da aufwändig um numerisch zu verwenden und encoding von ausländischen Artisten "verhauen"
spotify_songs$track_name<- NULL # erst entfernen, wenn artist.s._name cleaned ist


# artist.s._name -> vorerst behalten und versuchen zu bereinigung (encoding fixen)
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

factor_column_artist.s._name <- as.factor(spotify_songs$artist.s._name)
levels(factor_column_artist.s._name)
spotify_songs$artist.s._name <- as.numeric(as.factor(spotify_songs$artist.s._name))



# artist_count -> belassen, keine missings und bereits numerisch
# belassen


# released_day -> vorerst belassen (falls möglich weekend eruieren)
# ???? Ansatz unten: Released Datum aus year, month und day zusammensetzen; weekday eruieren und numerisch konvertieren

spotify_songs$released_date <- as.Date(paste(spotify_songs$released_year, spotify_songs$released_month, spotify_songs$released_day, sep="-"), format="%Y-%m-%d")
spotify_songs$weekday <- weekdays(spotify_songs$released_date)
day_mapping <- c("Montag" = 1, "Dienstag" = 2, "Mittwoch" = 3, "Donnerstag" = 4, "Freitag" = 5, "Samstag" = 6, "Sonntag" = 7)
spotify_songs$weekday <- as.numeric(day_mapping[spotify_songs$weekday])



spotify_songs$released_date <- NULL


# released_year -> umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon
spotify_songs$released_year <- 2023 - spotify_songs$released_year


# released_month -> belasssen bereits numerisch und keine missings
# belassen

# in_spotify_playlists -> belassen
# belassen


# in_apple_charts -> vorerst belassen (Recherche, was es genau ist)
# ????

# streams -> ZIELVARIABLE! numerisch konvertieren
spotify_songs$streams <- as.numeric(spotify_songs$streams)
spotify_songs$streams[is.na(spotify_songs$streams)]
spotify_songs <-spotify_songs[-575,] # wenn Index bekannt so löschen

# in_apple_playlists -> belassen und verwenden
# belassen
# in_apple_charts -> belassen und verwenden
# belassen
# in_deezer_playlists -> numerisch konvertieren und verwenden

spotify_songs$in_deezer_playlists <- gsub(",", "", spotify_songs$in_deezer_playlists)
spotify_songs$in_deezer_playlists <- as.numeric(spotify_songs$in_deezer_playlists)
spotify_songs$in_deezer_playlists[is.na(spotify_songs$in_deezer_playlists)] # keine NA's mehr

# in_deezer_charts -> belassen und verwenden
# belassen

# in_shazam_charts -> numerisch konvertieren und verwenden; missings
spotify_songs$in_shazam_charts <- gsub(",", "", spotify_songs$in_shazam_charts)
spotify_songs$in_shazam_charts <- as.numeric(spotify_songs$in_shazam_charts)
spotify_songs$in_shazam_charts[is.na(spotify_songs$in_shazam_charts)] # viele NA's 
# Wie NA's handeln??? unten Variante mit median
spotify_songs$in_shazam_charts[is.na(spotify_songs$in_shazam_charts)] <- median(spotify_songs$in_shazam_charts, na.rm = TRUE) 
spotify_songs$in_shazam_charts <- round(spotify_songs$in_shazam_charts)

# bpm -> verwenden und belassen
# belassen


# key -> numerisch konvertieren (encoden) und verwenden
spotify_songs$key[is.na(spotify_songs$key)]
key_value <- as.character(names(sort(table(spotify_songs$key), decreasing=TRUE)[1]))
spotify_songs$key[spotify_songs$key == ""] <- key_value

factor_column_key <- as.factor(spotify_songs$key)
levels(factor_column_key) # 1 - 11 : "A"  "A#" "B"  "C#" "D"  "D#" "E"  "F"  "F#" "G"  "G#"
spotify_songs$key <- as.numeric(as.factor(spotify_songs$key))


# mode -> numerisch konvertieren (encoden) und verwenden
mode_value <- as.character(names(sort(table(spotify_songs$mode), decreasing=TRUE)[1]))
spotify_songs$mode[spotify_songs$mode == ""] <- mode_value
factor_column_mode <- as.factor(spotify_songs$mode)
levels(factor_column_mode) # 1 = "Major"; 2 = "Minor"
spotify_songs$mode <- as.numeric(as.factor(spotify_songs$mode))
# danceability_. -> verwenden und belassen
# belassen


# valence_. -> Recherche was es ist. ggf. verwenden und belassen
#????


# energy_. -> verwenden und belassen
# belassen

# acousticness_. -> verwenden und belassen
# belassen

# instrumentalness_.-> evtl. verwerfen, da praktisch alle Werte = 0
# ???

# liveness_. -> verwenden und belassen
# belassen


# speechiness_. -> verwenden und belassen
# belassen

sum(is.na(spotify_songs)) # prüfen ob noch NA's vorhanden
na_per_column <- colSums(is.na(spotify_songs))
na_per_column # 1 NA von artist.s._name; jener, der auf NA gesetzt wurde
spotify_songs <- na.omit(spotify_songs)
sum(is.na(spotify_songs))

#### 3 Alle Werte "messbar" machen, transformieren #####

#### 4 Alle Werte, wo notwendig transformieren #####


# Datentypen in R auf Korrektheit prüfen

###### Plausibilisierung #######

#1 Verteilung, Symetrie, Ausreisser pro Prädiktor plotten und beschreiben

library(ggplot2)

# Plots von spotify_songs$artist_count
ggplot(spotify_songs, aes(x=artist_count)) + geom_histogram() + ggtitle('Histogram of artist_count')
#ggplot(spotify_songs, aes(y=artist_count)) + geom_boxplot() + ggtitle('Boxplot of artist_count')
ggplot(spotify_songs, aes(x=artist_count)) + geom_bar() + ggtitle('Bar plot of artist_count')


# Plots von spotify_songs$released_year
ggplot(spotify_songs, aes(x=released_year)) + geom_histogram() + ggtitle('Histogram of released_year')
ggplot(spotify_songs, aes(y=released_year)) + geom_boxplot() + ggtitle('Boxplot of released_year')
ggplot(spotify_songs, aes(x=released_year)) + geom_bar() + ggtitle('Bar plot of released_year')

# Plots von spotify_songs$released_month
ggplot(spotify_songs, aes(x=released_month)) + geom_histogram() + ggtitle('Histogram of released_month')
ggplot(spotify_songs, aes(y=released_month)) + geom_boxplot() + ggtitle('Boxplot of released_month')
ggplot(spotify_songs, aes(x=released_month)) + geom_bar() + ggtitle('Bar plot of released_month')


# Plots von spotify_songs$weekday
ggplot(spotify_songs, aes(y=weekday)) + geom_boxplot() + ggtitle('Boxplot of weekday')
ggplot(spotify_songs, aes(x=weekday)) + geom_bar() + ggtitle('Bar plot of weekday')


# Plots von spotify_songs$key
ggplot(spotify_songs, aes(y=key)) + geom_boxplot() + ggtitle('Boxplot of key')
ggplot(spotify_songs, aes(x=key)) + geom_bar() + ggtitle('Bar plot of key')


# Plots von spotify_songs$mode
ggplot(spotify_songs, aes(x=mode)) + geom_bar() + ggtitle('Bar plot of mode')




#2 Fehlende Variablen beschreiben