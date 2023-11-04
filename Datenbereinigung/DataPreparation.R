

################################# Datenbereinigung ###################################


spotify_songs <- read.csv("spotify-2023.csv", encoding="latin1") # dataframe mit csv von kaggle
spotify_songs_cleaned <- data.frame() # leeres Dataframe, in welchem die "bereinigten" Prädiktoren abgelegt werden


summary(spotify_songs) # Summary des Datasets
str(spotify_songs) # Struktur des Datasets

sapply(spotify_songs, function(x) sum(is.nan(x))) # gibt Spaltenweise Anzahl NaN's zurück -> keine
sapply(spotify_songs, function(x) sum(is.na(x))) # gibt Spaltenweise Anzahl Na's zurück -> keine
sapply(spotify_songs, function(x) sum(x == "")) # gibt Spaltenweise Anzahl leerer Strings zurück zurück -> key = 95
sapply(spotify_songs, function(x) grep("ï¿½", x)) # gibt Spaltenweise Indices der Einträge mit fehlerhaftem Encoding zurück zurück -> track_name und artist.s._name 

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

## streams -> ZIELVARIABLE! numerisch konvertieren
spotify_songs$streams <- as.numeric(spotify_songs$streams)
sum(is.na(spotify_songs$streams)) # missings erkennen -> 1
spotify_songs <-spotify_songs[-575,] # wenn Index bekannt so löschen
sum(is.na(spotify_songs$streams)) # missings erkennen -> 0

# streams dem "cleanded" dataframe hinzufügen
spotify_songs_cleaned <- data.frame(streams = spotify_songs$streams)

## track_name -> drop, da aufwändig um numerisch zu verwenden und encoding von ausländischen Artisten "verhauen"
indices_with_encoding_errors <-grep("ï¿½", spotify_songs$track_name)
values_with_encoding_errors <- spotify_songs$track_name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (58 Outputs)
spotify_songs$track_name <- NULL # erst entfernen, wenn artist.s._name cleaned ist


## artist.s._name -> vorerst behalten und versuchen zu bereinigung (encoding fixen)
# mittels web scraping versuchen die Streams pro artist pro Monat zu ermitteln; sind in einem track_name 
# mehrere artisten beteiligt -> Mittelwert verwenden
spotify_songs$artist.s._name
sum(is.na(spotify_songs$artist.s._name)) # missings erkennen -> 0
Encoding(spotify_songs$artist.s._name) # viele "unknowns" -> encoding kann nicht angepasst werden -> manuelle Korrektur

indices_with_encoding_errors <-grep("ï¿½", spotify_songs$artist.s._name)
values_with_encoding_errors <- spotify_songs$artist.s._name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (40 Outputs)

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


indices_with_encoding_errors <-grep("ï¿½", spotify_songs$artist.s._name)
values_with_encoding_errors <- spotify_songs$artist.s._name[indices_with_encoding_errors]
values_with_encoding_errors # alle spotify_songs$artist.s._name mit encoding errors (0 outputs verbleibend)

#Falls nach cleaning neues csv benötigt wird:

# write.csv(spotify_songs, "spotify-2023_cleaned.csv", row.names = FALSE)

## artist_count -> belassen, keine missings und bereits numerisch
spotify_songs$artist_count
sum(is.na(spotify_songs$artist_count)) # missings erkennen -> 0
barplot(table(spotify_songs$artist_count), main="Barplot artist_count", xlab="artist_count", ylab="Frequency")

# artist_count dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["artist_count"] <- data.frame(artist_count = spotify_songs$artist_count)

# released_day -> z. B. Datum zusammenfügen und den Wochentag daraus extrahieren 
# ???? Ansatz unten: Released Datum aus year, month und day zusammensetzen; weekday eruieren und numerisch konvertieren
spotify_songs$released_date <- as.Date(paste(spotify_songs$released_year, spotify_songs$released_month, spotify_songs$released_day, sep="-"), format="%Y-%m-%d")
spotify_songs$released_weekday <- weekdays(spotify_songs$released_date)
spotify_songs$released_weekday
day_mapping <- c("Montag" = 1, "Dienstag" = 2, "Mittwoch" = 3, "Donnerstag" = 4, "Freitag" = 5, "Samstag" = 6, "Sonntag" = 7)
spotify_songs$released_weekday <- as.numeric(day_mapping[spotify_songs$released_weekday])
spotify_songs$released_weekday
sum(is.na(spotify_songs$released_weekday)) # missings erkennen -> 0

# plot
barplot(table(spotify_songs$released_weekday), main="Barplot weekday", xlab="weekday", ylab="Frequency")
# spotify_songs$released_date <- NULL # -> zusammengesetztes release_date wieder entfernen

# released_weekday dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["released_weekday"] <- data.frame(released_weekday = spotify_songs$released_weekday)

## released_year -> umwandeln in numerischen Wert (2023 - released_year) -> Wie lange gibt es den Song schon
spotify_songs$years_since_release <- 2023 - spotify_songs$released_year
sum(is.na(spotify_songs$years_since_release)) # missings erkennen -> 0
spotify_songs$years_since_release
hist(spotify_songs$years_since_release)

# years_since_release dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["years_since_release"] <- data.frame(years_since_release = spotify_songs$years_since_release)


## released_month -> belasssen bereits numerisch und keine missings
spotify_songs$released_month
sum(is.na(spotify_songs$released_month)) # missings erkennen -> 0
barplot(table(spotify_songs$released_month), main="Barplot released_month", xlab="Month", ylab="Frequency")

# released_month dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["released_month"] <- data.frame(released_month = spotify_songs$released_month)


## in_spotify_playlists -> belassen
spotify_songs$in_spotify_playlists
sum(is.na(spotify_songs$in_spotify_playlists)) # missings erkennen -> 0
hist(spotify_songs$in_spotify_playlists) # rechtsschief -> Transformierung mittels Log() sinnvoll?

hist(log(spotify_songs$in_spotify_playlists))
qqnorm(log(spotify_songs$in_spotify_playlists))
qqline(log(spotify_songs$in_spotify_playlists), col = "red")

# log(in_spotify_playlists) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_spotify_playlists"] <- data.frame(in_spotify_playlists = log(spotify_songs$in_spotify_playlists))

# in_spotify_charts -> vorerst belassen (Recherche, was es genau ist)
spotify_songs$in_spotify_charts
sum(is.na(spotify_songs$in_spotify_charts)) # missings erkennen -> 0
hist(spotify_songs$in_spotify_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs$in_spotify_charts))

# log(in_spotify_charts) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_spotify_charts"] <- data.frame(in_spotify_charts = log(spotify_songs$in_spotify_charts))

## in_apple_playlists -> belassen und verwenden
spotify_songs$in_apple_playlists
sum(is.na(spotify_songs$in_apple_playlists)) # missings erkennen -> 0
hist(spotify_songs$in_apple_playlists) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs$in_apple_playlists))

# log(in_apple_playlists) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_apple_playlists"] <- data.frame(in_apple_playlists = log(spotify_songs$in_apple_playlists))


## in_apple_charts -> belassen und verwenden
spotify_songs$in_apple_charts
sum(is.na(spotify_songs$in_apple_charts)) # missings erkennen -> 0
hist(spotify_songs$in_apple_charts)

# in_apple_charts dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_apple_charts"] <- data.frame(in_apple_charts = spotify_songs$in_apple_charts)

## in_deezer_playlists -> numerisch konvertieren und verwenden
sum(is.na(spotify_songs$in_deezer_playlists)) # missings erkennen -> 0
spotify_songs$in_deezer_playlists # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs$in_deezer_playlists <- gsub(",", "", spotify_songs$in_deezer_playlists) # ersetzt "," durch ""
spotify_songs$in_deezer_playlists <- as.numeric(spotify_songs$in_deezer_playlists)
spotify_songs$in_deezer_playlists[is.na(spotify_songs$in_deezer_playlists)] # keine NA's mehr

hist(spotify_songs$in_deezer_playlists) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs$in_deezer_playlists))

# log(in_deezer_playlists) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_deezer_playlists"] <- data.frame(in_deezer_playlists = log(spotify_songs$in_deezer_playlists))


## in_deezer_charts -> belassen und verwenden
spotify_songs$in_deezer_charts
sum(is.na(spotify_songs$in_deezer_charts)) # missings erkennen -> 0
hist(spotify_songs$in_deezer_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs$in_deezer_charts))

# log(in_deezer_charts) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_deezer_charts"] <- data.frame(in_deezer_charts = log(spotify_songs$in_deezer_charts))

## in_shazam_charts -> numerisch konvertieren und verwenden; missings
spotify_songs$in_shazam_charts # -> enthält Werte > 1000, welche jedoch als z. B. 1,959 erfasst wurden
spotify_songs$in_shazam_charts <- gsub(",", "", spotify_songs$in_shazam_charts)
spotify_songs$in_shazam_charts <- as.numeric(spotify_songs$in_shazam_charts)
spotify_songs$in_shazam_charts[is.na(spotify_songs$in_shazam_charts)] # viele NA's
sum(is.na(spotify_songs$in_shazam_charts))  # missings erkennen -> 50
# Wie NA's handeln??? unten Variante mit median
spotify_songs$in_shazam_charts[is.na(spotify_songs$in_shazam_charts)] <-round(median(spotify_songs$in_shazam_charts, na.rm = TRUE))
hist(spotify_songs$in_shazam_charts) # rechtsschief -> Transformierung mittels Log() sinnvoll?
hist(log(spotify_songs$in_shazam_charts))

# log(in_shazam_charts) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["in_shazam_charts"] <- data.frame(in_shazam_charts = log(spotify_songs$in_shazam_charts))


## bpm -> verwenden und belassen
spotify_songs$bpm
sum(is.na(spotify_songs$bpm))  # missings erkennen -> 0
hist(spotify_songs$bpm)
qqnorm(spotify_songs$bpm)
qqline(spotify_songs$bpm, col = "red")

# bpm dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["bpm"] <- data.frame(bpm = spotify_songs$bpm)


## key -> numerisch konvertieren (encoden) und verwenden
spotify_songs$key # -> enthält leere Strings ""
sum(spotify_songs$key == "")  # 95 leere Strings
#key_value <- as.character(names(sort(table(spotify_songs$key), decreasing=TRUE)[1])) # ermittelt häfigst verwendeter Key
spotify_songs$key[spotify_songs$key == ""] <- "Keine Angabe" # -> setzt den ermittelten key für den leeren String ein

factor_column_key <- as.factor(spotify_songs$key) # faktorisiert den key
levels(factor_column_key) # 1 - 11 : "A"  "A#" "B"  "C#" "D"  "D#" "E"  "F"  "F#" "G"  "G#" "Keine Angabe"
spotify_songs$key <- as.factor(spotify_songs$key)
barplot(table(spotify_songs$key), main="Barplot key", xlab="Key", ylab="Frequency")
spotify_songs$key <- as.numeric(spotify_songs$key)

# key dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["key"] <- data.frame(key = spotify_songs$key)


## mode -> numerisch konvertieren (encoden) und verwenden
spotify_songs$mode
sum(is.na(spotify_songs$mode))  # missings erkennen -> 0
spotify_songs$mode <-ifelse(spotify_songs$mode == "Minor", 1, 0)
barplot(table(spotify_songs$mode), main="Barplot mode", xlab="Mode", ylab="Frequency", names.arg=c("Major", "Minor"), col=c("blue", "red"))


# mode dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["mode"] <- data.frame(mode = spotify_songs$mode)


## danceability_. -> verwenden und belassen
spotify_songs$danceability_.
sum(is.na(spotify_songs$danceability_.))  # missings erkennen -> 0
hist(spotify_songs$danceability_.)
qqnorm(spotify_songs$danceability_.)
qqline(spotify_songs$danceability_., col = "red")

# danceability_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["danceability_."] <- data.frame(danceability_. = spotify_songs$danceability_.)


## valence_. -> Recherche was es ist. ggf. verwenden und belassen
spotify_songs$valence_.
sum(is.na(spotify_songs$valence_.))  # missings erkennen -> 0
hist(spotify_songs$valence_.)
qqnorm(spotify_songs$valence_.)
qqline(spotify_songs$valence_., col = "red")


# valence_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["valence_."] <- data.frame(valence_. = spotify_songs$valence_.)

## energy_. -> verwenden und belassen
spotify_songs$energy_.
sum(is.na(spotify_songs$energy_.))  # missings erkennen -> 0
hist(spotify_songs$energy_.)
qqnorm(spotify_songs$energy_.)
qqline(spotify_songs$energy_., col = "red")

# energy_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["energy_."] <- data.frame(energy_. = spotify_songs$energy_.)


## acousticness_. -> verwenden und belassen
spotify_songs$acousticness_.
sum(is.na(spotify_songs$acousticness_.))  # missings erkennen -> 0
hist(spotify_songs$acousticness_.)
qqnorm(spotify_songs$acousticness_.)
qqline(spotify_songs$acousticness_., col = "red")

# acousticness_. dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["acousticness_."] <- data.frame(acousticness_. = spotify_songs$acousticness_.)


## instrumentalness_.-> evtl. verwerfen, da praktisch alle Werte = 0
spotify_songs$instrumentalness_.
sum(is.na(spotify_songs$instrumentalness_.))  # missings erkennen -> 0
sum(spotify_songs$instrumentalness_.== 0) # 865 Einträge mit dem Wert 0!!


## liveness_. -> verwenden und belassen
spotify_songs$liveness_.
sum(is.na(spotify_songs$liveness_.))  # missings erkennen -> 0
hist(spotify_songs$liveness_.) # rechtsschief -> Transformierung mittels Log() sinnvoll
hist(log(spotify_songs$liveness_.))
qqnorm(log(spotify_songs$liveness_.))
qqline(log(spotify_songs$liveness_.), col = "red")

# log(liveness_.) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["liveness_."] <- data.frame(liveness_. = log(spotify_songs$liveness_.))


## speechiness_. -> verwenden und belassen
spotify_songs$speechiness_.
sum(is.na(spotify_songs$speechiness_.))  # missings erkennen -> 0
hist(spotify_songs$speechiness_.) # rechtsschief -> Transformierung mittels Log() sinnvoll
hist(log(spotify_songs$speechiness_.))
qqnorm(log(spotify_songs$speechiness_.))
qqline(log(spotify_songs$speechiness_.), col = "red")


# log(speechiness_.) dem "cleaned dataframe" hinzufügen
spotify_songs_cleaned["speechiness_."] <- data.frame(speechiness_. = log(spotify_songs$speechiness_.))


#### 3 Alle Werte "messbar" machen ######

# integriert in 2.

#### 4 Datentypen in R auf Korrektheit prüfen #####



###### Plausibilisierung #######

#### 1 Verteilung, Symmetrie, Ausreisser pro Prädiktor plotten und beschreiben #### 

spotify_songs$artist.s._name <- NULL # ACHTUNG: sobald artist.s._name numerisch , diese Zeile nicht mehr verwenden!


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
lapply(names(spotify_songs_cleaned), create_plots, spotify_songs_cleaned)

#### 2 Alle Werte, wo notwendig transformieren #####

# Transformierungen anhand Plots in Betracht ziehen (Achtung durch Transformierungen kann die Interpretierbarkeit schwieriger werden)

# Mögliche Transformierungen:

# released_year -> Log / Sqrt    
# in_spotify_playlists -> Log / Sqrt 
# in_spotify_charts -> Log / Sqrt 
# streams  -> Log / Sqrt           
# in_apple_playlists -> Log / Sqrt 
# in_deezer_playlists -> Log / Sqrt 
# in_deezer_charts  -> Log / Sqrt 
# in_shazam_charts -> Log / Sqrt    
# acousticness_. -> Log / Sqrt    
# instrumentalness_.-> Log / Sqrt
# liveness_. -> Log / Sqrt  
# speechiness_. -> Log / Sqrt     


#### 3 Fehlende Variablen beschreiben #####

# Hat es fehlende Werte? 

# Wenn Dataset bereit für Modellbildung:

write.csv(spotify_songs_cleaned, "spotify-2023_cleaned.csv", row.names = FALSE)
