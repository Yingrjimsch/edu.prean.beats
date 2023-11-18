
load("./edu.prean.beats/Datenbereinigung/spotify_songs_cleaned_without_trans.RData")

print("-------WITH TRANSFORMATION------")
print(head(spotify_songs_cleaned_with_trans))

print("-------WITHOUT TRANSFORMATION------")
print(head(spotify_songs_cleaned_without_trans))