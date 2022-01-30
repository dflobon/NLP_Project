library(stringr)

# Read the differents CSV in the folder Data
data_artists <- read.csv("./Data/artists-data.csv")
data_lyrics <- read.csv("./Data/lyrics-data.csv")

# Select just the song in english
data_lyrics_eng <- subset(data_lyrics, Idiom == "ENGLISH")

print("Check Numbers NaN")
print("#####################################################")
# Resume data of Artist CSV
columns_artist <- colnames(data_artists)
for (col in columns_artist){
  print(paste0("Number of Nan ",paste(col, sum(is.na(data_artists$col)), sep = ": ")))
}

columns_lyrics <- colnames(data_lyrics_eng)
for (col in columns_lyrics){
  print(paste0("Number of Nan ",paste(col, sum(is.na(data_lyrics_eng$col)), sep = ": ")))
}
print("#####################################################")
print("Check Numbers of lyrics")
print("#####################################################")
n_total_lyrics <- length(data_lyrics_eng$Lyric)
print(paste0("Number total of lyrics: ", n_total_lyrics))
# Delete the Lyrics that are empty
count <- 0
for (i in range(1,length(data_lyrics_eng$Lyric))){
  if (data_lyrics_eng$Lyric[[i]] == ''){
    count <- count + 1
  }
}
print(paste0("Number of lyrics emptys: ", count))
# There arent any Lyrics that are empty
print("#####################################################")
print("Genres")
print("#####################################################")
genres <- unique(data_artists$Genre)
print(genres)
print("#####################################################")

# GENERATE THE CSV THAT JOIN LYRICS WITH ITS GENRE
genre_aux <- NULL
lyrics_eng <- NULL
for (i in seq_len(nrow(data_lyrics_eng))){
    artirst <- data_lyrics_eng[i,1]
    for (j in seq_len(nrow(data_artists))){
        if (artirst == data_artists[j,4]){
          genre_aux <- c(genre_aux, data_artists[j,5])
          lyrics_eng <- c(lyrics_eng, str_replace_all(data_lyrics_eng[i,4], "\\[[a-zA-Z0-9/:, '\\(\\)]+\\]", ""))
          break
        }
    }
}

new_dataframe <- data.frame("lyrics" = lyrics_eng, "genre" = genre_aux)
write.csv(new_dataframe, "./Data/lyrics-genre.csv")






