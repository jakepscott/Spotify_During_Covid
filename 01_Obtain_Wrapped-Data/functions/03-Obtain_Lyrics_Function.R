# loading libs------------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(lubridate)
library(tidytext)
library(genius)
library(stringr)
library(rvest)
library(remotes)
library(stringi)


Lyric_Generation_Function <- function(track_data){
  #only works if there is at least 1 song in the track_data object
  if(nrow(track_data)==0){
    return("Sorry, no tracks in this input!")
  }
  
  
  # Lyrics Loading --------------------------------------------------------
  Lyrics <- track_data %>% select(Id,Song,Artist,Album) %>% distinct()
  
  ##Throwing the kitchen sink to try to get it to work- this time ridding of non-english letters/accents
  Lyrics <- Lyrics %>% mutate(song2=Song) %>% 
    separate(col = song2, into = c("song2", "extra"), sep = " [(]") %>%
    select(-extra) %>% 
    separate(song2, into = c("song2", "extra"), sep = " -") %>% select(-extra) %>% 
    separate(song2, into = c("song2", "extra"), sep = "-") %>% select(-extra) %>%
    mutate(song2=str_remove_all(string = song2, pattern = "[[:punct:]]"),
           artist2=str_remove_all(string = Artist, pattern = "[[:punct:]]")) %>%
    mutate(song2= stri_trans_general(str = song2, id = "Latin-ASCII"),
           artist2= stri_trans_general(str = artist2, id = "Latin-ASCII"))
  
  #This function inputs and artist and song and outputs a single character string of the lyrics of the song
  obtain_lyrics <- function(artist,song) {
    genius_lyrics(artist = artist,
                  song = song,
                  info = "simple") %>% 
      dplyr::select(lyric) %>% 
      unnest_tokens(output = word,input = lyric,token = "words") %>% 
      pull(word)  %>% 
      paste(collapse = " ")
  }
  
  #This is making the function "safe". If obtain_Lyrics results in an error, pmap will put the number 1
  #See: https://aosmith.rbind.io/2020/08/31/handling-errors/ for an explanation
  obtain_lyrics_safe=possibly(.f = obtain_lyrics,otherwise = "1")
  
  #Planning for parallelization
  plan(multiprocess)
  
  #For each row, grab the artist and song, then make a column where the entry is a character string of the lyrics
  Lyrics <- Lyrics %>% 
    mutate(full_lyrics=future_pmap_chr(list(artist2, song2), ~obtain_lyrics_safe(..1, ..2),.progress = T)) 
  
  missed <- Lyrics %>% filter(full_lyrics=="1")
  percent_captured <- round((nrow(Lyrics)-nrow(missed))/nrow(Lyrics)*100,digits = 1)
  cat("I managed to get lyrics for", percent_captured, "percent of songs!") 
  Lyrics <- Lyrics %>% select(-c(song2,artist2))
}
  