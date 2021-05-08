# loading libs and key------------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(lubridate)
library(tidytext)
library(geniusr)
library(stringr)
library(rvest)
library(remotes)
library(genius)
library(stringi)

source("Spotify_Key.R")

Lyric_Function <- function(playlist,attempts=1){
  
  # Cleaning the input ------------------------------------------------------
  if (str_detect(playlist,"spotify")==T) {
    playlist <- str_remove(playlist,"spotify:playlist:")
  }
  
  # Playlist Loading --------------------------------------------------------
  Lyrics <- getPlaylistSongs("spotify",playlist,token=keys) %>% 
    as_tibble() %>% select(-popularity) %>% distinct(tracks, artist)
  
  # Getting lyrics from Genius using the genius::genius_lyrics function --------
  
  #Attempt 1: Using full track names
  cat("Attempt 1 ")
  Lyrics <- Lyrics %>% mutate(lyrics=1)
  for (i in 1:nrow(Lyrics)) {
    tryCatch({
      print(i)
      Lyrics$lyrics[i] <- genius_lyrics(artist = Lyrics$artist[i],
                                        song = Lyrics$tracks[i],
                                        info = "simple") %>% 
        dplyr::select(lyric)
    }, error=function(e){print(e)}
    )
  }
  
  #Checking the ones that I missed
  missed <- Lyrics[1,]
  missed <- missed[-1,]
  
  for (i in 1:nrow(Lyrics)) {
    print(i)
    if (!is.character(Lyrics$lyrics[[i]])) {
      missed[i,] <- Lyrics[i,]
    } else {
      missed[i,] <- NA
    }
  }
  
  missed <- missed %>% filter(!is.na(tracks))
  
  percent_captured <- round((nrow(Lyrics)-nrow(missed))/nrow(Lyrics)*100,digits = 1)
  cat("I managed to get lyrics for", percent_captured, "percent of songs in attempt 1!")
  
  if (attempts>1 & nrow(missed)!=0) {
    cat("Attempt 2 ")
    # Attempt 2: Simplifying the Track Names -----------------------------------
    #Simplifying track names by ridding of features
    missed <- missed %>% mutate(Track_Name2=tracks) %>% 
      separate(col = Track_Name2, into = c("tracks2", "extra"), sep = " [(]") %>%
      select(-extra) 
    
    attempt2 <- missed
    ##Trying to get lyrics using simplified names, resulting in attempt2
    for (i in 1:nrow(missed)) {
      tryCatch({
        print(i)
        attempt2$lyrics[i] <- genius_lyrics(artist = missed$artist[i],
                                            song = missed$tracks2[i],
                                            info = "simple") %>% 
          dplyr::select(lyric)
      }, error=function(e){print(e)}
      )
    }
    attempt2
    
    ##Figuring out which songs I did not catch the lyrics of in attempt2
    missed2 <- missed[1,]
    missed2 <- missed2[-1,]
    
    for (i in 1:nrow(attempt2)) {
      print(i)
      if (!is.character(attempt2$lyrics[[i]])) {
        missed2[i,] <- attempt2[i,]
      }  else {
        missed2[i,] <- NA
      }
    }
    
    missed2 <- missed2 %>% filter(!is.na(tracks))
    percent_captured <- round((nrow(Lyrics)-nrow(missed2))/nrow(Lyrics)*100,digits = 1)
    cat("I managed to get lyrics for", percent_captured, "percent of songs in attempt 2!")
    
  } 
  if (attempts>2 & nrow(missed)!=0 & nrow(missed2)!=0) {
    cat("Attempt 3 ")
    # Attempt 3: Removing Punctuation --------------------------------------------------------------
    ##3rd attempt to get all lyrics, this time removing punctuation from artist name and track name
    attempt3 <- missed2 %>% mutate(tracks3=str_remove_all(string = tracks2, pattern = "[[:punct:]]"),
                                   artist2=str_remove_all(string = artist, pattern = "[[:punct:]]"))
    
    ##Removing the + sign, replacing with hyphen
    attempt3 <- attempt3 %>% mutate(artist2=str_replace(artist2, pattern = " //+ ", replacement = "-"))
    
    
    ##Tring to catch the songs I didn't before, in attempt3
    
    for (i in 1:nrow(attempt3)) {
      tryCatch({
        print(i)
        attempt3$lyrics[i] <- genius_lyrics(artist = attempt3$artist2[i],
                                            song = attempt3$tracks3[i],
                                            info = "simple") %>% 
          dplyr::select(lyric)
      }, error=function(e){print(e)}
      )
    }
    
    ##Figuring out which songs I did not catch the lyrics of in attempt3
    missed3 <- attempt3[1,]
    missed3 <- missed3[-1,]
    
    for (i in 1:nrow(attempt3)) {
      print(i)
      if (!is.character(attempt3$lyrics[[i]])) {
        missed3[i,] <- attempt3[i,]
      }   else {
        missed3[i,] <- NA
      }
    }
    
    missed3 <- missed3%>% filter(!is.na(tracks))
    percent_captured <- round((nrow(Lyrics)-nrow(missed3))/nrow(Lyrics)*100,digits = 1)
    cat("I managed to get lyrics for", percent_captured, "percent of songs in attempt 3!")
  }
  
  if (attempts>3 & nrow(missed)!=0 & nrow(missed2)!=0 & nrow(missed3)!=0) {
    cat("Attempt 4 ")
    # Attempt 4. Simplifying a lot- parentheses, hypens, punctuation --------
    ##Making a fourth attempt
    attempt4 <- missed3 %>% select(tracks, artist, lyrics)
    
    ##Throwing the kitchen sink to try to get it to work- this time ridding of non-english letters/accents
    attempt4 <- attempt4 %>% mutate(tracks4=tracks) %>% 
      separate(col = tracks4, into = c("tracks4", "extra"), sep = " [(]") %>%
      select(-extra) %>% 
      separate(tracks4, into = c("tracks4", "extra"), sep = " -") %>% select(-extra) %>% 
      separate(tracks4, into = c("tracks4", "extra"), sep = "-") %>% select(-extra) %>%
      mutate(tracks4=str_remove_all(string = tracks4, pattern = "[[:punct:]]"),
             artist2=str_remove_all(string = artist, pattern = "[[:punct:]]")) %>%
      mutate(tracks4= stri_trans_general(str = tracks4, id = "Latin-ASCII"),
             artist2= stri_trans_general(str = artist2, id = "Latin-ASCII"))
    
    
    ##Trying to get lyrics using simplified names, resulting in attempt4
    for (i in 1:nrow(attempt4)) {
      tryCatch({
        print(i)
        attempt4$lyrics[i] <- genius_lyrics(artist = attempt4$artist2[i],
                                            song = attempt4$tracks4[i],
                                            info = "simple") %>% 
          dplyr::select(lyric)
      }, error=function(e){print(e)}
      )
    }
    
    missed4 <- attempt4[1,]
    missed4 <- missed4[-1,]
    for (i in 1:nrow(attempt4)) {
      print(i)
      if (!is.character(attempt4$lyrics[[i]])) {
        missed4[i,] <- attempt4[i,]
      }   else {
        missed4[i,] <- NA
      }
    }
    
    missed4 <- missed4 %>% filter(!is.na(tracks))
    percent_captured <- round((nrow(Lyrics)-nrow(missed4))/nrow(Lyrics)*100,digits = 1)
    cat("I managed to get lyrics for", percent_captured, "percent of songs in attempt 4!")
  }
  
  
  # Joining All the Attempts -------------------------------------------------
  
  
  # Joining if Attempts=1 ---------------------------------------------------
  
  
  if(attempts==1) {
    return(Lyrics)
  }
  
  # Joining if Attempts=2 ---------------------------------------------------
  
  if(attempts==2) {
    attempt2 <- attempt2 %>% select(-tracks2)
    Lyrics <- left_join(Lyrics,attempt2, by=c("tracks","artist")) %>% mutate(lyrics=lyrics.x)
    
    ##Combining lyrics..x with lyrics.y
    for (i in 1:nrow(Lyrics)) {
      if (is.double(Lyrics$lyrics.x[[i]])) {
        Lyrics$lyrics[i] <- Lyrics$lyrics.y[i]
      }
    }
    
    Lyrics %>% select(tracks,artist,lyrics)
  }
  
  # Joining if Attempts=3 ---------------------------------------------------
  
  if(attempts==3) {
    ##Joining Attempt 2
    attempt2 <- attempt2 %>% select(-tracks2)
    Lyrics <- left_join(Lyrics,attempt2, by=c("tracks","artist")) %>% mutate(lyrics=lyrics.x)
    
    ##Combining lyrics..x with lyrics.y
    for (i in 1:nrow(Lyrics)) {
      if (is.double(Lyrics$lyrics.x[[i]])) {
        Lyrics$lyrics[i] <- Lyrics$lyrics.y[i]
      }
    }
    Lyrics <- Lyrics %>% select(tracks,artist,lyrics)
    
    ##Joining Attempt 3
    attempt3 <- attempt3 %>% select(tracks,artist,lyrics)
    Lyrics <- left_join(Lyrics,attempt3, by=c("tracks","artist")) %>% mutate(lyrics=lyrics.x)
    
    ##Combining lyrics..x with lyrics.y
    for (i in 1:nrow(Lyrics)) {
      if (is.double(Lyrics$lyrics.x[[i]])) {
        Lyrics$lyrics[i] <- Lyrics$lyrics.y[i]
      }
    }
    Lyrics %>% select(tracks,artist,lyrics)
  }
  
  # Joining if Attempts=4 ---------------------------------------------------
  if(attempts==4) {
    ##Joining Attempt 2
    attempt2 <- attempt2 %>% select(-tracks2)
    Lyrics <- left_join(Lyrics,attempt2, by=c("tracks","artist")) %>% mutate(lyrics=lyrics.x)
    
    ##Combining lyrics..x with lyrics.y
    for (i in 1:nrow(Lyrics)) {
      if (is.double(Lyrics$lyrics.x[[i]])) {
        Lyrics$lyrics[i] <- Lyrics$lyrics.y[i]
      }
    }
    Lyrics <- Lyrics %>% select(tracks,artist,lyrics)
    
    ##Joining Attempt 3
    attempt3 <- attempt3 %>% select(tracks,artist,lyrics)
    Lyrics <- left_join(Lyrics,attempt3, by=c("tracks","artist")) %>% mutate(lyrics=lyrics.x)
    
    ##Combining lyrics..x with lyrics.y
    for (i in 1:nrow(Lyrics)) {
      if (is.double(Lyrics$lyrics.x[[i]])) {
        Lyrics$lyrics[i] <- Lyrics$lyrics.y[i]
      }
    }
    Lyrics <- Lyrics %>% select(tracks,artist,lyrics)
    
    ##Joining Attempt 4
    attempt4 <- attempt4 %>% select(tracks,artist,lyrics)
    Lyrics <- left_join(Lyrics,attempt4, by=c("tracks","artist")) %>% mutate(lyrics=lyrics.x)
    
    ##Combining lyrics..x with lyrics.y
    for (i in 1:nrow(Lyrics)) {
      if (is.double(Lyrics$lyrics.x[[i]])) {
        Lyrics$lyrics[i] <- Lyrics$lyrics.y[i]
      }
    }
    Lyrics <- Lyrics %>% select(tracks,artist,lyrics)
    percent_captured <- round((nrow(Lyrics)-nrow(missed4))/nrow(Lyrics)*100,digits = 1)
  }
  return(Lyrics)
}

