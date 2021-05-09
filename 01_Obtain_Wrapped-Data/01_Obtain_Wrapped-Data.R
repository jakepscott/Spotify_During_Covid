# Loading Libs ------------------------------------------------------------
library(stringr)
library(Rspotify)
library(tidyverse)
library(here)


# Loading Necessary Data and Functions ------------------------------------
load(here("01_Obtain_Wrapped-Data/data/keys"))
source(here("01_Obtain_Wrapped-Data/functions/01-Obtain_Tracks_Function.R"))
source(here("01_Obtain_Wrapped-Data/functions/02-Obtain_Features_Function.R"))
source(here("01_Obtain_Wrapped-Data/functions/03-Obtain_Lyrics_Function.R"))
source(here("01_Obtain_Wrapped-Data/functions/04-Analyze_Lyrics_Function.R"))
source(here("01_Obtain_Wrapped-Data/functions/05-Compare_Playlists_Function.R"))


# Getting Tracks ----------------------------------------------------------
#Get track info for my wrapped playlists
tracks_full <- Tracks_Function(user = "jakerocksalot",playlists=c("Your Top Songs 2017",
                                                                  "Your Top Songs 2018",
                                                                  "Your Top Songs 2019",
                                                                  "Your Top Songs 2020"))   

# Only keeping songs for which I don't already have data from the top 200 analysis  --------
#What I am doing here is the following: I currently have a massive dataset of songs, with all their feature
#information already downloaded. I see if there are any of the "wrapped" songs already in that massive dataset.
#If I do, I put them in "already_have_tracks". There is no sense in me downloading all their data, just a time waste.
#For those songs not in the massive dataset, I put them in "needed_Tracks" and download their song feature info.
#Then at the end I bind the needed_tracks and already_have_tracks and bind them together
Full_Top_200 <- read_rds(here("01_Obtain_Wrapped-Data/data/Popular_Songs_Data.rds")) %>% select(-Playlist)
needed_tracks <- tracks_full %>% anti_join(Full_Top_200,by="Id")
already_have_tracks <- tracks_full %>% semi_join(Full_Top_200,by="Id")

# Getting Features of Wrapped Playlists -----------------------------------
Features <- Features_Function(track_data = needed_tracks)

# Getting Lyrics for Wrapped Songs ----------------------------------------
Lyrics <- Lyric_Generation_Function(needed_tracks)

# *Specific to Me*: Getting some songs manually ---------------------------
source(here("01_Obtain_Wrapped-Data/02_Obtain_Missed-Lyrics-Manually.R"))
#Note: this creates Full_Lyrics object
#Then I can left_join() on the full data after I do the Lyrics analysis
#saveRDS(Full_Lyrics,here("01_Obtain_Wrapped-Data/data/Full_Wrapped_Lyrics.rds"))

# Getting Lyric Features for Wrapped Songs --------------------------------
Lyric_Features <- Lyric_Analysis_Function(Full_Lyrics)
Lyric_Features_To_Join <- Lyric_Features %>% select(!c(Song,Artist,Album))


# Joining the Data All Together -------------------------------------------
Full_Wrapped_Feat_Lyrics_Data <- Features %>% left_join(Lyric_Features_To_Join) %>% distinct(Id,.keep_all = T)

# Joining Full Data for the songs I already had ---------------------------
#So there were X number of songs in the wrapped playlists for which I already had data. I took them
#out of the list so I didn't collect data for them. That'd be slow and redundant. Now that I
#have the data for the songs not in the top 200 list, I need to join those songs for which I just got data
#to the ones I already had in the top 200. 


#Get the song data for the songs in thw wrapped data that are also found in the top 200 data
already_have_tracks <- already_have_tracks %>% select(Id,Playlist) %>% left_join(Full_Top_200,by="Id")

#Bind the song data I got from above to the song data from top 200
#So X songs were in Wrapped and not in top 200. So I get information for them with my functions
#Y songs are in both wrapped and top 200. Instead of puttin those in the function, I just take the info I already have
#In the saved top 200 dataset
Full_Wrapped_Feat_Lyrics_Data <- Full_Wrapped_Feat_Lyrics_Data %>% bind_rows(already_have_tracks)

#saveRDS(Full_Wrapped_Feat_Lyrics_Data,here("data/Full_Wrapped_Feat_Lyrics_Data.rds"))
#saveRDS(Full_Wrapped_Feat_Lyrics_Data,here("01_Obtain_Wrapped-Data/data/Full_Wrapped_Feat_Lyrics_Data.rds"))


# Comparing the Wrapped Playlists -----------------------------------------
Wrapped_Playlist_Data <- Full_Wrapped_Feat_Lyrics_Data %>% Playlist_Comparison_Function(wrapped = T)
#saveRDS(Wrapped_Playlist_Data,here("01_Obtain_Wrapped-Data/data/Wrapped_Playlist_Data.rds"))
#saveRDS(Wrapped_Playlist_Data,here("data/Wrapped_Playlist_Data.rds"))
