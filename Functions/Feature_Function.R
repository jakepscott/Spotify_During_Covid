library(stringr)
library(tidyverse)
library(Rspotify)
playlist_function <- function(playlist){

  # Cleaning the input ------------------------------------------------------
  if (str_detect(playlist,"spotify")==T) {
    playlist <- str_remove(playlist,"spotify:playlist:")
  }
  tryCatch({
    # Playlist Loading --------------------------------------------------------
    
    Playlist <- getPlaylistSongs("spotify",playlist,token=keys) %>% 
      as_tibble() %>% 
      rename("Artist"=artist)
    
    ids <- Playlist$id 
    
    
    # Getting Features --------------------------------------------------------
    Features <- getFeatures("0nbXyq5TXYPCO7pr3N8S4I",token = keys)
    Features <- Features[-1,]
    
    for(i in 1:length(ids)){
      Features<-rbind(Features,getFeatures(ids[i],token=keys))
    }
    
    Playlist <- left_join(Playlist,Features)
    
    
    # Getting Explicit/non-Explicit and Featured Artist name and id ------------------------------------------------------
    
    ##Download features about the tracks
    track_info <- getTrack("0nbXyq5TXYPCO7pr3N8S4I",token = keys)
    track_info <- track_info[-1,]
    
    for(i in 1:length(ids)){#for each song
      tryCatch({
        track_info<-rbind(track_info,getTrack(ids[i],token=keys))
      }, error=function(e){})
    }
    
    track_info <- track_info %>% rename("id"=track_id) %>% 
      separate(artists_id,into = c("main_id","feat_id"), sep = ";") %>%
      separate(artists, into = c("main_artist", "feat_artist"), sep = ";") %>% 
      dplyr::select(-c(main_artist,popularity,album,album_id,main_id,name,feat_id,feat_artist)) 
    
    Playlist <- left_join(Playlist,track_info)
    
    
    # Ablum Info --------------------------------------------------------------
    
    Album_info <- getAlbumInfo("6eV8O8De1mibErM1XM1tEc",token = keys)
    Album_info <- Album_info[-1,]
    album_ids <- unique(Playlist$album_id)
    
    for(i in 1:length(album_ids)){
      tryCatch({
        Album_info<-rbind(Album_info,getAlbumInfo(album_ids[i],token=keys)[1,])
      }, error=function(e){})
    }
    
    Album_info <- Album_info %>% rename("album_id"=id, "album_popularity"=popularity, "album_release_date"=release_date) %>% 
      dplyr::select(-c(artist,name))
    
    #Fixing the album release date.
    #Some release dates are just a year, using nchar I find these, and correct them by just 2020-01-01, for example
    fixed_rds <- as_tibble(Album_info) %>% select(album_release_date) %>% distinct() %>%
      mutate(album_release_date=as.character(album_release_date), nchar=nchar(album_release_date)) %>% 
      mutate(album_rd_correct=case_when(nchar==10~album_release_date,
                                        nchar==7~paste(album_release_date, "01", sep = "-"),
                                        nchar==4~paste(album_release_date,"01","01",sep = "-"))) %>% 
      select(-nchar) %>% 
      mutate(days_since_release=Sys.Date()-as.Date(album_rd_correct))
    Album_info <- left_join(Album_info,fixed_rds)
    
    Playlist <- left_join(Playlist,Album_info)
    
    
    # Artist Info -------------------------------------------------------------
    ##Getting each artist just one time to save time
    unique_artists <- unique(Playlist$artist_id)
    
    ##Just use a for loop, pretty standard
    #These just initialize the tibble I will be binding my newly created rows to in the for loop
    Artist_info <- getArtist("757aE44tKEUQEqRuT6GnEB",token = keys)
    Artist_info <- Artist_info[-1,]
    
    
    for(i in 1:length(unique_artists)){#for each song
      tryCatch({
        Artist_info<-rbind(Artist_info,getArtist(unique_artists[i],token=keys))
      }, error=function(e){})
    }
    
    Artist_info <- Artist_info %>% rename("artist_id"=id,"Artist"=name, "Artist_Popularity"=popularity)
    Playlist <- left_join(Playlist,Artist_info)
    
    
    
    # Joining with Gender of Artist -------------------------------------------
    genders <- read_rds("Data/artist_genders.rds")
    Playlist <- left_join(Playlist,genders)
    
    # Ridding of ID vars ------------------------------------------------------
    Playlist <- Playlist %>% select(-c(id,artist_full,artist_id,album_id,uri,analysis_url))
    
    
    # Making Some Dummy Variables for album type and tempo --------------------
    ##Making a dummy that says whether song is from an album or a single/other
    Playlist <- Playlist %>% mutate(album_dummy=if_else(album_type=="album",TRUE,FALSE)) 
    
    ##Making a time signature dummy that is 1 if time signature is 4 zero otherwise
    Playlist <- Playlist %>% mutate(time_signature_dummy=if_else(time_signature==4,1,0)) 
    
    Playlist
  }, error=function(e){})
}

