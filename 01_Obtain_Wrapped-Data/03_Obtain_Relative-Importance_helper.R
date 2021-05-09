# Load Libraries and Data -------------------------------------------------
library(tidyverse)
library(readr)
library(tidytext)
library(furrr)

#This is the full features and lyrics data
data <- read_rds(here("01_Obtain_Wrapped-Data/data/Full_Wrapped_Feat_Lyrics_Data.rds"))

#Just keep columns I need
Lyrics <- data %>% select(Playlist,Id,Song,full_lyrics)

#Get a row for every word in the data (so now I have playlist-word pairs)
words <- Lyrics %>% 
  unnest_tokens(input = full_lyrics,output = "word",token="words") %>% 
  #Removing stop words
  anti_join(stop_words) %>% 
  #Removing more stop words
  filter(!(word %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) #Could also remove ("ah","em","nah","na","yah")

#Getting the sum of the playlist-word pairs. That way I can just sum rather than use nrow in the function below
playlist_word_sums <- words %>% count(Playlist,word)

#Getting just distinct playlist-word pairs
distinct_words <- words %>% distinct(Playlist,word)

# Function to Calculate Outside Percent -----------------------------------
calc_percent_outside <- function(playlist, curr_word) {
  #Get the number of words outside the album
  total_outside_words <- playlist_word_sums %>%  #*******NOTE: Sadly, this "playlist_word_sums" is something I need to define in the environment:(, see above
    filter(Playlist != playlist) %>% 
    pull(n) %>% 
    sum()
  
  #Get the number of times the given word appears outside the given album
  total_word_of_interest_outside <- playlist_word_sums %>% 
    filter(Playlist!=playlist,
           word==curr_word) %>% 
    pull(n) %>% 
    sum()
  
  prop <- (total_word_of_interest_outside/total_outside_words)*100
  return(prop)
}


# Calculating Outside Percent ---------------------------------------------
#This sets a plan for furrr
plan(multiprocess)

#Calculating and saving outside percent
tictoc::tic()
distinct_words_outside <- distinct_words %>% 
  mutate(percent_outside = future_pmap_dbl(list(Playlist, word), ~calc_percent_outside(..1, ..2),.progress = T)) 
tictoc::toc()

#saveRDS(needed_outside,here("03_Obtain_Top-200-Data/data/full_outside_percent.rds"))

#Joining this outside percent data with the full interesting words data
Relative_Importance <- words %>% 
  left_join(distinct_words_outside,by=c("Playlist","word"))

# Getting Within Album Proportion -----------------------------------------
Relative_Importance <- Relative_Importance %>% 
  group_by(Playlist,word) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  group_by(Playlist) %>% 
  mutate(total_word=n()) %>% 
  ungroup() %>%
  mutate(percent_inside=(n/total_word)*100)



# Getting Difference ------------------------------------------------------
Relative_Importance <- Relative_Importance %>% mutate(difference=percent_inside-percent_outside)


# Saving ------------------------------------------------------------------
#Just keeping distinct playlist word pairs
Relative_Importance <- Relative_Importance %>% select(Playlist,word,difference) %>% distinct()
#Saving so I don't need to run the above for loop each time
saveRDS(Relative_Importance,here("01_Obtain_Wrapped-Data/data/Relative_Importance.rds"))
saveRDS(Relative_Importance,here("data/Relative_Importance.rds"))
