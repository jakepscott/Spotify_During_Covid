# Loading Libraries and Data-------------------------------------------------------
library(tidyverse)
library(ggridges)
library(here)
library(patchwork)
library(stringr)
library(scales)
library(tools)
library(ggiraph)
library(glue)

data <- read_rds(here("data/Full_Wrapped_Feat_Lyrics_Data.rds"))
comparison_data <- read_rds(here("data/Wrapped_Playlist_Data.rds"))
comparison_data <- comparison_data %>% rename("Median Loudness (dB)"=`Median Loudness`,
                                              "Median Tempo (BPM)"=`Median Tempo`,
                                              "Percent of Songs That Are Explicit"=`Percent Explicit`)

theme_set(theme_minimal(base_size = 12))


# Overall Distributions ---------------------------------------------------
#Song Feats
data %>% 
  select(Danceability,Energy,Loudness,Valence:`Duration (Minutes)`) %>%
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value") %>% 
  ggplot(aes(Value)) +
  geom_density(fill="lightblue") +
  facet_wrap(~Feature, scales = "free")

#Lyrical Sentiments
lyrical_sent <- data %>% 
  select(trust_percent:overall_sentiment_corrected) %>%
  select(-overall_sentiment) 

names(lyrical_sent) <- names(lyrical_sent) %>% str_replace_all(.,"_"," ") %>% toTitleCase()

lyrical_sent %>% 
  rename("Afinn Sentiment"=`Overall Sentiment Corrected`) +
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value") %>% 
  ggplot(aes(Value)) +
  geom_density(fill="lightblue") +
  facet_wrap(~Feature, scales = "free")
