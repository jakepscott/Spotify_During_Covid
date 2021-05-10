# Loading Libraries and Data-------------------------------------------------------
library(tidyverse)
library(here)
library(stringr)
library(scales)
library(tools)
library(ggiraph)
library(glue)
library(ggtext)
library(RVAideMemoire)
library(broom)

raw_data <- read_rds(here("data/Full_Wrapped_Feat_Lyrics_Data.rds"))


# Clean Data --------------------------------------------------------------
data <- raw_data %>% 
  mutate(Year=parse_number(Playlist)) %>% 
  filter(Year>=2019)


# Mood's Median -----------------------------------------------------------
#Grab just relevant columns
cols <- data %>% select(Danceability, Energy, Loudness,Valence:`Duration (Minutes)`,
               total_words,
               trust_percent:anticipation_percent,
               overall_sentiment_corrected)

#Create tibble to bind with
moods_tests <- tibble(p.value=numeric(0), 
       method=character(0),
       variable=character(0))

#Run test on each relevant col and save in moods_tests
for (i in names(cols)) {
  model_data <- data %>% select(i,Year) %>% na.omit()
  to_bind <- mood.medtest(model_data %>% pull(i)~model_data$Year) %>% 
    tidy() %>% mutate(variable=i)
  moods_tests <- moods_tests %>% bind_rows(to_bind)
}
moods_tests


# Prop.Tests --------------------------------------------------------------
#Major
major_data <- data %>% 
  select(Year,Mode) %>% 
  na.omit() %>% 
  mutate(Major=ifelse(Mode=="Major",1,0))

major_data %>% 
  group_by(Year) %>% 
  summarise(Succs=sum(Major),
            n=n())

prop.test(x=c(42,49),n = c(63,89))

Explicit_data <- data %>% 
  select(Year,`Explicit?`) %>% 
  na.omit() %>% 
  mutate(Major=ifelse(`Explicit?`==TRUE,1,0))

Explicit_data %>% 
  group_by(Year) %>% 
  summarise(Succs=sum(Major),
            n=n())

prop.test(x=c(32,52),n = c(58,90))
