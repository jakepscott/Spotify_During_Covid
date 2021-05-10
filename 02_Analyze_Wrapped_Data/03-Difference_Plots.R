# Loading Libraries and Data-------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)
library(stringr)
library(scales)
library(tools)
library(ggiraph)
library(ggtext)
library(ggiraph)
library(glue)
windowsFonts(`Roboto`=windowsFont("Roboto Condensed"))
theme_set(theme_minimal(base_size = 12,base_family = "Roboto"))

# Getting Wrapped Data ----------------------------------------------------
#Playlist comparison wrapped data
Comparison_Wrapped <- read_rds(here("data/Wrapped_Playlist_Data.rds"))
Comparison_Wrapped <- Comparison_Wrapped %>% 
  rename("Median Loudness (dB)"=`Median Loudness`,
         "Median Tempo (BPM)"=`Median Tempo`,
         "Percent of Songs That Are Explicit"=`Percent Explicit`) %>% 
  mutate(Wrapped="Yes",
         Playlist=parse_number(Playlist))


# Getting Top 200 Data -----------------------------------------------------
#Playlist comparison top 200 data
Comparison_Top_200 <- read_rds(here("data/Top200_Weighted_Playlist_Data.rds")) %>% 
  rename("Median Loudness (dB)"=`Median Loudness`,
         "Median Tempo (BPM)"=`Median Tempo`,
         "Percent of Songs That Are Explicit"=`Percent Explicit`) %>% 
  mutate(Wrapped="No",
         `Median Years Since Release (Adj)`=`Median Days Since Release (Adj)`/365) %>% 
  select(-`Median Days Since Release (Adj)`) %>% 
  mutate(Playlist=as.numeric(Playlist))



# Joining Wrapped and Top 200 ---------------------------------------------
Full_Comparison <- Comparison_Wrapped %>% bind_rows(Comparison_Top_200)


# Features ----------------------------------------------------------------
Feats_Data <- Full_Comparison %>% 
  select(Wrapped,Playlist,`Median Danceability`:`Percent in Major`,
         `Average Words Per Song`,
         `Percent of Songs That Are Explicit`) %>% 
  pivot_longer(`Median Danceability`:`Percent of Songs That Are Explicit`,names_to="Feature") %>% 
  mutate(whose_songs=ifelse(Wrapped=="Yes", "my songs", "popular songs"))


 diff_in_diff_Feats_Data <- Feats_Data %>% 
  select(-whose_songs) %>% 
  pivot_wider(names_from = Wrapped, values_from=value) %>%
  filter(Playlist>=2019) %>% 
  mutate(per_diff=(Yes-No)/Yes*100) %>% #Percent diff between my music and popular music
  select(-Yes,-No) %>%
  pivot_wider(names_from=Playlist, values_from = per_diff) %>% 
  mutate(diff_in_diff=(`2020`-`2019`)/`2020`*100)   #Percent change in the difference between my music and popular music
 
 diff_in_diff_Feats_Data <- diff_in_diff_Feats_Data %>% 
   mutate(up_or_down=ifelse(diff_in_diff>=0,"up","down"))
 
 
 diff_in_diff_Feats_Data_plot <- diff_in_diff_Feats_Data %>% 
  ggplot(aes(x=fct_reorder(Feature,diff_in_diff),y=diff_in_diff)) +
  geom_col_interactive(aes(tooltip=str_wrap(glue("The difference in {tolower(Feature)} between my music and popular music \nwent {up_or_down}  by {round(diff_in_diff,2)}% between 2019 and 2020"),25)),
                       fill="#1DB954") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x=NULL,
       y="Difference in Difference \n(% change in difference from 2019 to 2020)") +
  coord_flip() 

girafe(ggobj = diff_in_diff_Feats_Data_plot)


# Genres ------------------------------------------------------------------
diff_in_diff_genre <- Full_Comparison %>% 
  select(Wrapped, Playlist,`Percent Hip Hop`,`Percent Pop`,
         `Percent Rap`, `Percent Trap`, `Percent Country`,
         `Percent Rock`, `Percent R&B`, 
         `Percent Other`) %>% 
  pivot_longer(cols=`Percent Hip Hop`:`Percent Other`,
               names_to="Genre",
               values_to="Percent") %>% 
  filter(Playlist>=2019) %>% 
  pivot_wider(names_from = Wrapped,values_from = Percent) %>%
  mutate(diff=(Yes-No)) %>% #Diff between my music and popular music. So in 2019, 15.1 percentage points more songs in my music were hip hop compared to popular music
  select(-Yes,-No) %>%
  pivot_wider(names_from=Playlist, values_from = diff) %>% 
  mutate(diff_in_diff=(`2020`-`2019`))   #Change in the difference between my music and popular music from 2019 to 2020

diff_in_diff_genre <- diff_in_diff_genre %>% 
  mutate(up_or_down=ifelse(diff_in_diff>=0,"up","down"),
         Genre=str_remove(Genre,"Percent "))

diff_in_diff_genre_plot <- diff_in_diff_genre %>% 
  ggplot(aes(x=fct_reorder(Genre,diff_in_diff),y=diff_in_diff)) +
  geom_col_interactive(aes(tooltip=str_wrap(glue("The difference in percent of songs in the {tolower(Genre)} genre between my music and popular music \nwent {up_or_down}  by {round(diff_in_diff,2)} percentage points between 2019 and 2020"),25)),
                       fill="#1DB954") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x=NULL,
       y="Difference in Difference \n(Percentage point change in difference from 2019 to 2020)") +
  coord_flip() 

girafe(ggobj = diff_in_diff_genre_plot)


# Sentiments --------------------------------------------------------------
sentiments <- Full_Comparison %>% 
  select(Wrapped,Playlist,
         `Percent of Words in Trust Category`:`Percent of Words in Anticipation Category`)
names(sentiments) <- names(sentiments) %>% 
  str_replace("Percent of Words in ","") %>% 
  str_replace(" Category","")



sentiments_diff_in_diff <- sentiments %>% 
  pivot_longer(cols=Trust:Anticipation,
               names_to="Sentiment",
               values_to="Percent") %>% 
  filter(Playlist>=2019) %>% 
  pivot_wider(names_from = Wrapped,values_from = Percent) %>%
  mutate(diff=(Yes-No)) %>% #Diff between my music and popular music. So in 2019, 15.1 percentage points more songs in my music were hip hop compared to popular music
  select(-Yes,-No) %>%
  pivot_wider(names_from=Playlist, values_from = diff) %>% 
  mutate(diff_in_diff=(`2020`-`2019`)) 

sentiments_diff_in_diff <- sentiments_diff_in_diff %>% 
  mutate(up_or_down=ifelse(diff_in_diff>=0,"up","down"))

sentiments_diff_in_diff <- sentiments_diff_in_diff %>% 
  ggplot(aes(x=fct_reorder(Sentiment,diff_in_diff),y=diff_in_diff)) +
  geom_col_interactive(aes(tooltip=str_wrap(glue("The difference in percent of words in the {tolower(Sentiment)} sentiment category between my music and popular music \nwent {up_or_down}  by {round(diff_in_diff,2)} percentage points between 2019 and 2020"),25)),
                       fill="#1DB954") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x=NULL,
       y="Difference in Difference \n(Percentage point change in difference from 2019 to 2020)") +
  coord_flip() 

girafe(ggobj = sentiments_diff_in_diff)
