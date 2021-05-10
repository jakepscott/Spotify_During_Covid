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

Feats_Data_Plot <- Feats_Data %>% 
  ggplot(aes(x=Playlist,y=value,group=Wrapped)) +
  geom_vline(xintercept = 2019) +
  geom_line(aes(color=Wrapped),size=1.5) +
  geom_point_interactive(aes(color=Wrapped,
                             tooltip=glue("{Feature} was {value} in {Playlist} for {whose_songs}")),
                         size=3) +
  scale_color_manual(values=c("#A9A9A9","#1DB954")) +
  facet_wrap(~Feature,scales = "free_y",
             labeller = labeller(Feature = label_wrap_gen(10))) +
  labs(title="<span style='color: #1DB954'>**My**</span> most common genres versus <br/>which were <span style='color: #A9A9A9'>**popular**</span>") +
  theme(plot.title = element_markdown(size = rel(2)),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(.8)),
        legend.position = "none")

girafe(ggobj = Feats_Data_Plot)

# Genres ------------------------------------------------------------------
genres <- Full_Comparison %>% 
  select(Wrapped, Playlist,`Percent Hip Hop`,`Percent Pop`,
         `Percent Rap`, `Percent Trap`, `Percent Country`,
         `Percent Rock`, `Percent R&B`, 
         `Percent Other`) 

genres_plot <- genres %>% 
  pivot_longer(cols=`Percent Hip Hop`:`Percent Other`,
               names_to="Genre",
               values_to="Percent") %>% 
  mutate(Genre_label=str_remove(Genre,"Percent ")) %>% 
  mutate(whose_songs=ifelse(Wrapped=="Yes", "my songs", "popular songs")) %>% 
  ggplot(aes(x=Playlist,y=Percent,group=Wrapped)) +
  geom_vline(xintercept = 2019) +
  geom_line(aes(color=Wrapped),size=1.5) +
  geom_point_interactive(aes(color=Wrapped,
                             tooltip=glue("{round(Percent,2)}% of {whose_songs} were classified as {Genre_label} in {Playlist}")),
                         size=3) +
  scale_color_manual(values=c("#A9A9A9","#1DB954")) +
  facet_wrap(~Genre,scales = "free_y") +
  labs(title="<span style='color: #1DB954'>**My**</span> most common genres versus <br/>which were <span style='color: #A9A9A9'>**popular**</span>") +
  theme(plot.title = element_markdown(size = rel(2)),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(.8)),
        legend.position = "none")

girafe(ggobj = genres_plot)

# NRC Sentiments --------------------------------------------------------------
sentiments <- Full_Comparison %>% 
  select(Wrapped,Playlist,
         `Percent of Words in Trust Category`:`Percent of Words in Anticipation Category`)
names(sentiments) <- names(sentiments) %>% 
  str_replace("Percent of Words in ","") %>% 
  str_replace(" Category","")


sentiments_plot <- sentiments %>% 
  pivot_longer(cols=Trust:Anticipation,
               names_to="Sentiment",
               values_to="Percent") %>% 
  mutate(whose_songs=ifelse(Wrapped=="Yes", "my songs", "popular songs")) %>% 
  ggplot(aes(x=Playlist,y=Percent,group=Wrapped)) +
  geom_vline(xintercept = 2019) +
  geom_line(aes(color=Wrapped),
            size=1.5) +
  geom_point_interactive(aes(color=Wrapped,
                             tooltip=glue("{round(Percent,2)}% of words in {whose_songs} were in the {tolower(Sentiment)} category in {Playlist}")),
                         size=2) +
  scale_color_manual(values=c("#A9A9A9","#1DB954")) +
  facet_wrap(~Sentiment,scales = "free_y") +
  labs(title="Sentiments of <span style='color: #1DB954'>**my**</span> music versus <br/> <span style='color: #A9A9A9'>**popular**</span> music",
       subtitle = "Using EmoLex Data") +
  theme(plot.title = element_markdown(size = rel(2)),
        plot.subtitle = element_text(colour = "grey70"),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(.8)),
        legend.position = "none")

girafe(ggobj = sentiments_plot)

# Overall Sentiment -------------------------------------------------
Overall_sentiment_plot <- Full_Comparison %>% 
  select(Wrapped, Playlist, "Average Overall Sentiment"=`Average Corrected Sentiment`) %>% 
  mutate(whose_songs=ifelse(Wrapped=="Yes", "my songs", "popular songs")) %>% 
  ggplot(aes(x=Playlist,y=`Average Overall Sentiment`,color=Wrapped,group=Wrapped)) +
  geom_vline(xintercept = 2019) +
  geom_line(size=1.5) +
  geom_point_interactive(aes(tooltip=glue("The overall sentiment of the lyrics of {whose_songs} was {`Average Overall Sentiment`} in {Playlist}")),
                         size=5) +
  geom_hline(yintercept = 0,linetype="dashed") +
  scale_color_manual(values = c("#1DB954","#A9A9A9")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  guides(color="none") +
  labs(title="Overall sentiment of <span style='color: #1DB954'>**my**</span> music <br/>versus <span style='color: #A9A9A9'>**popular**</span> music",
       subtitle = "Using Afinn Lexicon") +
  theme(plot.title = element_markdown(size = rel(2)),
        axis.title.x = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_text(colour = "grey70"))

girafe(ggobj = Overall_sentiment_plot)

