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
library(ggtext)
windowsFonts(`Roboto`=windowsFont("Roboto Condensed"))
theme_set(theme_minimal(base_size = 12,base_family = "Roboto"))

data <- read_rds(here("data/Full_Wrapped_Feat_Lyrics_Data.rds"))
comparison_data <- read_rds(here("data/Wrapped_Playlist_Data.rds"))
comparison_data <- comparison_data %>% rename("Median Loudness (dB)"=`Median Loudness`,
                                              "Median Tempo (BPM)"=`Median Tempo`,
                                              "Percent of Songs That Are Explicit"=`Percent Explicit`)



# Overall Distributions ---------------------------------------------------
#Song Feats
data %>% 
  select(Danceability,Energy,Loudness,Valence:`Duration (Minutes)`) %>%
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value") %>% 
  ggplot(aes(Value)) +
  geom_density(fill="lightblue") +
  facet_wrap(~Feature, scales = "free") + 
  labs(y=NULL,
       title = "Most song features appear roughly normally distributed") +
  theme(plot.title.position = "plot")

#Lyrical Sentiments
lyrical_sent <- data %>% 
  select(trust_percent:overall_sentiment_corrected) %>%
  select(-overall_sentiment) 

names(lyrical_sent) <- names(lyrical_sent) %>% str_replace_all(.,"_"," ") %>% toTitleCase()

lyrical_sent %>% 
  rename("Afinn Sentiment"=`Overall Sentiment Corrected`) %>% 
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value") %>% 
  ggplot(aes(Value)) +
  geom_density(fill="lightblue") +
  scale_x_continuous(labels = function(x){round(x,1)}) +
  facet_wrap(~Feature, scales = "free",
             labeller = labeller(Feature = label_wrap_gen(10))) +
  labs(y=NULL,
       title = "The distribution of lyrical sentiments appear roughly normally \ndistributed",
       subtitle = "Though most categories exhibit some degree of right skew") +
  theme(plot.title.position = "plot",
        axis.text.x = element_text(size=rel(0.8)),
        plot.subtitle = element_text(color="grey50"))


# Overall distribution by year ---------------------------------------------
#Song Feats
data %>% 
  select(Playlist,Danceability,Energy,Loudness,Valence:`Duration (Minutes)`) %>%
  mutate(Playlist=parse_number(Playlist) %>% as.character()) %>% 
  pivot_longer(cols = -Playlist,names_to = "Feature",values_to = "Value") %>% 
  filter(Playlist=="2019" | Playlist=="2020") %>% 
  ggplot(aes(Value)) +
  geom_density(aes(group=Playlist, fill=Playlist),alpha=.5) +
  scale_fill_manual(values=c("#A9A9A9","#1DB954")) +
    facet_wrap(~Feature, scales = "free",
             labeller = labeller(Feature = label_wrap_gen(10))) +
  labs(title="Distribution of feature values in <span style='color: #A9A9A9'>**2019**</span> versus <span style='color: #1DB954'>**2020**</span>",
       subtitle = "Perhaps surprisingly, the features seem to differ at most marginally \nby year") +
  theme(plot.title = element_markdown(size = rel(1.25)),
        plot.subtitle = element_text(color="grey50"),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(0.7)),
        legend.position = "none")
  
#Lyrical Sentiments
lyrical_sent <- data %>% 
  select(Playlist,trust_percent:overall_sentiment_corrected) %>%
  mutate(Playlist=parse_number(Playlist) %>% as.character()) %>% 
  select(-overall_sentiment) 

names(lyrical_sent) <- names(lyrical_sent) %>% str_replace_all(.,"_"," ") %>% toTitleCase()

lyrical_sent %>% 
  rename("Afinn Sentiment"=`Overall Sentiment Corrected`) %>% 
  pivot_longer(cols = -Playlist,names_to = "Feature",values_to = "Value") %>% 
  filter(Playlist=="2019" | Playlist=="2020") %>% 
  ggplot(aes(Value)) +
  geom_density(aes(group=Playlist, fill=Playlist),alpha=.5) +
  scale_x_continuous(labels = function(x){round(x,1)}) +
  scale_fill_manual(values=c("#A9A9A9","#1DB954")) +
  facet_wrap(~Feature, scales = "free",
             labeller = labeller(Feature = label_wrap_gen(10))) +
  labs(title="Lyrical sentiments in <span style='color: #A9A9A9'>**2019**</span> versus <span style='color: #1DB954'>**2020**</span>",
       subtitle = "Perhaps surprisingly, lyrical sentiments seem to differ at most marginally \nby year, besides a modestly stronger right skew in 2020 for some features") +
  theme(plot.title = element_markdown(size = rel(1.5)),
        plot.subtitle = element_text(color="grey50"),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(0.7)),
        legend.position = "none")

