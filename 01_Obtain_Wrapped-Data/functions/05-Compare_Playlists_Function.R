# Loading Libraries and Data ----------------------------------------------
library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(tools)

Playlist_Comparison_Function <- function(data,wrapped=0) {
  #Getting something to combine all the feature summaries to
  Playlist_Data <- data %>% group_by(Playlist) %>% summarise(`Number of Songs`=n())
  
  
  #Song Features ---------------------------------------
  #if danceability is in the colnames, then I must have gotten song features, so summarise them
  if ("Danceability" %in% colnames(data)) {
     #Continuous
    Song_Features <- data %>% 
      group_by(Playlist) %>% 
      summarise(across(c("Danceability","Energy","Loudness","Valence","Tempo",`Duration (Minutes)`),list(median=median), na.rm=T)) %>% 
      #Adding median in front of the name
      rename_if(str_detect(names(.),"_median"),~paste("Median",.,sep=" ")) 
      
      colnames(Song_Features) <- Song_Features %>% colnames() %>% str_remove("_median") 
      
      Song_Features <- Song_Features %>% rename("Median Song Duration (Minutes)"=`Median \`Duration (Minutes)\``)
      
    
    Playlist_Data <- left_join(Playlist_Data,Song_Features)
    
    #Binary
    Mode <- data %>% 
      select(Playlist,Mode) %>% 
      mutate(Mode=ifelse(Mode=="Major",1,0)) %>% 
      group_by(Playlist) %>% 
      summarise(`Percent in Major`=mean(Mode,na.rm = T))
    Playlist_Data <- left_join(Playlist_Data,Mode)
    
    #Median of Key
    Key <- data %>% 
      select(Playlist,Key) %>% 
      mutate(Key=as.numeric(Key)) %>% 
      group_by(Playlist) %>% 
      summarise(`Median Key (0 is C)`=median(Key,na.rm = T))
    Playlist_Data <- left_join(Playlist_Data,Key)
    
    #Gender
    Gender <- data %>% 
      mutate(`Artist Gender`=ifelse(`Artist Gender`=="Male",1,0)) %>% 
      group_by(Playlist) %>% 
      summarise(`Percent Male`=mean(`Artist Gender`,na.rm=T)*100)
    
    Playlist_Data <- left_join(Playlist_Data,Gender)
  }

  
  
  if("Years Since Release" %in% colnames(data)){
    #IF Years Since Release is in colnames, I must have gotten it, so summarise

# This if else is only for wrapped analysis -------------------------------
    if (wrapped==1) {
      Release_Dates <- data %>% 
        select(Playlist,`Years Since Release`) %>% 
        mutate(release_date=Sys.Date()-365*`Years Since Release`,
               years_since_release=parse_number(Playlist)-year(release_date)) %>% 
        group_by(Playlist) %>% 
        summarise(`Median Years Since Release (Adj)`=median(years_since_release,na.rm = T))
    } else {
      Release_Dates <- data %>% 
        select(Playlist,`Years Since Release`) %>% 
        group_by(Playlist) %>%  
        summarise(`Median Years Since Release`=median(`Years Since Release`,na.rm = T))
      
    }
   Playlist_Data <- left_join(Playlist_Data,Release_Dates)
  }
  
  #Genres
  if ("Artist Genres" %in% colnames(data)) {
    #IF Artist Genres is in colnames, I must have gotten it, so summarise
    genres <- data %>% select(Playlist,`Artist Genres`) %>% 
      mutate(`Hip Hop`=str_detect(`Artist Genres`,pattern=regex("hip|hop",ignore_case = T)),
             Pop=str_detect(`Artist Genres`,pattern=regex("pop",ignore_case = T)),
             Rap=str_detect(`Artist Genres`,pattern=regex("rap",ignore_case = T)),
             Trap=str_detect(`Artist Genres`,pattern=regex("trap",ignore_case = T)),
             Teen=str_detect(`Artist Genres`,pattern=regex("teen",ignore_case = T)),
             Electronic=str_detect(`Artist Genres`,pattern=regex("electronic|electro|edm|dub",ignore_case = T)),
             Rock=str_detect(`Artist Genres`,pattern=regex("rock|metal|grunge",ignore_case = T)),
             Jazz=str_detect(`Artist Genres`,pattern=regex("blues|jazz",ignore_case = T)),
             Alternative=str_detect(`Artist Genres`,pattern=regex("emo|punk",ignore_case = T)),
             `R&B`=str_detect(`Artist Genres`,pattern=regex("R&B",ignore_case = T)),
             Conscious=str_detect(`Artist Genres`,pattern=regex("conscious|progressive",ignore_case = T)),
             Country=str_detect(`Artist Genres`,pattern=regex("country",ignore_case = T)),
             LGBT=str_detect(`Artist Genres`,pattern=regex("lgbt",ignore_case = T)),
             Other=ifelse(`Hip Hop`==FALSE &
                            Pop==FALSE &
                            Rap==FALSE &
                            Trap==FALSE &
                            Teen==FALSE &
                            Electronic==FALSE &
                            Rock==FALSE &
                            Jazz==FALSE &
                            `R&B`==FALSE &
                            Alternative==FALSE &
                            Conscious==FALSE &
                            Country==FALSE &
                            LGBT==FALSE,yes = TRUE,no = FALSE))
    #Making a median times 100 function, should be able to do this within the summarise function but I cannot figure out how
    mean_100 <- function(x){
      mean(x*100,na.rm=T)
    }
    
    Genres <- genres %>% 
      select(-`Artist Genres`) %>% 
      group_by(Playlist) %>% 
      summarise(across(where(is.logical),list(percent=mean_100))) %>% 
      rename_if(str_detect(names(.),"_percent"), ~paste("Percent",.,sep=" "))
      
    
    colnames(Genres) <- Genres %>% colnames() %>% str_remove("_percent")
    Genres <- Genres %>% rename("Percent Hip Hop" = `Percent \`Hip Hop\``,
                                "Percent R&B" = `Percent \`R&B\``)
    
    Playlist_Data <- left_join(Playlist_Data,Genres)
  }
  
  if ("Explicit?" %in% colnames(data)) {
    #If Explicit? in colnames then I must have got it and can summarise it
    #Binary Variables
    Explicit <- data %>% 
      select(Playlist,`Explicit?`) %>% 
      group_by(Playlist) %>% 
      summarise(`Percent Explicit`=mean(`Explicit?`,na.rm = T))
    
    Playlist_Data <- left_join(Playlist_Data,Explicit)
    
  }
  
  if ("total_words" %in% colnames(data)) {
    Total_Words_and_Overall_Sentiment <- data %>% 
      select(Playlist, total_words,overall_sentiment,overall_sentiment_corrected) %>% 
      group_by(Playlist) %>% 
      summarise(`Total Words` = sum(total_words,na.rm = T),
                `Average Words Per Song` = median(total_words,na.rm = T),
                `Average Overall Sentiment` = median(overall_sentiment,na.rm = T),
                `Average Corrected Sentiment`= median(overall_sentiment_corrected,na.rm = T))
    
    Emotion_Words <- data %>% 
      select(Playlist,trust:anticipation) %>% 
      group_by(Playlist) %>% 
      summarise(across(where(is.double),sum,na.rm=T)) 
    
    Words <- left_join(Emotion_Words,Total_Words_and_Overall_Sentiment)
    Words <- Words %>% mutate(across(trust:anticipation,~./`Total Words`*100))
    Words <- Words %>% 
      rename_at(vars(trust:anticipation),toTitleCase) %>% 
      rename_at(vars(Trust:Anticipation),~paste("Percent of Words in",.,"Category"))
    Playlist_Data <- left_join(Playlist_Data,Words)
  }
  
  Playlist_Data %>% mutate(across(where(is.numeric),round,4))
}
