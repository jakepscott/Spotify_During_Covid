#NOTE:
#This works for like ten songs, but when I try it for more than that it starts to not work for a random subsection of songs each time, without
#Rhyme or reason. I imagine it has to do with making so many rapid requests to the Spotify API


test <- tracks %>% head(10)

ids <- test$Id

  # Getting Features --------------------------------------------------------
  Features <- tibble(id=as.character(),danceability=as.double(), energy=as.double(),
                     key=as.integer(),loudness=as.double(), mode=as.integer(),
                     speechiness=as.double(),acousticness=as.double(),instrumentalness=as.integer(),
                     liveness=as.double(),valence=as.double(),tempo=as.double(),duration_ms=as.integer(),
                     time_signature=as.integer(),uri=as.character(),analysis_url=as.character())
  tic()
  for(i in ids){
    tryCatch({
      Sys.sleep(.01)
      Features<-bind_rows(Features,getFeatures(i,token=keys))},
      error=function(e){})
  }
  toc()
  
  
feature_funct <- function(id){
  getFeatures(id,token=keys)
}

tic()
Features <- test %>% mutate(features = future_pmap(list(Id), ~feature_funct(..1), .progress = T))
toc()




Features <- Features %>% unnest(features)


#Fixing the column names to make them pretty
Features <- Features %>% rename(`Duration`="duration_ms",
                                `Time Signature`="time_signature") %>% 
  select(id:mode,valence:Duration)
colnames(Features) <- Features %>% colnames() %>% tools::toTitleCase()

test <- left_join(test,Features) %>% 
  mutate(`Duration (Minutes)`=round(Duration/60000,1)) %>% 
  select(-Duration) %>%
  mutate(Key=ifelse(Key==-1,NA,Key),
         Mode=ifelse(Mode==1,"Major","Minor"),
         Key=as.factor(Key))
