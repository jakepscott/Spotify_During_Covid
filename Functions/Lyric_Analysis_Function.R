Lyric_Analysis_Function <- function(Lyrics){
  #Using the EmoLex to get word sentiments
  word_sentiment <- get_sentiments("nrc")
  
  ##Getting it into useable form, so you can see which emotions a given word does or does not match with
  word_sentiment <- word_sentiment %>% 
    mutate(na=NA) %>% 
    pivot_wider(names_from = sentiment, 
                values_from = sentiment,
                values_fill = "0") %>%
    dplyr::select(-na)
  
  #Changing to dummy vars with 1s and 0s
  word_sentiment <- word_sentiment %>% 
    mutate(across(trust:anticipation, ~if_else(.x=="0",0,1)))
  
  ##Giving a unique row identifier which I will use to bind with song sentiment information
  Lyrics <- Lyrics %>% mutate(unique_id=1:nrow(Lyrics))
  
  ##Getting an initial row to connect song sentiments to
  song_sentiment <- Lyrics$lyrics[[1]] %>% 
    tibble(word=.) %>%
    unnest_tokens(output = "word",input = "word",token = "words") %>% 
    left_join(., word_sentiment, by="word") %>% 
    na.omit %>% 
    mutate(total_words=nrow(.)) %>% mutate_if(.predicate = is.double, .funs= ~sum(.)) %>% 
    head(1) %>% 
    mutate(tracks=Lyrics$tracks[[1]], artist=Lyrics$artist[[1]],
           unique_id=Lyrics$unique_id[[1]]) %>% 
    select(-word) %>% 
    select(tracks, artist, unique_id, trust:total_words) %>% 
    head(0)
  
  
  for (i in 1:nrow(Lyrics)) {
    print(i)
    tryCatch({
      sentiments_to_bind <- Lyrics$lyrics[[i]] %>% 
        tibble(word=.) %>%
        unnest_tokens(output = "word",input = "word",token = "words") %>% 
        left_join(., word_sentiment, by="word") %>% 
        na.omit %>% 
        mutate(total_words=nrow(.)) %>% mutate_if(.predicate = is.double, .funs= ~sum(.)) %>% 
        head(1) %>% 
        mutate(tracks=Lyrics$tracks[[i]], artist=Lyrics$artist[[i]],
               unique_id=Lyrics$unique_id[[i]]) %>% 
        select(-word) %>% 
        select(tracks, artist, unique_id, trust:total_words) 
      song_sentiment <- rbind(song_sentiment, sentiments_to_bind)
    }, error=function(e){print(e)})
  }

  Lyrics <- left_join(Lyrics, song_sentiment, by=c("unique_id", "tracks", "artist"))
  

# Overall Song Sentiment --------------------------------------------------
  
  
  song_sentiment <- Lyrics$lyrics[[1]] %>% 
    tibble(word=.) %>%
    unnest_tokens(output = "word",input = "word",token = "words") %>% 
    left_join(., afinn, by="word") %>% 
    na.omit %>% 
    mutate(overall_sentiment_of_song=mean(value), 
           tracks=Lyrics$tracks[1],
           artist=Lyrics$artist[1]) %>% 
    select(tracks, artist, overall_sentiment_of_song) %>% 
    head(0)
  
  for (i in 1:nrow(Lyrics)) {
    print(i)
    tryCatch({
      if(is.double(Lyrics$lyrics[[i]])==F){
      sentiments_to_bind <- Lyrics$lyrics[[i]] %>% 
        tibble(word=.) %>%
        unnest_tokens(output = "word",input = "word",token = "words") %>% 
        left_join(., afinn, by="word") %>% 
        na.omit %>% 
        mutate(overall_sentiment_of_song=mean(value), 
               tracks=Lyrics$tracks[i],
               artist=Lyrics$artist[i]) %>% 
        select(tracks, artist, overall_sentiment_of_song) %>% 
        head(1)} else {
          sentiments_to_bind <- tibble(tracks=Lyrics$tracks[i],
                                       artist=Lyrics$artist[i],
                                       overall_sentiment_of_song=NA)
        }
      
      song_sentiment <- rbind(song_sentiment, sentiments_to_bind)
    }, error=function(e){print(e)})
  }
  
  left_join(Lyrics,song_sentiment)

}


