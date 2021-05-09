top_words_2017 <- interesting_words %>% 
  filter(Playlist=="Your Top Songs 2017") %>% 
  count(words) 

wordcloud(words = top_words_2017$words, freq = top_words_2017$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


top_words_2018 <- interesting_words %>% 
  filter(Playlist=="Your Top Songs 2018") %>% 
  count(words) 

wordcloud(words = top_words_2018$words, freq = top_words_2018$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


top_words_2019 <- interesting_words %>% 
  filter(Playlist=="Your Top Songs 2019") %>% 
  count(words) 

wordcloud(words = top_words_2019$words, freq = top_words_2019$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


top_words_2020 <- interesting_words %>% 
  filter(Playlist=="Your Top Songs 2020") %>% 
  count(words) 

wordcloud(words = top_words_2020$words, freq = top_words_2020$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
