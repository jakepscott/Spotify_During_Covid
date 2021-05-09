library(readr)


#This is just the distinct entries for all popular songs since 2017, not including the nested Lyrics column. I use 
#this to speed up 01_Obtain_Wrapped-Data.R. I basically don't get data for any songs already in this list
Full_Top_200 <- read_rds(here("data/Full_Top_200_Feat_Lyrics_Data.rds"))
Popular_Songs_Data <- Full_Top_200 %>% select(-Lyrics) %>% distinct(Id,.keep_all = T)
saveRDS(Popular_Songs_Data,here("01_Obtain_Wrapped-Data/data/Popular_Songs_Data.rds"))
