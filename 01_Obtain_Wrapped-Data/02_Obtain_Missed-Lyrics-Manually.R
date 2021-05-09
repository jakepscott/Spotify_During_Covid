# Which Songs Lyric Function Failed On ------------------------------------
#These are the tough songs that my function couldn't get
tough_songs <- Lyrics %>% filter(full_lyrics=="1")

#A handful of songs are messed up by parentheses. Let's get rid of those
tough_songs <- tough_songs %>% mutate(song2=str_replace(Song,"\\(",replacement = ""),
                          song2=str_replace(song2,"\\)",replacement = ""))
#Get the lyrics if I remove those parenthesis, works for a few
for (i in 1:nrow(tough_songs)) {
  tryCatch({
    print(i)
    tough_songs$Lyrics[i] <- genius_lyrics(artist = tough_songs$Artist[i],
                                    song = tough_songs$song2[i],
                                    info = "simple") %>% 
      dplyr::select(lyric)
  }, error=function(e){print(e)}
  )
}


# Getting the Songs I *Still* missed --------------------------------------

missed <- tough_songs %>% head(0)
for (i in 1:nrow(tough_songs)) {
  print(i)
  if (!is.character(tough_songs$Lyrics[[i]])) {
    missed[i,] <- tough_songs[i,]
  }   else {
    missed[i,] <- NA
  }
}
#Saving which ones I got
captured_tough_songs <- tough_songs %>% anti_join(missed)

#Going to have to get these ones manually
manual_lyrics <- missed %>% filter(!is.na(Song))


# Getting Songs Manually --------------------------------------------------

#Just getting the songs manually. Bummer. But only 20ish out of 400, not bad. 
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Electric Love")] <- get_lyrics_url("https://genius.com/Brns-electric-love-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="10,000 Emerald Pools")] <- get_lyrics_url("https://genius.com/Brns-10000-emerald-pools-lyrics") %>% select(line)
#manual_lyrics$Lyrics[which(manual_lyrics$Song=="Hold on to Her (feat. Dom Kennedy)")] <- get_lyrics_url("") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Rap Saved Me" )] <- get_lyrics_url("https://genius.com/21-savage-offset-and-metro-boomin-rap-saved-me-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Buy U a Drank (Shawty Snappin') (feat. Yung Joc)")] <- get_lyrics_url("https://genius.com/T-pain-buy-u-a-drank-shawty-snappin-feat-yung-joc-acappella-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song== "Pray For Me (with Kendrick Lamar)")] <- get_lyrics_url("https://genius.com/The-weeknd-and-kendrick-lamar-pray-for-me-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Song For You")] <- get_lyrics_url("https://genius.com/Donny-hathaway-a-song-for-you-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song== "22 (OVER S∞∞N)")] <- get_lyrics_url("https://genius.com/Bon-iver-22-over-sn-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="LOVE. FEAT. ZACARI." )] <- get_lyrics_url("https://genius.com/Kendrick-lamar-love-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="My Choppa Hate Niggas")] <- get_lyrics_url("https://genius.com/21-savage-and-metro-boomin-my-choppa-hate-niggas-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="3005")] <- get_lyrics_url("https://genius.com/Childish-gambino-v-3005-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Falling For You with Justin Bieber")] <- get_lyrics_url("https://genius.com/Jaden-falling-for-you-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Smile (with The Weeknd)")] <- get_lyrics_url("https://genius.com/Juice-wrld-and-the-weeknd-smile-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="BOSS")] <- get_lyrics_url("https://genius.com/Lil-pump-boss-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Life is Beautiful")] <- get_lyrics_url("https://genius.com/Sixx-am-life-is-beautiful-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Stuart")] <- get_lyrics_url("https://genius.com/Maxd-stuart-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="I Got 5 On It (Tethered Mix from US)")] <- get_lyrics_url("https://genius.com/Luniz-i-got-5-on-it-tethered-mix-from-us-lyrics") %>% select(line)
manual_lyrics$Lyrics[which(manual_lyrics$Song=="Drip Too Hard (Lil Baby & Gunna)")] <- get_lyrics_url("https://genius.com/Lil-baby-and-gunna-drip-too-hard-lyrics") %>% select(line)


manual_lyrics$Lyrics[which(manual_lyrics$Song=="715 - CR∑∑KS")] <- "Down along the creek
I remember something
Her, the heron hurried away
When first I breeched that last Sunday

Low moon don the yellow road
I remember something
That leaving wasn't easing
All that heaving in my vines
And as certain it is evening 'at is now is not the time
Ooh

Toiling with your blood
I remember something
In B, unrationed kissing on a night second to last
Finding both your hands as second sun came past the glass
And oh, I know it felt right and I had you in my grasp

Oh, then how we gonna cry
Cause it once might not mean something
Love, a second glance it is not something that we'll need
Honey, understand that I have been left here in the reeds
But all I'm trying to do is get my feet out from the crease

And I'll see you

Turn around, you're my A-Team
Turn around now, you're my A-Team
God damn, turn around now, you're my A-Team"

manual_lyrics$Lyrics[which(manual_lyrics$Song=="10 d E A T h b R E a s T ⚄ ⚄" )] <- get_lyrics_url("https://genius.com/Bon-iver-10-d-e-a-t-h-b-r-e-a-s-t-lyrics") %>% select(line)


# Joining all the lyrical data  -------------------------------------------
#Songs I got in the above process
retrieved <- bind_rows(captured_tough_songs,manual_lyrics) %>% select(-song2)
for (i in 1:nrow(retrieved)) {
  retrieved$full_lyrics[i] <- retrieved$Lyrics[[i]] %>% paste(collapse = " ")
}

retrieved <- retrieved %>% select(-Lyrics)

#Dropping from my lyrics object the rows for the songs I just got (which are NA's in the Lyrics object for the lyrics column right now)
Got_Lyrics <- Lyrics %>% anti_join(retrieved, by="Id")

#Binding the lyrics I just got onto the lyrics on got from my function 
Full_Lyrics <- bind_rows(Got_Lyrics,retrieved)
