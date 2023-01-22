library(spotifyr)
library(tidyverse)
library(dplyr)
library(ggridges)

#Set up account with Spotify Web Api
Sys.setenv(SPOTIFY_CLIENT_ID = "8e34b444b1324ede8825713277a974f9")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "d8d7a0df6ea54b44927658c21211206f")

#Pull audio features from Spotify. In this case I selected The Beatles
beatles <- get_artist_audio_features('the beatles')

#Check the names of the album
table(beatles$album_name)

#Filter out for record albums (no live and recopilation albums)
beatles_albums <- beatles %>% 
  filter(!grepl('Deluxe|Live|Performance|Naked|Songtrack|1', album_name))

#Filter out "The Beatles" album as it is repeated
beatles_albums <- filter(beatles_albums, album_name != "The Beatles")

#Check for duplicates songs
duplicated(beatles_albums$track_name)
beatles_albums$track_name[which(duplicated(beatles_albums$track_name))]

#Rename album names to do away with the suffix Remastered
beatles_albums$album_name <- gsub("(Remastered)" , "", beatles_albums$album_name)
#Remove ()
beatles_albums <- beatles_albums %>% mutate(album_name_updated = gsub("[()]", "", album_name))
#Check results
table(beatles_albums$album_name_updated)


#Calculate albums by level of positiveness 
beatles_albums %>% 
  group_by(album_name_updated) %>%  
  summarise_at(vars(valence), list(name=median))

#Create a distribution graph of Green Day's albums by positive level
beatles_albums %>%
  arrange(album_release_year) %>%   
  mutate(album_name_updated=factor(album_name_updated, levels = rev(unique(beatles_albums$album_name_updated)))) %>%  #Unique level for albums
  ggplot(aes(x= valence, y= album_name_updated, fill= album_name_updated)
  )+
  geom_density_ridges(from = 0, to = 1)+
  labs(
    x = "Valence scale: Tracks with high valence sound more positive and viceversa",
    y = "",
    title = "Distribution of The Beatles' Musical Positiveness",
    subtitle = "Studio Albums based on Valence Scale",
    caption = " Gersán Vásquez Gutiérrez (@gervagu) | Source: Spotify's Web API"
  ) +
  scale_fill_cyclical(
    values = c("#495e7b")
  ) +
  theme_ridges()+ 
  theme(
    plot.title=element_text(hjust = 0.5, size = 20, color = "#495e7b", face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", hjust = 0.5, size=16),
    axis.text.y = element_text(size = 14, face="bold")
  )

ggsave("beatles.png",width=60,height=30,units='cm',dpi=300)

