# Load libraries
library(readr)
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(wordcloud2)

# Load the dataset from GitHub
urlfile <- "https://raw.githubusercontent.com/dateascientist/wrangling-musical-data/master/datasets/bb_chords.csv"
mydata <- read.csv(url(urlfile), stringsAsFactors=T)

# Understand the data
head(mydata)
sum(is.na(mydata))
summary(mydata)
str(mydata)

# Identify the most common chords and their frequency
# Counting the most common chords
chord_count <- mydata %>% 
               count(chord,sort = TRUE)

# Display the top 20 chords
chord_count[1:20,]

# Visualize most common chords
# Creating a bar plot from the data
chord_count_fraction <- chord_count %>%
                          slice(1:20) %>%   
                            mutate(fraction = n / sum(n),
                            chord = reorder(chord, fraction))

ggplot(chord_count_fraction, aes(x=chord, y=fraction, fill = chord)) +
  geom_bar(stat = "identity")  +
  theme(legend.position='none')+
  ylab("Fraction of total chords") +
  xlab("Chords") +
  coord_flip() +
  scale_fill_viridis(discrete = T, option="H")  # Apply the Viridis gradient

# Change of top 5 most common chords over the year
selected_chords_over_time <- mydata %>%
  filter(chord %in% chord_count[c(1:5),]$chord) %>%
    group_by(chord, year) %>%
      count(chord) %>%
        arrange(chord, desc(n), desc(year))

ggplot(selected_chords_over_time, aes(x = year, y = n, fill=chord)) +
  geom_bar(position="fill", stat = "identity") +  # Create stacked bars
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentages
  scale_fill_viridis(discrete = TRUE, option = "H")  # Apply viridis to fill

# Chord "bigrams"
# Wrangling and counting bigrams
mydata_2 <- mydata %>%
  mutate(next_chord = lead(chord),
         next_title = lead(title)) %>%
    filter(title == next_title) %>%  # Filter rows where title equals next_title
  mutate(bigram = paste(chord, next_chord, sep=' '))

chord_bigram_count <- mydata_2 %>%
    count(bigram, sort=TRUE)
  
# Displaying the top 20 bigrams
chord_bigram_count[1:20,]
  
# Visualizing the most common chord progressions
bigram_count_fraction <- chord_bigram_count %>%
  slice(1:20) %>%   
  mutate(fraction = n / sum(n),
         bigram = reorder(bigram, fraction))

ggplot(bigram_count_fraction, aes(x=bigram, y=fraction, fill = bigram)) +
  geom_bar(stat = "identity")  +
  theme(legend.position='none')+
  ylab("Fraction of total bigrams") +
  xlab("Bigrams") +
  coord_flip() +
  scale_fill_viridis(discrete = T, option="H")

# Change of top 5 most common chord bigrams over the year
selected_bigrams_over_time <- mydata_2 %>%
  filter(bigram %in% chord_bigram_count[c(1:5),]$bigram) %>%
  group_by(bigram, year) %>%
  count(bigram) %>%
  arrange(bigram, desc(n), desc(year))

ggplot(selected_bigrams_over_time, aes(x = year, y = n, fill=bigram)) +
  geom_bar(position="fill", stat = "identity") +  # Create stacked bars
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentages
  scale_fill_viridis(discrete = TRUE, option = "H")  # Apply viridis to fill

# Find most common artists
# Find 30 artists with the most songs in the corpus
top_30_artists <- mydata %>%
  select(artist, title) %>% 
  unique() %>%
  count(artist,sort=TRUE) %>%
  slice(1:30)

# Display 30 artists with the most songs in the corpus
top_30_artists
set.seed(999)
wordcloud2(data=top_30_artists, size=0.3, color= viridis(5), shape='star')

# Tag the corpus
tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))

# Create a new dataframe `bb_tagged` that includes a new column `instrument` from `tags`
mydata_tagged <- mydata %>%
  inner_join(tags) #this will remove rows without empty "tags"

# Display the new dataframe
mydata_tagged

# Compare chords in piano-driven and guitar-driven songs
# The top 20 most common chords
top_20_chords <- chord_count[1:20,]

# Comparing the frequency of the 20 most common chords in piano- and guitar-driven songs
tagged_chord_data <- mydata_tagged %>%
  filter(chord %in% top_20_chords$chord) %>%
    count(chord, instrument, sort = TRUE)
tagged_chord_data  
  
ggplot(tagged_chord_data, aes(x = chord, y = n, fill=chord))+
  geom_bar(stat = 'identity')+
  coord_flip() +
  xlab("Chords") +
  ylab("Number of Chords") +
  scale_fill_viridis(discrete = T, option="H") +
  theme(legend.position="none") +
  facet_grid(~instrument)

# Compare chord bigrams in piano-driven and guitar-driven songs
# Compare the frequency of the 20 most common chord bigrams in piano- and guitar-driven songs
mydata_2_tagged <- mydata_2 %>%
  inner_join(tags)

tagged_bigram_data <- mydata_2_tagged %>%
  filter(bigram %in% chord_bigram_count[1:20,]$bigram) %>%
  count(bigram, instrument, sort = TRUE)
tagged_bigram_data  

ggplot(tagged_bigram_data, aes(x = bigram, y = n, fill=bigram))+
  geom_bar(stat = 'identity')+
  coord_flip() +
  xlab("Bigrams") +
  ylab("Number of Bigrams") +
  scale_fill_viridis(discrete = T, option="H") +
  theme(legend.position="none") +
  facet_grid(~instrument)
