## Tidy Tuesday 1/12/21
## Messy script (will figure out a viz here, then put it into a .Rmd file)

library(tidyverse)
library(tidytext)
library(tm)
library(sentimentr)
library(magrittr)

## QUESTIONS OF INTEREST ----
# How have titles evolved over time? i.e. in sentiment/word choice?
# Most common words used in titles? 
# Possible interesting add-ons: does gender or country of origin play a difference in title sentiment/word choice?
# Most positive words, most neg words, most emotionally charged?

## RESOURCES USED ----
# https://www.codementor.io/@alexander-k/r-word-frequency-in-dataframe-165jgfxxqe
# https://www.tidytextmining.com/tidytext.html#:~:text=The%20stop_words%20dataset%20in%20the,appropriate%20for%20a%20certain%20analysis.


## LOAD THE DATA ----
tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
artwork <- tuesdata$artwork


## VIZ ----
artwork %>%
  count(artist) %>%
  arrange(desc(n)) -> test
  
## Get word count frequency for titles ----
data("stop_words")
artwork %>%
  select(artist,title,year) %>%
  # Take out works without a title ("blank")
  filter(title != 'Blank') %>%
  # some things have "no title" or are "untitled" and it's messing up my count
  filter(str_detect(title,"title",TRUE)) %>%
  # Take out works without a year
  filter(!is.na(year)) %>%
  unnest_tokens(word,title) %>%
  # Remove stopwords (this is a dict in tidytext pkg; combo of three stopword lexicons)
  anti_join(stop_words) -> artwork_df

artwork_df %>%
  count(word) %>%
  arrange(desc(n)) -> frequency_df
## So, this is a little messy, but could be fun to throw in there... as a "tidbit"
# top 10 words:
# Turner is #4, but that is artist's name...the way the data is structured, it incl artist's name for some of the works
# Or maybe he's a cocky artist. :shrug: 
frequency_df %>% head(10)

## Get sentiment by year ----
#(ref to "stemming_lemmatisation_TLDR.R" script)
artwork_df %>%
  mutate(text_stem = textstem::stem_words(word),
         # USE LEMMA'D VERSION! CLEANER/BETTER FOR TEXT ANALYSIS
         text_lemma = textstem::lemmatize_words(word)) -> artwork_df_stem

# Lemma'd version much cleaner/is better for emotion tracking. For deeper comparison, see the full stemming_lemmatisation.R script.
# Get emotion on cleaned and lemma'd text
# FYI: %$% operator passes dataframe into pipe and exposes the original column names (needed for some functions that aren't totally integrated in the tidyverse)
artwork_df_stem %$%
  emotion_by(get_sentences(text_lemma)) %>%
  select(element_id,emotion_type,emotion_count) %>%
  pivot_wider(names_from = emotion_type,values_from = emotion_count) %>%
  cbind(.,artwork_df_stem) -> text_data_lemma

# Get sentiment scores and bind to emo data. 
artwork_df_stem %$%
  sentiment_by(get_sentences(text_lemma)) %>%
  cbind(.,artwork_df_stem) -> text_data_lemma_sentiment

artwork_lemma_NLP <- cbind(text_data_lemma,text_data_lemma_sentiment)
artwork_lemma_NLP %<>% select(2:26) # had some duplicate columns from cbind (TODO: find a better way?!)


## some summarisations and VIZZING on the data
# Just sentiment
artwork_lemma_NLP %>% 
  select(year,ave_sentiment) %>%
  group_by(year) %>%
  mutate(avg_sentiment = mean(ave_sentiment,na.rm = TRUE)) %>%
  arrange(desc(avg_sentiment)) %>%
  select(-2) %>%
  distinct() %>%
  ggplot(aes(x=year,y=avg_sentiment)) +
  geom_line() +
  theme_bw()

# Title sentiment and word count over time (more words = really long titles, or more pieces of art. it's a proxy)
artwork_lemma_NLP %>% 
  select(year,ave_sentiment) %>%
  group_by(year) %>%
  mutate(`Average Sentiment` = mean(ave_sentiment,na.rm = TRUE),
         `Word Count` = n()) %>%
  arrange(year) %>%
  select(-2) %>%
  distinct() %>%
  pivot_longer(2:3) %>%
  ggplot(aes(x=year,y=value)) +
  geom_line() +
  facet_grid(name~.,scales = 'free') +
  labs(x='Year',y='Word Count and Sentiment Score',
       title = 'How Does Title Sentiment Change Over Time?',
       subtitle = 'The first half of the 19th century has the most title words and least variation in sentiment.\nPerhaps titles were really bland, or there were a lot positive and negative titles that balanced each other out.',
       caption = 'Most popular title words, you ask?\nCastle, view, river, mountains, bridge and church, just to name a few.') +
  theme_bw()
