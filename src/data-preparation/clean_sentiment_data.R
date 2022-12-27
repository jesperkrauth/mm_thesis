##############################
### CLEAN SENTIMENT DATA #####
##############################



### Load libraries for data cleaning ###
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(tidytext)
library(vader)
library(lubridate)
library(tokenizers)
library(textdata)
library(yardstick)
library(ggplot2)
library(zoo)

### TO DO CHECK WHICH LIBRARIES NEEDED AND REMOVE UNNECESSARY LIBRARIES


### LOAD DATA ###
# Text review data
goodreads <- read_rds('labeled_gr_with_dates.rds')
amazon <- read_rds('labeled_am_with_dates.rds')
goodreads <- rename(goodreads, goodreads_id = asin)
goodreads$goodreads_id <- as.double(goodreads$goodreads_id)

# Data on which Amazon ASIN belongs to which Goodreads book ID
overlap_titles <- read_tsv('overlap_titles_amazon_gr.txt')



### CREATE SAMPLES (160,000 = ABOUT 33%) ###
# Note: forgot to set a seed, so every time this code runs it will take another random sample (my bad)
# Note: did save the sample datasets, can be shared if necessary
goodreads <- goodreads[sample(nrow(goodreads), 160000), ]
saveRDS(goodreads, 'goodreads_review_sample_text_analysis.rds')
amazon <- amazon[sample(nrow(amazon), 160000), ]
saveRDS(amazon, 'amazon_review_sample_text_analysis.rds')



### Using overlapping titles dataset to create and attach single book ID to both datasets ###
# Create ID for each unique ASIN
overlap_titles <- rename(overlap_titles, goodreads_id = book_id)
overlap_titles <- transform(overlap_titles, book_id = as.numeric(factor(asin)))
# Merge samples with overlap_titles to get the same ID for the same books
overlap_titles_amazon <- overlap_titles %>% select(asin, book_id)
overlap_titles_amazon <- distinct(overlap_titles_amazon)
overlap_titles_goodreads <- overlap_titles %>% select(goodreads_id, book_id)
goodreads <- goodreads %>%
  mutate(goodreads_id = as.numeric(goodreads_id)) %>%
  left_join(overlap_titles_goodreads, by = "goodreads_id")
amazon <- amazon %>% left_join(overlap_titles_amazon, by = "asin")



### Merge datasets and prepare for cleaning ###
# Take necessary columns
goodreads <- goodreads %>% select(book_id, after, reviewText, Label, timestamp_formatted)
goodreads <- rename(goodreads, timestamp = timestamp_formatted)
amazon <- amazon %>% select(book_id, after, reviewText, Label, timestamp)

# Merge goodreads & amazon together
text_sample_final <- rbind(amazon, goodreads)

# Restore 'after' dummy to proper values (in this dataset, 'after'=1 was taken around March instead of July)
text_sample_final <- text_sample_final %>% mutate(
  after = case_when(
    timestamp < '2013-07-01' ~ 0,
    TRUE ~ 1
  )
)

# Save dataset
saveRDS(text_sample_final, 'text_sample_final.rds')



#### DATA CLEANING ####
# Clean any junk
text_sample_final <-
  text_sample_final %>%
  mutate(
    # remove links
    reviewText = str_remove_all(reviewText, "https\\S*"),
    reviewText = str_remove_all(reviewText, "http\\S*"),
    # remove annoying html stuff
    reviewText = str_remove_all(reviewText, "amp"),
    reviewText = str_remove_all(reviewText, "&S*"),
    reviewText = str_replace_all(reviewText, "&#x27;|&quot;|&#x2F;", "'"),
    reviewText = str_replace_all(reviewText, "<a(.*?)>", " "),
    reviewText = str_replace_all(reviewText, "&gt;|&lt;|&amp;", " "),
    reviewText = str_replace_all(reviewText, "&#[:digit:]+;", " "),
    reviewText = str_remove_all(reviewText, "<[^>]*>"),
    # remove numbers
    reviewText = str_remove_all(reviewText, "[:digit:]"),
    # remove excess whitespace
    reviewText = str_squish(reviewText),
    reviewText = str_trim(reviewText),
  ) %>%
  filter(count_words(reviewText) > 1) %>%
  rownames_to_column("id")




### Use VADER sentiment lexicon on dataset to obtain sentiment ###
vader_sample <- vader_df(text_sample_final$reviewText)
saveRDS(vader_sample, 'vader_sample.rds')

# Use compound score for negative/positive/neutral score:
vader_sample2 <- 
  vader_sample %>% 
  rowid_to_column("id") %>%
  select(id, compound) %>%
  mutate(
    vader_sample = case_when(
      compound > 0.05 ~ "positive",
      compound < -0.05 ~ "negative",
      TRUE ~ "neutral"
    )
  )

# Attach book IDs back
vader_sample2 <- vader_sample2 %>%
  mutate(id = as.character(id)) %>%
  left_join(text_sample_final, by = "id")

# Add time-fixed effects
first_date <- as.Date('1998-01-02')
vader_sample2$t <- (as.yearmon(vader_sample2$timestamp) - as.yearmon(first_date))*12
vader_sample2$t <- round(vader_sample2$t, 0)

# Add dominant genre for books to vader_sample2
goodreads_genres_filtered <- read_rds('goodreads_genres_filtered.rds')
vader_sample2 <- vader_sample2 %>% left_join(goodreads_genres_filtered, by = "book_id")

# Save dataset
saveRDS(vader_sample2, 'vader_sample2.rds')




## COMPUTE POS-NEG RATIO ON MONTH LEVEL
pnratio_m_amazon <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Amazon') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
saveRDS(pnratio_m_amazon, 'pnratio_m_amazon.rds')

pnratio <- read_rds('pnratio_m_amazon.rds')

pnratio_m_goodreads <- vader_sample2 %>%
  filter(vader_sample != 'neutral' & Label == 'Goodreads') %>%
  group_by(t, vader_sample) %>%
  summarise(N = n()) %>%
  mutate(pos_neg_ratio = N[2]/N[1])
saveRDS(pnratio_m_goodreads, 'pnratio_m_goodreads.rds')

pnratio2 <- read_rds('pnratio_m_goodreads.rds')








