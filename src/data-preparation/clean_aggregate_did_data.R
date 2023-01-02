########################
### CLEAN DID DATA #####
########################



### Load libraries for data cleaning ###
library(tidyverse) # for compiling   Y
library(readr) # to read datasets   Y
library(lubridate) # for time stuff   Y
library(stringr) # for string stuff   Y
library(zoo) # for as.yearmon   Y

### TO DO CHECK WHICH LIBRARIES NEEDED AND REMOVE UNNECESSARY LIBRARIES

### Load data ###
# Ratings data
goodreads <- read_rds('../../data/goodreads_rev_overlap.Rds')
amazon <- read_rds('../../data/amazon_rev_overlap.rds')

# Data on which Amazon ASIN belongs to which Goodreads book ID
overlap_titles <- read_tsv('../../data/overlap_titles_amazon_gr.txt')

# How many times a book is added to a shelf --> data to select dominant genre
goodreads_genres <- read_tsv('../../data/goodreads_genres.txt')



### Cleaning Amazon rating data  ###
# Remove comma in reviewTime column
amazon$reviewTime <- gsub(",", "", amazon$reviewTime)

# Convert dates to yyyy-mm-dd
amazon$reviewTime <- mdy(amazon$reviewTime)

# Create column for year
amazon$year <- year(amazon$reviewTime)

# Create dummy "Acquisition", which takes 0 if before 1 July 2013, and 1 if on or after 1 July 2013
cutoff_date <- as.Date('2013-07-01')
amazon$acquisition <- ifelse(amazon$reviewTime < cutoff_date, 0, 1)
amazon$amazon_dummy <- 1
amazon$amazon_after <- amazon$acquisition * amazon$amazon_dummy



### Cleaning Goodreads rating data ###
# Format dates. Note: taking "date updated"
goodreads$date_updated <- str_sub(goodreads$date_updated, 5)
goodreads$date_updated <- str_replace(goodreads$date_updated, pattern = "-\\d+\\ ", replacement = "")
goodreads$date_updated <- as.Date(lubridate::parse_date_time(goodreads$date_updated,"mdTY"))
goodreads$date_updated <- as.Date(ymd(goodreads$date_updated))

# Create year column
goodreads$year <- year(goodreads$date_updated)

# Create dummy "Acquisition", which takes 0 if before 1 July 2013, and 1 if on or after 1 July 2013
goodreads$acquisition <- ifelse(goodreads$date_updated < cutoff_date, 0, 1)
goodreads$amazon_dummy <- 0
goodreads$amazon_after <- 0



### Using overlapping titles dataset to create and attach single book ID to both datasets ###
# Create ID for each unique ASIN
overlap_titles <- rename(overlap_titles, goodreads_id = book_id)
overlap_titles <- transform(overlap_titles, book_id = as.numeric(factor(asin)))

# Merge samples with overlap_titles to get the same ID for the same books
# Amazon dataset
overlap_titles_amazon <- overlap_titles %>% select(asin, book_id)
overlap_titles_amazon <- distinct(overlap_titles_amazon)
amazon <- amazon %>% left_join(overlap_titles_amazon, by = "asin")

# Goodreads dataset
overlap_titles_goodreads <- overlap_titles %>% select(goodreads_id, book_id)
goodreads <- rename(goodreads, goodreads_id = book_id)
goodreads <- goodreads %>% left_join(overlap_titles_goodreads, by = "goodreads_id")



### Add time fixed effect to both datasets ###
# Which dataset has oldest observation?
amazon %>% arrange(reviewTime) %>% head()
goodreads %>% arrange(date_updated) %>% head()
# Oldest: 1996-05-31
first_date <- as.Date('1996-05-31')
# Compute time fixed effect as number of months since first review
amazon$t <- (as.yearmon(amazon$reviewTime) - as.yearmon(first_date))*12
goodreads$t <- (as.yearmon(goodreads$date_updated) - as.yearmon(first_date))*12



### Create final datasets to be merged into one for DiD analysis ###
# Amazon
amazon_final <- amazon %>% select(book_id, year, reviewTime, overall, amazon_after, amazon_dummy, acquisition, t)
amazon_final <- rename(amazon_final, rating = overall)
amazon_final <- rename(amazon_final, date = reviewTime)
dir.create('../../gen')
dir.create('../../gen/data-preparation')
dir.create('../../gen/data-preparation/output')
saveRDS(amazon_final, '../../gen/data-preparation/output/amazon_final.rds')

# Goodreads
goodreads_final <- goodreads %>% select(book_id, year, date_updated, rating, amazon_after, amazon_dummy, acquisition, t)
goodreads_final <- rename(goodreads_final, date = date_updated)
saveRDS(goodreads_final, '../../gen/data-preparation/output/goodreads_final.rds')



### Merge final datasets into one for DiD analysis ###
final_data <- rbind(amazon_final, goodreads_final)



### Look for 'dominant' genre of each book using goodreads_genres dataset ###
# Filter for rows that have at least one value >=10
# Sum column values up to get a grasp of how often a book was shelved
goodreads_genres$count <- rowSums(goodreads_genres[3:12], na.rm = TRUE)

# Add book ID to each book computed earlier
goodreads_genres <- rename(goodreads_genres, goodreads_id = book_id)
goodreads_genres <- goodreads_genres %>%
  left_join(overlap_titles_goodreads, by = "goodreads_id")

# Filter for books that are not in the overlap_titles dataset
goodreads_genres <- goodreads_genres %>% drop_na(book_id)

# Change values of NA to 0
goodreads_genres[is.na(goodreads_genres)] = 0

# Drop goodreads_id column
goodreads_genres <- goodreads_genres %>% select(-goodreads_id)

# For multiple observations of same book ID, count column values together
goodreads_genres <- aggregate(.~book_id, data = goodreads_genres, FUN = sum)

# Remove books who have been shelved <10 times summed over all genres  
goodreads_genres <- goodreads_genres %>% filter(count >= 10)

# Compute dominant genre
temp <- goodreads_genres[3:12]
goodreads_genres$dominant_genre <- colnames(temp)[apply(temp,1,which.max)]
# Validation check
goodreads_genres %>% group_by(dominant_genre) %>% summarize(n = n())
# Take columns of interest for merge with DiD dataset
goodreads_genres_filtered <- goodreads_genres %>% select(book_id, dominant_genre)
# Save dataset
saveRDS(goodreads_genres_filtered, '../../gen/data-preparation/output/goodreads_genres_filtered.rds')

# Merge with final_data
final_data <- final_data %>% left_join(goodreads_genres_filtered, by = "book_id")
saveRDS(final_data, '../../gen/data-preparation/output/final_diff_in_diff_data.rds')
