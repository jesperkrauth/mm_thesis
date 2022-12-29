# Thesis MSc Marketing Management (MM): the Amazon-Goodreads acquisition
This repository contains the code used in the analysis part of my master's thesis for the MSc Marketing Management at Tilburg University.

## Repository overview

```
- src
  - analysis
  - data-preparation
- README.md
```

## Dependencies

- R. [Installation guide](https://tilburgsciencehub.com/building-blocks/configure-your-computer/statistics-and-computation/r/).

- For R, make sure you have installed the following packages:
```
library(broom)
library(lubridate)
library(fixest)
library(readr)
library(stringr)
library(textdata) (maybe not?)
library(tidytext) (maybe not?)
library(tidyverse)
library(tokenizers)
library(vader)
library(yardstick) (maybe not?)
library(zoo)
```

## Running the code
### Step-by-step
To generate the outputs used in the thesis, follow these instructions:
1. Obtain the datasets used in this thesis. The following datasets were provided by Tilburg University and are used in this research: 
```
- amazon_rev_overlap: a dataset containing book ratings on Amazon
- goodreads_rev_overlap: a dataset containing book ratings on Goodreads
- goodreads_genres: a dataset indicating how often a book has been 'shelved' on a Goodreads genre shelf to determine the 'dominant' genre of a book
- labeled_am_with_dates: a dataset containing a sample of Amazon book reviews' text
- labeled_gr_with_dates: a dataset containing a sample of Goodreads book reviews' text
- overlap_titles_amazon_gr: a dataset indicating which Amazon ASIN corresponds to which Goodreads book ID
```
2. Run src/data-preparation/clean_aggregate_did_data.R to generate the dataset used in the difference-in-differences analysis.
3. Run src/data-preparation/clean_sentiment_data.R to generate the dataset used in the sentiment text analysis.
4. Run src/analysis/data_chapter.R to generate the plots and table data used in the "Data" chapter.
5. Run src/analysis/results_chapter.R to generate the plots and table data used in the "Results" chapter.


## Authors
- [Jesper Krauth](https://github.com/jesperkrauth),         e-mail: j.krauth@tilburguniversity.edu 
