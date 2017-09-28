# SESSION ----------------------------------------------

library(rvest)
library(tidyverse)
library(stringr)
library(tidytext)

setwd("~/Documents/Github/hardwax_bot")


# FUNCTIONS --------------------------------------------

# web scraper for hardwax reviews
hardwax_scrape <- function(page, no) {
  
  # construct url
  x <- paste0("https://hardwax.com/", page, "/?page=", no, "&paginate_by=50")
  
  # scrape reviews
  reviews <- x %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  
  return(reviews)
  
}


# SCRAPE --------------------------------------------------------

# scrape news
news <- lapply(seq_along(1:2), hardwax_scrape, page="this-week") %>% unlist()

# scrape last weeks news
last_news <- lapply(seq_along(1:3), hardwax_scrape, page="last-week") %>% unlist()

# scrape back in stock
back_in_stock <- lapply(seq_along(1:15), hardwax_scrape, page="back-in-stock") %>% unlist()

# scrape downloads
downloads <- lapply(seq_along(1:202), hardwax_scrape, page="downloads") %>% unlist()

# get electro
electro <- lapply(seq_along(1:11), hardwax_scrape, page="electro") %>% unlist()

# get grime
grime <- lapply(seq_along(1:22), hardwax_scrape, page="grime") %>% unlist()

# get techno
techno <- lapply(seq_along(1:66), hardwax_scrape, page="techno") %>% unlist()

# get house
house <- lapply(seq_along(1:42), hardwax_scrape, page="house") %>% unlist()

# bind all of these
reviews <- c(news, last_news, back_in_stock, downloads, electro, grime, techno, house)


# CLEAN ---------------------------------------------------------

# check strings 140 chrs at most
reviews <- reviews[str_length(reviews) <= 140]

# remove empty or NA strings
reviews <- reviews[!is.na(reviews) & reviews != ""]

# remove punctuation
# foo <- gsub('[[:punct:] ]+',' ', foo)

# remove duplicates
reviews <- reviews[!duplicated(reviews)]

# turn too datframe
reviews <- as_data_frame(reviews)

write_csv(reviews, "reviews.csv")
