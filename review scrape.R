# SESSION ----------------------------------------------

library(rvest)
library(tidyverse)
library(stringr)
library(tidytext)
library(ngram)

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
foo <- reviews[str_length(reviews) <= 140]

# remove empty or NA strings
foo <- foo[!is.na(foo) & foo != ""]

# remove punctuation
# foo <- gsub('[[:punct:] ]+',' ', foo)

# remove duplicates
foo <- foo[!duplicated(foo)]

# turn too datframe
foo <- as_data_frame(foo)

# TEXT MINING ---------------------------------------------------

# get unique words
words <- foo %>%
  unnest_tokens(word, value, token = "ngrams", to_lower = TRUE, n = 1)

# get word counts
word_counts <- count(words, word, sort = TRUE) %>% filter(word != "")

# get sentence openers
openers <- str_extract(foo$value, '\\w*') %>% str_to_lower() %>% as_data_frame()

# get opener counts
opener_counts <- count(openers, value, sort = TRUE) %>% rename(word=value) %>% filter(word != "")

# get words preceding commas
comma_precede <- str_split_fixed(foo$value, ',', 5)  %>% as_data_frame() %>% filter(`V1` != "")

a <- comma_precede %>% filter(V2 != "") 
b <- a %>% filter(`V3` != "") %>% select(`V2`) %>% as.vector()
c <- a %>% filter(`V4` != "") %>% select(`V3`)
d <- a %>% filter(`V5` != "") %>% select(`V4`)
comma_precede <- c(a$V1, b$V2, c$V3, d$V4) %>% as_data_frame()

# get last words preceding commas
comma_precede$value <- word(comma_precede$value, -1)

# make lower case
comma_precede$value <- tolower(comma_precede$value)

# get word proceeding comma counts
comma_precede_counts <- count(comma_precede, value, sort = TRUE) %>% rename(comma_n=n)

# join to abs word counts
word_counts <- left_join(word_counts, comma_precede_counts, by=c("word"="value"))
opener_counts <- left_join(opener_counts, comma_precede_counts, by=c("word"="value"))

# probability of comma after word
word_counts$comma_prob <- round((word_counts$comma_n / word_counts$n) * 100)
opener_counts$comma_prob <- round((opener_counts$comma_n / opener_counts$n) * 100)

# create bigrams
bigrams <- foo %>%
  unnest_tokens(bigram, value, token = "ngrams", to_lower = TRUE, n = 2) %>% 
  # separate bigram col
  separate(bigram, c("word1", "word2"), sep = " ")
  # filter(!word1 %in% stop_words$word) %>%
  # filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams %>% 
  count(word1, word2, sort = TRUE)

# create bigrams
trigrams <- foo %>%
  unnest_tokens(trigram, value, token = "ngrams", to_lower = TRUE, n = 3) %>% 
  # separate bigram col
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
  # filter(!word1 %in% stop_words$word) %>%
  # filter(!word2 %in% stop_words$word) %>%
  # filter(!word3 %in% stop_words$word)

# new bigram counts:
trigram_counts <- trigrams %>% 
  count(word1, word2, word3, sort = TRUE)