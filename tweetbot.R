# SESSION ----------------------------------------------

library(tidyverse)
library(stringr)
library(tidytext)

setwd("~/Documents/Github/hardwax_bot")

reviews <- read_csv("reviews.csv")

# TEXT MINING ---------------------------------------------------

# get unique words
words <- reviews %>%
  unnest_tokens(word, value, token = "ngrams", to_lower = TRUE, n = 1)

# get word counts
word_counts <- count(words, word, sort = TRUE) %>% filter(word != "")

# get sentence openers
openers <- str_extract(reviews$value, '\\w*') %>% str_to_lower() %>% as_data_frame()

# get opener counts
opener_counts <- count(openers, value, sort = TRUE) %>% rename(word=value) %>% filter(word != "")

# get words preceding commas
comma_precede <- str_split_fixed(reviews$value, ',', 5)  %>% as_data_frame() %>% filter(`V1` != "")

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
word_counts$comma_prob[is.na(word_counts$comma_prob)] <- 0
opener_counts$comma_prob[is.na(opener_counts$comma_prob)] <- 0

# create bigrams
bigrams <- reviews %>%
  unnest_tokens(bigram, value, token = "ngrams", to_lower = TRUE, n = 2) %>% 
  # separate bigram col
  separate(bigram, c("word1", "word2"), sep = " ")
# filter(!word1 %in% stop_words$word) %>%
# filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams %>% 
  count(word1, word2, sort = TRUE)

# create bigrams
trigrams <- reviews %>%
  unnest_tokens(trigram, value, token = "ngrams", to_lower = TRUE, n = 3) %>% 
  # separate bigram col
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
# filter(!word1 %in% stop_words$word) %>%
# filter(!word2 %in% stop_words$word) %>%
# filter(!word3 %in% stop_words$word)

# new bigram counts:
trigram_counts <- trigrams %>% 
  count(word1, word2, word3, sort = TRUE)

# NEXT WORD PREDICTION -------------------------------------------

# capitalise first letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# function to return third word
return_third_word <- function(woord1, woord2){
  
  # sample a word to add to first two words
  woord <- trigram_counts %>%
    filter_(~word1 == woord1, ~word2 == woord2) %>%
    sample_n(1, weight = n) %>%
    .[["word3"]]
  
  # if no word found in trigram, resort to bigram
  if(length(woord) == 0){
    bleh <- filter_(bigram_counts, ~word1 == woord2) %>%
      sample_n(1, weight = n)
    warning("no word found, adding ", bleh, "to", woord1 , woord2)
    woord <- bleh
  }
  
  # print
  woord
}

# sentence generator
generate_sentence <- function(word1, word2, sentencelength, debug =FALSE){
  
  # comma chance sample
  commas <- sample(0:100, 1)
  
  #input validation
  if(sentencelength <3)stop("I need more to work with")
  sentencelength <- sentencelength -2
  
  # starting to add words
  if(commas <= as.numeric(word1$comma_prob)) {
    sentence <- paste(word1$word, ", ", word2$word, sep="")
  } else {
    sentence <- c(word1$word, word2$word)
  }
  
  woord1 <- word1$word
  woord2 <- word2$word
  for(i in seq_len(sentencelength)){
    
    commas <- sample(0:100, 1)
    
    if(debug == TRUE)print(i)
    word <- return_third_word( woord1, woord2)
    
    word <- left_join(as_data_frame(word), word_counts, by=c("value"="word"))
    
    if(commas <= as.numeric(word$comma_prob)) {
      sentence <- paste(sentence, ", ", word$value[1], sep="")
    } else {
      sentence <- c(sentence, word$value)
    }
    
    woord1 <- woord2
    woord2 <- word$value[1]
  }
  
  # paste sentence together
  output <- paste(sentence, collapse = " ")
  
  # add tip sometimes
  tip_n <- sample(1:20, 1)
  if(tip_n %in% c(1, 2)){
    output <- paste(output, "- TIP!")
  } else if(tip_n %in% c(3, 4)){
    output <- paste(output, "(one per customer)")
  } else if(tip_n %in% c(5)){
    output <- paste(output, "- Killer!")
  } else if(tip_n %in% c(6, 7)){
    output <- paste(output, "- Warmly Recommended!")
  } else if(tip_n %in% c(8, 9)){
    output <- paste(output, "- Highly Recommended!")
  } else if(tip_n %in% c(10, 11)){
    output <- paste(output, "(w/ download code)")
  }
  
  # print
  firstup(output)
}


# GENERATOR -------------------------------------------------


# generate sentence
reviewer <- function(x) {
  success <- FALSE
  while (!success) {
    # do something
    a <- sample_n(opener_counts, size=1, weight = n)
    b <- sample_n(word_counts, size=1, weight = n)
    len <- sample(5:12, 1, replace = TRUE)
    review <- generate_sentence(word1=a, word2=b, sentencelength=len)
    # check for success
    success <-  str_length(review) > 0
  }
  return(review)
}

reviewer()



