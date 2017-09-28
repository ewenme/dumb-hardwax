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
generate_sentence <- function(word1, word2, sentencelength=5, debug =FALSE){
  
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
    if(debug == TRUE)print(i)
    word <- return_third_word( woord1, woord2)
    sentence <- c(sentence, word)
    woord1 <- woord2
    woord2 <- word
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


# first word sample (from openers)
a <- sample_n(opener_counts, size=1, weight = n)

# second word sample
b <- sample_n(word_counts, size=1, weight = n)

# sentence length sample
len <- sample(5:10, 1)

# comma chance sample
commas <- sample(0:100, 1)

# generate sentence
generate_sentence(word1=a, word2=b, 
                  sentencelength=len)
