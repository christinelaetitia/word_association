
#load required libraries
library(tm)

#create a term-document matrix
CreateTdm <- function(text.data, stopwords='english', 
                      removePunctuation=TRUE, tolower=TRUE) {
  # create corpus
  corpus <-Corpus(VectorSource(incidents))
  
  # create term document matrix 
  tdm <-TermDocumentMatrix(corpus, 
                           control=list(removePunctuation=removePunctuation, 
                                        stopwords=stopwords('SMART'), tolower=tolower))
  
  return(tdm)
}

#calculate lower correlation limit for association mining
GetLowerCorrLimit <- function(text.data, factor=1.0) {
  # compute optimal lower correlation  limit
  tokens = MC_tokenizer(text.data)
  tokens.unique.len = length(unique(tokens))
  corr.limit = tokens.unique.len / length(tokens) * factor
  
  return(corr.limit)
}

#remove hyper-link from text
RemoveLink <-function(text){
  text = gsub("http.*", "", text)
  return(text)
}
