
#load pre-defined scripts
source('text_process.R')
source('UshaR.R')

#load required libraries
library(igraph)
library(arules)
library(tm)

#get data
mapurl = 'https://bindup.crowdmap.com/'
data = get_all_incidents(mapurl)
incidents = data$incidentdescription


#create term-document matrix
tdm <-CreateTdm(incidents)

a = MC_tokenizer(incidents)
low_freq <- round(length(a)/length(unique(a)))*2

#find nost frequent terms
f = findFreqTerms(x=tdm,lowfreq=low_freq)

# find associations for keyword
factor_number = 10000-nrow(data)
limit <- GetLowerCorrLimit(text.data=tdm,factor=factor_number)
s <- findAssocs(tdm,f[1],limit)

assoc_words = c()
for (i in 1:10){
  s <- findAssocs(tdm,f[i],limit)
  assoc_words = rbind(assoc_words,rownames(s)[1:10])
}

#transpose
words = t(assoc_words)
r = t(f[1:10])
w = as.data.frame(rbind(words,f[1:10]))
colnames(w) = f[1:10]
w$asterick = c(r,'asterick')

#extract word tokens from data frame
tokens = c()
for(j in 1:11){
  tok = MC_tokenizer(w[,j])
  tokens = c(tokens,tok)
}

#get unique words tokens
unique_tokens <-unique(tokens)

#create an adjacency matrix
mat <-matrix(nrow=length(unique_tokens),ncol=length(unique_tokens))
colnames(mat) <-unique_tokens
rownames(mat) <-unique_tokens

for(k in 1:ncol(mat)){
  for (l in 1:nrow(mat)){
    g <-grep(colnames(mat)[k],colnames(w))
    while(length(g !=0)){
        mat[k,g] = 1
      }
    } 
  }
}




