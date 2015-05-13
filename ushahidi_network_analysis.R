
#load pre-defined scripts
source('text_process.R')
source('UshaR.R')

#load required libraries
library(igraph)
library(tm)

#get data
mapurl = 'https://bindup.crowdmap.com/'
data = get_all_incidents(mapurl)
incidents = data$incidentdescription

# create corpus
corpus = Corpus(VectorSource(incidents))

# term-document matrix
tdm2 = TermDocumentMatrix(corpus)

# convert tdm to matrix
m2 = as.matrix(tdm2)

# word counts
wc = rowSums(m2)

# get those words above the 3rd quantile
lim = quantile(wc, probs=0.5)
good = m2[wc > lim,]

# remove columns (docs) with zeroes
good = good[,colSums(good)!=0]

# adjacency matrix
M2 = good %*% t(good)

# set zeroes in diagonal
diag(M2) = 0

# graph
g = graph.adjacency(M2, weighted=TRUE, mode="undirected",
                    add.rownames=TRUE)
# layout
glay = layout.fruchterman.reingold(g)
