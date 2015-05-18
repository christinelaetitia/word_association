
#load pre-defined scripts
source('UshaR.R')
source('text_process.R')

#load required libraries
library(igraph)
library(arules)
library(tm)
library(xml)
library(RColorBrewer)

#get data
mapurl = 'http://www.hatari.co.ke/'
data = get_all_incidents(mapurl)
incidents = data$incidentdescription

# create corpus
corpus = Corpus(VectorSource(incidents))

# term-document matrix
tdm= TermDocumentMatrix(corpus,control=list(removePunctuations=TRUE,tolower=TRUE,stopwords=stopwords('SMART')))

# convert tdm to matrix
m= as.matrix(tdm)

# word counts
wc = rowSums(m)

# get  words above the 3rd quantile
lim<- GetLowerCorrLimit(text.data=tdm,factor=factor_number) & quantile(wc, probs=0.5 )
good = m[wc > lim,]

# remove columns (docs) with zeroes
good = good[,colSums(good)!=0]

# adjacency matrix
M = good %*% t(good)

# set zeroes in diagonal
diag(M) = 0

# graph
g= graph.adjacency(M, weighted=TRUE, mode="undirected",
                     add.rownames=TRUE)

# superimpose a cluster structure with k-means clustering
kmg = kmeans(M, centers=8)
gk = kmg$cluster

# prepare ingredients for plot
V(g)$size = 10
V(g)$label = V(g)$name
V(g)$degree = degree(g)
V(g)$label.cex = 1.5 * log10(V(g)$degree)
V(g)$label.color = hsv(0, 0, 0.2, 0.55)
V(g)$frame.color = NA
V(g)$color = gcols
E(g)$color = hsv(0, 0, 0.7, 0.3)

# create nice colors for each cluster
gbrew = c("red", brewer.pal(8, "Dark2"))
gpal = rgb2hsv(col2rgb(gbrew))
gcols = rep("", length(gk))
for (k in 1:8) {
  gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
}


# plot
glay = layout.fruchterman.reingold(g)
plot(g, layout=glay)
title("\nWord Relation",
      col.main="gray40", cex.main=1.5, family="serif")
      

#write to disk
write.csv(M,'data.csv')











