
#load pre-defined scripts
source('UshaR.R')

#load required libraries
library(igraph)
library(tm)
library(xml)
library(RColorBrewer)

#get data
mapurl = 'https://bindup.crowdmap.com/'
data = get_all_incidents(mapurl)
incidents = data$incidentdescription

# create corpus
corpus = Corpus(VectorSource(incidents))

# term-document matrix
tdm2 = TermDocumentMatrix(corpus,control=list(removePunctuations=TRUE,tolower=TRUE,stopwords=stopwords('SMART')))

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
g3 = graph.adjacency(M2, weighted=TRUE, mode="undirected",
                     add.rownames=TRUE) 

# superimpose a cluster structure with k-means clustering
kmg = kmeans(M2, centers=8)
gk = kmg$cluster


# prepare ingredients for plot
V(g3)$size = 10
V(g3)$label = V(g3)$name
V(g3)$degree = degree(g3)
#V(g3)$label.cex = 1.5 * log10(V(g3)$degree)
V(g3)$label.color = hsv(0, 0, 0.2, 0.55)
V(g3)$frame.color = NA
V(g3)$color = gcols
E(g3)$color = hsv(0, 0, 0.7, 0.3)

# create nice colors for each cluster
gbrew = c("red", brewer.pal(8, "Dark2"))
gpal = rgb2hsv(col2rgb(gbrew))
gcols = rep("", length(gk))
for (k in 1:8) {
  gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
}


# plot
glay = layout.fruchterman.reingold(g3)
png(filename="graph.png")
plot(g3, layout=glay)
title("\nWord Relation",
      col.main="gray40", cex.main=1.5, family="serif")
      











