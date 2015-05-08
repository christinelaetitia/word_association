
#load required libraries
library(igraph)
library(RSQLite)

#set-up DB
drv <-dbDriver(drv = 'SQLite')
con <-dbConnect(drv,'/home/chris/umati/twitter2.sqlite')

#get data from DB
sql <-"SELECT * FROM tweet_data WHERE [text] like '%garissa%' LIMIT 200"
data <-dbGetQuery(con,sql)

#replies
reply <-is.na(data$in_reply_to_status_id_str)
data$reply <-reply
sub <-subset(data,data$reply == FALSE)

test <-sub[c('screen_name','in_reply_to_screen_name')]

user1 <-unique(test$screen_name)
user2 <-unique(test$in_reply_to_screen_name)
users <-as.data.frame(x = c(user1,user2),row.names = c(user1,user2),optional = FALSE)

#create adjacency matrix
mat <-matrix(nrow = nrow(users),ncol = nrow(users))
colnames(mat) <- rownames(users)
rownames(mat) <-rownames(users)


#function to get replied tweet
response <- function(i){
  return(subset(test,test$screen_name == (rownames(mat)[i]))$in_reply_to_screen_name)
}

#get index function
getIndex <-function(mat,r){
  
  #creat indexing matrix
  m <-col(mat)
  colnames(m) <- rownames(mat)
  rownames(m) <-rownames(mat)
  m <-as.data.frame(m)
  
  #get index
  frame <-c()
  for(item in r){
    g <-rbind(grep(pattern = item,x = names(m)))
    frame <-c(frame,g)
    frame <-unique(frame)
  }
  return(frame)
}

#populate matrix
for (k in 1:nrow(mat)){
  z <-c(1:ncol(mat))
  ind <-getIndex(mat,response(k))
  if(length(ind) == 0){
    mat[k,1:ncol(mat)] = 0
  } else {
    mat[k,ind] = 1
    mat[k,z[-c(ind)]] = 0
  }
  mat[k,k] = 0
}

graph <-graph.adjacency(adjmatrix = mat,mode = 'undirected',diag = FALSE,colnames(mat),rownames(mat))
plot(graph)





