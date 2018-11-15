library(XML)
library(easyPubMed)
library(ggplot2)

############### PART 1: Information extraction ###############

## Dataset importation
#---------------------
# Option 1: 698 documents via une requete
#----------
# Querry_String <- "AIDS"
# Ids <- get_pubmed_ids(Querry_String)
# papers <- fetch_pubmed_data(Ids)

# Option 2: 52349 documents via importation du fichier xml.
#----------
papers <- xmlParse(file = "pubmed18n0924.xml")


## Information Extraction from dataset ("papers")
#------------------------------------------------
xmltop = xmlRoot(papers) # top node of "papers" xml structure
Article_Num <- xmlSize(xmltop) # number of nodes (Articles) "in papers"
# xmlSApply(xmltop[[1]], xmlName) # shows names of child nodes

ID <- vector()
Abstract <- vector()
Title <- vector()
Date <- vector()
Author_lastname <- vector()
Author_forename <- vector()
Author <- vector()

for (i in 1:Article_Num) {
  ID[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["PMID"]])
  Abstract[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["Abstract"]])
  Title[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]])
  Date[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleDate"]])
  Author_lastname[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["LastName"]])
  Author_forename[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["ForeName"]])
  Author[i] <- paste(Author_lastname[i],Author_forename[i])
}

rm(papers)

# create dataframe
df <- data.frame(ID, Abstract, Title, Date, Author)
rm(ID, Abstract, Title, Date, Author, Author_forename, Author_lastname)
# export dataframe
# saveRDS(df, file = "Dataframe", ascii = FALSE, version = NULL,
#         compress = TRUE, refhook = NULL)



# Remove Na's and too long or too short Abstracts.
df <- df[complete.cases(df[ , 2]),] 
df <- df[nchar(as.character(df[ , 2]))<3000 & nchar(as.character(df[ , 2]))>100,] 

# visualize abstract lengths
a<- vector()
for (i in 1:length(df$Abstract)) {a[i]<-nchar(as.character(df$Abstract[i]))}
plot(a)
rm(a)


############### PART 2: text mining  ###############

## Bag of words approach:
#------------------------

library(quanteda)

Abstract <- as.character(df$Abstract)

NbrDoc <- 1000
Abstract <- Abstract[1:NbrDoc]

# Tokenize
tokens <- tokens(Abstract, what = "word", 
                 remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE, remove_hyphens = FALSE)

# for bigrams.
# test.tokens <- tokens_ngrams(test.tokens, n = 1:2)

# minimize capital letters
tokens <- tokens_tolower(tokens)

# Stopwords
stop<-stopwords()
new_stopwords<-append(stop,c("fig.","eq.","e.g"))
tokens <- tokens_select(tokens, new_stopwords, selection = "remove")
tokens <- tokens_select(tokens,min_nchar = 3, selection ="keep")

# Steming
# tokens <- tokens_wordstem(tokens, language = "english")
# print(tokens)

# Create our first bag-of-words model dataframe.
# tokens <- tokens[1:(length(tokens)/2)]
tokens.dfm <- dfm(tokens)

# Transform to a matrix and inspect.
# tokens.matrix <- as.matrix(tokens.dfm)
library(Matrix)
isValue <- tokens.dfm != 0
iValue <- rowSums(isValue)
isparse <- vector(length = sum(isValue))
k <- 1
for (i in (1:dim(tokens.dfm)[1])){
  l <- k + iValue[i] -1
  isparse[k:l] <- i
  k <- k + iValue[i]
}

jValue <- colSums(isValue)
jsparse <- vector(length = sum(isValue))
k <- 1
for (i in (1:dim(tokens.dfm)[2])){
  l <- k + jValue[i] -1
  jsparse[k:l] <- i
  k <- k + jValue[i]
}

Data <- tokens.dfm[isValue]

sparseTokens <- sparseMatrix(isparse,jsparse,x=Data)

tokens.matrix <- sparseTokens
# Tokenfrequence visualizations
# In corpus
# freq <- sort(colSums(tokens.matrix), decreasing=TRUE)
# wf <- data.frame(word=names(freq), freq=freq)
# 
# # In specific document
# Doc<-5
# freqInDoc <- sort(tokens.matrix[Doc,], decreasing=TRUE)
# wfindoc <- data.frame(word=names(freqInDoc), freq=freqInDoc)

# plot word frequence
# in corpus:
# pl <- ggplot(subset(wf, freq > 1) ,aes(word, freq))

# in specific doc:
# # pl <- ggplot(subset(wfindoc, freq > 1) ,aes(word, freq))

# make plot:
# pl <- pl + geom_bar(stat="identity", fill="darkred", colour="white")
# pl + theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("Uni-Gram Frequency")

# Word Cloud visualization
# library(wordcloud)
# set.seed(100)
# wordcloud(names(freq), freq, min.freq=2, colors=brewer.pal(6, "Dark2"))


## analizing Tokens:
#-------------------

# Our function for calculating relative term frequency (TF)
term.frequency <- function(row) {
  row / sum(row)
}

# Our function for calculating inverse document frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  log10(corpus.size / doc.count)
}

# Our function for calculating TF-IDF.
tf.idf <- function(tf, idf) {
  tf * idf
}



# First step, normalize all documents via TF.
# tokens.df <- apply(tokens.matrix, 1, term.frequency) -> breaks sparse matrix
tokens.tf <- term.frequency(tokens.matrix)

# Second step, calculate the IDF vector that we will use - both
# tokens.idf <- apply(tokens.matrix, 2, inverse.doc.freq)
tokens.idf <- inverse.doc.freq(tokens.matrix)

# Lastly, calculate TF-IDF for our training corpus.
# tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)
tokens.tfidf <- tf.idf(tokens.tf,tokens.idf)

# Transpose the matrix
# tokens.tfidf <- t(tokens.tfidf)

# Check for incopmlete cases.
# incomplete.cases <- which(!complete.cases(tokens.tfidf)) -> doesn't work on sparse matrix
# Abstract[incomplete.cases]

# Fix incomplete cases
# tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(tokens.tfidf))
# dim(tokens.tfidf)
# sum(which(!complete.cases(tokens.tfidf)))

# Make a clean data frame.
# tokens.tfidf.df <- as.data.frame(as.matrix(tokens.tfidf))
# names(tokens.tfidf.df) <- make.names(names(tokens.tfidf.df))



## Perform SVD. Specifically, reduce dimensionality down to 'nv' columns
#-----------------------------------------------------------------------
library(irlba)

# for our latent semantic analysis (LSA).
irlba <- irlba(tokens.tfidf, nv = 5, maxit = 1000)

# Take a look at the new feature data up close.
# View(irlba$v)


## Make plots:

# line names
rownames(irlba$v) <- colnames(tokens.dfm)
rownames(irlba$u) <- row.names(tokens.dfm)

# 2D Plot:
# plot(eig1,eig2,col="blue")
# text(eig1,eig2,row.names(irlba$v), cex=0.6, pos=4, col="red")

# 3D Plot:
# library("scatterplot3d")

# s3d<-scatterplot3d(eig1,eig2,eig3, pch = 16, color="steelblue")
# text(s3d$xyz.convert(eig1,eig2,eig3), labels = rownames(irlba$v),
#      cex= 0.7, col = "red")

# library(car) # faut aussi installer lib("rgl")
# # 3D plot with the regression plane
# scatter3d(x = eig1, y = eig2, z = eig3)


# topics Visualization
Topics <- vector(length = dim(irlba$v)[1])
TopicsWords <- matrix("txt",nrow = 10, ncol = dim(irlba$v)[2])
ColTag <- vector(length = dim(irlba$v)[2])
for (i in (1:dim(irlba$v)[2])) {
  # sort tokens in each dimension (to find the most relevant words in each topic)
  Topics <- irlba$v[,i]
  names(Topics) <- row.names(irlba$v)
  Topics <- Topics[order(-Topics),drop=FALSE]
  # build the table with words
  TopicsWords[,i] <- names(Topics[1:10])
  ColTag[i] = paste('Topic ',i)
}
colnames(TopicsWords) <- ColTag
View(TopicsWords)


############### Querries (based on SVD) ###############
#------------------------------------------------------

# give a positive querry: as a vector of strings ('querry','querry',...)
posQuerry_String <- c('cancer')
flag <- match(posQuerry_String, rownames(irlba$v))
try(if(is.na(flag) == TRUE) stop("Query not found"))
# give a negative querry:
negQuerry_String <- c('pancreatic')
flag <- match(negQuerry_String, rownames(irlba$v))
try(if(is.na(flag) == TRUE) stop("Query not found"))

posIndex <- vector(length = length(posQuerry_String))
for (i in (1:length(posQuerry_String))) {
  posIndex[i] <- match(posQuerry_String[i], rownames(irlba$v))
}
eig_posQuerry <- irlba$v[posIndex,]

if (length(negQuerry_String)>1 | negQuerry_String != ''){
  negIndex <- vector(length = length(negQuerry_String))
  for (i in (1:length(negQuerry_String))) {
    negIndex[i] <- match(negQuerry_String[i], rownames(irlba$v))
  }
  eig_negQuerry <- irlba$v[negIndex,]
}

# 
# s3d<-scatterplot3d(eig1,eig2,eig3, pch = 16, color="steelblue")
# text(s3d$xyz.convert(eig1,eig2,eig3), labels = rownames(irlba$v),
#      cex= 0.7, col = "green")
# s3d$points3d(eig_posQuerry[1],eig_posQuerry[2],eig_posQuerry[3],pch=16,color="red")

# This function computes the euclidean distance between the querry and each document
euc.dist <- function(docs,querry){ 
  dimDocs <- dim(docs)
  squareSum <- 0
  euc.dist <- vector(length=dimDocs[1])
  for (i in (1:dimDocs[1])) {
    for (j in (1:dimDocs[2])){ # TODO: remove the loop and calculate with the whole vectors
      squareSum <- squareSum + (docs[i,j] - querry[j])^2
    }  
    euc.dist[i] <- squareSum ^ 0.5
    squareSum <- 0
  }
  return(euc.dist)
}
# Calculate distance, order and name the rows
posdistMatrix <- matrix(nrow = length(posQuerry_String),ncol=dim(irlba$u)[1])
posdist <- rep(1,length = dim(irlba$u)[1])
if (length(posQuerry_String) > 1){
  for (i in (1:length(posQuerry_String))) {
    posdistMatrix[i,] <- euc.dist(irlba$u, eig_posQuerry[i,])
    posdist <- posdist + posdistMatrix[i,]
  }
}else{posdist <- euc.dist(irlba$u, eig_posQuerry)}

if (negQuerry_String[1] != ""){
  negdistMatrix <- matrix(1L,nrow = length(negQuerry_String),ncol=dim(irlba$u)[1])
  negdist <- rep(1,length = dim(irlba$u)[1])
  if (length(negQuerry_String) > 1){
    for (i in (1:length(negQuerry_String))) {
      negdistMatrix[i,] <- euc.dist(irlba$u, eig_negQuerry[i,])
      negdist <- negdist + negdistMatrix[i,]
    }
  }else{negdist <- euc.dist(irlba$u, eig_negQuerry)}
  distMatrix <- 0.8*posdist - 0.2*negdist
}else distMatrix <- posdist

names(distMatrix) <- rownames(irlba$u)
distMatrix <- distMatrix[order(distMatrix),drop=FALSE]


# Compute 10 most relevant (tf-idf) words in each documents
# bestWords <- function(tokens.tfidf,docId,irlba){
#   docTfidf <- tokens.tfidf[docId,]
#   names(docTfidf) <- rownames(irlba$v)
#   docTfidf <- docTfidf[is.na(docTfidf) == FALSE]
#   docTfidf <- docTfidf[order(-docTfidf),drop=FALSE]
#   return(names(docTfidf[1:10]))
# }

# matrix with 10 most relevant keywords of the 10 nearests documents 
names <- names(distMatrix)
Result <- matrix(nrow = 10,ncol = 10)
colnames(Result) <- names[1:10]
for (i in (1:10)) {
  index <- match(names[i],rownames(irlba$v))
  Result[i,] <- bestWords(tokens.tfidf,index,irlba)
}
View(Result)

# Clustering ?
#--------------
# res <- kmeans(irlba$v,centers = 2)
# plot(irlba$v,col = res$cluster , pch = res$cluster)
# points(res$centers, col = 1:5, pch = 8)
