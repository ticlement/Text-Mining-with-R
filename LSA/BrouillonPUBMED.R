library(XML)
library(easyPubMed)
library(ggplot2)
library(bigmemory)

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

ptm <- proc.time()
# info extraction
for (i in 1:Article_Num) {
  ID[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["PMID"]])
  Abstract[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["Abstract"]])
  Title[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]])
  Date[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleDate"]])
  Author_lastname[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["LastName"]])
  Author_forename[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["ForeName"]])
  Author[i] <- paste(Author_lastname[i],Author_forename[i])
}
proc.time() - ptm
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
# a<- vector()
# for (i in 1:length(df$Abstract)) {a[i]<-nchar(as.character(df$Abstract[i]))}
# plot(a)
# rm(a)

############### PART 2: text mining  ###############

## Bag of words approach:
#------------------------

library(quanteda)

Abstract <- as.character(df$Abstract)

# NbrDoc <- 10000
# Abstract <- Abstract[1:NbrDoc]

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
# tokens <- tokens_wordstem(tokens[1,3], language = "english")
# print(tokens)

# Create our first bag-of-words model dataframe.
tokens.matrix <- dfm(tokens)

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
tokens.tf <- term.frequency(tokens.matrix)

# Second step, calculate the IDF vector that we will use - both
tokens.idf <- inverse.doc.freq(tokens.matrix)

# Lastly, calculate TF-IDF for our training corpus.
tokens.tfidf <- tf.idf(tokens.tf,tokens.idf)

## Perform SVD. Specifically, reduce dimensionality down to 'nv' columns
#-----------------------------------------------------------------------

library(irlba)

irlba <- irlba(tokens.tfidf, nv = 100, maxit = 1000)

# line names
rownames(irlba$v) <- colnames(tokens.dfm)
rownames(irlba$u) <- row.names(tokens.dfm)

# topics Visualization

Topics <- vector(length = dim(irlba$v)[1])
TopicsWords <- matrix("txt",nrow = 50, ncol = dim(irlba$v)[2])
ColTag <- vector(length = dim(irlba$v)[2])
for (i in (1:dim(irlba$v)[2])) {
  # sort tokens in each dimension (to find the most relevant words in each topic)
  Topics <- irlba$v[,i]
  names(Topics) <- row.names(irlba$v)
  Topics <- Topics[order(-Topics),drop=FALSE]
  # build the table with words
  TopicsWords[,i] <- names(Topics[1:50])
  ColTag[i] = paste('Topic ',i)
}
colnames(TopicsWords) <- ColTag
View(TopicsWords)


############### Queries (based on SVD) ###############
#------------------------------------------------------

# give a positive query: as a vector of strings ('querry','querry',...)
posQuerry_String <- c('kidney')
# posQuerry_String <- stemDocument(posQuerry_String) # IF STEMMING
flag <- match(posQuerry_String, rownames(irlba$v))
try(if(sum(is.na(flag)) > 0) stop("Query not found"))
# give a negative query:
negQuerry_String <- c('')
# negQuerry_String <- stemDocument(negQuerry_String) # IF STEMMING
flag <- match(negQuerry_String, rownames(irlba$v))
try(if(negQuerry_String != '' & sum(is.na(flag)) > 0) stop("Query not found"))

# Compute the queries' coordinates in SVD matrix
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

# This function computes the euclidean distance between the queries and each document
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

Result <- names(distMatrix)
Result <- gsub(pattern = 'text',replacement = '',x = Result)

cat("Positive queries:",posQuerry_String,"\n","Negative Queries:",negQuerry_String,"\n")
for (i in (1:10)) {
  num <- as.numeric(Result[i])
  cat("Result",i,"\n","Abstract",num,"\n",Abstract[num],"\n")
}
