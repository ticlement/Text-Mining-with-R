query_system <- function(irlba,posQueryString,negQueryString,Abstract){

  # posQuerry_String <- stemDocument(posQuerry_String) # IF STEMMING
  flag <- match(posQuerry_String, rownames(irlba$v))
  try(if(sum(is.na(flag)) > 0) stop("Query not found"))

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
}