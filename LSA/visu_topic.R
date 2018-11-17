visu_topic <- function(irlba){
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
}