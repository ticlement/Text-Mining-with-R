LSA <- function(df,nv){
  library(quanteda)
  library(irlba)
  
  Abstract <- as.character(df$Abstract)
  
  # NbrDoc <- 10000
  # Abstract <- Abstract[1:NbrDoc]
  
  # Tokenize
  print("tokenization")
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
  
  ## analyzing Tokens:
  #-------------------
  
  # Our function for calculating relative term frequency (TF)
  term.frequency <- function(row) {
    row / rowSums(row)
  }
  
  # Our function for calculating inverse document frequency (IDF)
  inverse.doc.freq <- function(col) {
    corpus.size <- length(col[,1])
    doc.count <- colSums(col > 0)
    log10(corpus.size / doc.count)
  }
  
  # Our function for calculating TF-IDF.
  tf.idf <- function(tf, idf) {
    tf * idf
  }
  print("tf-idf")
  # First step, normalize all documents via TF.
  tokens.tf <- term.frequency(tokens.matrix)
  
  # Second step, calculate the IDF vector that we will use - both
  tokens.idf <- inverse.doc.freq(tokens.matrix)
  
  # Lastly, calculate TF-IDF for our training corpus.
  tokens.tfidf <- tf.idf(tokens.tf,tokens.idf)
  
  ## Perform SVD. Specifically, reduce dimensionality down to 'nv' columns
  #-----------------------------------------------------------------------

  # for our latent semantic analysis (LSA).
  print("SVD")
  irlba <- irlba(tokens.tfidf, nv = nv, maxit = 1000)

  # line names
  rownames(irlba$v) <- colnames(tokens.matrix)
  rownames(irlba$u) <- row.names(tokens.matrix)
  
  saveRDS(irlba, file = "irlba", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
  
  return(irlba)
}