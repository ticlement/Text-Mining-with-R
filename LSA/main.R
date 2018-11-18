## Extraction or load
extract_data <- FALSE # TRUE if new load needed
query <- '' # keep empty if you want full database
new_LSA <- FALSE # TRUE if you want to recalculate LSA
nv <- 100 # number of dimensions for LSA
flag <- TRUE # working version   ---------------------> TODO: find the bug in LSA.R
show_topics <- FALSE # to show the best words of the topics

## ---- QUERIES --------- ##
# give a positive query as a vector of strings ('querry','querry',...)
posQuerry_String <- c('kidney')
# give a negative query as a vector of strings ('querry','querry',...)
negQuerry_String <- c('')


if (new_LSA | file.exists("irlba") == FALSE){
  if (extract_data == FALSE & file.exists("Dataframe")){
    df <- readRDS("Dataframe")
  }else{
    Extract_Data <- dget("Extract_Data.R")
    df <- Extract_Data(query)
  }
  
  df <- df[complete.cases(df[ , 2]),] 
  df <- df[nchar(as.character(df[ , 2]))<3000 & nchar(as.character(df[ , 2]))>100,] 
  
  ## Data processing (preprocessing & SVD)
  if (flag) {
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
  } else{
    LSA <- dget("LSA.R")
    irlba <- LSA(df,nv)
  }
}else {
  irlba <- readRDS("irlba")
  df <- readRDS("Dataframe")
  df <- df[complete.cases(df[ , 2]),] 
  df <- df[nchar(as.character(df[ , 2]))<3000 & nchar(as.character(df[ , 2]))>100,]
}
## Query system

if (show_topics){
  visu_topic <- dget("visu_topic.R")
  visu_topic(irlba)
}

Abstract <- as.character(df$Abstract)
query_system <- dget("query_system.R")
query_system(irlba,posQuerry_String,negQuerry_String,Abstract) # TODO better solution that Abstract
