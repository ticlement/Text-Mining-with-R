Extract_Data <- function(query){
  library(XML)
  library(easyPubMed)

  ############### PART 1: Information extraction ###############
  
  ## Dataset importation
  #---------------------
  # Option 1: 698 documents via une requete
  #----------
  if (query != ''){
    Ids <- get_pubmed_ids(query)
    papers <- fetch_pubmed_data(Ids)
  }
  # Option 2: 52349 documents via importation du fichier xml.
  #----------
  if (query == ''){
    # papers <- xmlParse(file = "/home/francois/Documents/Projet_Text_mining/pubmed18n0924.xml")
    papers <- xmlParse(file = "pubmed18n0924.xml")
  }
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
  
  parallel <- 0
  if (parallel){
    library(foreach)
    library(doParallel)
    
    #setup parallel backend to use many processors
    cores=detectCores()
    cl <- makeCluster(cores[1]-1) #not to overload your computer
    registerDoParallel(cl)
    
    multiResultClass <- function(ID=NULL,Abstract=NULL,Title=NULL,Date=NULL,Author=NULL){
    me <- list(
      ID = ID,
      Abstract = Abstract,
      Title = Title,
      Date = Date,
      Author = Author
    )
    class(me) <- append(class(me),"multiResultClass")
    return(me)
    }
    res <- foreach(i = 1:Article_Num,.packages =c("XML")) %dopar% {
      res <- multiResultClass()
      res$ID <- xmlValue(xmltop[[i]][["MedlineCitation"]][["PMID"]])
      res$Abstract <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["Abstract"]])
      res$Title <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]])
      res$Date <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleDate"]])
      res$Author <- paste(xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["LastName"]])
                             ,xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["ForeName"]]))
      return(res)
    }
    #stop cluster
    stopCluster(cl)
  }else{
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
  }
  rm(papers)
  
  # create dataframe
  df <- data.frame(ID, Abstract, Title, Date, Author)
  rm(ID, Abstract, Title, Date, Author, Author_forename, Author_lastname)
  
  # export dataframe
  saveRDS(df, file = "Dataframe", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)

  return(df)  
}