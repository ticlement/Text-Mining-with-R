setwd("/home/francois/Documents/Projet_Text_mining/Text-Mining-with-R/PubMed-app")
library(shiny)
library(topicmodels)
library(dplyr)
df <- readRDS("data/Dataframe.rds")
ap_documents <- readRDS("data/ap_documents.rds")
ap_lda <- readRDS("data/ap_lda.rds")
ap_topics <- readRDS("data/ap_topics.rds")
ap_top_terms <- readRDS("data/ap_top_terms.rds")

Abstract <- as.character(df$Abstract) # ID, Abstract, title, date, Author



############################## User Interface ##############################
ui <- fluidPage(
  
  titlePanel("Search PubMed"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("positive_querry","Search:",value = "drug"),
      textInput("negative_querry","negative querry:",value = "pancreatic"),
      helpText("It is possible to add a negative querry in order to avoid certain topics"),
      
      selectInput("method", 
                  label = "Select your search method:",
                  choices = c("LSA",
                              "LDA"),
                  selected = "LDA"),
      
      textOutput("error")
    ),
    
    
    mainPanel(
      textOutput("OK"),
      textOutput("show_tot_text"),
      hr(),
      tableOutput("table")
      
      
    )
  )
)

############################## Server logic ##############################
server <- function(input, output) {
  
  
  output$OK <- renderPrint({ 
    
    if (input$method == "LSA") {
      
      print("LSA code not yet implemented")
      
      
      DT = data.table(
        Title = NA,
        abstarct = NA,
        id = NA,
        date = NA,
        author = NA
      )
      
      output$show_tot_text <- NA
      output$table <- renderTable(DT)
      
    }

    if (input$method == "LDA") {
      
      ind_pos <- which(ap_top_terms$term==input$positive_querry)
      topic_int_pos <- ap_top_terms$topic[ind_pos]
      
      ind_neg <- which(ap_top_terms$term==input$negative_querry)
      topic_int_neg <- ap_top_terms$topic[ind_neg]

      topic_int_tot <- setdiff(topic_int_pos,topic_int_neg)
      
      if (length(topic_int_tot)<1) {
        topic_int_tot <- topic_int_pos
      }
      
      output$error <- renderPrint({ 
        tryCatch(if(length(topic_int_pos)<1) print("Your positive querry isn't significant in any of our topics, try an other research"))
        tryCatch(if(length(topic_int_neg)<1) print("Your negative querry isn't significant in any of our topics, try an other research or continue"))
        tryCatch(if(length(topic_int_tot)<1) print("Sorry, we will not take into account the negative request"))

      })
      
      ind3 <- which(ap_documents$topic %in% c(topic_int_tot))

      dfr <- ap_documents[c(ind3),]
      dfr$beta1=0
      for (i in 1:nrow(dfr)) {
        for (j in 1:length(topic_int_tot)) {
          if (dfr$topic[i]==topic_int_tot[j]) {
            dfr$beta1[i]<-ap_top_terms$beta[ind_pos[j]]
          }
        }
      }
      
      dfr$pond1 <- 0
      dfr$pond1 <- dfr$gamma+dfr$beta1
      
      result <- dfr[order(-dfr$pond1),]
      
      top_text <- select(head(result,200),"document")
      
      top_text_number <- 0
      right_text <- NA
      wrong_text <- NA
      tot_text <- NA
      perfect_match <- 0
      
      for (j in 1:200) {
        top_text_number[j] <- as.numeric(as.character(gsub("text",'',top_text[j,1])))
      }
      
      for (k in top_text_number){
        right_text[match(k,top_text_number)]<-grepl(positive_querry, Abstract[k])
        wrong_text[match(k,top_text_number)]<-grepl(negative_querry, Abstract[k])
      }
      
      tot_text <- setdiff(top_text_number[right_text],top_text_number[wrong_text])
      DT = data.table(
        Title = df[tot_text[],3],
        abstarct = df[tot_text[],2],
        id = df[tot_text[],1],
        date = df[tot_text[],4],
        author = df[tot_text[],5]
      )
      
      
      
      # Printed output: ------------------------------------------
      output$show_tot_text <- renderPrint({ cat("These are the output text positions in 'ap_documents':",tot_text) })
      output$table <- renderTable(DT)
      
    } # end if LDA
    
  }) # end Render OK
  
}
# Run the app ----
shinyApp(ui = ui, server = server)





