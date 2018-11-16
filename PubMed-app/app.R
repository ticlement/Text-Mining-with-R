setwd("/home/francois/Documents/Projet_Text_mining/Text-Mining-with-R/PubMed-app")
library(shiny)
library(topicmodels)

df <- readRDS("data/Dataframe.rds")
ap_documents <- readRDS("data/ap_documents.rds")
ap_lda <- readRDS("data/ap_lda.rds")
ap_topics <- readRDS("data/ap_topics.rds")
ap_top_terms <- readRDS("data/ap_top_terms.rds")

Abstract <- as.character(df$Abstract)

# ID, Abstract, title, date, Author

# Define UI ----
ui <- fluidPage(
  
  titlePanel("Search PubMed"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("positive_querry","Search:",value = "drug"),
      helpText("It is possible to add a negative querry in order to avoid sertain topics"),
      textInput("negative_querry","negative querry:",value = "pancreatic"),
      
      selectInput("var", 
                  label = "What are you looking for?",
                  choices = c("Abstracts",
                              "Author",
                              "Date"),
                  selected = "Abstracts"),
      numericInput("num", label = h3("Abstract position"), value = 1)
    ),
    
    mainPanel(
      # tableOutput('table'),
      textOutput("OK"),
      
      textOutput("selected_var"),
      br(),
      strong("Title:"),
      textOutput("chosen_title"),
      hr(),
      strong("Abstract:"),
      verbatimTextOutput("chosen_abstract"),
      textOutput("chosen_id"),
      textOutput("chosen_date"),
      textOutput("chosen_author"), 
      
      hr()
      
    )
  )
)

# Define server logic --------------------------
server <- function(input, output) {
  
  output$OK <- renderPrint({ 
    
    positive_querry <- input$positive_querry
    negative_querry <- input$negative_querry
    
    ind_pos <- which(ap_top_terms$term==positive_querry)
    topic_int_pos <- ap_top_terms$topic[ind_pos]
    
    if (length(topic_int_pos)<1) {
      # print("Your positive querry isn't significant in any of our topics, try an other research")
    }
  
    ind_neg <- which(ap_top_terms$term==input$negative_querry)
    topic_int_neg <- ap_top_terms$topic[ind_neg]
  
    if (length(topic_int_neg)<1) {
      # print("Your negative querry isn't significant in any of our topics, try an other research or continue" )
    }
    
    topic_int_tot <- setdiff(topic_int_pos,topic_int_neg)
    
    if (length(topic_int_tot)<1) {
      # print("Sorry, we will not take into account the negative request")
      topic_int_tot <- topic_int_pos
    }
    
    ind3 <- which(ap_documents$topic %in% c(topic_int_tot))
    # head(ind3)
    
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
    # head(result)
    

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

    # length(top_text_number[right_text])
    # length(top_text_number[wrong_text])
    # length(tot_text)
    print(tot_text)
    # print(Abstract[head(tot_text,10)])
    
    #------------------------------------------
    output$chosen_id <- renderText({ 
      paste("Text ID:", df[tot_text[1],1])
    })
    output$chosen_abstract <- renderText({ 
      paste(df[tot_text[1],2])
    })
    output$chosen_title <- renderText({ 
      paste(df[tot_text[1],3])
    })
    output$chosen_date <- renderText({ 
      paste("Published on:", df[tot_text[1],4])
    })
    output$chosen_author <- renderText({ 
      paste("Author:", df[tot_text[1],5])
    })
    
  })
  
  
  
}
# Run the app ----
shinyApp(ui = ui, server = server)





