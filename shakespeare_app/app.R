library('tidyverse')
library('tidytext')
library('wordcloud')
library('ggplot2')
library('shiny')
library('shinythemes')
library('RColorBrewer')

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}


# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    sidebarPanel(
    # task2: add in the inputs in the sidebarPanel
      # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
      selectInput(inputId = "book_choice", label = "Choose a book:",
                  choices = books, 
                  selected = NULL),
      
      #create check box for stop words
      checkboxInput("stopwords", "Stop Words", TRUE),
      
      ## add button to run the app
      actionButton(inputId = "run_app", 
                   label = "Rerun app"),
      
      ## new section of input space
      hr(),
      
      h3("Word Cloud Settings"),
      
      ## create slider input for max number of words in word cloud 
      sliderInput(inputId = "maxwords", label = "Max # of words",
                  min = 10, max = 200, value = 100, step = 10),
      
      sliderInput(inputId = "size_largest_words", label = "Size of largest words",
                  min = 1, max = 8, value = 4),
      
      sliderInput(inputId = "size_smallest_words", label = "Size of smallest words",
                  min = 0.1, max = 4, value = 0.5),
      
      hr(),
      
      h3("Word Count Settings"),
      ## create slider input for min number of words for count charts 
      sliderInput(inputId = "minwords", label = "Minimum words for count charts",
                  min = 10, max = 100, value = 25),
      
      sliderInput(inputId = "size_words", label = "Words size for count charts",
                  min = 8, max = 30, value = 14),
    ),
    
    mainPanel(
      # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Word Cloud",
                           plotOutput(outputId = "cloud", height = "600px")),
                  tabPanel(title = "Word Counts",
                           plotOutput(outputId = "freq", height = "600px"))
                  )
              )
    )
    
  )
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights


server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(
    input$run_app,
    {
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$book_choice, input$stopwords) # ... = replace with the two inputs from Task 2
    })
  })

  output$cloud <- renderPlot({
  v <- freq()
  pal <- brewer.pal(8,"Dark2")
  v %>%
    with(
      wordcloud(
        word,
        n,
        scale = c(input$size_smallest_words, input$size_largest_words),
        random.order = FALSE,
        max.words = input$maxwords,
        colors=pal))
  })
  
  output$freq <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    ggplot(
      filter(v, n > input$minwords),
      aes(
        x = reorder(word, n),
        y = n
      )
    ) +
      geom_col() +
      coord_flip() +
      theme(
        text = element_text(size = input$size_words),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  })
}


shinyApp(ui = ui, server = server)
