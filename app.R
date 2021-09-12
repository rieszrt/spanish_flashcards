#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

vocab<-read.csv("vocab.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tom's Spanish Flashcards"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("category", "Category Selection:",unique(vocab$category)),
            selectInput("test_language", "Test Language:",c("esp","eng")),
            actionButton("start_test", "Start Test"),
            actionButton("next_word","Next Word"),
            actionButton("show_translation","Show Translation"),
            htmlOutput("word_display")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DTOutput('myTable'),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    word_index<-reactiveVal()
    words<-reactiveVal()
    
    observeEvent(input$start_test,{
        category_words = vocab[which(vocab$category==input$category),]
        words(category_words[sample(nrow(category_words)),])
        print(words())
        word_index(1)
    })
    
    observeEvent(input$next_word,{
        word<-words()[word_index(),input$test_language]
        print(word)
        word_index(word_index()+1)
        output$word_display <- renderText({
            c("<p><b>","Word: ","</b>",word,"</p>")
        })
    })
    observeEvent(input$show_translation,{
        if (input$test_language=="eng"){
            other_language <-"esp"
        } else{
            other_language<-"eng"
        }
        word<-words()[word_index()-1,other_language]
        print(word)
        output$word_display <- renderText({
            c("<p><b>","Word: ","</b>",word,"</p>")
        })
    })
    
    output$myTable <- renderDT({
        print("mytable")
        words()
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
