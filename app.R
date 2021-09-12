#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('vocab.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tom's Spanish Flashcards"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("questions",
                        "Number of Questions:",
                        min = 5,
                        max = 100,
                        value = 20),
            selectInput("category", "Category Selection:",names(vocab)),
            actionButton("start_test", "Start Test"),
            actionButton("next_word","Next Word"),
            htmlOutput("word_display")
            

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    word_index<-reactiveVal()
    words<-reactiveVal()
    
    observeEvent(input$start_test,{
        words(sample(vocab[[input$category]]))
        print(words())
        word_index(1)
    })
    
    observeEvent(input$next_word,{
        print("next word")
        word<-words()[word_index()]
        print(word)
        word_index(word_index()+1)
        output$word_display <- renderText({
            c("<p><b>","Word: ","</b>",word,"</p>")
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
