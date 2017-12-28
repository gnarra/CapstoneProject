#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Load the Common Function
source("NWCommonFunctions.R")

# Load the N Grams First
biGramsDF <- readRDS("biGrams-markov.rds")
triGramsDF <- readRDS("triGrams-markov.rds")

getWords <- function(inputStr) {
    # get the Next Words
    searchStr <- getSearchStr(inputStr)
    resultsDF <- predictWords(searchStr, triGramsDF, biGramsDF)
    if(is.null(resultsDF)) {
        resultsDF <- data.frame(Content = character(0), Frequency = integer(0), CurrentWord = character(0), NextWord = character(0))
    }
    message(paste("Input String = ", inputStr, ", Search String = ", searchStr, ", Count = ", nrow(resultsDF), sep=""))
    return (resultsDF)
}

shinyServer(function(input, output, session) {
    # Get the Next Words
    nextWords <- reactive({
        getWords(input$inputStrID)
    })
    
    # Populate the next words
    obs <- observe({
        words <- nextWords()
        choicesDF <- data.frame(value=words$NextWord, label=paste(words$NextWord, " (Count: ", words$Frequency, ")", sep="" ))
        
        # update the render function for selectize
        updateSelectizeInput(session,
                             "nextWordsID",
                             server = TRUE,
                             choices = choicesDF,
                             options = list(maxOptions = 10))

    })
    
    # Update the text area based on what was selected in the dropdown list
    observeEvent(input$nextWordsID, {
        updateTextAreaInput(session, 
                            "inputStrID", 
                            value = paste(trim(input$inputStrID), " ", trim(input$nextWordsID), sep=""))
    })
    
    # Plot the Word Cloud
    output$wordPlot <- renderPlot({
        words <- nextWords()
        if(nrow(words) != 0) {
            # Word Cloud
            wordcloud(words$NextWord, words$Frequency,
                      min.freq = input$freq, max.words = input$max,
                      random.order = FALSE, colors=brewer.pal(8, "Dark2"))
        }
    })
})
