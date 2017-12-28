#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

# The Next Word Prediction UI for application
shinyUI(
    navbarPage("Next Word Prediction",
        tabPanel("Plot",
            sidebarLayout(
                sidebarPanel(
                    # Input Text Area
                    textAreaInput("inputStrID", "Enter String:", value = "Hello"),
                                
                    # Next Words
                    selectInput('nextWordsID', 'Next 10 Possible Words', choices = NULL, multiple=FALSE, selectize=TRUE),
                                
                    # Action Button
                    actionButton("button1", "Calculate Next Words"),
                                
                    hr(),
                                
                    helpText("Word Cloud plot parameters."),
                                
                    # Sliders for Frequency and Max Words to Plot
                    sliderInput("freq", "Minimum Frequency:",
                                min = 1,  max = 50, value = 5),
                    sliderInput("max", "Maximum Number of Words:",
                                min = 1,  max = 300,  value = 50)
                ),
                mainPanel(
                    plotOutput("wordPlot")
                )
            )
        ),
        tabPanel("About", includeHTML("About.html"))
    )
)
