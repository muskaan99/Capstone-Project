library(NLP)
library(stringr)
library(tm)
library(shiny)
library(rsconnect)

shinyUI(navbarPage("Assignment: Final Project Submission_Capstone Project",
                   tabPanel("by Muskaan Parmar",
                                sidebarLayout(
                                sidebarPanel(
                                helpText("A phrase i.e. a set of words has to be entered below to predict the next word"),
                                textInput("inp_str", "Enter input sentence:",value = ""),
                                br()
                              ),
                              mainPanel(
                                h4("Next word prediction"),
                                verbatimTextOutput("prediction"),
                                strong("Input phrase:"),
                                tags$style(type='text/css'), 
                                textOutput('t1'),
                                br(),
                                br(),
                                br(),
                                strong("N-Gram:"),
                                tags$style(type='text/css'),
                                textOutput('t2'),
                                br(),
                                br()
                              ))
                            
                   )
          )
)
