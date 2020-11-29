library(NLP)
library(stringr)
library(tm)
library(shiny)
library(rsconnect)

m <<- ""
quadgram <- readRDS("quadgram_muskaan.RData");
trigram <- readRDS("trigram_muskaan.RData");
bigram <- readRDS("bigram_muskaan.RData");

Predict <- function(i1) {
icl <- removeNumbers(removePunctuation(tolower(i1)))#to remove previously entered input 
split_i <- strsplit(icl, " ")[[1]]
    
#quadgram    
    if (length(split_i)>= 3) {
        split_i <- tail(split_i,3)
        if (identical(character(0),head(quadgram[quadgram$unigram == split_i[1] & quadgram$bigram == split_i[2] & quadgram$trigram == split_i[3], 4],1))){
            Predict(paste(split_i[2],split_i[3],sep=" "))
        }
        else {m <<- "Predicted using 4-gram"; head(quadgram[quadgram$unigram == split_i[1] & quadgram$bigram == split_i[2] & quadgram$trigram == split_i[3], 4],1)}
    }
#trigram
    else if (length(split_i) == 2){
        split_i <- tail(split_i,2)
        if (identical(character(0),head(trigram[trigram$unigram == split_i[1] & trigram$bigram == split_i[2], 3],1))) {
            Predict(split_i[2])
        }
        else {m<<- "Predicted using 3-gram"; head(trigram[trigram$unigram == split_i[1] & trigram$bigram == split_i[2], 3],1)}
    }
#bigram
    else if (length(split_i) == 1){
        split_i <- tail(split_i,1)
        if (identical(character(0),head(bigram[bigram$unigram == split_i[1], 2],1))) {m<<-"No match found. Most common word 'the' is returned."; head("the",1)}
        else {m <<- "Predicted using 2-gram"; head(bigram[bigram$unigram == split_i[1],2],1)}
    }
}


shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- Predict(input$inp_str)
        output$t2 <- renderText({m})
        result
    });
    
    output$t1 <- renderText({
        input$inp_str});
}
)
