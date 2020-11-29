#File to generate N grams: 2-grams, 3-grams and 4-grams: Muskaan Parmar

setwd("~/R/Coursera 10/Week 2")

#loading libraries
library(rJava)
library(ggplot2)
library(NLP)
library(tm)
library(wordcloud)
library(R.utils)
library(parallel)
library(dplyr)
library(RWeka)

#Reading datasets US News, US Twitter and US Blogs
us_twitter <- readLines("C:/Users/muska/Documents/R/Coursera 10/Week 2/Coursera-SwiftKey/final/en_US/en_US.us_twitter.txt", encoding = "UTF-8", skipNul = TRUE)
us_blogs <- readLines("C:/Users/muska/Documents/R/Coursera 10/Week 2/Coursera-SwiftKey/final/en_US/en_USus_blogs.txt", encoding = "UTF-8", skipNul = TRUE)
us_news <- readLines("C:/Users/muska/Documents/R/Coursera 10/Week 2/Coursera-SwiftKey/final/en_US/en_USus_news.txt", warn = FALSE,encoding = "UTF-8", skipNul = TRUE)

#function to sample files
samp <- function(x1, size) {
  t1 <- sample(1:length(x1), length(x1)*size)
  res <- x1[t1]
  res
}

set.seed(123)
size <- 25/50
samp_news <- samp(us_news, size)
samp_twitter <- samp(us_twitter, size)
samp_blog <- samp(us_blogs, size)
total1 <- c(samp_blog, samp_news, samp_twitter)
writeLines(total1, "~/R/Coursera 10/Week 2/sampleall/total1.txt")#saving file


#Cleaning corpus
clean_corpus <- function (tc) {
  tc <- tm_map(tc, content_transformer(tolower))
  tc <- tm_map(tc, stripWhitespace)
  tc <- tm_map(tc, removePunctuation)
  tc <- tm_map(tc, removeNumbers)
  tc
}
total1 <- VCorpus(DirSource("~/R/Coursera 10/Week 2/sampleall", encoding = "UTF-8"))
total1 <- clean_corpus(total1)

#For n grams:
#function to generate n grams
generate_ngram <- function (tc, n) {
  NgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))}
  res2 <- TermDocumentMatrix(tc, control = list(tokenizer = NgramTokenizer))
  res2
}
#performing the function for each of the cases
tdm_1gram <- generate_ngram(total1, 1)
tdm_2gram <- generate_ngram(total1, 2)
tdm_3gram <- generate_ngram(total1, 3)
tdm_4gram <- generate_ngram(total1, 4)


# function to extract n grams and sort
ngram_sorted_df <- function (res2) {
  m <- as.matrix(res2)
  df_ngram <- as.data.frame(m)
  colnames(df_ngram) <- "Count"
  df_ngram <- df_ngram[order(-df_ngram$Count), , drop = FALSE]
  df_ngram
}

#performing the function for each of the cases
tdm_1gram_df <- ngram_sorted_df(tdm_1gram)
tdm_2gram_df <- ngram_sorted_df(tdm_2gram)
tdm_3gram_df <- ngram_sorted_df(tdm_3gram)
tdm_4gram_df <- ngram_sorted_df(tdm_4gram)

#saving each of the files
quad <- data.frame(rows=rownames(tdm_4gram_df),count=tdm_4gram_df$Count)
quad$rows <- as.character(quad$rows)
quadgram_split <- strsplit(as.character(quad$rows),split=" ")
quad <- transform(quad,first = sapply(quadgram_split,"[[",1),second = sapply(quadgram_split,"[[",2),third = sapply(quadgram_split,"[[",3), fourth = sapply(quadgram_split,"[[",4))
quad <- data.frame(unigram = quad$first,bi = quad$second, tri = quad$third, quad = quad$fourth, freq = quad$count,stringsAsFactors=FALSE)
write.csv(quad[quad$freq > 1,],"./ShinyApp/quad.csv",row.names=F)
quad <- read.csv("./ShinyApp/quad.csv",stringsAsFactors = F)
saveRDS(quad,"~/R/Coursera 10/Week 2/ShinyApp/quadgram_muskaan.RData")


tri <- data.frame(rows=rownames(tdm_3gram_df),count=tdm_3gram_df$Count)
tri$rows <- as.character(tri$rows)
trigram_split <- strsplit(as.character(tri$rows),split=" ")
tri <- transform(tri,first = sapply(trigram_split,"[[",1),second = sapply(trigram_split,"[[",2),third = sapply(trigram_split,"[[",3))
tri <- data.frame(unigram = tri$first,bi = tri$second, tri = tri$third, freq = tri$count,stringsAsFactors=FALSE)
write.csv(tri[tri$freq > 1,],"./ShinyApp/tri.csv",row.names=F)
tri <- read.csv("./ShinyApp/tri.csv",stringsAsFactors = F)
saveRDS(tri,"~/R/Coursera 10/Week 2/ShinyApp/trigram_muskaan.RData")


bi <- data.frame(rows=rownames(tdm_2gram_df),count=tdm_2gram_df$Count)
bi$rows <- as.character(bi$rows)
bigram_split <- strsplit(as.character(bi$rows),split=" ")
bi <- transform(bi,first = sapply(bigram_split,"[[",1),second = sapply(bigram_split,"[[",2))
bi <- data.frame(unigram = bi$first,bi = bi$second,freq = bi$count,stringsAsFactors=FALSE)
write.csv(bi[bi$freq > 1,],"./ShinyApp/bi.csv",row.names=F)
bi <- read.csv("./ShinyApp/bi.csv",stringsAsFactors = F)
saveRDS(bi,"~/R/Coursera 10/Week 2/ShinyApp/bigram_muskaan.RData")