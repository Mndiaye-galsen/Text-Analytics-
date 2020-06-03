library(quanteda)
library(RColorBrewer)
library(ggplot2)
library(textreadr)
library(dplyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(topicmodels) 
library(twitteR)
library(tm)

#necessary file for Windows
setwd("C:/Users/nabic/Desktop/Msc Business Analytics/Text Analytics/Final Presentation")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
serve <- read_document(file = "C:/Users/nabic/Desktop/Msc Business Analytics/Text Analytics/Final Presentation")

a <- 30 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- serve[i*b+z-b]
  }#closing z loop
}
names(my_df) <- c("name", "Q1", "Q2","Q3","Q4","Q5")
#View(my_df)
df <- unite(my_df, "text", Q4 ,sep = " ")
#Q1, Q2, Q3,
#View(df)
df$binary <-c(1,0,0,0,1,1,1,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,0)



#we need to convert the VCorpus from the previous point to
#a regular corpus using the corpus() function.
head(df)
twitter_corpus <- corpus(df$text) #creating the corpus on the $text var
msg.dfm <- dfm(twitter_corpus, tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 3, min_docfreq = 0)
msg.dfm <- dfm_weight(msg.dfm)

head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[1:24,]
msg.dfm.test<-msg.dfm[25:30,]

NB_classifier <- textmodel_nb(msg.dfm.train, c(1,0,0,0,1,1,1,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,0))
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred
  
