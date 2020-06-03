library(textreadr)
library(dplyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)

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
cust_stop <- data_frame(
  word=c("Q1","Q2","Q3","Q4","Q5","q2","q1","q3","q4","q5","NA"),
  lexicon=rep("custom",each=11) 
)

frequencies_tokens_nostop_Q1 <- data_frame(name=my_df$name,text=my_df$Q1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  count(bigram,sort=TRUE) 




frequencies_tokens_nostop_Q2 <-data_frame(name=my_df$name, text=my_df$Q2) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  top_n(20) %>%
  count(bigram,sort=TRUE) 

frequencies_tokens_nostop_Q3 <- data_frame(name=my_df$name, text=my_df$Q3) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  top_n(20) %>%
  count(bigram,sort=TRUE)  

frequencies_tokens_nostop_Q4 <- data_frame(name=my_df$name, text=my_df$Q4) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  top_n(20) %>%
  count(bigram,sort=TRUE) 

frequencies_tokens_nostop_Q5 <-data_frame(name=my_df$name, text=my_df$Q5) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  count(bigram,sort=TRUE) 

df <- bind_rows(
  mutate(frequencies_tokens_nostop_Q1,question="Q1"),
  mutate(frequencies_tokens_nostop_Q2,question="Q2"),
  mutate(frequencies_tokens_nostop_Q3,question="Q3"),
  mutate(frequencies_tokens_nostop_Q4,question="Q4"),
  mutate(frequencies_tokens_nostop_Q5,question="Q5")
)
#View(df)
book_words <- df %>%
    bind_tf_idf(bigram, question, n) %>%
    arrange(desc(tf_idf)) %>%
  mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>% #rev() 一个向量逆转
  group_by(question) %>%
  top_n(3) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()
book_words

 