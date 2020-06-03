library(textreadr)
library(dplyr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(reshape2)

serve <- read_document(file = "C:/Users/nabic/Desktop/Msc Business Analytics/Text Analytics/Class 5 Group Work/NLP_team6.txt")

a <- 21 #how many observations to you have
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
  word=c("Q1","Q2","Q3","Q4","Q5","q2","q1","q3","q4","q5"),
  lexicon=rep("custom",each=10) 
)

data(stop_words)
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

frequencies_tokens_nostop_Q1 <- my_df$Q1 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q1) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 
 

frequencies_tokens_nostop_Q2 <- my_df$Q2 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q2) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE)

frequencies_tokens_nostop_Q3 <- my_df$Q3 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q3) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 

frequencies_tokens_nostop_Q4 <- my_df$Q4 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q4) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 

frequencies_tokens_nostop_Q5 <- my_df$Q5 %>%
  substr(start=4 , stop = 10000) %>%
  data_frame(name=my_df$name, text=my_df$Q5) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>%
  count(word, sort=TRUE) 

df <- bind_rows(
       mutate(frequencies_tokens_nostop_Q1,question="Q1"),
       mutate(frequencies_tokens_nostop_Q2,question="Q2"),
       mutate(frequencies_tokens_nostop_Q3,question="Q3"),
       mutate(frequencies_tokens_nostop_Q4,question="Q4"),
       mutate(frequencies_tokens_nostop_Q5,question="Q5")
       )

book_words <- df %>%
  bind_tf_idf(word, question, n) #book is location info

book_words # we get all the zeors because we are looking at stop words ... too common
tail(book_words)

book_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()



