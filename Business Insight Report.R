install.packages("shapeR")
install.packages("pdftools")
install.packages("tm")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("wordcloud")
install.packages("tidyverse")
install.packages("textreadr")
install.packages("textshape")
install.packages("scales")
install.packages("tidyr")
install.packages("textdata")
install.packages("plotly")
install.packages("RColorBrewer")
library(shapeR)
library(pdftools)
library(tm)
library(NLP)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(wordcloud)
library(shapeR)
library(tidyverse)
library(textreadr)
library(textshape)
library(scales)
library(tidyr)
library(textdata)
library(plotly)
#######################################################################################################################
setwd("C:/Users/nabic/Desktop/articles") #PDF files location
nm <- list.files(path="C:/Users/nabic/Desktop/articles")# PDF file storage
########################################################################################################
#######CREATING A DATA frame for the reports #########################################################################
#####################################################################################################
BP_1<-as.data.frame(pdf_text('British Petroleum Report.pdf'))%>%mutate(name='BP')
names(BP_1)[1]<-"text"
view(BP_1)
#######################################################################################################

library(pdftools)#call this library to read text
library(tm)
setwd("C:/Users/nabic/Desktop/articles")
nm <- list.files(path="/Users/nabic/Desktop/articles")


####################################################################################
#######CREATING A DATA FRAME for KOSMOS####################################################
####################################################################################
KR_2<-as.data.frame(pdf_text('Kosmos Report.pdf'))%>%mutate(name='Kosmos')
names(KR_2)[1]<-"text"
view(KR_2)

#########################################################################################
setwd("C:/Users/nabic/Desktop/articles") #where are your PDF files
nm <- list.files(path="/Users/nabic/Desktop/articles")#were are your PDF files stored?


#######################################################################################
#######CREATING A DATA FRAME FOR Cairn####################################################
########################################################################################3
CRN_3<-as.data.frame(pdf_text('Cairn Report.pdf'))%>%mutate(name='Cairn')
names(CRN_3)[1]<-"text"
view(CRN_3)
###########################################################################################################
#################TOKENIZING THE CSR REPORTS###############################################
##########################################################################################################
BP_1$text<-as.character(BP_1$text)
tidy_BP_1<-BP_1%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words,by = "word")%>%
  count(word, sort=T)

View(tidy_BP_1)

KR_2$text<-as.character(KR_2$text)
tidy_KR_2<-KR_2%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words,by = "word")%>%
  count(word, sort=T)

View(tidy_KR_2)

CRN_3$text<-as.character(CRN_3$text)
tidy_CRN_3<-CRN_3%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words,by = "word")%>%
  count(word, sort=T)

View(tidy_CRN_3)

##########################################################################################################
##### TOKEN FREQUENCY HISTOGRAMS FOR MY DATA FRAMES ########################################################
############################################################################################################

#BRITISH PETROLEUM 2018 CORPORATE SOCIAL RESPONSIBILITY REPORT
library(ggplot2)
freq_hist <-tidy_BP_1%>%
  #anti_join(stop_words) %>%
  #count(word, sort=TRUE) %>%
  filter(n>100)%>%
  ggplot(aes(word,n))+
  geom_col()+
  xlab=(NULL)+
  coord_flip()

freq_hist

#KOSMOS ENERGY 2018 CORPORATE SOCIAL RESPONSIBILITY REPORT
library(ggplot2)
freq_hist <-tidy_KR_2%>%
  #anti_join(stop_words) %>%
  #count(word, sort=TRUE) %>%
  filter(n>75)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

freq_hist

#CAIRN ENERGY 2018 CORPORATE SOCIAL RESPONSIBILITY REPORT
library(ggplot2)
freq_hist <-tidy_CRN_3%>%
  #anti_join(stop_words) %>%
  #count(word, sort=TRUE) %>%
  filter(n>80)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

freq_hist

#############################################################################################################
#################RUNNING A CORRELOGRAM WITH MY THREE DATAFRAME FOR CORRELATION#################################
########################################################################################################
library(tidyr)
frequency<-bind_rows(mutate(tidy_BP_1,file="BP"),
                     mutate(tidy_KR_2,file="Kosmos"),
                     mutate(tidy_CRN_3,file="Cairn"))%>%
  
  
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(file, word) %>%
  group_by(file) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(file, proportion) %>%
  gather(file, proportion, `BP`, `Cairn`)

#plotting the correlograms:

library(scales)

ggplot(frequency, aes(x=proportion, y=`Kosmos`, 
                      color = abs(`Kosmos`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~file, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Kosmos", x=NULL)

#Taking a look at correlation coefficients
cor.test(data=frequency[frequency$file == "BP",],
         ~proportion + `Kosmos`)

cor.test(data=frequency[frequency$author == "Cairn",],
         ~proportion + `Kosmos`)

####################################################################################
##RUNNING A SENTIMENT ANALYSIS#####################################################
#################################################################################
library(tidytext)
get_sentiments('afinn') # Show example of the table

# pulling in sentiment for these 3 tokenized datasets
tidy_BP_1%>%
  inner_join(get_sentiments("afinn"))%>%
  #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")



tidy_KR_2 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_CRN_3 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#let's take a look at the most positive and most negative tokens in the BP report

tidy_BP_1_sentiment <- tidy_BP_1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

print(tidy_BP_1_sentiment)

tidy_BP_1_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
#################################################################################
###RUNNING A WORD CLOUD FRAME WORK################################################
####################################################################################
#install.packages("wordcloud")
library(wordcloud)



tidy_BP_1_sentiment <- tidy_BP_1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)%>%
  acast(word~sentiment,value.var = "n",fill=0)%>%
  comparison.cloud(colors=c("grey20","gray80"),
                   max.words=100, scale=c (1,1),
                   fixed.asp=TRUE,title.size=1)

tidy_KR_2_sentiment <- tidy_KR_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)%>%
  acast(word~sentiment,value.var = "n",fill=0)%>%
  comparison.cloud(colors=c("grey20","gray80"),
                   max.words=100, scale=c (1,1),
                   fixed.asp=TRUE,title.size=1)

tidy_CRN_3_sentiment <- tidy_CRN_3 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)%>%
  acast(word~sentiment,value.var = "n",fill=0)%>%
  comparison.cloud(colors=c("grey20","gray80"),
                   max.words=100, scale=c (1,1),
                   fixed.asp=TRUE,title.size=1)
warnings()
