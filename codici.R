library(shiny)
library(shinydashboard)
library (DT)
library(tidyverse)
library(googlesheets)
library(readxl)
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)
library(ggpubr)
library(ldatuning)
library(wordcloud2)
library(igraph)
library(dendextend)
#library(circlize)
#library(shinyBS)
library(wordcloud)
library(Rmpfr)
library(reshape2)



dataset <- read_excel("D:/Dati/vito.tranquillo/gitProject/DBricercaPBI/prizsler.xlsx")
#save(dataset, file = "dbricerca.RData")


p2<-dataset %>% 
  group_by(Anno) %>% 
  summarise(n=n()) %>% 
  mutate(Tipologia="Tutti") %>% 
  select(Anno,Tipologia, n) %>% 
  data.frame()

p1<-dataset %>% 
  group_by(Anno, Tipologia) %>% 
  summarise(n=n()) %>% 
  data.frame()
p<-rbind(p1,p2)

rm(p1,p2)







p %>% 
  ggplot(aes(x=Anno, y=n))+ geom_point(stat="identity", fill="steelblue3")+labs(x="")+
  geom_line()+ labs(x= "Anno", y="N.Progetti")+
  theme(axis.text=element_text(size=12))+
  scale_x_continuous(breaks = c(min(dataset$Anno):max(dataset$Anno)))


####una cazzo di perdita di tempo###
###figa banana###

