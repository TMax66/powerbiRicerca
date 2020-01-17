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
rm(list=ls())
ds <- read_excel("prizsler.xlsx")

ds2<-ds %>% 
  filter(Tipologia=="Corrente")

tryTolower<-function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus<-tm_map(corpus, content_transformer(tryTolower))
  corpus<-tm_map(corpus, removeWords, custom.stopwords)
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  return(corpus)
}



title<-data.frame(doc_id=seq(1:nrow(ds2)),text=ds2$Titolo)
custom.stopwords<-c(stopwords('italian'),"valutazione", "studio","animali", "animale","punto",
                    "messa","progetto","ricerca", "finanziamento", "specie", "test", "riferimento",
                    "particolare", "spp", "agenti", "mediante", "ceppi", "utilizzo", "protocolli",
                    "analisi", "sistema", "applicazione","origine", "indagine", "malattie", "malattia", "prova",
                    "strategie", "fattori","potenziale", "campo", "campioni", "presenza", "procedure", "nuovi", "attraverso")
corpus <- VCorpus(DataframeSource(title))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "prodotti", replacement = "produzione")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "\\b(metodiche|metodica|metodo)\\b", replacement = "metodi")
# corpus<-tm_map(corpus, content_transformer(gsub), pattern = "metodiche", replacement = "metodi")
# corpus<-tm_map(corpus, content_transformer(gsub), pattern = "metodo", replacement = "metodi")
# corpus<-tm_map(corpus, content_transformer(gsub), pattern = "metodica", replacement = "metodi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "suini", replacement = "suino")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "suina", replacement = "suino")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "\\b(diagnostiche|diagnostici|diagnostico|diagnostica)\\b", replacement = "diagnosi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "allevamenti", replacement = "allevamento")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "virali", replacement = "virus")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "epidemiologiche", replacement = "epidemiologia")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "epidemiologica", replacement = "epidemiologia")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "avium", replacement = "paratubercolosi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "subsp", replacement = "paratubercolosi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "virali", replacement = "virus")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "bovini", replacement = "bovina")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "infezioni", replacement = "infezione")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "\\b(paratuberculosis|paratubercolosis)\\b", replacement = "paratubercolosi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "map", replacement = "paratubercolosi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "italia", replacement = "nazionale")




tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.title.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.title.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))





####WORD CLOUD###
# m<-tdm.title.m
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)

# dtm <- DocumentTermMatrix(corpus)
# dtm_td <- tidy(dtm)
# dtm_td %>%
#   count(term, sort = TRUE) %>%
#   filter(n > 20) %>%
#   mutate(word = reorder(term, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col(fill="yellow",col="black") +
#   xlab(NULL) +
#   coord_flip() +
#   
#   theme(plot.title = element_text(size = 10, face = "bold")) + theme_bw()

# pr_words <- dtm_td %>%
#   count(term,sort=TRUE) %>%
#   filter(n >= 10)












# ###########NETWORK#############
#
# freq.term<-findFreqTerms(tdm, lowfreq = 20)
# 
# plot(tdm, term=freq.term, corThreshold = 0.2,weighting=T)
# 
# 
# # ######TOPICS MODEL#####
# dtm<-DocumentTermMatrix(corpus, control=list(weighting=weightTf))
# term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) *
#   log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))
# 
# 
# dtm <- dtm[,term_tfidf >= 0.7015]
# summary(slam::col_sums(dtm))
# 
# 
# rowTotals<-apply(dtm,1,sum) #running this line takes time
# empty.rows<-dtm[rowTotals==0,]$dimnames[1][[1]]
# corpus<-corpus[-as.numeric(empty.rows)]
# dtm <- DocumentTermMatrix(corpus) 
# # # determina il numero di topics###
# burnin <- 1000
# iter <- 1000
# keep <- 50
# # determining k (the number of topics)
# seqk <- seq(2,100,1)
# system.time(fitted_many <- lapply(seqk,function(k) topicmodels::LDA(dtm,k=k,
#                                                                     method="Gibbs",control=list(burnin=burnin,iter=iter,keep=keep))))
# # extract logliks from each topic
# logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])
# # compute harmonicMean
# harmonicMean <- function(logLikelihoods, precision = 2000L) {
#   llMed <- median(logLikelihoods)
#   as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
#                                        prec = precision) + llMed))))
# }
# hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
# ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
#   theme(text = element_text(family= NULL),
#         axis.title.y=element_text(vjust=1, size=16),
#         axis.title.x=element_text(vjust=-.5, size=16),
#         axis.text=element_text(size=16),
#         plot.title=element_text(size=20)) +
#   xlab('Number of Topics') +
#   ylab('Harmonic Mean') +
#   ggtitle("Determining the number of topics")
# k <- seqk[which.max(hm_many)]
# # 
# k<-10
# 
# seedNum <- 50
# system.time(ldaOut <- topicmodels::LDA(dtm,k = k,method="Gibbs",
#                                        control=list(burnin=burnin,keep=keep,iter=2000,seed=seedNum)))
# 
# ###word-topics relationships
# ldaOut.terms <- as.matrix(terms(ldaOut,10))
# 
# DT::datatable(ldaOut.terms)
# 
# topics_beta <- tidy(ldaOut,matrix="beta")
# top_terms <- topics_beta %>%
#   group_by(topic) %>%
#   top_n(5,beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# top_terms %>%
#   mutate(term=reorder(term,beta)) %>%
#   ggplot(aes(term,beta,fill=factor(topic))) +
#   geom_col(show.legend=FALSE) +
#   facet_wrap(~ topic,scales="free") +
#   coord_flip() +
#   ggtitle("Probabilità dei primi 5 termini per topic")
# 
# 
# 
# topic1_5 <- topics_beta %>%
#   group_by(term) %>%
#   top_n(1,beta) %>%
#   group_by(topic) %>%
#   top_n(10,beta) %>%
#   filter(topic < 6) %>%
#   acast(term ~ topic,value.var="beta",fill=0) %>%
#   comparison.cloud(title.size=1,colors = brewer.pal(5,"Set1"))
# 
# topics6_10 <- topics_beta %>%
#   group_by(term) %>%
#   top_n(1,beta) %>%
#   group_by(topic) %>%
#   top_n(10,beta) %>%
#   filter(topic >= 6 & topic <= 10) %>%
#   acast(term ~ topic,value.var="beta",fill=0) %>%
#   comparison.cloud(title.size=1,colors = brewer.pal(5,"Set1"))
# 
# 
# ML.topics <- topicmodels::topics(ldaOut, 1)
# ML.terms <- as.data.frame(topicmodels::terms(ldaOut, 30), stringsAsFactors = FALSE)
# topicTerms <- tidyr::gather(ML.terms, Topic)
# topicTerms <- cbind(topicTerms, Rank = rep(1:30))
# topTerms <- dplyr::filter(topicTerms, Rank < 4)
# topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
# topTerms$Topic <- as.numeric(topTerms$Topic)
# topicLabel <- data.frame()
# for (i in 1:10){
#   z <- dplyr::filter(topTerms, Topic == i)
#   l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2], sep = " " ), stringsAsFactors = FALSE)
#   topicLabel <- rbind(topicLabel, l)
#   
# }
# colnames(topicLabel) <- c("Label")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# topicProbabilities <- as.data.frame(ldaOut@gamma)
# head(topicProbabilities)
# 
# bgg.post <- posterior(ldaOut)
# bgg.cor_mat <- cor(t(bgg.post[["terms"]]))
# bgg.cor_mat[bgg.cor_mat < .001 ] <- 0
# diag(bgg.cor_mat) <- 0
# bgg.graph <- graph.adjacency(bgg.cor_mat, weighted=TRUE, mode="lower")
# bgg.graph <- delete.edges(bgg.graph, E(bgg.graph)[ weight < 0.05])
# E(bgg.graph)$edge.width <- E(bgg.graph)$weight * 50
# V(bgg.graph)$size <- colSums(bgg.post[["topics"]])/200
# par(mar=c(0, 0, 3, 0))
# set.seed(110)
# plot.igraph(bgg.graph, edge.width = E(bgg.graph)$edge.width,
#             main = "Strength Between Topics Based On Word Probabilities",
#             edge.color = "orange",
#             vertex.color = "orange",
#             vertex.frame.color = NA)
# 
# clp <- cluster_label_prop(bgg.graph)
# class(clp)
# plot(clp,bgg.graph,main="Community detection in topic network")
# 

# 
# 
# 
# 
# 
# ldaOut.terms <- as.matrix(terms(ldaOut,10))
# ldaOut.terms[1:10,]
# 
# topics_beta <- tidy(ldaOut,matrix="beta")
# 
# topic1_5 <- topics_beta %>%
#   group_by(term) %>%
#   top_n(1,beta) %>%
#   group_by(topic) %>%
#   top_n(10,beta) %>%
#   filter(topic < 6) %>%
#   acast(term ~ topic,value.var="beta",fill=0) %>%
#   comparison.cloud(title.size=1,max.words=40,colors = brewer.pal(5,"Set1"))
# 
# 
# 
# ML.topics <- topicmodels::topics(ldaOut, 1)
# ML.terms <- as.data.frame(topicmodels::terms(ldaOut, 30), stringsAsFactors = FALSE)
# topicTerms <- tidyr::gather(ML.terms, Topic)
# topicTerms <- cbind(topicTerms, Rank = rep(1:30))
# topTerms <- dplyr::filter(topicTerms, Rank < 4)
# topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
# topTerms$Topic <- as.numeric(topTerms$Topic)
# topicLabel <- data.frame()
# for (i in 1:13){
#   z <- dplyr::filter(topTerms, Topic == i)
#   l <- as.data.frame(paste(z[1,2], z[2,2], sep = " " ), stringsAsFactors = FALSE)
#   topicLabel <- rbind(topicLabel, l)
#   
# }
# colnames(topicLabel) <- c("Label")
# topicLabel



# burnin <- 4000
# iter <- 2000
# thin <- 500
# seed <-list(2003,5,63,100001,765)
# nstart <- 5
# best <- TRUE
# k <- 10
# # 
# ldaOut <-LDA(dtm,k, method="Gibbs",
#              control=list(nstart=nstart,
#                           seed = seed, best=best,
#                           burnin = burnin, iter = iter, thin=thin))
# # 
# topics <- tidy(ldaOut, matrix = "beta")
# ap_top_terms <- ap_topics %>%
#   group_by(topic) %>%
#   top_n(5, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# # 
# ap_top_terms %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip() +
#   scale_x_reordered()
# # 
# ap_documents <- tidy(ldaOut, matrix = "gamma")
# 
# 
# tidy(dtm) %>%
#   filter(document == 4) %>%
#   arrange(desc(count))
# 
# 
# 
# x <- topics(ldaOut)
# new.df <- data.frame('response'=names(x), 'topic'=x, row.names=NULL)
# 
# new.df %>% 
#   group_by(topic) %>% 
#   summarise(n=n())
# 
# 
# 
# 
# ldaOut.topics <- as.matrix(topics(ldaOut))
# 
# ldaOut.terms <- as.matrix(terms(ldaOut,20))
#



# 
# rowTotals <- apply(dtm , 1, sum)
# dtm<-dtm.new<- dtm[rowTotals> 0, ]
# 
# 
# topic <- LDA(dtm, k = 5, control = list(seed = 1234))
# 
# topic <- tidy(topic, matrix = "beta")
# 
# ap_top_terms <- topic %>%
#   group_by(topic) %>%
#   top_n(20, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# ap_top_terms %>%
#   mutate(term = reorder(term, beta)) %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   ggplot(aes(term, beta)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()
# 
# 
# beta_spread <- topic %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   spread(topic, beta) %>%
#   filter(topic1 > .001 | topic2 > .001) %>%
#   mutate(log_ratio = log2(topic2 / topic1)) %>%
#   group_by(log_ratio < 0) %>%
#   top_n(15, abs(log_ratio)) %>%
#   ungroup() %>%
#   mutate(word = reorder(term, log_ratio)) %>%
#   ggplot(aes(word, log_ratio)) +
#   geom_col(show.legend = FALSE) +
#   coord_flip() +
#   ylab("log odds ratio")
# 
# beta_spread %>%
#   group_by(log_ratio < 0) %>%
#   top_n(15, abs(log_ratio)) %>%
#   ungroup() %>%
#   mutate(word = reorder(term, log_ratio)) %>%
#   ggplot(aes(word, log_ratio)) +
#   geom_col(show.legend = FALSE) +
#   coord_flip() +
#   ylab("log odds ratio")
