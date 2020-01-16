
server <- function(input, output, session) {
  
  
  output$Quest <- renderValueBox({
    valueBox(
      dim(ds)[1], "# Progetti", icon = icon("keyboard"),
      color = "red"
    )
  })
  
  output$corr <- renderValueBox({
    valueBox(
      ds %>% 
        group_by(Tipologia) %>% 
        filter(Tipologia=="Corrente") %>% 
        summarise(n=n()) %>% 
        select(n), "# Ricerca Corrente", icon = icon("gauss"),
      color = "blue"
    )
  })

  output$fin <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Finalizzato") %>%
        summarise(n=n()) %>%
        select(n), "# Ricerca Finalizzata", icon = icon("gauss"),
      color = "blue"
    )
  })

  output$eu <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Europeo") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca Europei", icon = icon("gauss"),
      color = "blue"
    )
  })
  
  output$reg <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Regionali") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca Regionali ", icon = icon("gauss"),
      color = "orange"
    )
  })
  
  output$af <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Autofinanziato") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca Autofinanziati ", icon = icon("gauss"),
      color = "purple"
    )
  })
  
  output$ccm <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="CCM") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca CCM ", icon = icon("gauss"),
      color = "aqua"
    )
  })
  
  output$at <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Altro tipo") %>%
        summarise(n=n()) %>%
        select(n), "# Altre tipologie ", icon = icon("gauss"),
      color = "navy"
    )
  })
  
  output$capo <- renderValueBox({
    valueBox(
      ds %>%
        filter(Tipologia=="Corrente") %>%
        group_by(Coinvolgimento) %>%
        filter(Coinvolgimento=="Capofila") %>% 
        summarise(n=n()) %>%
        select(n), "# IZSLER capofila ", icon = icon("gauss"),
      color = "light-blue"
    )
  })
  
  output$uo <- renderValueBox({
    valueBox(
      ds %>%
        filter(Tipologia=="Corrente") %>%
        group_by(Coinvolgimento) %>%
        filter(Coinvolgimento=="U.O.") %>% 
        summarise(n=n()) %>%
        select(n), "# IZSLER U.O. ", icon = icon("gauss"),
      color = "red"
    )
  })
  output$solo <- renderValueBox({
    valueBox(
      ds %>%
        filter(Tipologia=="Corrente") %>%
        group_by(Coinvolgimento) %>%
        filter(Coinvolgimento=="Solo") %>% 
        summarise(n=n()) %>%
        select(n), "# Solo IZSLER ", icon = icon("gauss"),
      color = "yellow"
    )
  })
  
  output$trend<-renderPlot(
    
    if(input$tipo=="Tutte")
    {    
      ds %>% 
        group_by(Anno) %>% 
        summarise(n=n()) %>% 
        ggplot(aes(x=Anno, y=n))+ geom_point(stat="identity", fill="steelblue3")+labs(x="")+
        geom_line()+ labs(x= "Anno", y="N.Progetti")+
        theme(axis.text=element_text(size=12))+
        scale_x_continuous(breaks = c(min(ds$Anno):max(ds$Anno)))
    }
    else
    
    {   
    
    ds %>% 
      group_by(Anno, Tipologia) %>% 
      summarise(n=n()) %>% 
      filter(Tipologia==input$tipo) %>% 
      ggplot(aes(x=Anno, y=n))+ geom_point(stat="identity", fill="steelblue3")+labs(x="")+
      geom_line()+labs(x= "Anno", y="N.Progetti")+
      theme(axis.text=element_text(size=12))+
        scale_x_continuous(breaks = c(min(ds$Anno):max(ds$Anno)))
    }
  )
  
  
  output$db<- DT::renderDataTable(
    ds %>% 
      select(-8, -9, -5),
    server= FALSE, filter= "top", class = 'cell-border stripe',
    rownames = FALSE, options = list(searching = TRUE,
      dom = 'Bfrtip',paging = TRUE,autoWidth = TRUE,
      pageLength = 10)
  )
  
  ds2<-reactive({ds %>%
    filter(Tipologia=="Corrente") %>% 
    group_by(`Responsabile Scientifico`,Coinvolgimento) %>% 
    summarise(n=n()) %>%
    arrange(n) %>% 
    filter(n>=3) %>% 
    data.frame()})
  
  
  output$rs<-renderPlot(
      ggdotchart(ds2(), x = "Responsabile.Scientifico", y ="n",
                 color = "Coinvolgimento",                                # Color by groups
                 palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                 sorting = "descending",                       # Sort value in descending order
                 add = "segments",                             # Add segments from y = 0 to dots
                 rotate = TRUE,                                # Rotate vertically
                 group = "Coinvolgimento",                                # Order by groups
                 dot.size = 6,                                 # Large dot size
                 label = round(ds2()$n),                        # Add mpg values as dot labels
                 font.label = list(color = "white", size = 9, 
                                   vjust = 0.5),               # Adjust label parameters
                 ggtheme = theme_pubr()                        # ggplot2 theme
      )
    
  )
  
  output$w<-renderPlot(
    freq.df %>% 
      top_n(input$nterm,frequency ) %>% 
      ggplot(aes(x=reorder(word, frequency), y=frequency))+geom_bar(stat = "identity", fill='steelblue3')+
      coord_flip()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)+
      theme(axis.text=element_text(size=12))+labs(x="termini", y="frequenza")
  
  )
  
  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(pr_words, size=input$size)
  })
  
  ####cluster###
  tdm2<-reactive({removeSparseTerms(tdm, sparse=input$sparse)})
    output$clust<-renderPlot({   
    hc<-hclust(dist(tdm2(), method = "euclidean"), method="complete")
    plot(hc, yaxt="n",main="", hang=0.8, cex=0.8)}
   
  )
    
    ass<-reactive({    
      a<-findAssocs(tdm, input$term, input$ass)
      a<-data.frame(a)
      a$terms<-row.names(a)
      a$terms<-factor(a$terms, levels = a$terms)
      print(a)})
    
    
    output$asso<-renderPlot({ 
      ggplot(ass(),  aes(y=terms))+
      geom_point(aes(x=ass()[,1]), data=ass(), size=1)+labs(x=input$term)+
       geom_text(aes(x= ass()[,1], label=ass()[,1]),
                                colour="darkred", hjust=-.25, size=6)+
        theme(text=element_text(size=12),
              axis.title.y = element_blank())
    })
  
  
    # ggplot(associations, aes(y=terms))+
    #   geom_point(aes(x=virus
    #   ), data=associations, size=1)+
    #   theme_gdocs()+geom_text(aes(x=virus, label=virus),
    #                           colour="darkred", hjust=-.25, size=5)+
    #   theme(text=element_text(size=8),
    #         axis.title.y = element_blank())
    # 
    # 
    
  
  
  
  
  
}
