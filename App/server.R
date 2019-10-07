##server
shinyServer(function(input, output,session) {
  ## dynamicaly update selection
  observeEvent(input$Geog,updateSelectInput(session,"Cancer.Alliance","Region",
                                            choices = unique(ncpes$CALNCV18NM[ncpes$Geog == input$Geog])))
  tolist <-reactive({
    list(input$Geog,input$Cancer.Alliance)
  })
  
  observeEvent(tolist(),updateSelectInput(session,"Trust.Name","Organisation Name",
                                            choices = unique(factor(ncpes$Trust.Name[ncpes$Geog == input$Geog & ncpes$CALNCV18NM == input$Cancer.Alliance]))))
 
 observeEvent(input$Question.Type, updateSelectInput(session,"Question.Text","Select Question",
                                                     choices =  unique(factor(ncpes$Question.Text[ncpes$Question.Type == input$Question.Type
                                                                                                  & order(ncpes$Question.Number)]))))
  
  observeEvent(input$Question.Type,updateSelectInput(session,"Question.Text2","Select Question",
                                                     choices =  unique(factor(ncpes$Question.Text[ncpes$Question.Type == input$Question.Type
                                                                                                  & order(ncpes$Question.Number)]))))
  
  
  #####################################
  ##       Page 1- Overview          ##
  #####################################

   ## row 2
  output$overallnumber <- renderValueBox({
    valueBox( 
      value = paste0(ncpes$scored.percentage[ncpes$Trust.Name == input$Trust.Name & ncpes$qnum == 59 & ncpes$Year == 2018 &
                                               ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                               ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear],"/10"),
      "Overall, how would you rate your care?", icon = icon("thermometer-half"), color = "blue"
    )
  })
  
  output$Overallcare <- renderValueBox({
    valueBox( 
  value = paste0(ncpes$Number.of.responses[ncpes$Trust.Name == input$Trust.Name & ncpes$qnum == 59 & ncpes$Year == 2018 &
                              ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear]),
     "Number of Responses", icon = icon("users"), color = "yellow"
    )
  })
  
output$overallsigdif <- renderValueBox({
  valueBox(
    paste0(ncpes$abrvperformance[ncpes$Trust.Name == input$Trust.Name & ncpes$qnum == 59 & ncpes$Year == 2018 &
                                     ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                     ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear]),
    "Overall Care compared to Expected", icon = icon("hospital"), color = "purple", 
    href = "https://www.ncpes.co.uk/reports/2018-reports/guidance-material-and-survey-materials-2018/4541-2018-national-cancer-patient-experience-survey-technical-documentation/file#page=9"
  )
})  
    
  
## row 3

  ## graph
  output$oveallgraph <- renderPlot({
    overviewbarplot <- ncpes %>%
      filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" & ncpes$Year == 2018 &
               ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear & ncpes$Cancer.Type == "All Cancers" &
               ncpes$qnum != 59 & ncpes$cpesqtype %in% input$Question.Type) %>% 
      select(Trust.Name,Question.Number,Performance,scored.percentage,Lower.95..confidence.interval,Upper.95..confidence.interval)
    
if(input$adjusted.unadjusted.yearonyear == "adjusted"){
      ggplot(overviewbarplot,aes(reorder(Question.Number,desc(Question.Number)),scored.percentage)) + geom_bar(stat = "identity",aes(fill = Performance)) +
        geom_errorbar(aes(ymin = Lower.95..confidence.interval,ymax =Upper.95..confidence.interval), width = 0.2, position=position_dodge(0.9)) + 
        xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
        theme(legend.position = "bottom")+ guides(fill=guide_legend(nrow=3,byrow=TRUE))+ scale_y_continuous(expand = c(0,0)) + coord_flip()
}else {
  ggplot(overviewbarplot,aes(reorder(Question.Number,desc(Question.Number)),scored.percentage)) + geom_bar(stat = "identity",aes(fill = Performance)) +
    geom_errorbar(aes(ymin = Lower.95..confidence.interval,ymax =Upper.95..confidence.interval), width = 0.2, position=position_dodge(0.9)) + 
    xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
    theme(legend.position = "none")+ guides(fill=guide_legend(nrow=3,byrow=TRUE))+ scale_y_continuous(expand = c(0,0)) + coord_flip()
}

 
  })
  
## table
  output$overalltable <- DT::renderDataTable({
    overviewtable <- ncpes %>%
      filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" & ncpes$Year == 2018 &
               ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear & ncpes$Cancer.Type == "All Cancers" & ncpes$cpesqtype %in% input$Question.Type) %>% 
      select(Question.Number,Question.Text,Number.of.responses,scored.percentage) %>% 
      datatable(rownames = FALSE, colnames = c("Question Number","Question","Number of Responses","Score"),
                extensions = c('Buttons',"Scroller"),
                options = list(
                  pageLength = 11,
                  dom = 'Bfrtip',
                  buttons = c('copy','csv', 'excel'),
                  deferRender = TRUE,
                  scrollY = "700px",
                  scrollCollapse = TRUE,
                  paging = FALSE
                  ))
    
    overviewtable
  })

  #####################################
  ##       Page 2- yearonyear        ##
  ##################################### 
  

##value box 
  output$yearonyearcomp <- renderValueBox({
    valueBox( 
      value = paste0(ncpes$Significant.Change.2017.2018[ncpes$Trust.Name == input$Trust.Name & ncpes$Year == 2018 &
                                               ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                               ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & ncpes$Question.Text == input$Question.Text]),
      "Has the Question Score changes based on 2017 CPES",  color = "purple"
    )
  })
  
    output$yearonyearlongcomp <- renderValueBox({
      valueBox( 
        value = paste0(ncpes$SignificantChangeOverallyears[ncpes$Trust.Name == input$Trust.Name &  ncpes$Year == 2018 &
                                                            ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                                            ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & ncpes$Question.Text == input$Question.Text]),
        "Has the Question Score changes based on earlier CPES",  color = "green"
      ) 
  })  
  
  output$yearonyeardif <- renderValueBox({
    valueBox(
      value = ncpes$scored.percentage[ncpes$Trust.Name == input$Trust.Name &  ncpes$Year == 2018 &
                                                    ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                                    ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & ncpes$Question.Text == input$Question.Text] -
        ncpes$scored.percentage[ncpes$Trust.Name == input$Trust.Name &  ncpes$Year == 2017 &
                                              ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                              ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & ncpes$Question.Text == input$Question.Text],
      "Differance between score in 2018 and 2017",icon = icon("ruler"), color = "blue"
    )
  })
  

#graph 1    
    output$yearonyeargraphnum <- renderPlot({
      yearonyeargraphnum <- ncpes %>% 
        filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                 ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & 
                 ncpes$Cancer.Type == "All Cancers" & ncpes$Question.Text == input$Question.Text) %>% 
        select(Question.Number,Question.Text,Year,Number.of.responses) %>% 
        ggplot(aes(Year,Number.of.responses)) + geom_bar(stat = "identity", aes(fill = Year)) +theme(legend.position = "none") 
      
      yearonyeargraphnum   
    
    })  
## graph 2
  output$yearonyeargraph <- renderPlot({
    yearonyeargraph <- ncpes %>% 
      filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
               ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & 
               ncpes$Cancer.Type == "All Cancers" & ncpes$Question.Text == input$Question.Text) %>% 
      select(Question.Number,Question.Text,Year,scored.percentage) %>% 
      ggplot(aes(Year,scored.percentage)) + geom_bar(stat = "identity",aes(fill = Year)) + theme(legend.position = "none")
    
    yearonyeargraph
      
  })
## Data table
  output$yearonyeardatatable <- DT::renderDataTable({
    yearonyeardatatable <- ncpes %>% 
      filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
               ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & 
               ncpes$Cancer.Type == "All Cancers" & ncpes$Question.Text == input$Question.Text) %>% 
      select(Question.Number,Question.Text,Year,Number.of.responses,scored.percentage) %>%
      datatable(rownames = FALSE, colnames = c("Question Number","Question","Year","Number of Responses","Score"),
                extensions = c('Buttons',"Scroller"),
                options = list(
                  pageLength = 11,
                  dom = 'Bfrtip',
                  buttons = c('copy','csv', 'excel'),
                  deferRender = TRUE,
                  scrollY = "700px",
                  scrollCollapse = TRUE,
                  paging = FALSE
                ))
  })

  #####################################
  ##       Page 3- Cancer Type       ##
  #####################################  
  output$cancertypenumber <- renderValueBox({
      valueBox( 
    value = paste0(ncpes$Number.of.responses[ncpes$Trust.Name == input$Trust.Name & ncpes$qnum == 59 & ncpes$Year == 2018 &
                                               ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == input$Cancer.Type &
                                               ncpes$adjusted.unadjusted.yearonyear == "unadjusted"]),
    "Number of Responses", icon = icon("users"), color = "yellow"
  )
  })
  output$cancertypecare <- renderValueBox({
    valueBox( 
      value = paste0(ncpes$scored.percentage[ncpes$Trust.Name == input$Trust.Name & ncpes$qnum == 59 & ncpes$Year == 2018 &
                                                 ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == input$Cancer.Type &
                                                 ncpes$adjusted.unadjusted.yearonyear == "unadjusted"],"/10"),
      "Overall, how would you rate your care?", icon = icon("thermometer-half"), color = "blue"
    )
  })
 output$bycancergraph <- renderPlot({
   bycancergraph <- ncpes %>%
     filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" & ncpes$Year == 2018 &
              ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & ncpes$Cancer.Type == input$Cancer.Type &
              ncpes$qnum != 59 & ncpes$cpesqtype %in% input$Question.Type) %>% 
     select(Trust.Name,Question.Number,Question.Text,scored.percentage)
   
   ggplot(bycancergraph,aes(reorder(Question.Number,desc(Question.Number)),scored.percentage)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
     xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
     theme(legend.position = "bottom")+ scale_y_continuous(expand = c(0,0)) +coord_flip()
 })
 
output$bycancertable <- DT::renderDataTable({
  bycancertable <- ncpes %>% 
    filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
             ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
             ncpes$Cancer.Type == input$Cancer.Type  & ncpes$Year == 2018 & ncpes$cpesqtype %in% input$Question.Type) %>% 
    select(Question.Number,Question.Text,Cancer.Type,Number.of.responses,scored.percentage) %>%
    datatable(rownames = FALSE, colnames = c("Question Number","Question","Cancer Type","Number of Responses","Score"),
              extensions = c('Buttons',"Scroller"),
              options = list(
                pageLength = 11,
                dom = 'Bfrtip',
                buttons = c('copy','csv', 'excel'),
                deferRender = TRUE,
                scrollY = "700px",
                scrollCollapse = TRUE,
                paging = FALSE
              ))
})

#####################################
##       Page 4- Comparison       ##
#####################################  

    output$compgraph <- renderPlot({
      if(input$Demographic == "Gender"){
      
      comgraph <-  ncpes %>% 
        filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                 ncpes$Gender %in% c("Male","Female"), ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                 ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2) %>% 
        select(Question.Number,Question.Text,Gender,Number.of.responses,scored.percentage) %>% 
        ggplot(aes(Gender,scored.percentage)) + geom_bar(stat = "identity",aes(fill = Gender)) + ylab("Score") +
        theme(legend.position = "none")
      
      comgraph
      }else if (input$Demographic == "Deprivation") {
        comgraph <-  ncpes %>% 
          filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD %in% c(1,2,3,4,5) &
                   ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                   ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2) %>% 
          select(Question.Number,Question.Text,IMD,Number.of.responses,scored.percentage) %>% 
          ggplot(aes(IMD,scored.percentage)) + geom_bar(stat = "identity",aes(fill = IMD)) + ylab("Score") +
          theme(legend.position = "none") + xlab("IMD quintile (1 = most deprived) ")
        
        comgraph
        }else if (input$Demographic == "Cancer Type"){
          comgraph <-  ncpes %>% 
            filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                     ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                     ncpes$Cancer.Type != "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2) %>% 
            select(Question.Number,Question.Text,Cancer.Type,Number.of.responses,scored.percentage) %>% 
            ggplot(aes(Cancer.Type,scored.percentage)) + geom_bar(stat = "identity",aes(fill = Cancer.Type)) + ylab("Score") +
            theme(legend.position = "none")
          comgraph
        }
      })
      
  
    output$comtable <- DT::renderDataTable({
      if(input$Demographic == "Gender"){
      comptable <- ncpes %>% 
        filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                 ncpes$Gender %in% c("Male","Female"), ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                 ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2) %>% 
        select(Question.Number,Question.Text,Gender,Number.of.responses,scored.percentage) %>% 
        datatable(rownames = FALSE, colnames = c("Question Number","Question","Gender","Number of Responses","Score"),
                  extensions = c('Buttons',"Scroller"),
                  options = list(
                    pageLength = 11,
                    dom = 'Bfrtip',
                    buttons = c('copy','csv', 'excel'),
                    deferRender = TRUE,
                    scrollY = "700px",
                    scrollCollapse = TRUE,
                    paging = FALSE
                  )) 
      }else if (input$Demographic == "Deprivation") {
        comptable <- ncpes %>% 
          filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD %in% c(1,2,3,4,5) &
                   ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                   ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2) %>% 
          select(Question.Number,Question.Text,IMD,Number.of.responses,scored.percentage) %>% 
          datatable(rownames = FALSE, colnames = c("Question Number","Question","IMD","Number of Responses","Score"),
                    extensions = c('Buttons',"Scroller"),
                    options = list(
                      pageLength = 11,
                      dom = 'Bfrtip',
                      buttons = c('copy','csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = "700px",
                      scrollCollapse = TRUE,
                      paging = FALSE
                    )) 
      }else if (input$Demographic == "Cancer Type"){
        comptable <- ncpes %>% 
          filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                   ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                   ncpes$Cancer.Type != "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2) %>% 
          select(Question.Number,Question.Text,Cancer.Type,Number.of.responses,scored.percentage) %>% 
          datatable(rownames = FALSE, colnames = c("Question Number","Question","Cancer Type","Number of Responses","Score"),
                    extensions = c('Buttons',"Scroller"),
                    options = list(
                      pageLength = 11,
                      dom = 'Bfrtip',
                      buttons = c('copy','csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = "700px",
                      scrollCollapse = TRUE,
                      paging = FALSE
                    )) 
      }
        
    })    
  output$compvalueboxdif <-renderValueBox({
    if(input$Demographic == "Gender"){
    valueBox(
      value = ncpes$scored.percentage[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                                      ncpes$Gender == "Male" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                      ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2] -
        ncpes$scored.percentage[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                                  ncpes$Gender == "Female" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                  ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2],
      "Difference between Male and Female score",icon = icon("venus-mars"),color = "blue"
    ) 
    }else if (input$Demographic == "Deprivation") {
      valueBox(
        value = ncpes$scored.percentage[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == 1 &
                                          ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                          ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2& is.na(ncpes$scored.percentage) != TRUE ] -
          ncpes$scored.percentage[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == 5 &
                                    ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                    ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2& is.na(ncpes$scored.percentage) != TRUE ],
        "Difference between most and least deprived score",icon = icon("pound-sign"),color = "blue"
      ) 
    }else if (input$Demographic == "Cancer Type"){
      valueBox(
        value = max(ncpes$scored.percentage[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                                          ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                          ncpes$Cancer.Type != "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2 & is.na(ncpes$scored.percentage) != TRUE ]) -
          min(ncpes$scored.percentage[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                                        ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                        ncpes$Cancer.Type != "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2& is.na(ncpes$scored.percentage) != TRUE]),
        "Range of scores between the different cancer types",icon = icon("dna"),color = "blue"
      ) 
    }  
    })
    output$compvalueboxsig <-renderValueBox({
      if(input$Demographic == "Gender"){
      valueBox(
      value =  ncpes$Significance.test[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                                  ncpes$Gender == "Male" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                  ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2],
      "Is the difference between male and female performance Significant?",color = "purple"
      )
      }else if (input$Demographic == "Deprivation") {
         valueBox(
          value =  ncpes$Significance.test[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == 1 &
                                             ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                             ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2],
          "Is the difference between the most and least deprived score Significant?",color = "purple"
        ) 
      }else if (input$Demographic == "Cancer Type"){
  
        valueBox(
          value =  sum(ncpes$Number.of.responses[ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                                                 ncpes$Gender == "Both" & ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                                                 ncpes$Cancer.Type != "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2]),
          "Total Number of responses across the cancer types",icon = icon("users"),color = "purple"
        )
  
      }
        
    })
 



})
