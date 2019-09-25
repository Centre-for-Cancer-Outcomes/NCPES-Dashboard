##server
shinyServer(function(input, output,session) {
  ## dynamicaly update selection
  observeEvent(input$Geog,updateSelectInput(session,"Trust.Name","Organization Name",
                                            choices = unique(factor(ncpes$Trust.Name[ncpes$Geog == input$Geog]))))
  
  
  
  
  #####################################
  ##       Page 1- Overview          ##
  #####################################

   ## row 2
  output$overallnumber <- renderValueBox({
    valueBox( 
      value = paste0(ncpes$scored.percentage[ncpes$Trust.Name == input$Trust.Name & ncpes$qnum == 59 & ncpes$Year == 2018 &
                                               ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                               ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear]),
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
    "Overall Care compared to Expexted", icon = icon("hospital"), color = "purple", width = 8 
  )
})  
  
## row 3

## graph
  output$oveallgraph <- renderPlot({
    overviewbarplot <- ncpes %>%
      filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" & ncpes$Year == 2018 &
               ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear & ncpes$Cancer.Type == "All Cancers" &
               ncpes$qnum != 59) %>% 
      select(Trust.Name,Question.Number,Performance,scored.percentage,Lower.95..confidence.interval,Upper.95..confidence.interval)
    
if(input$adjusted.unadjusted.yearonyear == "adjusted"){
      ggplot(overviewbarplot,aes(Question.Number,scored.percentage)) + geom_bar(stat = "identity",aes(fill = Performance)) +
        geom_errorbar(aes(ymin = Lower.95..confidence.interval,ymax =Upper.95..confidence.interval), width = 0.2, position=position_dodge(0.9)) + 
        xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
        theme(legend.position = "bottom")+ guides(fill=guide_legend(nrow=3,byrow=TRUE))+ scale_y_continuous(expand = c(0,0)) +coord_flip()
}else {
      ggplot(overviewbarplot,aes(Question.Number,scored.percentage)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
        geom_errorbar(aes(ymin = Lower.95..confidence.interval,ymax =Upper.95..confidence.interval), width = 0.2, position=position_dodge(0.9)) + 
        xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
        theme(legend.position = "bottom")+ scale_y_continuous(expand = c(0,0)) +coord_flip()
      }

 
  })
  
## table
  output$overalltable <- DT::renderDataTable({
    overviewtable <- ncpes %>%
      filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" & ncpes$Year == 2018 &
               ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == input$adjusted.unadjusted.yearonyear & ncpes$Cancer.Type == "All Cancers" ) %>% 
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
      "Has the Question Score changes based on previous CPES", icon = icon("calendar-alt"), color = "blue"
    )
  })
  
    output$yearonyearlongcomp <- renderValueBox({
      valueBox( 
        value = paste0(ncpes$SignificantChangeOverallyears[ncpes$Trust.Name == input$Trust.Name &  ncpes$Year == 2018 &
                                                            ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                                                            ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & ncpes$Question.Text == input$Question.Text]),
        "Has the Question Score changes based on earlier CPES", icon = icon("chart-line"), color = "green"
      ) 
  })  
  
  
  

#graph 1    
    output$yearonyeargraphnum <- renderPlot({
      yearonyeargraphnum <- ncpes %>% 
        filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                 ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & 
                 ncpes$Cancer.Type == "All Cancers" & ncpes$Question.Text == input$Question.Text) %>% 
        select(Question.Number,Question.Text,Year,Number.of.responses) %>% 
        ggplot(aes(Year,Number.of.responses)) + geom_bar(stat = "identity") 
      
      yearonyeargraphnum   
    
    })  
## graph 2
  output$yearonyeargraph <- renderPlot({
    yearonyeargraph <- ncpes %>% 
      filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
               ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "yearonyear" & 
               ncpes$Cancer.Type == "All Cancers" & ncpes$Question.Text == input$Question.Text) %>% 
      select(Question.Number,Question.Text,Year,scored.percentage) %>% 
      ggplot(aes(Year,scored.percentage)) + geom_bar(stat = "identity") 
    
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
                                                 ncpes$adjusted.unadjusted.yearonyear == "unadjusted"]),
      "Number of Responses", icon = icon("thermometer-half"), color = "blue"
    )
  })
 output$bycancergraph <- renderPlot({
   bycancergraph <- ncpes %>%
     filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" & ncpes$Year == 2018 &
              ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & ncpes$Cancer.Type == input$Cancer.Type &
              ncpes$qnum != 59) %>% 
     select(Trust.Name,Question.Number,Question.Text,scored.percentage)
   
   ggplot(bycancergraph,aes(Question.Number,scored.percentage)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
     xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
     theme(legend.position = "bottom")+ scale_y_continuous(expand = c(0,0)) +coord_flip()
 })
 
output$bycancertable <- DT::renderDataTable({
  bycancertable <- ncpes %>% 
    filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
             ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
             ncpes$Cancer.Type == input$Cancer.Type  & ncpes$Year == 2018) %>% 
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
    
    output$compgraph <- renderPlot({
      comgraph <-  ncpes %>% 
        filter(ncpes$Geog == input$Geog & ncpes$Trust.Name == input$Trust.Name & ncpes$IMD == "All" &
                 ncpes$Gender %in% c("Male","Female"), ncpes$adjusted.unadjusted.yearonyear == "unadjusted" & 
                 ncpes$Cancer.Type == "All Cancers"  & ncpes$Year == 2018 & ncpes$Question.Text == input$Question.Text2) %>% 
        select(Question.Number,Question.Text,Gender,Number.of.responses,scored.percentage) %>% 
        ggplot(aes(Gender,scored.percentage)) + geom_bar(stat = "identity")
      
      comgraph
    })
    
  
  }else if (input$Demographic == "Deprivation") {
    
  }else if (input$Demographic == "Cancer Type") {
  
  }
})

})
