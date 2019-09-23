##server
shinyServer(function(input, output,session) {
  ## dynamicaly update selection
  observeEvent(input$Geog,updateSelectInput(session,"Trust.Name","Organization Name",
                                            choices = unique(factor(ncpes$Trust.Name[ncpes$Geog == input$Geog]))))
  
  
  
  
  #####################################
  ##       Page 1- Overview          ##
  #####################################
  ## row 1
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
     "Number of Responses", icon = icon("users"), color = "navy"
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
  
## row 2

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
  

})
