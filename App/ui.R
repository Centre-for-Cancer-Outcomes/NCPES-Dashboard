## UIpage
header <- dashboardHeader(title = "NCPES 2018 Results",
                          tags$li(class = "dropdown",tags$a(href ="https://github.com/Centre-for-Cancer-Outcomes?tab=repositories",icon("github"),
                                                            "Source Code", target = "_blank")),
                          tags$li(class = "dropdown",tags$a(href ="https://www.uclh.nhs.uk/OurServices/ServiceA-Z/Cancer/NCV/CCO/Pages/Home.aspx#targetText=The%20Centre%20for%20Cancer%20Outcomes,their%20treatment%20and%20care%20pathway.",icon("internet-explorer"),
                                                            "Centre for Cancer Outcomes", target = "_blank")),
                          tags$li(class = "dropdown",tags$a(href ="https://www.ncpes.co.uk/reports/2018-reports/local-reports-2018",icon("file-medical-alt"),
                                                            "NCPES Reports", target = "_blank"))
                          )


sidebar <- dashboardSidebar(sidebarMenu(
                             selectInput("Geog","Organization Type",choices = unique(factor(ncpes$Geog)), selected = "Trust"),
                             selectInput("Trust.Name","Organization Name", choices = "",selected = ""),
                             pickerInput("Question.Type","Question Type",choices = unique(ncpes$cpesqtype), 
                                         options = list(
                                           `actions-box` = TRUE, 
                                           size = 10,
                                           `selected-text-format` = "count > 20"
                                           ),multiple = TRUE,selected = unique(ncpes$cpesqtype)  ),
                             menuItem("Overview Dashoard", tabName = "overview",icon = icon("dashboard")),
                             menuItem("Timeseries", tabName  = "timeseries" , icon = icon("chart-line")),
                             menuItem("By Cancer",tabName = "cancer",icon = icon("x-ray")),
                             menuItem("Comparison", tabName = "demographic",icon = icon("venus-mars"))
))



body <- dashboardBody(
  tabItems(
    tabItem(tabName ="overview",
     fluidRow(
       box(radioGroupButtons("adjusted.unadjusted.yearonyear","",choices = c("adjusted","unadjusted"), selected = "adjusted",
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square", 
                                            style = "color: steelblue"),
                               no = tags$i(class = "fa fa-square-o", 
                                           style = "color: steelblue")),justified = TRUE),
          title = "Data Type", status = "info" )
       ),
     fluidRow(
       valueBoxOutput("overallnumber"),
       valueBoxOutput("Overallcare"),
       valueBoxOutput("overallsigdif")
     ),
     fluidRow(
       box(plotOutput("oveallgraph",height = 900), title = "NCPES Results",status = "primary",solidHeader = TRUE),
       box(DT::dataTableOutput("overalltable",height = 900),title = "NCPES Data Table",status = "success",solidHeader = TRUE)
     )),
    tabItem(tabName = "timeseries",
            fluidRow(
       box(selectInput("Question.Text","Question",choices = unique(factor(ncpes$Question.Text[order(ncpes$Question.Number)])),
                       selected = "Before you were told you needed to go to hospital about cancer, how many times did you see
                       your GP (family doctor) about the health problem caused by cancer?" )
       ,width = 10)),
       fluidRow(
         box(plotOutput("yearonyeargraph"), title = "CPES Results Over Time",status = "primary",solidHeader = TRUE),
               valueBoxOutput("yearonyearcomp"),
         valueBoxOutput("yearonyearlongcomp")
               ),
      fluidRow(
         box(plotOutput("yearonyeargraphnum"), title = "CPES Number of Responses Over Time",status = "primary",solidHeader = TRUE),
      box(DT::dataTableOutput("yearonyeardatatable"),title = "Year on Year Results Data Tabel", status = "success", solidHeader = TRUE)
       )),
    tabItem(tabName = "cancer",
           fluidRow( box(  radioGroupButtons(inputId = "Cancer.Type",label = "Cancer Type",
                                         choices = unique(ncpes$Cancer.Type), selected = "Lung" ,checkIcon = list(
                                           yes = tags$i(class = "fa fa-check-square", 
                                                        style = "color: steelblue"),
                                           no = tags$i(class = "fa fa-square-o", 
                                                       style = "color: steelblue")))), "", status = "info" ),
           fluidRow(
             valueBoxOutput("cancertypenumber"),
             valueBoxOutput("cancertypecare")
           ),
           fluidRow(
             box(plotOutput("bycancergraph", height = 850),title = "NCPES Results",status = "primary",solidHeader = TRUE),
             box(DT::dataTableOutput("bycancertable"),title = "Cancer Type CPES Data Table",status = "success", solidHeader = TRUE )
           )
            
            
            ),
    tabItem(tabName = "demographic",
          fluidRow(box(print("The Data needed for this page is only available for Cancer Alliances and National Data"),background = "maroon")),
          fluidRow(box(radioGroupButtons("Demographic","",choices = c("Gender","Deprivation","Cancer Type"), selected = "Gender",
                                         checkIcon = list(
                                           yes = tags$i(class = "fa fa-check-square", 
                                                        style = "color: steelblue"),
                                           no = tags$i(class = "fa fa-square-o", 
                                                       style = "color: steelblue")),justified = TRUE),
                       title = "Data Type", status = "info" ),
                   box(selectInput("Question.Text2","Question",choices = unique(factor(ncpes$Question.Text[order(ncpes$Question.Number)])),
                                   selected = "Before you were told you needed to go to hospital about cancer, how many times did you see
                       your GP (family doctor) about the health problem caused by cancer?" ),
                                     title = "CPES Question", status = "info" 
                   )),
          fluidRow(
            plotOutput("compgraph")
          ),
          fluidRow(
            box(DT::dataTableOutput("comtable"),"CPES Data table", status = "success", solidHeader = TRUE),
            valueBoxOutput("compvalueboxdif"),
            valueBoxOutput("compvalueboxsig")
          )
          )
  ))











    
dashboardPage(header,sidebar,body)
