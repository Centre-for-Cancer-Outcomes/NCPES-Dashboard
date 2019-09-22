## UIpage
header <- dashboardHeader(title = "NCPES 2018 Results",
                          tags$li(class = "dropdown",tags$a(href ="https://github.com/Centre-for-Cancer-Outcomes?tab=repositories",icon("github"),
                                                            "Source Code", target = "_blank")))


sidebar <- dashboardSidebar(sidebarMenu(
                             selectInput("Geog","Organization Type",choices = unique(factor(ncpes$Geog)), selected = "Trust"),
                             selectInput("Trust.Name","Organization Name", choices = "",selected = ""),
                             menuItem("Overview Dashoard", tabName = "overview",icon = icon("dashboard")),
                             menuItem("Timeseries", tabName  = "timeseries" , icon = icon("chart-line")),
                             menuItem("By Cancer",tabName = "cancer",icon = icon("x-ray"))
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName ="overview",
     fluidRow(
       selectInput("adjusted.unadjusted.yearonyear","Adjusted/Unadjusted",choices = c("adjusted","unadjusted"), selected = "adjusted"),
       valueBoxOutput("overallnumber"),
       valueBoxOutput("Overallcare"),
       valueBoxOutput("overallsigdif")
     ),
     fluidRow(
       box(plotOutput("oveallgraph",height = 800), title = "NCPES Results",status = "primary",solidHeader = TRUE),
       box(DT::dataTableOutput("overalltable",height = 500),title = "NCPES Data Table",status = "success",solidHeader = TRUE)
     ))
  )
)
    
dashboardPage(header,sidebar,body)
