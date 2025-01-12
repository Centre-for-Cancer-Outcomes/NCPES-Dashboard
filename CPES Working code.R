## download in packages
list.of.packages <- c("tidyverse","shiny","shinydashboard","lubridate","DT","scales","qicharts","shinyjs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE, repos='http://cran.rstudio.com/')

## load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(scales)
library(qicharts)
library(shinyjs)
library(readr)
library(httr)
library(googledrive)
library(RColorBrewer)

ncpes<-read.csv("C:/Users/DEGAN001/Documents/GIT clones/COSD_Level2 App/NCPES Dashboard/CPESDataset.csv",sep = ",", na.strings = "NA",
                stringsAsFactors = FALSE)
ncpes<-read.csv("https://raw.githubusercontent.com/Centre-for-Cancer-Outcomes/NCPES-Dashboard/master/CPESDataset.csv",sep = ",", na.strings = "NA",
                stringsAsFactors = FALSE)
ncpes$qnum <- as.numeric(gsub("[a-zA-Z ]", "", ncpes$Question.Number))
ncpes$scored.percentage <-  ifelse(ncpes$scored.percentage > 1,ncpes$scored.percentage,ncpes$scored.percentage * 100)
ncpes$Lower.95..confidence.interval <-ifelse(ncpes$Lower.95..confidence.interval >1,ncpes$Lower.95..confidence.interval,ncpes$Lower.95..confidence.interval *100)
ncpes$Upper.95..confidence.interval <- ifelse(ncpes$Upper.95..confidence.interval >1,ncpes$Upper.95..confidence.interval,ncpes$Upper.95..confidence.interval *100)
ncpes$Expected.Range..lower. <- ifelse(ncpes$Expected.Range..lower. > 1,ncpes$Expected.Range..lower.,ncpes$Expected.Range..lower. * 100)
ncpes$Expected.Range..upper. <- ifelse(ncpes$Expected.Range..upper. > 1,ncpes$Expected.Range..upper.,ncpes$Expected.Range..upper. * 100)



ncpes$abrvperformance <- ifelse(ncpes$Performance.Rating == 1, "Above",
                                ifelse(ncpes$Performance.Rating == 2,"As Expected","Below"))

ncpes$Number.of.responses <- as.numeric(gsub(",","",ncpes$Number.of.responses))



levels(ncpes$Question.Number)
regionlookup <- read.csv("https://raw.githubusercontent.com/Centre-for-Cancer-Outcomes/NCPES-Dashboard/master/CPES_geog_lookuptable.csv",
                         sep=",",header = TRUE)
regionlookup$CALNCV18NM <- trimws(regionlookup$CALNCV18NM)

ncpes <- left_join(ncpes,select(regionlookup,CCG18CDH,CALNCV18NM), by = c("Trust.Code" ="CCG18CDH" ))

unique((ncpes$CALNCV18NM))


unique(factor(ncpes$Trust.Name[ncpes$Geog == "Trust" & trimws(ncpes$CALNCV18NM) == "North Central And North East London"]))
##ncpes q type 

cpesqnum <- c("Q1","Q2","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20",
              "Q21","Q22","Q23","Q25","Q26","Q28","Q29","Q30","Q31","Q32","Q33","Q34","Q35","Q36","Q37","Q38","Q39",
              "Q41","Q42","Q44","Q45","Q47","Q48","Q49","Q50","Q51","Q52","Q53","Q54","Q55","Q56","Q57","Q58","Q59"
)
cpesqtype <- c("Seeing your GP","Seeing your GP","Diagnostic Test","Diagnostic Test","Diagnostic Test",
               "Finding out about your condition ","Finding out about your condition ","Finding out about your condition ",
               "Finding out about your condition ","Deciding the best treatment","Deciding the best treatment",
               "Deciding the best treatment","Deciding the best treatment","Deciding the best treatment",
               "Clinical Nurse Specialist","Clinical Nurse Specialist","Clinical Nurse Specialist","Support","Support",
               "Support","Support","Operations","Operations","Hospital care as an Inpatient ","Hospital care as an Inpatient ",
               "Hospital care as an Inpatient ","Hospital care as an Inpatient ","Hospital care as an Inpatient ",
               "Hospital care as an Inpatient ","Hospital care as an Inpatient ","Hospital care as an Inpatient ",
               "Hospital care as an Inpatient ","Hospital care as an Inpatient ","Hospital care as an Inpatient ",
               "Hospital care as an Inpatient ","Hospital casre as a day paytient/outpatient",
               "Hospital casre as a day paytient/outpatient","Hospital casre as a day paytient/outpatient",
               "Hospital casre as a day paytient/outpatient","Hospital casre as a day paytient/outpatient",
               "Hospital casre as a day paytient/outpatient","Home care and support", "Home care and support", 
               "Home care and support", "Care from your GP","Care from your GP","Your overall NHS care",
               "Your overall NHS care","Your overall NHS care","Your overall NHS care","Your overall NHS care",
               "Your overall NHS care"
)
cpesqtypelookup <- data.frame(cpesqnum,cpesqtype,stringsAsFactors = FALSE)
ncpes<- left_join(ncpes,cpesqtypelookup, by = c("Question.Number" = "cpesqnum"))
ncpes$Question.Number <- factor(ncpes$Question.Number, levels = c("Q1","Q2","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16",
                                                                  "Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q25","Q26","Q28","Q29","Q30",
                                                                  "Q31","Q32","Q33","Q34","Q35","Q36","Q37","Q38","Q39","Q41","Q42","Q44",
                                                                  "Q45","Q47","Q48","Q49","Q50","Q51","Q52","Q53","Q54","Q55","Q56","Q57",
                                                                  "Q58","Q59"
))

unique(factor(ncpes$cpesqtype))

unique(factor(ncpes$Trust.Name[ncpes$Geog == "Trust"]))

unique(factor(ncpes$Cancer.Type[ncpes$adjusted.unadjusted.yearonyear == "unadjusted"]))
## summary value box
ncpes$scored.percentage[ncpes$Trust.Name == trust & ncpes$qnum == 59 & ncpes$Year == 2018 &
                          ncpes$Gender == "Both" & ncpes$IMD == "All" & ncpes$Cancer.Type == "All Cancers" &
                          ncpes$adjusted.unadjusted.yearonyear == "adjusted"]
## summary bar chart
unique(ncpes$Trust.Name)
trust <- "University College London Hospitals NHS Foundation Trust" 
trust <-"Whittington Health NHS Trust"
theme_set(theme_classic())

overviewbarplot <- ncpes %>%
                  filter(ncpes$Geog == "Trust" & ncpes$Trust.Name == trust & ncpes$IMD == "All" & ncpes$Year == 2018 &
                         ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "adjusted" & ncpes$Cancer.Type == "All Cancers" &
                         ncpes$qnum != 59) %>% 
                  select(Trust.Name,Question.Number,Performance,scored.percentage,Lower.95..confidence.interval,Upper.95..confidence.interval)
                  
if("adjusted" == "adjusted"){
ggplot(overviewbarplot,aes(Question.Number,scored.percentage)) + geom_bar(stat = "identity",aes(fill = Performance)) +
                  geom_errorbar(aes(ymin = Lower.95..confidence.interval,ymax =Upper.95..confidence.interval), width = 0.2, position=position_dodge(0.9)) + 
                  xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
                  theme(legend.position = "bottom")+ guides(fill=guide_legend(nrow=3,byrow=TRUE))+ scale_y_continuous(expand = c(0,0)) +coord_flip() + scale_fill_manual("legend", values = c("Adjusted Score above Expected Upper Range" = "#00BA38", 
                                                                                                                                                                                              "Adjusted Score between Expected Upper and Lower Ranges" = "#619CFF",
                                                                                                                                                                                              "Adjusted Score below Expected Lower Range" = "#F8766D"))
}else {

ggplot(overviewbarplot,aes(Question.Number,scored.percentage)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
    geom_errorbar(aes(ymin = Lower.95..confidence.interval,ymax =Upper.95..confidence.interval), width = 0.2, position=position_dodge(0.9)) + 
    xlab("Question Number") + ylab("Score") + ggtitle("Orginization CPES Results") + 
    theme(legend.position = "bottom")+ scale_y_continuous(expand = c(0,0)) +coord_flip() + scale_fill_manual(values = c("#00BA38","#F8766D","#619CFF"))
                                                                                                             
}
overviewbarplot   

## summary table

ggpoverviewtable <- ncpes %>%
  filter(ncpes$Geog == "Trust" & ncpes$Trust.Name == trust & ncpes$IMD == "All" & ncpes$Year == 2018 &
           ncpes$Gender == "Both", ncpes$adjusted.unadjusted.yearonyear == "adjusted" & ncpes$Cancer.Type == "All Cancers" ) %>% 
  select(Question.Number,Question.Text,Number.of.responses,scored.percentage) %>% 
  datatable(rownames = FALSE, colnames = c("Question Number","Question","Number of Responses","Score"),
             extensions = c('Buttons'),
            options = list(
              pageLength = 52,
              dom = 'Bfrtip',
              buttons = c('copy','csv', 'excel'),
              deferRender = TRUE,
              columnDefs = list(list(width = '50px', targets = "_all"))
            ))

ggpoverviewtable



unique(ncpes$Question.Text)


unique(ncpes$Question.Text[order(ncpes$Question.Number) & ncpes$cpesqtype %in% c("Operations")])


rsconnect::setAccountInfo(name='centre-for-cancer-outcomes',
                          token='E4914B3C908FB8F54D8FE9D8BF025C14',
                          secret='iqKcnDHcapcQpel2l6tLsSuS9zV9miS2lEDVXMIN')

library(rsconnect)
rsconnect::deployApp("C:/Users/david/Documents/GitHub/Dashboard/App")
"C:/Users/david/Documents/GitHub/Dashboard/App"


ncpesreverse <- ncpes %>% select(Question.Number,qnum) %>% filter(qnum != 59)

ncpes$cpesqtype <- factor(ncpes$cpesqtype, levels = c("Seeing your GP","Diagnostic Test","Finding out about your condition ",
                                                      "Deciding the best treatment","Clinical Nurse Specialist","Support","Operations",
                                                      "Hospital care as an Inpatient ","Hospital casre as a day paytient/outpatient","Home care and support",
                                                      "Care from your GP", "Your overall NHS care"))
+ scale_fill_manual(breaks = c("Adjusted Score above Expected Upper Range","Adjusted Score between Expected Upper and Lower Ranges",
                               "Adjusted Score below Expected Lower Range"),
                    values = c("#00BA38","#619CFF","#F8766D"))
scale_fill_manual("legend", values = c("Adjusted Score above Expected Upper Range" = "black", 
                                       "Adjusted Score between Expected Upper and Lower Ranges" = "orange",
                                       "Adjusted Score below Expected Lower Range" = "blue"))


unique(ncpes$Question.Text[ncpes$cpesqtype %in%c("Seeing your GP")])
