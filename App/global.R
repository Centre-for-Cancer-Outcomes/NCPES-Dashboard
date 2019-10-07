##load in packages needed in UI and Server 
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(dqshiny)

##load in data set for app
#ncpes<-read.csv("C:/Users/DEGAN001/Documents/GIT clones/COSD_Level2 App/NCPES Dashboard/CPESDataset.csv",sep = ",", na.strings = "NA",
 #               stringsAsFactors = FALSE)
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

regionlookup <- read.csv("https://raw.githubusercontent.com/Centre-for-Cancer-Outcomes/NCPES-Dashboard/master/CPES_geog_lookuptable.csv",
                         sep=",",header = TRUE)
#regionlookup<-read.csv("C:/Users/DEGAN001/Documents/GIT clones/COSD_Level2 App/NCPES Dashboard/CPES_geog_lookuptable.csv",sep = ",", na.strings = "NA",
#               stringsAsFactors = FALSE)
regionlookup$CALNCV18NM <- trimws(regionlookup$CALNCV18NM)

ncpes <- left_join(ncpes,select(regionlookup,CCG18CDH,CALNCV18NM), by = c("Trust.Code" ="CCG18CDH" ))
## add cpes question types 

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



ncpes$Significance.test[ncpes$Significance.test == "Sig"] <- "Significant"
ncpes$Significance.test[ncpes$Significance.test == "Not sig"] <- "Not Significant"

ncpes$cpesqtype <- factor(ncpes$cpesqtype, levels = c("Seeing your GP","Seeing your GP","Diagnostic Test","Finding out about your condition ",
                                                      "Deciding the best treatment","Clinical Nurse Specialist","Support","Operations",
                                                      "Hospital care as an Inpatient ","Hospital casre as a day paytient/outpatient","Home care and support",
                                                      "Care from your GP", "Your overall NHS care"))

## set theme for ggplot
theme_set(theme_classic())
