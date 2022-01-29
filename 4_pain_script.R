#Author: Rebecca Astrid Böhme 
#Pain script BA: The Construction of Pain - Linking Psychological Factors to Pain thresholds

    library(rio) 
    library(psych)
    library(tidyverse)
    library(lavaan)
    library(semPlot)
  
  
###Pain thresholds
    #in data set 1=female/2=male
  
###input data:
    VMP_pain_complete <- read.csv(file='VMP_qstThr.csv')
    VMP_pain_complete <- subset(VMP_pain_complete,
                                select = c(id,age,sex,task,quality,trialN,threshold))
    VMP_pain <- subset(VMP_pain_complete, task != "detect" & trialN != "0") # Remove row based on condition
    
  
###average thresholds

  ##warm pain 
   mean_warm <- VMP_pain %>%
      group_by(id) %>%
        filter(quality=="warm") %>%
          summarize(mean_warm=mean(threshold))
   mean_warm   

  ##cold pain 
   mean_cold <- VMP_pain %>%
     group_by(id) %>%
     filter(quality=="cold") %>%
     summarize(mean_cold=mean(threshold))
   mean_cold
  
  ##TGI 
   mean_tgi <- VMP_pain %>%
     group_by(id) %>%
     filter(quality=="tgi") %>%
     summarize(mean_tgi=mean(threshold))
   mean_tgi

   
   VMP_pain <- merge(VMP_pain, mean_tgi, by = ,all.x = TRUE, all.y = TRUE)
   VMP_pain <- merge(VMP_pain, mean_warm, by = ,all.x = TRUE, all.y = TRUE)
   VMP_pain <- merge(VMP_pain, mean_cold, by = ,all.x = TRUE, all.y = TRUE)
   
   
###rescale pain data
   #calculate tgi difference:  TGI = the difference between warm and cold temperatures
   #30 -(warm-30)
   
   VMP_pain$rescaled <- as.numeric(
     ifelse(VMP_pain$quality == "cold", 32 - VMP_pain$threshold,
            ifelse(VMP_pain$quality == "warm", (32 - VMP_pain$threshold)* - 1,
                   ifelse(VMP_pain$quality == "tgi", (32 - (30 - (VMP_pain$threshold - 30))),"incorrect data"))))
   
   
   #creating a data set for pain modelling
   VMP_pain_cold <- subset(VMP_pain, task != "detect" & quality != "warm" & quality != "tgi") 
   names(VMP_pain_cold)[names(VMP_pain_cold) == "quality"] <- 'quality cold'
   names(VMP_pain_cold)[names(VMP_pain_cold) == "threshold"] <- 'threshold_cold'
   names(VMP_pain_cold)[names(VMP_pain_cold) == "rescaled"] <- 'rescaled_cold'
   
   
   VMP_pain_warm <- subset(VMP_pain, task != "detect" & quality != "cold" & quality != "tgi")  
   names(VMP_pain_warm)[names(VMP_pain_warm) == "quality"] <- 'quality warm'
   names(VMP_pain_warm)[names(VMP_pain_warm) == "threshold"] <- 'threshold_warm'
   names(VMP_pain_warm)[names(VMP_pain_warm) == "rescaled"] <- 'rescaled_warm'
   
   
   VMP_pain_tgi <- subset(VMP_pain, task != "detect" & quality != "warm" & quality != "cold")  
   names(VMP_pain_tgi)[names(VMP_pain_tgi) == "quality"] <- 'quality tgi'
   names(VMP_pain_tgi)[names(VMP_pain_tgi) == "threshold"] <- 'threshold_tgi'
   names(VMP_pain_tgi)[names(VMP_pain_tgi) == "rescaled"] <- 'rescaled_tgi'
   
   
   VMP_pain_data <- cbind(VMP_pain_cold,VMP_pain_warm,VMP_pain_tgi)
   VMP_pain_data <- VMP_pain_data[,!duplicated(colnames(VMP_pain_data))]
   
   