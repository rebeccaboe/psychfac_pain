#Author: Rebecca Astrid Böhme 
#fSEM script BA: The Construction of Pain - Linking Psychological Factors to Pain thresholds

        library(psych)
        library(lavaan)
        library(semPlot)
        library(tidyverse)
        library(qpcR)


###regression analyses
  #outcome variable: pain threshold/ pain sensitivity - y
  #predictor: mental and physical well-being - x     
    

##2nd measurement model

#pain model data: has one observation per participant                 
      pain_model_data <- subset(VMP_pain_data, trialN != "1" & trialN != "2") #remove row based on condition
                
#pain model

      Model_pain <- "
      pain sensitivity =~ rescaled_cold + rescaled_warm + rescaled_tgi "
        
        
        Model_pain.fit <- cfa(model = Model_pain, data = pain_model_data, orthogonal = FALSE,
                              sample.nobs = NULL, std.lv = FALSE, estimator = "ML")    #orthogonal = FALSE, latents are not independent
                                                                                       #std.lv = FALSE, Marker Method as default 
                                                                                       #estimator = 'ML', not robust one
      
        semPaths(Model_pain.fit, what = "path", 
                 whatLabels = "std", 
                 #style = "lisrel", 
                 layout = "tree", 
                 edge.color = 'dark grey',
                 edge.label.cex = 0.75, 
                 rotation = 2,
                 exoCov = TRUE) #represent fitted model graphically
        
        summary(Model_pain.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
        
        head(predict(Model_pain.fit))
        pain_sensitivity_fac_score <- as.data.frame(predict(Model_pain.fit)) 
        pain_sensitivity_fac_score <- as.numeric(predict(Model_pain.fit)) 
        

##Full SEM: with 1st set of latent variables (2nd order model is technically already fSEM, splitted up to avoid errors)        
       
#creating a subset for FULL SEM
        VMP_data_FULL <- qpcR ::: cbind.na(VMP_data_analysis_final,pain_model_data)
        names(VMP_data_FULL)[names(VMP_data_FULL) == 'id'] <- 'ID'
        VMP_data_FULL <- VMP_data_FULL[,!duplicated(colnames(VMP_data_FULL))]
        VMP_data_FULL <- VMP_data_FULL[complete.cases(VMP_data_FULL[ , c(4:107)]),] 
      
      FULL_model <- "
            #measurement model
      affect =~ 
            MDI_1 + MDI_2 + MDI_3 + MDI_4 + MDI_5 + MDI_6 + MDI_8b
      psychological distress =~ 
            PSS_2 + PSS_3 + PSS_6 + PSS_10 + WEMWBS_3 + WEMWBS_10
      physiological state =~ 
            MFI20_1 + MFI20_3 +   
            MFI20_7 + MFI20_8 +
            MFI20_11 + MFI20_12 + MFI20_20 
      quality of life =~ 
                  WHO5_1 + WHO5_2 +  
                  WHOQOL_5 + WHOQOL_10 + 
                  WHOQOL_17 + WHOQOL_19 +
                  WHOQOL_26
                  
      affect ~~ psychologicaldistress
      affect ~~ physiologicalstate
      affect ~~ qualityoflife
      psychologicaldistress ~~     physiologicalstate
      psychologicaldistress ~~     qualityoflife
      physiologicalstate ~~        qualityoflife
       
      pain sensitivity =~ rescaled_cold + rescaled_warm + rescaled_tgi   
      
                  #regression

      pain sensitivity ~ affect + psychological distress + physiological state + quality of life
      "

      FULL_model.fit <- sem(model = FULL_model, data = VMP_data_FULL, orthogonal = TRUE,  
                            sample.nobs = NULL, std.lv = TRUE, estimator = "MLR")    #orthogonal=TRUE, because dependencies are defined in model
     
        vartable(FULL_model.fit) 
        
        semPaths(FULL_model.fit,
                 whatLabels = "std", 
                 layout = "tree", 
                 style = "Lisrel",
                 edge.color = 'dark grey',
                 edge.label.cex = 0.5, 
                 exoCov = TRUE)
                 
        
        summary(FULL_model.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)        
        


################CAVE###################################CAVE####################################CAVE##########################
        #to run this part of the script, run factor_score_script first
      
##regression analyses to examine relation between pain and mpwb (using the scale score)
        plot(MPWB_quest_data$MPWB_general_score,pain_sensitivity_fac_score)
        pain_score_MPWB_regression <- lm(pain_sensitivity_fac_score ~ MPWB_quest_data$MPWB_general_score)
        summary(pain_score_MPWB_regression)

        
##examines the pain thresholds as variables, instead of overall factor pain sensitivity
    #for TGI    
        plot(MPWB_quest_data$MPWB_general_score,VMP_data_FULL$rescaled_tgi)
        tgi_MPWB_regression <- lm(VMP_data_FULL$rescaled_tgi ~ MPWB_quest_data$MPWB_general_score)
        summary(tgi_MPWB_regression)
        
        plot(MPWB_quest_data$MPWB_affect_score,VMP_data_FULL$rescaled_tgi)
        tgi_affect_regression <- lm(VMP_data_FULL$rescaled_tgi ~ MPWB_quest_data$MPWB_affect_score)
        summary(tgi_affect_regression)
        
        plot(MPWB_quest_data$MPWB_distress_score,VMP_data_FULL$rescaled_tgi)
        tgi_distress_regression <- lm(VMP_data_FULL$rescaled_tgi ~ MPWB_quest_data$MPWB_distress_score)
        summary(tgi_distress_regression)
        
        plot(MPWB_quest_data$MPWB_psychstate_score,VMP_data_FULL$rescaled_tgi)
        tgi_psychstate_regression <- lm(VMP_data_FULL$rescaled_tgi ~ MPWB_quest_data$MPWB_psychstate_score)
        summary(tgi_psychstate_regression)
        
        plot(MPWB_quest_data$MPWB_qol_score,VMP_data_FULL$rescaled_tgi)
        tgi_qol_regression <- lm(VMP_data_FULL$rescaled_tgi ~ MPWB_quest_data$MPWB_qol_score)
        summary(tgi_qol_regression)
        
        
    #for warm pain
        plot(MPWB_quest_data$MPWB_general_score,VMP_data_FULL$rescaled_warm)
        warm_MPWB_regression <- lm(VMP_data_FULL$rescaled_warm ~ MPWB_quest_data$MPWB_general_score)
        summary(warm_MPWB_regression)
        
        plot(MPWB_quest_data$MPWB_affect_score,VMP_data_FULL$rescaled_warm)
        warm_affect_regression <- lm(VMP_data_FULL$rescaled_warm ~ MPWB_quest_data$MPWB_affect_score)
        summary(warm_affect_regression)
        
        plot(MPWB_quest_data$MPWB_distress_score,VMP_data_FULL$rescaled_warm)
        warm_distress_regression <- lm(VMP_data_FULL$rescaled_warm ~ MPWB_quest_data$MPWB_distress_score)
        summary(warm_distress_regression)
        
        plot(MPWB_quest_data$MPWB_psychstate_score,VMP_data_FULL$rescaled_warm)
        warm_psychstate_regression <- lm(VMP_data_FULL$rescaled_warm ~ MPWB_quest_data$MPWB_psychstate_score)
        summary(warm_psychstate_regression)
        
        plot(MPWB_quest_data$MPWB_qol_score,VMP_data_FULL$rescaled_warm)
        warm_qol_regression <- lm(VMP_data_FULL$rescaled_warm ~ MPWB_quest_data$MPWB_qol_score)
        summary(warm_qol_regression)
        
        
      #for cold pain
        plot(MPWB_quest_data$MPWB_general_score,VMP_data_FULL$rescaled_cold)
        cold_MPWB_regression <- lm(VMP_data_FULL$rescaled_cold ~ MPWB_quest_data$MPWB_general_score)
        summary(cold_MPWB_regression)
        
        plot(MPWB_quest_data$MPWB_affect_score,VMP_data_FULL$rescaled_cold)
        cold_affect_regression <- lm(VMP_data_FULL$rescaled_cold ~ MPWB_quest_data$MPWB_affect_score)
        summary(cold_affect_regression)
        
        plot(MPWB_quest_data$MPWB_distress_score,VMP_data_FULL$rescaled_cold)
        cold_distress_regression <- lm(VMP_data_FULL$rescaled_cold ~ MPWB_quest_data$MPWB_distress_score)
        summary(cold_distress_regression)
        
        plot(MPWB_quest_data$MPWB_psychstate_score,VMP_data_FULL$rescaled_cold)
        cold_psychstate_regression <- lm(VMP_data_FULL$rescaled_cold ~ MPWB_quest_data$MPWB_psychstate_score)
        summary(cold_psychstate_regression)
        
        plot(MPWB_quest_data$MPWB_qol_score,VMP_data_FULL$rescaled_cold)
        cold_qol_regression <- lm(VMP_data_FULL$rescaled_cold ~ MPWB_quest_data$MPWB_qol_score)
        summary(cold_qol_regression)
        