#Author: Rebecca Astrid Böhme 
#CFA script BA: The Construction of Pain - Linking Psychological Factors to Pain thresholds

    library(lavaan)
    library(semPlot)
    library(psych) #important help page for factor analysis as well
    library(tidyverse)
    

##creating a model
   head(VMP_data_analysis_final) #looking at the manifest variables 
  
##model C: includes all selected items after EFA 
   
      Model_C <- "
      affect =~ 
            MDI_1 + MDI_2 + MDI_3 + MDI_4 + MDI_5 + MDI_6 + MDI_7 + 
            MDI_8a + MDI_8b + 
            WEMWBS_1 + WEMWBS_2 + WEMWBS_3 + WEMWBS_5 + 
            WEMWBS_6 + WEMWBS_7 + WEMWBS_10 + WEMWBS_11 + WEMWBS_12 + 
            WEMWBS_13 + WEMWBS_14 
      stress =~ 
            PSS_1 + PSS_2 + PSS_3 + PSS_6 + PSS_9 + PSS_10 
      physiological state =~  
            MFI20_1 + MFI20_3 + MFI20_4 +  
            MFI20_6 + MFI20_7 + MFI20_8 +
            MFI20_11 + MFI20_12 + MFI20_20 +
            PSQI_9 
      quality of life =~ 
                  MPSSS_4 + MPSSS_6 + MPSSS_7 + 
                  MPSSS_8 + MPSSS_9 + MPSSS_12 +
                  WHO5_5 +
                  WHOQOL_1 + WHOQOL_2 + WHOQOL_5 + WHOQOL_6 + 
                  WHOQOL_7 + WHOQOL_10 + WHOQOL_11 + WHOQOL_13 +
                  WHOQOL_16 + WHOQOL_17 + WHOQOL_18 + WHOQOL_19 + 
                  WHOQOL_20 + WHOQOL_22 + WHOQOL_26 
       "
      
      
      ##CFA: visualization / model fit 
      Model_C.fit <- cfa(model = Model_C, data = VMP_data_analysis_final, orthogonal = FALSE,
                         sample.nobs = NULL, std.lv = TRUE, estimator = "MLR")    #visualize model with semPaths()
      #orthogonal = True, if latent variables independent
      #vartable(Model_C.fit) #check variances
      
      semPaths(Model_C.fit, 
               what = "path", 
               whatLabels = "std", 
               rotation = 4,
               style = "ram",
               layout = "tree2", 
               edge.label.cex = 0.75, 
               edge.color = "dark grey",
               exoCov = TRUE,
               residuals = FALSE,
               intercepts = FALSE,
               optimizeLatRes = TRUE,
               sizeMan = 3) 
      
      summary(Model_C.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
      
      ##model D includes reduced items - MPWB model
      
      Model_D <- "
      affect =~ 
            MDI_1 + MDI_2 + MDI_3 + MDI_4 + MDI_5 + MDI_6 + MDI_8b
            
      psychological distress =~ 
            PSS_2 + PSS_3 + PSS_6 + PSS_10 + WEMWBS_3 + WEMWBS_10
            
      physiological state =~ 
            MFI20_1 + MFI20_3 + MFI20_7 + MFI20_8 +
            MFI20_11 + MFI20_12 + MFI20_20 
            
      quality of life =~ 
                  WHO5_1 + WHO5_2 + WHOQOL_5 + WHOQOL_10 + 
                  WHOQOL_17 + WHOQOL_19 + WHOQOL_26
          
      mental physical well being =~ affect + psychological distress + physiological state + quality of life "
      
      
      ##CFA: visualization / model fit 
      Model_D.fit <- cfa(model = Model_D, data = VMP_data_analysis_final, orthogonal = FALSE,
                         sample.nobs = NULL, std.lv = TRUE, estimator = "MLR")   
      
      #vartable(Model_D.fit) #showing variances
      
      semPaths(Model_D.fit, what = "path", 
               whatLabels = "std", 
               layout = "tree2", 
               #style = "lisrel", 
               edge.color = 'dark grey',
               edge.label.cex = 0.75, 
               exoCov = TRUE) 
      
      #for appendix
      semPaths(Model_D.fit, what = "path", 
               whatLabels = "std", 
               layout = "tree2", 
               style = "ram",
               intStyle = "multi",
               rotation = 4,
               nCharNodes = 3, 
               edge.color = 'dark grey',
               edge.label.cex = 0.75, 
               exoCov = TRUE,
               combineGroups = FALSE,
               intercepts = FALSE,
               sizeMan = 3,
               optimizeLatRes = TRUE,
               residuals = FALSE) 
      
      summary(Model_D.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
      
      ##to check modification options
      modificationIndices(Model_D.fit, sort(TRUE))
      
      ##creates a factor score - presented in Z-values
      head(predict(Model_D.fit))
      MPWB_fac_score <- as.data.frame(predict(Model_D.fit)) 
      
      
      ##examine internal consistency for scales in created model
      #checking cronbachs alpha
      #sub set for every developed scale   
      
      affect_ca <- subset(VMP_data_analysis_final,
                          select = c(MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_8b))    
      alpha(affect_ca)
      
      psych_distr_ca <- subset(VMP_data_analysis_final,
                               select = c(PSS_2,PSS_3,PSS_6,PSS_10,WEMWBS_3,WEMWBS_10))    
      alpha(psych_distr_ca, check.keys = TRUE)
      
      physio_state_ca <- subset(VMP_data_analysis_final,
                                select = c(MFI20_1,MFI20_3,MFI20_7,MFI20_8,MFI20_11,MFI20_12,MFI20_20))
      alpha(physio_state_ca)
      
      qol_ca <- subset(VMP_data_analysis_final,
                       select = c(WHO5_1,WHO5_2,WHOQOL_5,WHOQOL_10,WHOQOL_17,WHOQOL_19,WHOQOL_26))
      alpha(qol_ca)    
      
      MPWB_ca <- subset(VMP_data_analysis_final,
                        select = c(MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_8b,
                                   PSS_2,PSS_3,PSS_6,PSS_10,WEMWBS_3,WEMWBS_10,
                                   MFI20_1,MFI20_3,MFI20_7,MFI20_8,MFI20_11,MFI20_12,MFI20_20,
                                   WHO5_1,WHO5_2,WHOQOL_5,WHOQOL_10,WHOQOL_17,WHOQOL_19,WHOQOL_26))
      alpha(MPWB_ca, check.keys = TRUE) #check.keys = TRUE, for qol scale (goes in "different" direction, items inverse)
      
      
      
      
      