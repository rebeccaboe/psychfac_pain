#Author: Rebecca Astrid Böhme 
#subsequent analyses script BA: The Construction of Pain - Linking Psychological Factors to Pain thresholds

    library(tidyverse)
    library(lavaan)
    library(semPlot)
    library(psych)
    library(qpcR)
    library(car)
    
    
##creating a scale score to perform subsequent analyses with subgroup of negative and postive MPWB participants

#MPWB quest: includes all items and all participants which are included in final analyses after screening 
    MPWB_quest_data <- subset(VMP_data_FULL, 
                              select = c(ID,GENDER,MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_8b,
                                         PSS_2,PSS_3,PSS_6,PSS_10,WEMWBS_3,WEMWBS_10,
                                         MFI20_1,MFI20_3,MFI20_7,MFI20_8,MFI20_11,MFI20_12,MFI20_20,
                                         WHO5_1,WHO5_2,WHOQOL_5,WHOQOL_10,WHOQOL_17,WHOQOL_19,WHOQOL_26))

#MPWB score
    #for the subscales
    MPWB_quest_data$MPWB_affect_score <- rowSums(subset(MPWB_quest_data,
                                                        select = c(MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_8b)),na.rm = TRUE)

    MPWB_quest_data$MPWB_distress_score <- rowSums(subset(MPWB_quest_data,
                                                          select = c(PSS_2,PSS_3,PSS_6,PSS_10,WEMWBS_3,WEMWBS_10)),na.rm = TRUE)    

    MPWB_quest_data$MPWB_psychstate_score <- rowSums(subset(MPWB_quest_data,
                                                            select = c(MFI20_1,MFI20_3,MFI20_7,MFI20_8,MFI20_11,MFI20_12,MFI20_20)),na.rm = TRUE)    

    #recoded items for scoring 
    MPWB_quest_data$WHO5_1_recoded <- recode(MPWB_quest_data$WHO5_1, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
    MPWB_quest_data$WHO5_2_recoded <- recode(MPWB_quest_data$WHO5_2, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
    MPWB_quest_data$WHOQOL_5_recoded <- recode(MPWB_quest_data$WHOQOL_5, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
    MPWB_quest_data$WHOQOL_10_recoded <- recode(MPWB_quest_data$WHOQOL_10, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
    MPWB_quest_data$WHOQOL_17_recoded <- recode(MPWB_quest_data$WHOQOL_17, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
    MPWB_quest_data$WHOQOL_19_recoded <- recode(MPWB_quest_data$WHOQOL_19, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
    MPWB_quest_data$WHOQOL_26_recoded <- recode(MPWB_quest_data$WHOQOL_26, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
    
    
    MPWB_quest_data$MPWB_qol_score <- rowSums(subset(MPWB_quest_data,
                                                     select = c(WHO5_1_recoded,WHO5_2_recoded,WHOQOL_5_recoded,
                                                                WHOQOL_10_recoded,WHOQOL_17_recoded,WHOQOL_19_recoded,WHOQOL_26_recoded)),na.rm = TRUE) 

    MPWB_quest_data$MPWB_general_score <- MPWB_quest_data$MPWB_affect_score + MPWB_quest_data$MPWB_distress_score + 
                                          MPWB_quest_data$MPWB_psychstate_score + MPWB_quest_data$MPWB_qol_score

#descriptive stat´s
    mean_mpwb <- MPWB_quest_data %>%
      summarize(mean_mpwb=mean(MPWB_general_score), sd_mpwb=sd(MPWB_general_score))
    mean_mpwb   
    

#subset subclinical and clinical participants
    depressed_VMP <- subset(MPWB_quest_data, MPWB_general_score > 70)
    happy_VMP <- subset(MPWB_quest_data, MPWB_general_score < 50)

    
#combine the relevant data frames
  #includes pain sensitivity score for depressed sub-group
    data_with_scores <- qpcR ::: cbind.na(VMP_data_FULL,pain_sensitivity_fac_score)
    depressed_VMP_model_data <- qpcR ::: cbind.na(depressed_VMP,data_with_scores)
    names(depressed_VMP_model_data)[names(depressed_VMP_model_data) == 'id'] <- 'ID'
    depressed_VMP_model_data <- depressed_VMP_model_data[,!duplicated(colnames(depressed_VMP_model_data))]
    depressed_VMP_model_data <- subset(depressed_VMP_model_data, !is.na(ID))


#regression   
  #for pain sensitivity and mpwb  
    plot(depressed_VMP_model_data$MPWB_general_score,depressed_VMP_model_data$pain_sensitivity_fac_score)
    pain_score_MPWB_regression <- lm(depressed_VMP_model_data$pain_sensitivity_fac_score 
                                     ~ depressed_VMP_model_data$MPWB_general_score)
    summary(pain_score_MPWB_regression)
    
    plot(depressed_VMP_model_data$MPWB_affect_score,depressed_VMP_model_data$pain_sensitivity_fac_score)
    pain_score_affect_regression <- lm(depressed_VMP_model_data$pain_sensitivity_fac_score 
                                     ~ depressed_VMP_model_data$MPWB_affect_score)
    summary(pain_score_affect_regression)
    
    plot(depressed_VMP_model_data$MPWB_distress_score,depressed_VMP_model_data$pain_sensitivity_fac_score)
    pain_score_distress_regression <- lm(depressed_VMP_model_data$pain_sensitivity_fac_score 
                                       ~ depressed_VMP_model_data$MPWB_distress_score)
    summary(pain_score_distress_regression)
    
    plot(depressed_VMP_model_data$MPWB_psychstate_score,depressed_VMP_model_data$pain_sensitivity_fac_score)
    pain_score_psychstate_regression <- lm(depressed_VMP_model_data$pain_sensitivity_fac_score 
                                         ~ depressed_VMP_model_data$MPWB_psychstate_score)
    summary(pain_score_psychstate_regression)
    
    plot(depressed_VMP_model_data$MPWB_qol_score,depressed_VMP_model_data$pain_sensitivity_fac_score)
    pain_score_qol_regression <- lm(depressed_VMP_model_data$pain_sensitivity_fac_score 
                                           ~ depressed_VMP_model_data$MPWB_qol_score)
    summary(pain_score_qol_regression)

  #for tgi  
    plot(depressed_VMP_model_data$MPWB_general_score,depressed_VMP_model_data$rescaled_tgi)
    TGI_mpwb_regression <- lm(depressed_VMP_model_data$rescaled_tgi ~ depressed_VMP_model_data$MPWB_general_score)
    summary(TGI_mpwb_regression)
    
    plot(depressed_VMP_model_data$MPWB_affect_score,depressed_VMP_model_data$rescaled_tgi)
    TGI_affect_regression <- lm(depressed_VMP_model_data$rescaled_tgi ~ depressed_VMP_model_data$MPWB_affect_score)
    summary(TGI_affect_regression)
    
    plot(depressed_VMP_model_data$MPWB_distress_score,depressed_VMP_model_data$rescaled_tgi)
    TGI_distress_regression <- lm(depressed_VMP_model_data$rescaled_tgi ~ depressed_VMP_model_data$MPWB_distress_score)
    summary(TGI_distress_regression)
    
    plot(depressed_VMP_model_data$MPWB_psychstate_score,depressed_VMP_model_data$rescaled_tgi)
    TGI_psychstate_regression <- lm(depressed_VMP_model_data$rescaled_tgi ~ depressed_VMP_model_data$MPWB_psychstate_score)
    summary(TGI_psychstate_regression)
    
    plot(depressed_VMP_model_data$MPWB_qol_score,depressed_VMP_model_data$rescaled_tgi)
    TGI_qol_regression <- lm(depressed_VMP_model_data$rescaled_tgi ~ depressed_VMP_model_data$MPWB_qol_score)
    summary(TGI_qol_regression)
    
    
  #for warm pain  
    plot(depressed_VMP_model_data$MPWB_general_score,depressed_VMP_model_data$rescaled_warm)
    warm_mpwb_regression <- lm(depressed_VMP_model_data$rescaled_warm ~ depressed_VMP_model_data$MPWB_general_score)
    summary(warm_mpwb_regression)
    
    plot(depressed_VMP_model_data$MPWB_affect_score,depressed_VMP_model_data$rescaled_warm)
    warm_affect_regression <- lm(depressed_VMP_model_data$rescaled_warm ~ depressed_VMP_model_data$MPWB_affect_score)
    summary(warm_affect_regression)
    
    plot(depressed_VMP_model_data$MPWB_distress_score,depressed_VMP_model_data$rescaled_warm)
    warm_distress_regression <- lm(depressed_VMP_model_data$rescaled_warm ~ depressed_VMP_model_data$MPWB_distress_score)
    summary(warm_distress_regression)
    
    plot(depressed_VMP_model_data$MPWB_psychstate_score,depressed_VMP_model_data$rescaled_warm)
    warm_psychstate_regression <- lm(depressed_VMP_model_data$rescaled_warm ~ depressed_VMP_model_data$MPWB_psychstate_score)
    summary(warm_psychstate_regression)
    
    plot(depressed_VMP_model_data$MPWB_qol_score,depressed_VMP_model_data$rescaled_warm)
    warm_qol_regression <- lm(depressed_VMP_model_data$rescaled_warm ~ depressed_VMP_model_data$MPWB_qol_score)
    summary(warm_qol_regression)
    
    
  #for cold pain  
    plot(depressed_VMP_model_data$MPWB_general_score,depressed_VMP_model_data$rescaled_cold)
    cold_mpwb_regression <- lm(depressed_VMP_model_data$rescaled_cold ~ depressed_VMP_model_data$MPWB_general_score)
    summary(cold_mpwb_regression)
    
    plot(depressed_VMP_model_data$MPWB_affect_score,depressed_VMP_model_data$rescaled_cold)
    cold_affect_regression <- lm(depressed_VMP_model_data$rescaled_cold ~ depressed_VMP_model_data$MPWB_affect_score)
    summary(cold_affect_regression)
    
    plot(depressed_VMP_model_data$MPWB_distress_score,depressed_VMP_model_data$rescaled_cold)
    cold_distress_regression <- lm(depressed_VMP_model_data$rescaled_cold ~ depressed_VMP_model_data$MPWB_distress_score)
    summary(cold_distress_regression)
    
    plot(depressed_VMP_model_data$MPWB_psychstate_score,depressed_VMP_model_data$rescaled_cold)
    cold_psychstate_regression <- lm(depressed_VMP_model_data$rescaled_cold ~ depressed_VMP_model_data$MPWB_psychstate_score)
    summary(cold_psychstate_regression)
    
    plot(depressed_VMP_model_data$MPWB_qol_score,depressed_VMP_model_data$rescaled_cold)
    cold_qol_regression <- lm(depressed_VMP_model_data$rescaled_cold ~ depressed_VMP_model_data$MPWB_qol_score)
    summary(cold_qol_regression)