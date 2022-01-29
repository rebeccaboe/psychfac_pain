#Author: Rebecca Astrid Böhme 
#data screening script BA: The Construction of Pain - Linking Psychological Factors to Pain thresholds


    library(rio) 
    library(tidyverse)
    library(dplyr)
    library(tidyr) 
    library(corrplot)
    library(shiny)
    library(MASS)
    library(data.table)
    library(psych)
    library(prettyR)
    library(car)
    library(lubridate)  
    library(QuantPsyc)
    library(corrplot)

    
##input data & subset with relevant variables:
    #in data set 1=female/2=male

        VMP1_data <- read.csv(file="DEPRESS_PAIN_VMP1.csv")  #VMP 1 file 
        VMP1_data[,c('weight','height','language_danish','language_english','psqi_1','psqi_2','psqi_3','psqi_4')] <- list(NULL)     
        names(VMP1_data)[names(VMP1_data) == 'record_id'] <- 'ID'
        names(VMP1_data)[names(VMP1_data) == 'gender'] <- 'GENDER'
        names(VMP1_data)[names(VMP1_data) == 'age'] <- 'AGE'
        
        VMP2_data <- read.csv(file="DEPRESS_PAIN_VMP2.csv")  #VMP 2 file 
        VMP2_data[,c('record_id','language_pre','family_eng','friends_eng','colleges_eng',
                     'neighbors_eng','online_friends_eng','involentarily_alone_eng',
                     'social_support_eng','living_significant_other_eng','living_young_kids_eng',
                     'living_old_kids_eng','education_eng','years_education_eng',
                     'under_education_eng','working_eng','smoking_eng','smoking_amount_eng',
                     'cannabis_eng','cannabis_last_year_eng','drinking_weekdays_eng','drinking_weekends_eng',
                     'butter_eng','margarine_eng','fat_eng','no_fat_eng','meat_eng','fish_eng','eeg_eng',
                     'salats_eng','warm_meat_eng','warm_poultry_eng','warm_fish_eng','warm_vegetable_eng',
                     'vegetable_salads_eng','raw_vegetable_eng','cooked_vegetable_eng','fat_1_eng','fat_2_eng',
                     'fat_3_eng','fat_4_eng','fat_5_eng','fat_6_eng','fat_7_eng','no_fat_1_eng',
                     'fruits_eng','more_healthy_eng','moderate_activities','hard_activities_eng',
                     'sitting_transport_eng','sitting_work_school_eng','sitting_freetime_by_screen_eng',
                     'sitting_freetime_other_eng','breathe_nose_mouth_eng','winter_bathing_eng',
                     'sauna_eng','satisfied_weight_eng','enviroment_eng','gps_eng','corona_eng','phq15_1_eng','phq15_2_eng',
                     'phq15_3_eng','phq15_4_eng','phq15_5_eng','phq15_6_eng','phq15_7_eng','phq15_8_eng','phq15_9_eng',
                     'phq15_10_eng','phq15_11_eng','phq15_12_eng','phq15_13_eng','phq15_14_eng','phq15_15_eng','sticsa_gm_1_eng','sticsa_gm_2_eng',
                     'sticsa_gm_3_eng','sticsa_gm_4_eng','sticsa_gm_5_eng','sticsa_gm_6_eng','sticsa_gm_7_eng','sticsa_gm_8_eng',
                     'sticsa_gm_9_eng','sticsa_gm_10_eng','sticsa_gm_11_eng','sticsa_gm_12_eng','sticsa_gm_13_eng','sticsa_gm_14_eng',
                     'sticsa_gm_15_eng','sticsa_gm_16_eng','sticsa_gm_17_eng','sticsa_gm_18_eng','sticsa_gm_19_eng','sticsa_gm_20_eng','sticsa_gm_21_eng',
                     'asra_a_1_eng','asra_a_2_eng','asra_a_3_eng','asra_a_4_eng','asra_a_5_eng','asra_a_6_eng','asrs_b_1_eng','asrs_b_2_eng',
                     'asrs_b_3_eng','asrs_b_4_eng','asrs_b_5_eng','asrs_b_6_eng','asrs_b_7_eng','asrs_b_8_eng','asrs_b_9_eng',
                     'asrs_b_10_eng','asrs_b_11_eng','asrs_b_12_eng','facit_f_1eng','facit_f_2_eng','facit_f_3_eng',
                     'facit_f_4_eng','facit_f_5_eng','facit_f_6_eng','facit_f_7_eng','facit_f_8_eng','facit_f_9_eng','facit_f_10_eng',
                     'facit_f_11_eng','facit_f_12_eng','facit_f_13_eng','phq_9_1_eng','phq_9_2_eng','phq_9_3_eng','phq_9_4_eng','phq_9_5_eng',
                     'phq_9_6_eng','phq_9_7_eng','phq_9_8_eng','phq_9_9_eng','phq_9_10_eng','stai_21_eng','stai_22_eng','stai_23_eng','stai_24_eng',
                     'stai_25_eng','stai_26_eng','stai_27_eng','stai_28_eng','stai_29_eng','stai_30_eng','stai_31_eng','stai_32_eng','stai_33_eng',
                     'stai_34_eng','stai_35_eng','stai_36_eng','stai_37_eng','stai_38_eng','stai_39_eng','stai_40_eng','maia_1_eng','maia_2_eng',
                     'maia_3_eng','maia_4_eng','maia_5_eng','maia_6_eng','maia_7_eng','maia_8_eng','maia_9_eng','maia_10_eng','maia_11_eng','maia_12_eng',
                     'maia_13_eng','maia_14_eng','maia_15_eng','maia_16_eng','maia_17_eng','maia_18_eng','maia_19_eng','maia_20_eng','maia_21_eng',
                     'maia_22_eng','maia_23_eng','maia_24_eng','maia_25_eng','maia_26_eng','maia_27_eng','maia_28_eng','maia_29_eng','maia_30_eng','maia_31_eng',
                     'maia_32_eng','isi_1_eng','isi_2_eng','isi_3_eng','isi_4_eng','isi_5_eng','isi_6_eng','isi_7_eng','sias_1_eng','sias_2_eng','sias_3_eng',
                     'sias_4_eng','sias_5_eng','sias_6_eng','sias_7_eng','sias_8_eng','sias_9_eng','sias_10_eng','sias_11_eng','sias_12_eng','sias_13_eng',
                     'sias_14_eng','sias_15_eng','sias_16_eng','sias_17_eng','sias_18_eng','sias_19_eng','sias_20_eng','aq10_1_eng','aq10_2_eng','aq10_3_eng','aq10_4_eng',
                     'aq10_5_eng','aq10_6_eng','aq10_7_eng','aq10_8_eng','aq10_9_eng','aq10_10_eng','iri_1_eng','iri_2_eng','iri_3_eng','iri_4_eng','iri_5_eng','iri_6_eng','iri_7_eng','iri_8_eng','iri_9_eng','iri_10_eng',
                     'iri_11_eng','iri_12_eng','iri_13_eng','iri_14_eng','iri_15_eng','iri_16_eng','iri_17_eng','iri_18_eng','iri_19_eng','iri_20_eng',
                     'iri_21_eng','iri_22_eng','iri_23_eng','iri_24_eng','iri_25_eng','iri_26_eng','iri_27_eng','iri_28_eng',
                     'cfq_1_eng','cfq_2_eng','cfq_3_eng','cfq_4_eng','cfq_5_eng','cfq_6_eng','cfq_7_eng','cfq_8_eng','cfq_9_eng','cfq_10_eng',
                     'cfq_11_eng','nsq_1_eng','other_1_eng','nsq_2_eng','nsq_3_eng','nsq_4_eng','nsq_5_eng',
                     'nsq_6_eng','other_6_eng','nsq_7_eng','other_7_eng','nsq_8_eng','nsq_9_eng','nsq_10_eng','nsq_11_eng','nsq_12_eng',
                     'nsq_13_eng','nsq_14_eng','other_14_eng','sbsds_1_eng','sbsds_2_eng','sbsds_3_eng','sbsds_4_eng','sbsds_5_eng','sbsds_6_eng',
                     'sbsds_7_eng','sbsds_8_eng','sbsds_9_eng','sbsds_10_eng','sbsds_11_eng','sbsds_12_eng','sbsds_13_eng','sbsds_14_eng',
                     'sbsds_15_eng','language_surveys','family','friends','colleges','neighbors','online_friends',
                     'involuntarily_alone','social_support','living_significant_other','living_young_kids','living_old_kids',
                     'education','years_education','under_education','working',
                     'smoking','amount_smoking','cannabis',
                     'cannabis_last_year','drinking_weekdays','drinking_weekends','butter','margarine',
                     'fat','no_fat','meat','fish','eeg','salats','warm_meat','warm_poultry','warm_fish','warm_vegetable',
                     'vegetable_salats','raw_vegetable','cooked_vegetable','fat_1',
                     'fat_2','fat_3','fat_4','fat_5','fat_6','fat_7','no_fat_1','fruits',
                     'more_healthy','moderat_activities','hard_activities','sitting_transport','sitting_work_school',
                     'sitting_freetime_by_screen','sitting_freetime_other','breathe_nose_mouth',
                     'winter_bathing','sauna','satisfied_weight','enviroment_dan','gps_dan','corona',
                     'phq15_1','phq15_2','phq15_3','phq15_4','phq15_5','phq15_6','phq15_7','phq15_8','phq15_9',
                     'phq15_10','phq15_11','phq15_12','phq15_13','phq15_14','phq15_15','phq15_complete',
                     'asra_a_1','asra_a_2','asra_a_3','asra_a_4','asra_a_5','asra_a_6',
                     'asrs_b_1','asrs_b_2','asrs_b_3','asrs_b_4','asrs_b_5','asrs_b_6','asrs_b_7','asrs_b_8','asrs_b_9',
                     'asrs_b_10','asrs_b_11','asrs_b_12','asrs_complete','facit_f_1','facit_f_2','facit_f_3','facit_f_4','facit_f_5',
                     'facit_f_6','facit_f_7','facit_f_8','facit_f_9','facit_f_10','facit_f_11','facit_f_12','facit_f_13','facitf_complete',
                     'phq_9_1','phq_9_2','phq_9_3','phq_9_4','phq_9_5','phq_9_6','phq_9_7','phq_9_8','phq_9_9','phq_9_10','phq9_complete',
                     'maia_1','maia_2','maia_3','maia_4','maia_5','maia_6','maia_7','maia_8','maia_9','maia_10','maia_11',
                     'maia_12','maia_13','maia_14','maia_15','maia_16','maia_17','maia_18','maia_19',
                     'maia_20','maia_21','maia_22','maia_23','maia_24','maia_25','maia_26','maia_27','maia_complete',
                     'maia_28','maia_29','maia_30','maia_31','maia_32','isi_1','isi_2',
                     'isi_3','isi_4','isi_5','isi_6','isi_7','isi_complete','sias_1','sias_2','sias_3','sias_4','sias_5','sias_6',
                     'sias_7','sias_8','sias_9','sias_10','sias_11','sias_12','sias_13','sias_14','sias_15','sias_16',
                     'sias_17','sias_18','sias_19','sias_20','sias_complete','aq10_1','aq10_2','aq10_3','aq10_4','aq10_5','aq10_6','aq10_7','aq10_8','aq10_9',
                     'aq10_10','aq10_complete',
                     'iri_1','iri_2','iri_3','iri_4','iri_5','iri_6','iri_7','iri_8','iri_9','iri_10','iri_11','iri_12',
                     'iri_13','iri_14','iri_15','iri_16','iri_17','iri_18','iri_19','iri_20','iri_21','iri_22','iri_23',
                     'iri_24','iri_25','iri_26','iri_27','iri_28','iri_complete','cfq_1','cfq_2','cfq_3','cfq_4','cfq_5','cfq_6','cfq_7',
                     'cfq_8','cfq_9','cfq_10','cfq_11','chalder_fatigue_scale_complete','nsq_1_dan','other_1_dan','nsq_2_dan','nsq_3_dan',
                     'nsq_4_dan','nsq_5_dan','nsq_6_dan','other_6_dan','nsq_7_dan','other_7_dan','nsq_8_dan','navigational_strategies_questionnaire_complete',
                     'nsq_9_dan','nsq_10_dan','nsq_11_dan','nsq_12_dan','nsq_13_dan','nsq_14_dan','other_14_dan',
                     'sbsds_1_dan','sbsds_2_dan','sbsds_3_dan','sbsds_4_dan','sbsds_5_dan','sbsds_6_dan','sbsds_7_dan',
                     'sbsds_8_dan','sbsds_9_dan','sbsds_10_dan','sbsds_11_dan','sbsds_12_dan','sbsds_13_dan','sbsds_14_dan',
                     'sbsds_15_dan','santa_barbara_senseofdirection_scale_complete','sticsa_gm_1','sticsa_gm_2','sticsa_gm_3','sticsa_gm_4','sticsa_gm_5',
                     'sticsa_gm_6','sticsa_gm_7','sticsa_gm_8','sticsa_gm_9','sticsa_gm_10','sticsa_gm_11','sticsa_gm_12',
                     'sticsa_gm_13','sticsa_gm_14','sticsa_gm_15','sticsa_gm_16','sticsa_gm_17','sticsa_gm_18','sticsa_gm_19',
                     'sticsa_gm_20','sticsa_gm_21','sticsa_complete','psi_1','psqi_2','psqi_3','psqi_4','psqi_5_other','psqi_5_other_often',
                     'psqi_10_4','psqi_10_other','psqi_10_other_often','psi_1_eng','psqi_2_eng','psqi_3_eng','psqi_4_eng','psqi_5_other_eng',
                     'psqi_5_other_often_eng','psqi_10_4_eng','psqi_10_other_eng','psqi_10_other_often_eng','stai_21','stai_22','stai_23','stai_24','stai_25','stai_26','stai_27','stai_28','stai_29','stai_30',
                     'stai_31','stai_32','stai_33','stai_34','stai_35','stai_36','stai_37','stai_38','stai_39','stai_40','stai_y2_complete')] <- list(NULL)
        names(VMP2_data)[names(VMP2_data) == 'stormdb'] <- 'ID'
        names(VMP2_data)[names(VMP2_data) == 'gender'] <- 'GENDER'
        names(VMP2_data)[names(VMP2_data) == 'age'] <- 'AGE'
  

##recode items: for the analyses inverted items needed to be recoded
        
        #MFI20
        #recode inverse items -> recode 2, 5, 9, 10, 13, 14, 16, 17, 18, 19
        VMP2_data$mfi20_2_recoded <- recode(VMP2_data$mfi20_2, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_5_recoded <- recode(VMP2_data$mfi20_5, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_9_recoded <- recode(VMP2_data$mfi20_9, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_10_recoded <- recode(VMP2_data$mfi20_10, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_13_recoded <- recode(VMP2_data$mfi20_13, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_14_recoded <- recode(VMP2_data$mfi20_14, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_16_recoded <- recode(VMP2_data$mfi20_16, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_17_recoded <- recode(VMP2_data$mfi20_17, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_18_recoded <- recode(VMP2_data$mfi20_18, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_19_recoded <- recode(VMP2_data$mfi20_19, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_2_eng_recoded <- recode(VMP2_data$mfi20_2_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_5_eng_recoded <- recode(VMP2_data$mfi20_5_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_9_eng_recoded <- recode(VMP2_data$mfi20_9_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_10_eng_recoded <- recode(VMP2_data$mfi20_10_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_13_eng_recoded <- recode(VMP2_data$mfi20_13_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_14_eng_recoded <- recode(VMP2_data$mfi20_14_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_16_eng_recoded <- recode(VMP2_data$mfi20_16_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_17_eng_recoded <- recode(VMP2_data$mfi20_17_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_18_eng_recoded <- recode(VMP2_data$mfi20_18_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        VMP2_data$mfi20_19_eng_recoded <- recode(VMP2_data$mfi20_19_eng, "1=5; 2=4; 3=3; 4=2; 5=1")
        
        #PSS        
        #recode inverse items: 4,5,7,8
        VMP2_data$pss_4_recoded <- recode(VMP2_data$pss_4, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
        VMP2_data$pss_5_recoded <- recode(VMP2_data$pss_5, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
        VMP2_data$pss_7_recoded <- recode(VMP2_data$pss_7, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
        VMP2_data$pss_8_recoded <- recode(VMP2_data$pss_8, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
        VMP2_data$pss_4_recoded_eng <- recode(VMP2_data$pss_4_eng, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
        VMP2_data$pss_5_recoded_eng <- recode(VMP2_data$pss_5_eng, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
        VMP2_data$pss_7_recoded_eng <- recode(VMP2_data$pss_7_eng, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
        VMP2_data$pss_8_recoded_eng <- recode(VMP2_data$pss_8_eng, "0 = 4; 1 = 3; 2 = 2; 3 = 1; 4 = 0")
       
        #WEMWBS
          #CAVE:not originally inverse, highly negative correlation with construct 
          #& recoded to create a consistence scale score
        VMP1_data$wemwsb_3_recoded <- recode(VMP1_data$wemwsb_3, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP1_data$wemwsb_10_recoded <- recode(VMP1_data$wemwsb_10, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$wemwsb_3_recoded <- recode(VMP2_data$wemwsb_3, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$wemwsb_10_recoded <- recode(VMP2_data$wemwsb_10, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$wemwsb_3_recoded_eng <- recode(VMP2_data$wemwsb_3_eng, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$wemwsb_10_recoded_eng <- recode(VMP2_data$wemwsb_10_eng, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        
        
        #WHOQOL
        #recode inverse items: 3,4,26 
        VMP1_data$whoqol_3_recoded <- recode(VMP1_data$whoqol_3, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP1_data$whoqol_4_recoded <- recode(VMP1_data$whoqol_4, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP1_data$whoqol_26_recoded <- recode(VMP1_data$whoqol_26, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        
        VMP2_data$whoqol_3_recoded <- recode(VMP2_data$whoqol_3, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$whoqol_4_recoded <- recode(VMP2_data$whoqol_4, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$whoqol_26_recoded <- recode(VMP2_data$whoqol_26, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$whoqol_3_recoded_eng <- recode(VMP2_data$whoqol_3_eng, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$whoqol_4_recoded_eng <- recode(VMP2_data$whoqol_4_eng, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
        VMP2_data$whoqol_26_recoded_eng <- recode(VMP2_data$whoqol_26_eng, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
       
        
        ##bind the data frames 
        VMP_data_sum_up <- merge(VMP1_data,VMP2_data, by = ,all.x = TRUE, all.y = TRUE) 
        
        
##combine items for danish and english speaking participants
        
        VMP_data <- VMP_data_sum_up %>%
          #MDI
          mutate(MDI_1 = coalesce(mdi_1,mdi_1_eng)) %>%
          mutate(MDI_2 = coalesce(mdi_2,mdi_2_eng)) %>%
          mutate(MDI_3 = coalesce(mdi_3,mdi_3_eng)) %>%
          mutate(MDI_4 = coalesce(mdi_4,mdi_4_eng)) %>%
          mutate(MDI_5 = coalesce(mdi_5,mdi_5_eng)) %>%
          mutate(MDI_6 = coalesce(mdi_6,mdi_6_eng)) %>%
          mutate(MDI_7 = coalesce(mdi_7,mdi_7_eng)) %>%
          mutate(MDI_8a = coalesce(mdi_8a,mdi_8a_eng)) %>%
          mutate(MDI_8b = coalesce(mdi_8b,mdi_8b_eng)) %>%
          mutate(MDI_9a = coalesce(mdi_9a,mdi_9a_eng)) %>%
          mutate(MDI_9b = coalesce(mdi_9b,mdi_9b_eng)) %>% 
          mutate(MDI_10a = coalesce(mdi_10a,mdi_10a_eng)) %>%
          mutate(MDI_10b = coalesce(mdi_10b,mdi_10b_eng)) %>%
          #MFI
          mutate(MFI20_1 = coalesce(mfi20_1,mfi20_1_eng)) %>%
          mutate(MFI20_2 = coalesce(mfi20_2_recoded,mfi20_2_eng_recoded)) %>%
          mutate(MFI20_3 = coalesce(mfi20_3,mfi20_3_eng)) %>%
          mutate(MFI20_4 = coalesce(mfi20_4,mfi20_4_eng)) %>%
          mutate(MFI20_5 = coalesce(mfi20_5_recoded,mfi20_5_eng_recoded)) %>%
          mutate(MFI20_6 = coalesce(mfi20_6,mfi20_6_eng)) %>%
          mutate(MFI20_7 = coalesce(mfi20_7,mfi20_7_eng)) %>% 
          mutate(MFI20_8 = coalesce(mfi20_8,mfi20_8_eng)) %>%
          mutate(MFI20_9 = coalesce(mfi20_9_recoded,mfi20_9_eng_recoded)) %>%
          mutate(MFI20_10 = coalesce(mfi20_10_recoded,mfi20_10_eng_recoded)) %>% 
          mutate(MFI20_11 = coalesce(mfi20_11,mfi20_11_eng)) %>%
          mutate(MFI20_12 = coalesce(mfi20_12,mfi20_12_eng)) %>%
          mutate(MFI20_13 = coalesce(mfi20_13_recoded,mfi20_13_eng_recoded)) %>%
          mutate(MFI20_14 = coalesce(mfi20_14_recoded,mfi20_14_eng_recoded)) %>%
          mutate(MFI20_15 = coalesce(mfi20_15,mfi20_15_eng)) %>%
          mutate(MFI20_16 = coalesce(mfi20_16_recoded,mfi20_16_eng_recoded)) %>%
          mutate(MFI20_17 = coalesce(mfi20_17_recoded,mfi20_17_eng_recoded)) %>%
          mutate(MFI20_18 = coalesce(mfi20_18_recoded,mfi20_18_eng_recoded)) %>%
          mutate(MFI20_19 = coalesce(mfi20_19_recoded,mfi20_19_eng_recoded)) %>%
          mutate(MFI20_20 = coalesce(mfi20_20,mfi20_20_eng)) %>%
          #MPSSS
          mutate(MPSSS_1 = coalesce(mpsss_1,mpsss_1_eng)) %>%
          mutate(MPSSS_2 = coalesce(mpsss_2,mpsss_2_eng)) %>%
          mutate(MPSSS_3 = coalesce(mpsss_3,mpsss_3_eng)) %>%
          mutate(MPSSS_4 = coalesce(mpsss_4,mpsss_4_eng)) %>%
          mutate(MPSSS_5 = coalesce(mpsss_5,mpsss_5_eng)) %>%
          mutate(MPSSS_6 = coalesce(mpsss_6,mpsss_6_eng)) %>%
          mutate(MPSSS_7 = coalesce(mpsss_7,mpsss_7_eng)) %>%
          mutate(MPSSS_8 = coalesce(mpsss_8,mpsss_8_eng)) %>%
          mutate(MPSSS_9 = coalesce(mpsss_9,mpsss_9_eng)) %>%
          mutate(MPSSS_10 = coalesce(mpsss_10,mpsss_10_eng)) %>%
          mutate(MPSSS_11 = coalesce(mpsss_11,mpsss_11_eng)) %>%
          mutate(MPSSS_12 = coalesce(mpsss_12,mpsss_12_eng)) %>%
          #PSQI
          mutate(PSQI_5_1 = coalesce(psqi_5_1,psqi_5_1_eng)) %>%
          mutate(PSQI_5_2 = coalesce(psqi_5_2,psqi_5_2_eng)) %>%
          mutate(PSQI_5_3 = coalesce(psqi_5_3,psqi_5_3_eng)) %>%
          mutate(PSQI_5_4 = coalesce(psqi_5_4,psqi_5_4_eng)) %>%
          mutate(PSQI_5_5 = coalesce(psqi_5_5,psqi_5_5_eng)) %>%
          mutate(PSQI_5_6 = coalesce(psqi_5_6,psqi_5_6_eng)) %>%
          mutate(PSQI_5_7 = coalesce(psqi_5_7,psqi_5_7_eng)) %>%
          mutate(PSQI_5_8 = coalesce(psqi_5_8,psqi_5_8_eng)) %>%
          mutate(PSQI_5_9 = coalesce(psqi_5_9,psqi_5_9_eng)) %>%
          mutate(PSQI_6 = coalesce(psqi_6,psqi_6_eng)) %>%
          mutate(PSQI_7 = coalesce(psqi7,psqi_7_eng)) %>%
          mutate(PSQI_8 = coalesce(psqi_8,psqi_8_eng)) %>%
          mutate(PSQI_9 = coalesce(psqi_9,psqi_9_eng)) %>%
          mutate(PSQI_10 = coalesce(psqi_10,psqi_10_eng)) %>%
          mutate(PSQI_10_1 = coalesce(psqi_10_1,psqi_10_1_eng)) %>%
          mutate(PSQI_10_2 = coalesce(psqi_10_2,psqi_10_2_eng)) %>%
          mutate(PSQI_10_3 = coalesce(psqi_10_3,psqi_10_3_eng)) %>%
          #PSS
          mutate(PSS_1 = coalesce(pss_1,pss_1_eng)) %>%
          mutate(PSS_2 = coalesce(pss_2,pss_2_eng)) %>%
          mutate(PSS_3 = coalesce(pss_3,pss_3_eng)) %>%
          mutate(PSS_4 = coalesce(pss_4_recoded,pss_4_recoded_eng)) %>%
          mutate(PSS_5 = coalesce(pss_5_recoded,pss_5_recoded_eng)) %>%
          mutate(PSS_6 = coalesce(pss_6,pss_6_eng)) %>%
          mutate(PSS_7 = coalesce(pss_7_recoded,pss_7_recoded_eng)) %>%
          mutate(PSS_8 = coalesce(pss_8_recoded,pss_8_recoded_eng)) %>%
          mutate(PSS_9 = coalesce(pss_9,pss_9_eng)) %>%
          mutate(PSS_10 = coalesce(pss_10,pss_10_eng)) %>%
          #WEMWBS
          mutate(WEMWBS_1 = coalesce(wemwsb_1,wemwsb_1_eng)) %>%
          mutate(WEMWBS_2 = coalesce(wemwsb_2,wemwsb_2_eng)) %>%
          mutate(WEMWBS_3 = coalesce(wemwsb_3_recoded,wemwsb_3_recoded_eng)) %>%
          mutate(WEMWBS_4 = coalesce(wemwsb_4,wemwsb_4_eng)) %>% 
          mutate(WEMWBS_5 = coalesce(wemwsb_5,wemwsb_5_eng)) %>%
          mutate(WEMWBS_6 = coalesce(wemwsb_6,wemwsb_6_eng)) %>%
          mutate(WEMWBS_7 = coalesce(wemwsb_7,wemwsb_7_eng)) %>%
          mutate(WEMWBS_10 = coalesce(wemwsb_10_recoded,wemwsb_10_recoded_eng)) %>%
          mutate(WEMWBS_11 = coalesce(wemwsb_11,wemwsb_11_eng)) %>%
          mutate(WEMWBS_12 = coalesce(wemwsb_12,wemwsb_12_eng)) %>%
          mutate(WEMWBS_13 = coalesce(wemwsb_13,wemwsb_13_eng)) %>%
          mutate(WEMWBS_14 = coalesce(wemwsb_14,wemwsb_14_eng)) %>%
          mutate(WEMWBS_15 = coalesce(wemwsb_15,wemwsb_15_eng)) %>%
          mutate(WEMWBS_16 = coalesce(wemwsb_16,wemwsb_16_eng)) %>%
          #WHO5
          mutate(WHO5_1 = coalesce(who5_1,who5_1_eng)) %>%
          mutate(WHO5_2 = coalesce(who5_2,who5_2_eng)) %>%
          mutate(WHO5_3 = coalesce(who5_3,who5_3_eng)) %>% 
          mutate(WHO5_4 = coalesce(who5_4,who5_4_eng)) %>% 
          mutate(WHO5_5 = coalesce(who5_5,who5_5_eng)) %>% 
          #WHOQOL
          mutate(WHOQOL_1 = coalesce(whoqol_1,whocol_1_eng)) %>%
          mutate(WHOQOL_2 = coalesce(whoqol_2,whoqol_2_eng)) %>%
          mutate(WHOQOL_3 = coalesce(whoqol_3_recoded,whoqol_3_recoded_eng)) %>%
          mutate(WHOQOL_4 = coalesce(whoqol_4_recoded,whoqol_4_recoded_eng)) %>%
          mutate(WHOQOL_5 = coalesce(whoqol_5,whoqol_5_eng)) %>% 
          mutate(WHOQOL_6 = coalesce(whoqol_6,whoqol_6_eng)) %>%
          mutate(WHOQOL_7 = coalesce(whoqol_7,whoqol_7_eng)) %>% 
          mutate(WHOQOL_8 = coalesce(whoqol_8,whoqol_8_eng)) %>%
          mutate(WHOQOL_9 = coalesce(whoqol_9,whoqol_9_eng)) %>%
          mutate(WHOQOL_10 = coalesce(whoqol_10,whoqol_10_eng)) %>% 
          mutate(WHOQOL_11 = coalesce(whoqol_11,whoqol_11_eng)) %>% 
          mutate(WHOQOL_12 = coalesce(whoqol_12,whoqol_12_eng)) %>% 
          mutate(WHOQOL_13 = coalesce(whoqol_13,whoqol_13_eng)) %>%
          mutate(WHOQOL_14 = coalesce(whoqol_14,whoqol_14_eng)) %>%
          mutate(WHOQOL_15 = coalesce(whoqol_15,whoqol_15_eng)) %>%
          mutate(WHOQOL_16 = coalesce(whoqol_16,whoqol_16_eng)) %>% 
          mutate(WHOQOL_17 = coalesce(whoqol_17,whoqol_17_eng)) %>%
          mutate(WHOQOL_18 = coalesce(whoqol_18,whoqol_18_eng)) %>% 
          mutate(WHOQOL_19 = coalesce(whoqol_19,whoqol_19_eng)) %>% 
          mutate(WHOQOL_20 = coalesce(whoqol_20,whoqol_20_eng)) %>%
          mutate(WHOQOL_21 = coalesce(whoqol_21,whoqol_21_eng)) %>% 
          mutate(WHOQOL_22 = coalesce(whoqol_22,whoqol_22_eng)) %>% 
          mutate(WHOQOL_23 = coalesce(whoqol_23,whoqol_23_eng)) %>%
          mutate(WHOQOL_24 = coalesce(whoqol_24,whoqol_24_eng)) %>%
          mutate(WHOQOL_25 = coalesce(whoqol_25,whoqol_25_eng)) %>%
          mutate(WHOQOL_26 = coalesce(whoqol_26_recoded,whoqol_26_recoded_eng))
        

##data preview 
        glimpse(VMP_data) #shows columns and rows in dataset + names, type                
        
##data frame for EFA     
  #data set with prescreened items (items, which were not asked in VMP1 & VMP2)
        
        VMP_data_analysis <- subset(VMP_data, 
                                    select = c(ID,AGE,GENDER,MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_7,MDI_8a,
                                               MDI_8b,MDI_9a,MDI_9b,MDI_10a,MDI_10b,MFI20_1,
                                               MFI20_3,MFI20_4,MFI20_6,MFI20_7,MFI20_8,
                                               MFI20_11,MFI20_12,MFI20_15,MFI20_20,MPSSS_1,MPSSS_2,MPSSS_3,MPSSS_4,
                                               MPSSS_5,MPSSS_6,MPSSS_7,MPSSS_8,MPSSS_9,MPSSS_10,MPSSS_11,MPSSS_12,
                                               PSQI_6,PSQI_8,PSQI_9,PSQI_10,PSS_1,PSS_2,PSS_3,
                                               PSS_6,PSS_9,PSS_10,WEMWBS_1,WEMWBS_2,WEMWBS_3,WEMWBS_4,WEMWBS_5,
                                               WEMWBS_6,WEMWBS_7,WEMWBS_10,WEMWBS_11,WEMWBS_12,WEMWBS_13,WEMWBS_14,WEMWBS_15,
                                               WEMWBS_16,WHO5_1,WHO5_2,WHO5_3,WHO5_4,WHO5_5,WHOQOL_1,WHOQOL_2,WHOQOL_3,WHOQOL_4,WHOQOL_5,WHOQOL_6,
                                               WHOQOL_7,WHOQOL_10,WHOQOL_11,WHOQOL_12,WHOQOL_13,WHOQOL_14,WHOQOL_15,WHOQOL_16,WHOQOL_17,
                                               WHOQOL_18,WHOQOL_19,WHOQOL_20,WHOQOL_21,WHOQOL_22,WHOQOL_23,WHOQOL_24,WHOQOL_25,WHOQOL_26))
        

##delete all participants with missing ID or fake ID     
    VMP_data_rows <- VMP_data_analysis[-c(436:451),]
      VMP_data_analysis <- VMP_data_rows

##creating data frame to compare original data set, with corrected data set = screening_data
    screening_data <- VMP_data_analysis

##OVERVIEW
    summary(screening_data)

##table presents MISSING ROWS in data frame in % and number of participants applied to the %
    percent_missing <- function(x){sum(is.na(x))/length(x) * 100}
    missing <- apply(VMP_data_analysis, 1, percent_missing)
    table(missing)
   
    #missing -> 0 participants have over 10% missing data (general exclusion criteria = 10%)

   
##table presents MISSING COLUMNS data in % and number of columns applied to the %
        missing_data <- apply(VMP_data_analysis, 2, percent_missing)  #CAVE: apply() on data frame with excluded missing rows!
        table(missing_data)
        
    ##count the missing values in all columns - shows specific values for items (columns)         
        VMP_data_analysis %>%
          map(~sum(is.na(.)))
        
##dealing with multivariate OUTLIERS: Mahalanobis Distance  

        mahalanobis_D <- mahalanobis(VMP_data_analysis[ , -c(1,2,3)], #without ID, GENDER & AGE 
                                     colMeans(VMP_data_analysis[ , -c(1,2,3)], na.rm=TRUE),
                                      cov(VMP_data_analysis[ , -c(1,2,3)], use ="pairwise.complete.obs"))
        
        mahalanobis_D_cutoff <- qchisq(p = 1 - .001, #1 minus alpha
                                df = ncol(VMP_data_analysis[ , -c(1,2,3)])) # number of columns
        
        no_outliers <- subset(VMP_data_analysis, mahalanobis_D < mahalanobis_D_cutoff)
        
        VMP_data_analysis_final <- no_outliers

        
###requirements SEM: Additivity or Multicolliniarity
        #should not correlate higher than .85 (to add additional information to the model)

    #1:Correlation plot - MDI
        corr_plot_MDI <- subset(VMP_data_analysis_final, 
                                    select = c(MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_7,
                                               MDI_8a,MDI_8b,MDI_9a,MDI_9b,MDI_10a,MDI_10b))
        corrplot(cor(corr_plot_MDI,use="complete.obs"))     #correlation plot
                                                            #NA <- represented through 'use="complete.obs"

    #2:Correlation plot - WHO5
        corr_plot_WHO5 <- subset(VMP_data_analysis_final, 
                                       select = c(WHO5_1,WHO5_2,WHO5_3,WHO5_4,WHO5_5))
        corrplot(cor(corr_plot_WHO5,use="complete.obs"))      
        
        
    #3:Correlation plot - MPSSS
        corr_plot_MPSSS <- subset(VMP_data_analysis_final, 
                                     select = c(MPSSS_1,MPSSS_2,MPSSS_3,MPSSS_4,MPSSS_5,MPSSS_6,
                                                MPSSS_7,MPSSS_8,MPSSS_9,MPSSS_10,MPSSS_11,MPSSS_12))
        corrplot(cor(corr_plot_MPSSS,use="complete.obs"))      
        
        
    #4:Correlation plot - PSQI
        #CAVE: only preselected items (items which were collected in VMP1 and VMP2)
        corr_plot_PSQI <- subset(VMP_data_analysis_final, 
                                      select = c(PSQI_6,PSQI_8,PSQI_9))
        corrplot(cor(corr_plot_PSQI,use="complete.obs"))      

        
    #5:Correlation plot - WHOQOL
        corr_plot_WHOQOL <- subset(VMP_data_analysis_final, 
                                     select = c(WHOQOL_1,WHOQOL_2,WHOQOL_3,WHOQOL_4,WHOQOL_5,WHOQOL_6,
                                                WHOQOL_7,WHOQOL_10,WHOQOL_11,WHOQOL_12,WHOQOL_13,
                                                WHOQOL_14,WHOQOL_15,WHOQOL_16,WHOQOL_17,WHOQOL_18,WHOQOL_19,WHOQOL_20,
                                                WHOQOL_21,WHOQOL_22,WHOQOL_23,WHOQOL_24,WHOQOL_25,WHOQOL_26))
        corrplot(cor(corr_plot_WHOQOL,use="complete.obs"))      
        
        
    #6:Correlation plot - WEMWSB
        corr_plot_WEMWBS <- subset(VMP_data_analysis_final, 
                                       select = c(WEMWBS_1,WEMWBS_2,WEMWBS_3,WEMWBS_4,
                                                  WEMWBS_5,WEMWBS_6,WEMWBS_7,
                                                  WEMWBS_10,WEMWBS_11,WEMWBS_12,
                                                  WEMWBS_13,WEMWBS_14,WEMWBS_15,WEMWBS_16))
        corrplot(cor(corr_plot_WEMWBS,use="complete.obs"))      
        
        
    #7:Correlation plot - PSS
        corr_plot_PSS <- subset(VMP_data_analysis_final, 
                                       select = c(PSS_1,PSS_2,PSS_3,PSS_6,
                                                  PSS_9,PSS_10))
        corrplot(cor(corr_plot_PSS,use="complete.obs"))      
        
        
    #8:Correlation plot - MFI20
        corr_plot_MFI20 <- subset(VMP_data_analysis_final, 
                                    select = c(MFI20_1,MFI20_3,MFI20_4,MFI20_6,
                                               MFI20_7,MFI20_8,MFI20_11,MFI20_12,
                                               MFI20_15,
                                               MFI20_20))
        corrplot(cor(corr_plot_MFI20,use="complete.obs"))      

        
        ##correlation plot: items MPWB model     
        
        correlation_items_model_D <- subset(VMP_data_analysis_final,
                                            select = c(MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_8b,
                                                       PSS_2,PSS_3,PSS_6,PSS_10,WEMWBS_3,WEMWBS_10,
                                                       MFI20_1,MFI20_3,
                                                       MFI20_7,MFI20_8,
                                                       MFI20_11,MFI20_12,MFI20_20, 
                                                       WHO5_1,WHO5_2,
                                                       WHOQOL_5,WHOQOL_10,
                                                       WHOQOL_17,WHOQOL_19,
                                                       WHOQOL_26)) 
        
        corr_MPWB <- cor(correlation_items_model_D)
        cor.plot(correlation_items_model_D)
        
        ##visualization for appendix
        corrplot(corr_MPWB, tl.col = "brown", tl.srt = 35, bg = "White",
                 title = "\n\n Correlation Plot: Items MPWB Model", tl.cex = 0.5,
                 type = "lower")        
        

##requirements: creating a 'fake' model to apply functions and check requirements  
        random_variable <- rchisq(nrow(VMP_data_analysis_final),7)
        fake_model <- lm(random_variable ~.,
                         data = VMP_data_analysis_final)
        standardized <- rstudent(fake_model)
        fitvalues <- scale(fake_model$fitted.values)
        
##requirements: Linearity
        #assume that the multivariate relationship between continuous variables is linear (i.e., not curved)
        plot(fake_model$fitted.values,studres(fake_model))    
        plot(fake_model, 2, sub = "\n\n Linearity: graphically proofen")
        
        
##requirements: Normality        
        #large sample size -> buffering against normality deviation
        #Multivariate Normality 
          #Mardia Test
        mult.norm(VMP_data_analysis_final)$mult.test
        

##requirements: Homoscedasticity
        #assumes that variance of residuals is equal
        
        {plot(standardized, fitvalues)
          abline(v = 0)
          abline(h = 0)
        }

