#Author: Rebecca Astrid Böhme 
#EFA script BA: The Construction of Pain - Linking Psychological Factors to Pain thresholds


    library(rio)
    library(psych)
    library(corrplot)
    library(tidyverse)
    library(dplyr)
    library(ggplot2)
    library(GPArotation)
    library(mirt)
    library(MASS)
    library(msm)
    library(polycor)

###Item Response Modelling
  #IRT analyses: items will be assigned to the factors on which they have the greatest loadings: EFA    
    
    
    ##item parameters    
    psych:: describe(VMP_data_analysis_final)   
    
    ##item difficulty 
    #as additional selection criterion (select between 2 items)
    item_diff = function (x, min, max, n) {
      p = sum (x-min)/(n*(max-min))
      p = p*100
      p
    }
    
    item_diff(VMP_data_analysis_final$MDI_1,0,5,398)  #CAVE: example for item MDI_1 (relevant items need to be added) 
    
    item.model <- mirt(data = VMP_data_analysis_final, #polytomous IRT  
                       model = 1,
                       itemtype = "gpcm")    
    summary(item.model)
    plot(item.model)
    plot(item.model, type = "trace")
    
    
    ##correlation: overview
    cor.tab.VMP <- cor(VMP_data_analysis_final)
    round(cor.tab.VMP, 3) #rounds to specified decimal place
    cor.plot(VMP_data_analysis_final[,-c(1,2,3)], numbers = FALSE,
             main = "Item Correlation", upper = FALSE)
    
    
    ##finding factors & components
    efa.df <- VMP_data_analysis_final
    number_items <- fa.parallel(efa.df,       
                                fm = "ml",    #Maximum Likelihood as estimator
                                fa = "fa",
                                use = "pairwise")   
    sum(number_items$fa.values > 1)                 #Eigenvalue > 1  (1 would be great)
    
    
    ##iterative process to assign items to factors
    
    VMP_data_wellbeing <- subset(VMP_data, 
                                 select = c(WEMWBS_1,WEMWBS_2,WEMWBS_3,WEMWBS_4,WEMWBS_5,
                                            WEMWBS_6,WEMWBS_7,WEMWBS_10,WEMWBS_11,WEMWBS_12,WEMWBS_13,WEMWBS_14,WEMWBS_15,
                                            WEMWBS_16,WHO5_1,WHO5_2,WHO5_3,WHO5_4,WHO5_5,WHOQOL_1,WHOQOL_2,WHOQOL_3,WHOQOL_4,WHOQOL_5,WHOQOL_6,
                                            WHOQOL_7,WHOQOL_10,WHOQOL_11,WHOQOL_12,WHOQOL_13,WHOQOL_14,WHOQOL_15,WHOQOL_16,WHOQOL_17,
                                            WHOQOL_18,WHOQOL_19,WHOQOL_20,WHOQOL_21,WHOQOL_22,WHOQOL_23,WHOQOL_24,WHOQOL_25,WHOQOL_26,
                                            MPSSS_1,MPSSS_2,MPSSS_3,MPSSS_4,
                                            MPSSS_5,MPSSS_6,MPSSS_7,MPSSS_8,MPSSS_9,MPSSS_10,MPSSS_11,MPSSS_12))
    
    VMP_data_affect <- subset(VMP_data,
                              select = c(MDI_1,MDI_2,MDI_3,MDI_4,MDI_5,MDI_6,MDI_7,MDI_8a,
                                         MDI_8b,MDI_9a,MDI_9b,MDI_10a,MDI_10b,WEMWBS_1,WEMWBS_2,WEMWBS_3,WEMWBS_4,WEMWBS_5,
                                         WEMWBS_6,WEMWBS_7,WEMWBS_10,WEMWBS_11,WEMWBS_12,WEMWBS_13,WEMWBS_14,WEMWBS_15,
                                         WEMWBS_16,
                                         MPSSS_1,MPSSS_2,MPSSS_3,MPSSS_4,
                                         MPSSS_5,MPSSS_6,MPSSS_7,MPSSS_8,MPSSS_9,MPSSS_10,MPSSS_11,MPSSS_12))    
    
    VMP_data_stress <- subset(VMP_data,
                              select = c(PSS_1,PSS_2,PSS_3,
                                         PSS_6,PSS_9,PSS_10,
                                         WEMWBS_1,WEMWBS_2,WEMWBS_3,WEMWBS_4,WEMWBS_5,
                                         WEMWBS_6,WEMWBS_7,WEMWBS_10,WEMWBS_11,WEMWBS_12,WEMWBS_13,WEMWBS_14,WEMWBS_15,
                                         WEMWBS_16,
                                         MPSSS_1,MPSSS_2,MPSSS_3,MPSSS_4,
                                         MPSSS_5,MPSSS_6,MPSSS_7,MPSSS_8,MPSSS_9,MPSSS_10,MPSSS_11,MPSSS_12))
    
    VMP_data_physio_state <- subset(VMP_data,
                                    select = c(MFI20_1,
                                               MFI20_3,MFI20_4,MFI20_6,MFI20_7,MFI20_8,
                                               MFI20_11,MFI20_12,MFI20_15,MFI20_20,
                                               PSQI_6,PSQI_8,PSQI_9,PSQI_10,
                                               WHOQOL_2,WHOQOL_3,WHOQOL_10,WHOQOL_11,WHOQOL_16,
                                               WHOQOL_24))
    
    VMP_data_social <- subset(VMP_data,
                              select = c(MPSSS_1,MPSSS_2,MPSSS_3,MPSSS_4,
                                         MPSSS_5,MPSSS_6,MPSSS_7,MPSSS_8,MPSSS_9,MPSSS_10,MPSSS_11,MPSSS_12,
                                         WEMWBS_1,WEMWBS_2,WEMWBS_3,WEMWBS_4,WEMWBS_5,
                                         WEMWBS_6,WEMWBS_7,WEMWBS_10,WEMWBS_11,WEMWBS_12,WEMWBS_13,WEMWBS_14,WEMWBS_15,
                                         WEMWBS_16))
    
    
    ##EFA with prescreened factors        
    fac.VMP.well = fa(VMP_data_wellbeing, 
                      nfactors = 1, 
                      fm = "pa", 
                      rotate = "oblimin")
    print.psych(fac.VMP.well, cut = 0.5)
    
    fac.VMP.aff = fa(VMP_data_affect, 
                     nfactors = 1, 
                     fm = "pa", 
                     rotate = "oblimin")
    print.psych(fac.VMP.aff, cut = 0.5)
    
    fac.VMP.stress = fa(VMP_data_stress, 
                        nfactors = 1, 
                        fm = "pa", 
                        rotate = "oblimin")
    print.psych(fac.VMP.stress, cut = 0.5)
    
    fac.VMP.physio = fa(VMP_data_physio_state, 
                        nfactors = 1, 
                        fm = "pa", 
                        rotate = "oblimin")
    print.psych(fac.VMP.physio, cut = 0.5)
    
    fac.VMP.social = fa(VMP_data_social, 
                        nfactors = 1, 
                        fm = "pa", 
                        rotate = "oblimin")
    print.psych(fac.VMP.social, cut = 0.5)
    
    fac.VMP = fa(VMP_data_analysis_final, 
                 nfactors = 1, 
                 fm = "pa", 
                 rotate = "oblimin")
    print.psych(fac.VMP, cut = 0.5)
    
    