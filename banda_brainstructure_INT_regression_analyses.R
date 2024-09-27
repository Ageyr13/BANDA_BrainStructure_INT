#Title: BANDA Residualized Change Regression Analysis of Baseline Cortical Thickness Predicting One-Year Change in INT Factor Scores
#Author: Adrienne L. Romer, PhD
#Date: 09-27-2024

#clear workspace
rm(list=ls())

##Load libraries, read files, data managmenet

#load packages
library(psych)              # use for descriptive statistics
library(ggplot2)            # use for plots
library(dplyr)              # load dplyr package
library(plyr)               # load plyr package
library(interactions)       # use for calculating simple intercepts and slopes
library(ggseg)              # use for creating brain images
library(patchwork)          # use for creating figures

#set working directory
setwd("/Users/adrienneromer/Library/CloudStorage/OneDrive-VirginiaTech/BANDA")

#import data
intfactor <- read.csv("banda_all_timepoints_reduced_dataset_for_analysis_071024.csv",header=TRUE)

#recode all missing values from -99 to NA
intfactor[intfactor == -99] <- NA

#remove missing data into reduced dataset
intfactor_red <- intfactor[(intfactor$Missing_smri)=="0" & is.na(intfactor$Adolint1_std)==FALSE,]

#create new variables by renaming them
intfactor_red$gender <- intfactor_red$demo_child_gender_i

##Descriptive statistics and histograms of factor scores

#Descriptives
describeBy(intfactor_red$Adolint1_std)
describeBy(intfactor_red$Adolint3_std)
describeBy(intfactor_red$Parint1_std)
describeBy(intfactor_red$Parint3_std)

#Histograms
hist(intfactor_red$Adolint1_std,
     main="Marginal Histogram",
     xlab="Baseline Adolescent-Reported INT Factor Scores",
     prob=TRUE,
     breaks=40)
hist(intfactor_red$Adolint3_std,
     main="Marginal Histogram",
     xlab="1-Year Follow-Up Adolescent-Reported INT Factor Scores",
     prob=TRUE,
     breaks=40)
hist(intfactor_red$Parint1_std,
     main="Marginal Histogram",
     xlab="Baseline Parent-Reported INT Factor Scores",
     prob=TRUE,
     breaks=40)
hist(intfactor_red$Parint3_std,
     main="Marginal Histogram",
     xlab="1-Year Follow-Up Parent-Reported INT Factor Scores",
     prob=TRUE,
     breaks=40)

#scale all variables (factor scores and CT parcels)
intfactor_red$adol_int1_scale <- scale(intfactor_red$Adolint1_std)
intfactor_red$adol_int3_scale <- scale(intfactor_red$Adolint3_std)
intfactor_red$par_int1_scale <- scale(intfactor_red$Parint1_std)
intfactor_red$par_int3_scale <- scale(intfactor_red$Parint3_std)
intfactor_red$lh_caudalmiddlefrontal_thickness_scale <- scale(intfactor_red$lh_caudalmiddlefrontal_thickness)
intfactor_red$lh_frontalpole_thickness_scale <- scale(intfactor_red$lh_frontalpole_thickness)
intfactor_red$lh_lateralorbitofrontal_thickness_scale <- scale(intfactor_red$lh_lateralorbitofrontal_thickness)
intfactor_red$lh_medialorbitofrontal_thickness_scale <- scale(intfactor_red$lh_medialorbitofrontal_thickness)
intfactor_red$lh_paracentral_thickness_scale <- scale(intfactor_red$lh_paracentral_thickness)
intfactor_red$lh_parsopercularis_thickness_scale <- scale(intfactor_red$lh_parsopercularis_thickness)
intfactor_red$lh_parsorbitalis_thickness_scale <- scale(intfactor_red$lh_parsorbitalis_thickness)
intfactor_red$lh_parstriangularis_thickness_scale <- scale(intfactor_red$lh_parstriangularis_thickness)
intfactor_red$lh_precentral_thickness_scale <- scale(intfactor_red$lh_precentral_thickness)
intfactor_red$lh_rostralmiddlefrontal_thickness_scale <- scale(intfactor_red$lh_rostralmiddlefrontal_thickness)
intfactor_red$lh_superiorfrontal_thickness_scale <- scale(intfactor_red$lh_superiorfrontal_thickness)
intfactor_red$lh_inferiorparietal_thickness_scale <- scale(intfactor_red$lh_inferiorparietal_thickness)
intfactor_red$lh_postcentral_thickness_scale <- scale(intfactor_red$lh_postcentral_thickness)
intfactor_red$lh_precuneus_thickness_scale <- scale(intfactor_red$lh_precuneus_thickness)
intfactor_red$lh_superiorparietal_thickness_scale <- scale(intfactor_red$lh_superiorparietal_thickness)
intfactor_red$lh_supramarginal_thickness_scale <- scale(intfactor_red$lh_supramarginal_thickness)
intfactor_red$lh_bankssts_thickness_scale <- scale(intfactor_red$lh_bankssts_thickness)
intfactor_red$lh_entorhinal_thickness_scale <- scale(intfactor_red$lh_entorhinal_thickness)
intfactor_red$lh_fusiform_thickness_scale <- scale(intfactor_red$lh_fusiform_thickness)
intfactor_red$lh_inferiortemporal_thickness_scale <- scale(intfactor_red$lh_inferiortemporal_thickness)
intfactor_red$lh_middletemporal_thickness_scale <- scale(intfactor_red$lh_middletemporal_thickness)
intfactor_red$lh_parahippocampal_thickness_scale <- scale(intfactor_red$lh_parahippocampal_thickness)
intfactor_red$lh_superiortemporal_thickness_scale <- scale(intfactor_red$lh_superiortemporal_thickness)
intfactor_red$lh_temporalpole_thickness_scale <- scale(intfactor_red$lh_temporalpole_thickness)
intfactor_red$lh_transversetemporal_thickness_scale <- scale(intfactor_red$lh_transversetemporal_thickness)
intfactor_red$lh_cuneus_thickness_scale <- scale(intfactor_red$lh_cuneus_thickness)
intfactor_red$lh_lateraloccipital_thickness_scale <- scale(intfactor_red$lh_lateraloccipital_thickness)
intfactor_red$lh_lingual_thickness_scale <- scale(intfactor_red$lh_lingual_thickness)
intfactor_red$lh_pericalcarine_thickness_scale <- scale(intfactor_red$lh_pericalcarine_thickness)
intfactor_red$lh_caudalanteriorcingulate_thickness_scale <- scale(intfactor_red$lh_caudalanteriorcingulate_thickness)
intfactor_red$lh_isthmuscingulate_thickness_scale <- scale(intfactor_red$lh_isthmuscingulate_thickness)
intfactor_red$lh_posteriorcingulate_thickness_scale <- scale(intfactor_red$lh_posteriorcingulate_thickness)
intfactor_red$lh_rostralanteriorcingulate_thickness_scale <- scale(intfactor_red$lh_rostralanteriorcingulate_thickness)
intfactor_red$lh_insula_thickness_scale <- scale(intfactor_red$lh_insula_thickness)
intfactor_red$rh_caudalmiddlefrontal_thickness_scale <- scale(intfactor_red$rh_caudalmiddlefrontal_thickness)
intfactor_red$rh_frontalpole_thickness_scale <- scale(intfactor_red$rh_frontalpole_thickness)
intfactor_red$rh_lateralorbitofrontal_thickness_scale <- scale(intfactor_red$rh_lateralorbitofrontal_thickness)
intfactor_red$rh_medialorbitofrontal_thickness_scale <- scale(intfactor_red$rh_medialorbitofrontal_thickness)
intfactor_red$rh_paracentral_thickness_scale <- scale(intfactor_red$rh_paracentral_thickness)
intfactor_red$rh_parsopercularis_thickness_scale <- scale(intfactor_red$rh_parsopercularis_thickness)
intfactor_red$rh_parsorbitalis_thickness_scale <- scale(intfactor_red$rh_parsorbitalis_thickness)
intfactor_red$rh_parstriangularis_thickness_scale <- scale(intfactor_red$rh_parstriangularis_thickness)
intfactor_red$rh_precentral_thickness_scale <- scale(intfactor_red$rh_precentral_thickness)
intfactor_red$rh_rostralmiddlefrontal_thickness_scale <- scale(intfactor_red$rh_rostralmiddlefrontal_thickness)
intfactor_red$rh_superiorfrontal_thickness_scale <- scale(intfactor_red$rh_superiorfrontal_thickness)
intfactor_red$rh_inferiorparietal_thickness_scale <- scale(intfactor_red$rh_inferiorparietal_thickness)
intfactor_red$rh_postcentral_thickness_scale <- scale(intfactor_red$rh_postcentral_thickness)
intfactor_red$rh_precuneus_thickness_scale <- scale(intfactor_red$rh_precuneus_thickness)
intfactor_red$rh_superiorparietal_thickness_scale <- scale(intfactor_red$rh_superiorparietal_thickness)
intfactor_red$rh_supramarginal_thickness_scale <- scale(intfactor_red$rh_supramarginal_thickness)
intfactor_red$rh_bankssts_thickness_scale <- scale(intfactor_red$rh_bankssts_thickness)
intfactor_red$rh_entorhinal_thickness_scale <- scale(intfactor_red$rh_entorhinal_thickness)
intfactor_red$rh_fusiform_thickness_scale <- scale(intfactor_red$rh_fusiform_thickness)
intfactor_red$rh_inferiortemporal_thickness_scale <- scale(intfactor_red$rh_inferiortemporal_thickness)
intfactor_red$rh_middletemporal_thickness_scale <- scale(intfactor_red$rh_middletemporal_thickness)
intfactor_red$rh_parahippocampal_thickness_scale <- scale(intfactor_red$rh_parahippocampal_thickness)
intfactor_red$rh_superiortemporal_thickness_scale <- scale(intfactor_red$rh_superiortemporal_thickness)
intfactor_red$rh_temporalpole_thickness_scale <- scale(intfactor_red$rh_temporalpole_thickness)
intfactor_red$rh_transversetemporal_thickness_scale <- scale(intfactor_red$rh_transversetemporal_thickness)
intfactor_red$rh_cuneus_thickness_scale <- scale(intfactor_red$rh_cuneus_thickness)
intfactor_red$rh_lateraloccipital_thickness_scale <- scale(intfactor_red$rh_lateraloccipital_thickness)
intfactor_red$rh_lingual_thickness_scale <- scale(intfactor_red$rh_lingual_thickness)
intfactor_red$rh_pericalcarine_thickness_scale <- scale(intfactor_red$rh_pericalcarine_thickness)
intfactor_red$rh_caudalanteriorcingulate_thickness_scale <- scale(intfactor_red$rh_caudalanteriorcingulate_thickness)
intfactor_red$rh_isthmuscingulate_thickness_scale <- scale(intfactor_red$rh_isthmuscingulate_thickness)
intfactor_red$rh_posteriorcingulate_thickness_scale <- scale(intfactor_red$rh_posteriorcingulate_thickness)
intfactor_red$rh_rostralanteriorcingulate_thickness_scale <- scale(intfactor_red$rh_rostralanteriorcingulate_thickness)
intfactor_red$rh_insula_thickness_scale <- scale(intfactor_red$rh_insula_thickness)

##Residualized Change Regression Models

#Regression of 68 Baseline CT Parcels Predicting Adolescent-Reported INT 1yr Later Controlling for Baseline INT 
ctparcel_adol_int_1yrchange <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta1 <- summary(lm(adol_int3_scale ~ gender + age + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[5,1]
  se1 <- summary(lm(adol_int3_scale ~ gender + age + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[5,2]
  pval1 <- summary(lm(adol_int3_scale ~ gender + age + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[5,4]
  
  stats_ctparcel_adol_int_1yrchange <- c(beta1, se1, pval1)
  ctparcel_adol_int_1yrchange <- cbind(ctparcel_adol_int_1yrchange,stats_ctparcel_adol_int_1yrchange)
  names(ctparcel_adol_int_1yrchange)[ncol(ctparcel_adol_int_1yrchange)] <- names(intfactor_red)[i]
}
ctparcel_adol_int_1yrchange
write.csv(ctparcel_adol_int_1yrchange, "Parcel_Wise_Thickness_1yrChange_AdolINT_Analysis.csv") #write csv file

#FDR correction
pval_ctparcel_adol_int_1yrchange <- dplyr::slice(ctparcel_adol_int_1yrchange, 3)
pval_ctparcel_adol_int_1yrchange = subset(pval_ctparcel_adol_int_1yrchange, select = -c(x))
pval_fdr_ctparcel_adol_int_1yrchange <- transform(pval_ctparcel_adol_int_1yrchange, 
                                                  adj.p = p.adjust(as.matrix(pval_ctparcel_adol_int_1yrchange),method = "BH"))
sum(pval_fdr_ctparcel_adol_int_1yrchange$adj.p<0.05)
write.csv(pval_fdr_ctparcel_adol_int_1yrchange, "FDR adjusted pvalues_Parcel_Wise_Thickness_1yrChange_adolINT_Analysis.csv")

#Regression of Baseline CT Parcels Predicting parent-Reported INT 1yr Later Controlling for Baseline INT 
ctparcel_par_int_1yrchange <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta2 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[5,1]
  se2 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[5,2]
  pval2 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[5,4]
  
  stats_ctparcel_par_int_1yrchange <- c(beta2, se2, pval2)
  ctparcel_par_int_1yrchange <- cbind(ctparcel_par_int_1yrchange,stats_ctparcel_par_int_1yrchange)
  names(ctparcel_par_int_1yrchange)[ncol(ctparcel_par_int_1yrchange)] <- names(intfactor_red)[i]
}
ctparcel_par_int_1yrchange
write.csv(ctparcel_par_int_1yrchange, "Parcel_Wise_Thickness_1yrChange_AdolINT_Analysis.csv") #write csv file

#FDR correction
pval_ctparcel_par_int_1yrchange <- dplyr::slice(ctparcel_par_int_1yrchange, 3)
pval_ctparcel_par_int_1yrchange = subset(pval_ctparcel_par_int_1yrchange, select = -c(x))
pval_fdr_ctparcel_par_int_1yrchange <- transform(pval_ctparcel_par_int_1yrchange, 
                                                 adj.p = p.adjust(as.matrix(pval_ctparcel_par_int_1yrchange),method = "BH"))
sum(pval_fdr_ctparcel_par_int_1yrchange$adj.p<0.05)
write.csv(pval_fdr_ctparcel_par_int_1yrchange, "FDR adjusted pvalues_Parcel_Wise_Thickness_1yrChange_ParINT_Analysis.csv")

##Sensitivity Analyses: added medication use, intracranial volume, Tanner stage, and SES as additional covariates; removed participants with missing follow-up data.

#Regression of Baseline CT Parcels Predicting Adolescent-Reported INT 1yr Later Controlling for Baseline INT with medication use as additional covariate
ctparcel_adol_int_1yrchange_med <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta3 <- summary(lm(adol_int3_scale ~ gender + age + med_use + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,1]
  se3 <- summary(lm(adol_int3_scale ~ gender + age + med_use + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,2]
  pval3 <- summary(lm(adol_int3_scale ~ gender + age + med_use + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,4]
  
  stats_ctparcel_adol_int_1yrchange_med <- c(beta3, se3, pval3)
  ctparcel_adol_int_1yrchange_med <- cbind(ctparcel_adol_int_1yrchange_med,stats_ctparcel_adol_int_1yrchange_med)
  names(ctparcel_adol_int_1yrchange_med)[ncol(ctparcel_adol_int_1yrchange_med)] <- names(intfactor_red)[i]
}
ctparcel_adol_int_1yrchange_med
write.csv(ctparcel_adol_int_1yrchange_med, "Parcel_Wise_Thickness_1yrChange_AdolINT_Med_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Parent-Reported INT 1yr Later Controlling for Baseline INT with medication use as additional covariate 
ctparcel_par_int_1yrchange_med <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta4 <- summary(lm(par_int3_scale ~ gender + age + med_use + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,1]
  se4 <- summary(lm(par_int3_scale ~ gender + age + med_use + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,2]
  pval4 <- summary(lm(par_int3_scale ~ gender + age + med_use + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,4]
  
  stats_ctparcel_par_int_1yrchange_med <- c(beta4, se4, pval4)
  ctparcel_par_int_1yrchange_med <- cbind(ctparcel_par_int_1yrchange_med,stats_ctparcel_par_int_1yrchange_med)
  names(ctparcel_par_int_1yrchange_med)[ncol(ctparcel_par_int_1yrchange_med)] <- names(intfactor_red)[i]
}
ctparcel_par_int_1yrchange_med
write.csv(ctparcel_par_int_1yrchange_med, "Parcel_Wise_Thickness_1yrChange_ParINT_Med_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Adolescent-Reported INT 1yr Later Controlling for Baseline INT with intracranial volume as additional covariate
ctparcel_adol_int_1yrchange_icv <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta5 <- summary(lm(adol_int3_scale ~ gender + age + EstimatedTotalIntraCranialVol + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,1]
  se5 <- summary(lm(adol_int3_scale ~ gender + age + EstimatedTotalIntraCranialVol + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,2]
  pval5 <- summary(lm(adol_int3_scale ~ gender + age + EstimatedTotalIntraCranialVol + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,4]
  
  stats_ctparcel_adol_int_1yrchange_icv <- c(beta5, se5, pval5)
  ctparcel_adol_int_1yrchange_icv <- cbind(ctparcel_adol_int_1yrchange_icv,stats_ctparcel_adol_int_1yrchange_icv)
  names(ctparcel_adol_int_1yrchange_icv)[ncol(ctparcel_adol_int_1yrchange_icv)] <- names(intfactor_red)[i]
}
ctparcel_adol_int_1yrchange_icv
write.csv(ctparcel_adol_int_1yrchange_icv, "Parcel_Wise_Thickness_1yrChange_AdolINT_ICV_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Parent-Reported INT 1yr Later Controlling for Baseline INT with intracranial volume as additional covariate
ctparcel_par_int_1yrchange_icv <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta6 <- summary(lm(par_int3_scale ~ gender + age + EstimatedTotalIntraCranialVol + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,1]
  se6 <- summary(lm(par_int3_scale ~ gender + age + EstimatedTotalIntraCranialVol + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,2]
  pval6 <- summary(lm(par_int3_scale ~ gender + age + EstimatedTotalIntraCranialVol + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,4]
  
  stats_ctparcel_par_int_1yrchange_icv <- c(beta6, se6, pval6)
  ctparcel_par_int_1yrchange_icv <- cbind(ctparcel_par_int_1yrchange_icv,stats_ctparcel_par_int_1yrchange_icv)
  names(ctparcel_par_int_1yrchange_icv)[ncol(ctparcel_par_int_1yrchange_icv)] <- names(intfactor_red)[i]
}
ctparcel_par_int_1yrchange_icv
write.csv(ctparcel_par_int_1yrchange_icv, "Parcel_Wise_Thickness_1yrChange_ParINT_ICV_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Adolescent-Reported INT 1yr Later Controlling for Baseline INT with Tanner stage as additional covariate
ctparcel_adol_int_1yrchange_tanner <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta7 <- summary(lm(adol_int3_scale ~ gender + age + tanner_total + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,1]
  se7 <- summary(lm(adol_int3_scale ~ gender + age + tanner_total + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,2]
  pval7 <- summary(lm(adol_int3_scale ~ gender + age + tanner_total + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,4]
  
  stats_ctparcel_adol_int_1yrchange_tanner <- c(beta7, se7, pval7)
  ctparcel_adol_int_1yrchange_tanner <- cbind(ctparcel_adol_int_1yrchange_tanner,stats_ctparcel_adol_int_1yrchange_tanner)
  names(ctparcel_adol_int_1yrchange_tanner)[ncol(ctparcel_adol_int_1yrchange_tanner)] <- names(intfactor_red)[i]
}
ctparcel_adol_int_1yrchange_tanner
write.csv(ctparcel_adol_int_1yrchange_tanner, "Parcel_Wise_Thickness_1yrChange_AdolINT_Tanner_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Parent-Reported INT 1yr Later Controlling for Baseline INT with Tanner stage as additional covariate 
ctparcel_par_int_1yrchange_tanner <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta8 <- summary(lm(par_int3_scale ~ gender + age + tanner_total + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,1]
  se8 <- summary(lm(par_int3_scale ~ gender + age + tanner_total + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,2]
  pval8 <- summary(lm(par_int3_scale ~ gender + age + tanner_total + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[6,4]
  
  stats_ctparcel_par_int_1yrchange_tanner <- c(beta8, se8, pval8)
  ctparcel_par_int_1yrchange_tanner <- cbind(ctparcel_par_int_1yrchange_tanner,stats_ctparcel_par_int_1yrchange_tanner)
  names(ctparcel_par_int_1yrchange_tanner)[ncol(ctparcel_par_int_1yrchange_tanner)] <- names(intfactor_red)[i]
}
ctparcel_par_int_1yrchange_tanner
write.csv(ctparcel_par_int_1yrchange_tanner, "Parcel_Wise_Thickness_1yrChange_ParINT_Tanner_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Adolescent-Reported INT 1yr Later Controlling for Baseline INT with SES (parent educ & fam income) as additional covariate
ctparcel_adol_int_1yrchange_ses <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta9 <- summary(lm(adol_int3_scale ~ gender + age + parent_educ_ave + demo_income_i + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[7,1]
  se9 <- summary(lm(adol_int3_scale ~ gender + age + parent_educ_ave + demo_income_i + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[7,2]
  pval9 <- summary(lm(adol_int3_scale ~ gender + age + parent_educ_ave + demo_income_i + adol_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[7,4]
  
  stats_ctparcel_adol_int_1yrchange_ses <- c(beta9, se9, pval9)
  ctparcel_adol_int_1yrchange_ses <- cbind(ctparcel_adol_int_1yrchange_ses,stats_ctparcel_adol_int_1yrchange_ses)
  names(ctparcel_adol_int_1yrchange_ses)[ncol(ctparcel_adol_int_1yrchange_ses)] <- names(intfactor_red)[i]
}
ctparcel_adol_int_1yrchange_ses
write.csv(ctparcel_adol_int_1yrchange_ses, "Parcel_Wise_Thickness_1yrChange_AdolINT_SES_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Parent-Reported INT 1yr Later Controlling for Baseline INT with SES (parent educ & fam income) as additional covariate 
ctparcel_par_int_1yrchange_ses <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta10 <- summary(lm(par_int3_scale ~ gender + age + parent_educ_ave + demo_income_i + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[7,1]
  se10 <- summary(lm(par_int3_scale ~ gender + age + parent_educ_ave + demo_income_i + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[7,2]
  pval10 <- summary(lm(par_int3_scale ~ gender + age + parent_educ_ave + demo_income_i + par_int1_scale + intfactor_red[[i]], data=intfactor_red))$coefficients[7,4]
  
  stats_ctparcel_par_int_1yrchange_ses <- c(beta10, se10, pval10)
  ctparcel_par_int_1yrchange_ses <- cbind(ctparcel_par_int_1yrchange_ses,stats_ctparcel_par_int_1yrchange_ses)
  names(ctparcel_par_int_1yrchange_ses)[ncol(ctparcel_par_int_1yrchange_ses)] <- names(intfactor_red)[i]
}
ctparcel_par_int_1yrchange_ses
write.csv(ctparcel_par_int_1yrchange_ses, "Parcel_Wise_Thickness_1yrChange_ParINT_SES_Analysis.csv") #write csv file

#Create new dataset excluding 54 participants with completely missing follow-up data (n=149)
intfactor_red_nonmiss <- intfactor_red[(intfactor_red$missing_12mo_adol_and_par_report)=="0",]

#Regression of Baseline CT Parcels Predicting Adolescent-Reported INT 1yr Later Controlling for Baseline INT excluding 54 participants with missing follow-up data (n=149)
ctparcel_adol_int_1yrchange_nonmiss <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta11 <- summary(lm(adol_int3_scale ~ gender + age + adol_int1_scale + intfactor_red_nonmiss[[i]], data=intfactor_red_nonmiss))$coefficients[5,1]
  se11 <- summary(lm(adol_int3_scale ~ gender + age + adol_int1_scale + intfactor_red_nonmiss[[i]], data=intfactor_red_nonmiss))$coefficients[5,2]
  pval11 <- summary(lm(adol_int3_scale ~ gender + age + adol_int1_scale + intfactor_red_nonmiss[[i]], data=intfactor_red_nonmiss))$coefficients[5,4]
  
  stats_ctparcel_adol_int_1yrchange_nonmiss <- c(beta11, se11, pval11)
  ctparcel_adol_int_1yrchange_nonmiss <- cbind(ctparcel_adol_int_1yrchange_nonmiss,stats_ctparcel_adol_int_1yrchange_nonmiss)
  names(ctparcel_adol_int_1yrchange_nonmiss)[ncol(ctparcel_adol_int_1yrchange_nonmiss)] <- names(intfactor_red_nonmiss)[i]
}
ctparcel_adol_int_1yrchange_nonmiss
write.csv(ctparcel_adol_int_1yrchange_nonmiss, "Parcel_Wise_Thickness_1yrChange_AdolINT_Nonmissing_Analysis.csv") #write csv file

#Regression of Baseline CT Parcels Predicting Parent-Reported INT 1yr Later Controlling for Baseline INT excluding 54 participants with missing follow-up data (n=149) 
ctparcel_par_int_1yrchange_nonmiss <- data.frame(x=c("beta","se","pval"))
for (i in 980:1047){ # columns are ct parcels
  beta12 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + intfactor_red_nonmiss[[i]], data=intfactor_red_nonmiss))$coefficients[5,1]
  se12 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + intfactor_red_nonmiss[[i]], data=intfactor_red_nonmiss))$coefficients[5,2]
  pval12 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + intfactor_red_nonmiss[[i]], data=intfactor_red_nonmiss))$coefficients[5,4]
  
  stats_ctparcel_par_int_1yrchange_nonmiss <- c(beta12, se12, pval12)
  ctparcel_par_int_1yrchange_nonmiss <- cbind(ctparcel_par_int_1yrchange_nonmiss,stats_ctparcel_par_int_1yrchange_nonmiss)
  names(ctparcel_par_int_1yrchange_nonmiss)[ncol(ctparcel_par_int_1yrchange_nonmiss)] <- names(intfactor_red_nonmiss)[i]
}
ctparcel_par_int_1yrchange_nonmiss
write.csv(ctparcel_par_int_1yrchange_nonmiss, "Parcel_Wise_Thickness_1yrChange_ParINT_Nonmissing_Analysis.csv") #write csv file

##Interactions with Chronic Life Event Stress (LES)

#mean center stress variables
intfactor_red$chronic_total_mc <- scale(intfactor_red$DiffCT, scale = FALSE)
intfactor_red$housing_chronic_total_mc <- scale(intfactor_red$DHDiffCT, scale = FALSE)
intfactor_red$educ_chronic_total_mc <- scale(intfactor_red$DEDiffCT, scale = FALSE)
intfactor_red$tx_chronic_total_mc <- scale(intfactor_red$DTDiffCT, scale = FALSE)
intfactor_red$marital_chronic_total_mc <- scale(intfactor_red$DMDiffCT, scale = FALSE)
intfactor_red$finance_chronic_total_mc <- scale(intfactor_red$DFDiffCT, scale = FALSE)
intfactor_red$legal_chronic_total_mc <- scale(intfactor_red$DLDiffCT, scale = FALSE)
intfactor_red$other_rel_chronic_total_mc <- scale(intfactor_red$DODiffCT, scale = FALSE) 
intfactor_red$death_chronic_total_mc <- scale(intfactor_red$DDDiffCT, scale = FALSE)
intfactor_red$parent_chronic_total_mc <- scale(intfactor_red$DGDiffCT, scale = FALSE)
intfactor_red$inter_loss_chronic_total_mc <- scale(intfactor_red$CIDiffCT, scale = FALSE)
intfactor_red$danger_chronic_total_mc <- scale(intfactor_red$CDDiffCT, scale = FALSE)
intfactor_red$humiliate_chronic_total_mc <- scale(intfactor_red$CHDiffCT, scale = FALSE) 
intfactor_red$entrap_chronic_total_mc <- scale(intfactor_red$CEDiffCT, scale = FALSE)
intfactor_red$role_change_chronic_total_mc <- scale(intfactor_red$CRDiffCT, scale = FALSE)

#mean center CT parcels
intfactor_red$lh_temporalpole_thickness_mc <- scale(intfactor_red$lh_temporalpole_thickness, scale = FALSE)
intfactor_red$rh_temporalpole_thickness_mc <- scale(intfactor_red$rh_temporalpole_thickness, scale = FALSE)
intfactor_red$lh_insula_thickness_mc <- scale(intfactor_red$lh_insula_thickness, scale = FALSE)

#scale stress variables
intfactor_red$chronic_total_scale <- scale(intfactor_red$DiffCT, scale = TRUE)
intfactor_red$housing_chronic_total_scale <- scale(intfactor_red$DHDiffCT, scale = TRUE)
intfactor_red$educ_chronic_total_scale <- scale(intfactor_red$DEDiffCT, scale = TRUE)
intfactor_red$tx_chronic_total_scale <- scale(intfactor_red$DTDiffCT, scale = TRUE)
intfactor_red$marital_chronic_total_scale <- scale(intfactor_red$DMDiffCT, scale = TRUE)
intfactor_red$finance_chronic_total_scale <- scale(intfactor_red$DFDiffCT, scale = TRUE)
intfactor_red$legal_chronic_total_scale <- scale(intfactor_red$DLDiffCT, scale = TRUE)
intfactor_red$other_rel_chronic_total_scale <- scale(intfactor_red$DODiffCT, scale = TRUE) 
intfactor_red$death_chronic_total_scale <- scale(intfactor_red$DDDiffCT, scale = TRUE)
intfactor_red$parent_chronic_total_scale <- scale(intfactor_red$DGDiffCT, scale = TRUE)
intfactor_red$inter_loss_chronic_total_scale <- scale(intfactor_red$CIDiffCT, scale = TRUE)
intfactor_red$danger_chronic_total_scale <- scale(intfactor_red$CDDiffCT, scale = TRUE)
intfactor_red$humiliate_chronic_total_scale <- scale(intfactor_red$CHDiffCT, scale = TRUE)
intfactor_red$entrap_chronic_total_scale <- scale(intfactor_red$CEDiffCT, scale = TRUE)
intfactor_red$role_change_chronic_total_scale <- scale(intfactor_red$CRDiffCT, scale = TRUE)

#Regression of Baseline Left Temporal Pole CT Predicting 1-Year Change in Parent-Reported INT as Moderated by Chronic LES
model_ltpole_chronic_stress <- lm(par_int3_scale ~ gender + age + par_int1_scale + lh_temporalpole_thickness_scale + 
                                    chronic_total_scale + lh_temporalpole_thickness_scale*chronic_total_scale, data=intfactor_red)
summary(model_ltpole_chronic_stress)

#Simple slopes analysis
sim_slopes(model_ltpole_chronic_stress, pred=lh_temporalpole_thickness_scale, modx=chronic_total_scale, modx.values=c(-1,0,1), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = intfactor_red)

#Plot interaction effect
interact_plot(model_ltpole_chronic_stress, pred = lh_temporalpole_thickness_scale, modx = chronic_total_scale, #plot.points = TRUE, 
              modx.values=c(-1,0,1),
              modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
              legend.main = "Total Chronic Stress",
              x.label = "Left Temporal Pole CT", y.label = "1-Year Change in Parent-Reported INT")

ggsave("figure_ltemppole_chronic_stress_parint_1yrchange_nopts.tiff", dpi=600)
dev.off()

#Regression of Baseline Right Temporal Pole CT Predicting 1-Year Change in Parent-Reported INT as Moderated by Chronic LES
model_rtpole_chronic_stress <- lm(par_int3_scale ~ gender + age + par_int1_scale + rh_temporalpole_thickness_scale + 
                                    chronic_total_scale + rh_temporalpole_thickness_scale*chronic_total_scale, data=intfactor_red)
summary(model_rtpole_chronic_stress)

#Simple slopes analysis
sim_slopes(model_rtpole_chronic_stress, pred=rh_temporalpole_thickness_scale, modx=chronic_total_scale, modx.values=c(-1,0,1), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = intfactor_red)

#Plot interaction effect
interact_plot(model_rtpole_chronic_stress, pred = rh_temporalpole_thickness_scale, modx = chronic_total_scale, #plot.points = TRUE, 
              modx.values=c(-1,0,1),
              modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
              legend.main = "Total Chronic Stress",
              x.label = "Right Temporal Pole CT", y.label = "1-Year Change in Parent-Reported INT")

ggsave("figure_rtemppole_chronic_stress_parint_1yrchange_nopts.tiff", dpi=600)
dev.off()

#Regression of Baseline Left Insula CT Predicting 1-Year Change in Parent-Reported INT as Moderated by Chronic LES
model_linsula_chronic_stress <- lm(par_int3_scale ~ gender + age + par_int1_scale + lh_insula_thickness_scale + 
                                     chronic_total_scale + lh_insula_thickness_scale*chronic_total_scale, data=intfactor_red)
summary(model_linsula_chronic_stress)

#Simple slopes analysis
sim_slopes(model_rtpole_chronic_stress, pred=rh_temporalpole_thickness_scale, modx=chronic_total_scale, modx.values=c(-1,0,1), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = intfactor_red)

#Plot interaction effect
interact_plot(model_linsula_chronic_stress, pred = lh_insula_thickness_scale, modx = chronic_total_scale, #plot.points = TRUE, 
              modx.values=c(-1,0,1),
              modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
              legend.main = "Total Chronic Stress",
              x.label = "Left Insula CT", y.label = "1-Year Change in Parent-Reported INT")
#main.title = "Std. B=-0.124, 95% CI [-0.202, -0.046], p=0.002")

ggsave("figure_linsula_chronic_stress_parint_1yrchange_nopts.tiff", dpi=600)
dev.off()

##Interactions with Different Types of Chronic LES
#Regression of Baseline Left Temporal Pole CT Predicting 1-Year Change in Parent-Reported INT as Moderated by Different Types of Chronic LES
ltpole_int_1yrchange_par_stress <- data.frame(x=c("beta","se","pval"))
for (i in 1067:1080){ # columns are chronic LES variables
  beta13 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + lh_temporalpole_thickness_scale + intfactor_red[[i]] + 
                        scale(lh_temporalpole_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,1]
  se13 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + lh_temporalpole_thickness_scale + intfactor_red[[i]] + 
                      scale(lh_temporalpole_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,2]
  pval13 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + lh_temporalpole_thickness_scale + intfactor_red[[i]] + 
                        scale(lh_temporalpole_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,4]
  
  stats_ltpole_int_1yrchange_par_stress <- c(beta13, se13, pval13)
  ltpole_int_1yrchange_par_stress <- cbind(ltpole_int_1yrchange_par_stress,stats_ltpole_int_1yrchange_par_stress)
  names(ltpole_int_1yrchange_par_stress)[ncol(ltpole_int_1yrchange_par_stress)] <- names(intfactor_red)[i]
}
ltpole_int_1yrchange_par_stress
write.csv(ltpole_int_1yrchange_par_scalar_stress, "Left_TemporalPole_Thickness_1yrChange_Par_INT_Analysis_Stress.csv") #write csv file

#FDR correction
pval_ltpole_int_1yrchange_par_stress <- dplyr::slice(ltpole_int_1yrchange_par_stress, 3)
pval_ltpole_int_1yrchange_par_stress = subset(pval_ltpole_int_1yrchange_par_stress, select = -c(x))
pval_fdr_ltpole_int_1yrchange_par_stress <- transform(pval_ltpole_int_1yrchange_par_stress, 
                                                      adj.p = p.adjust(as.matrix(pval_ltpole_int_1yrchange_par_stress),method = "BH"))
sum(pval_fdr_ltpole_int_1yrchange_par_stress$adj.p<0.05)
write.csv(pval_fdr_ltpole_int_1yrchange_par_stress, "FDR adjusted pvalues_Left_TemporalPole_Thickness_1yrChange_Par_INT_Analysis_Stress.csv")

#Regression of Baseline Right Temporal Pole CT Predicting 1-Year Change in Parent-Reported INT as Moderated by Different Types of Chronic LES
rtpole_int_1yrchange_par_stress <- data.frame(x=c("beta","se","pval"))
for (i in 1067:1080){ # columns are chronic LES variables
  beta14 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + rh_temporalpole_thickness_scale + intfactor_red[[i]] + 
                        scale(rh_temporalpole_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,1]
  se14 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + rh_temporalpole_thickness_scale + intfactor_red[[i]] + 
                      scale(rh_temporalpole_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,2]
  pval14 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + rh_temporalpole_thickness_scale + intfactor_red[[i]] + 
                        scale(rh_temporalpole_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,4]
  
  stats_rtpole_int_1yrchange_par_stress <- c(beta14, se14, pval14)
  rtpole_int_1yrchange_par_stress <- cbind(rtpole_int_1yrchange_par_stress,stats_rtpole_int_1yrchange_par_stress)
  names(rtpole_int_1yrchange_par_stress)[ncol(rtpole_int_1yrchange_par_stress)] <- names(intfactor_red)[i]
}
rtpole_int_1yrchange_par_stress
write.csv(rtpole_int_1yrchange_par_scalar_stress, "Right_TemporalPole_Thickness_1yrChange_Par_INT_Analysis_Stress.csv") #write csv file

#FDR correction
pval_rtpole_int_1yrchange_par_stress <- dplyr::slice(rtpole_int_1yrchange_par_stress, 3)
pval_rtpole_int_1yrchange_par_stress = subset(pval_rtpole_int_1yrchange_par_stress, select = -c(x))
pval_fdr_rtpole_int_1yrchange_par_stress <- transform(pval_rtpole_int_1yrchange_par_stress, 
                                                      adj.p = p.adjust(as.matrix(pval_rtpole_int_1yrchange_par_stress),method = "BH"))
sum(pval_fdr_rtpole_int_1yrchange_par_stress$adj.p<0.05)
write.csv(pval_fdr_rtpole_int_1yrchange_par_stress, "FDR adjusted pvalues_Right_TemporalPole_Thickness_1yrChange_Par_INT_Analysis_Stress.csv")

#Regression of Baseline Left Insula CT Predicting 1-Year Change in Parent-Reported INT as Moderated by Different Types of Chronic LES
linsula_int_1yrchange_par_stress <- data.frame(x=c("beta","se","pval"))
for (i in 1067:1080){ # columns are chronic LES variables
  beta15 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + lh_insula_thickness_scale + intfactor_red[[i]] + 
                         scale(lh_insula_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,1]
  se15 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + lh_insula_thickness_scale + intfactor_red[[i]] + 
                       scale(lh_insula_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,2]
  pval15 <- summary(lm(par_int3_scale ~ gender + age + par_int1_scale + lh_insula_thickness_scale + intfactor_red[[i]] + 
                         scale(lh_insula_thickness_scale*intfactor_red[[i]]), data=intfactor_red))$coefficients[7,4]
  
  stats_linsula_int_1yrchange_par_stress <- c(beta15, se15, pval15)
  linsula_int_1yrchange_par_stress <- cbind(linsula_int_1yrchange_par_stress,stats_linsula_int_1yrchange_par_stress)
  names(linsula_int_1yrchange_par_stress)[ncol(linsula_int_1yrchange_par_stress)] <- names(intfactor_red)[i]
}
linsula_int_1yrchange_par_stress
write.csv(linsula_int_1yrchange_par_scalar_stress, "Left_insula_Thickness_1yrChange_Par_INT_Analysis_Stress.csv") #write csv file

#FDR correction
pval_linsula_int_1yrchange_par_stress <- dplyr::slice(linsula_int_1yrchange_par_stress, 3)
pval_linsula_int_1yrchange_par_stress = subset(pval_linsula_int_1yrchange_par_stress, select = -c(x))
pval_fdr_linsula_int_1yrchange_par_stress <- transform(pval_linsula_int_1yrchange_par_stress, 
                                                      adj.p = p.adjust(as.matrix(pval_linsula_int_1yrchange_par_stress),method = "BH"))
sum(pval_fdr_linsula_int_1yrchange_par_stress$adj.p<0.05)
write.csv(pval_fdr_linsula_int_1yrchange_par_stress, "FDR adjusted pvalues_Left_Insula_Thickness_1yrChange_Par_INT_Analysis_Stress.csv")

##Figures

#Note: Figure 1 was created in ppt
#Figure 2A
adolint_1yr_results = data.frame(cbind(region=c("bankssts","caudal anterior cingulate","caudal middle frontal","cuneus","entorhinal",
                                                "fusiform","inferior parietal","inferior temporal","isthmus cingulate","lateral occipital",
                                                "lateral orbitofrontal","lingual","medial orbitofrontal","middle temporal","parahippocampal",
                                                "paracentral","pars opercularis","pars orbitalis","pars triangularis","pericalcarine",
                                                "postcentral","posterior cingulate","precentral","precuneus","rostral anterior cingulate",
                                                "rostral middle frontal","superior frontal","superior parietal","superior temporal",
                                                "supramarginal","frontal pole","temporal pole","transverse temporal","insula",
                                                "bankssts","caudal anterior cingulate","caudal middle frontal","cuneus","entorhinal",
                                                "fusiform","inferior parietal","inferior temporal","isthmus cingulate","lateral occipital",
                                                "lateral orbitofrontal","lingual","medial orbitofrontal","middle temporal","parahippocampal",
                                                "paracentral","pars opercularis","pars orbitalis","pars triangularis","pericalcarine",
                                                "postcentral","posterior cingulate","precentral","precuneus","rostral anterior cingulate",
                                                "rostral middle frontal","superior frontal","superior parietal","superior temporal",
                                                "supramarginal","frontal pole","temporal pole","transverse temporal","insula"),
                                       stdb=c(-0.01176693,0.05556539,-0.02973578,0.01715213,-0.003159242,-0.02960641,-0.05373482,-0.03023878,
                                              0.003860748,-0.02744513,0.03400102,0.01985518,-0.01705320,-0.03633844,0.03000084,0.02966995,
                                              -0.03175868,0.04491403,-0.02009942,0.01595009,-0.03373727,-0.01331125,0.02202431,-0.01562249,
                                              0.07780471,-0.02850027,0.01077964,-0.04822065,0.002559313,-0.02191617,-0.07389820,-0.08042282,
                                              -0.02430290,-0.08402832,
                                              -0.03856256,-0.08725358,-0.02620080,-0.01575795,-0.03227835,-0.07215199,-0.08806334,-0.05476943,
                                              -0.05873511,-0.06727597,0.03280339,-0.04104897,0.02197502,-0.08311601,0.03170812,-0.08292729,
                                              -0.03359439,-0.06124469,-0.008454136,-0.06579591,-0.05210971,-0.06875512,-0.02999253,-0.02782491,
                                              0.01293170,-0.01954166,-0.04230416,-0.02729491,-0.03715187,-0.06645073,-0.006925439,-0.11796406,
                                              -0.04420539,-0.07836220),
                                       hemi=c("left","left","left","left","left","left","left","left","left","left","left","left","left","left",
                                              "left","left","left","left","left","left","left","left","left","left","left","left","left","left",
                                              "left","left","left","left","left","left","right","right","right","right","right","right","right",
                                              "right","right","right","right","right","right","right","right","right","right","right","right","right",
                                              "right","right","right","right","right","right","right","right","right","right","right","right","right",
                                              "right")),
                                 stringsAsFactors=F)

adolint_1yr_results %>% 
  ggseg(mapping=aes(fill=as.numeric(stdb)), position="stacked", 
        colour="black",size=.1) + theme_classic() +
  scale_fill_gradient(limits=c(-0.17, 0.10), low = "yellow",
                      high = "red") +
  labs(fill = "Std. B") + guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle("1-Year Change in Adolescent-Reported INT") 

ggsave("figure_adolint_1yrchange_classic.tiff", dpi=600)
dev.off()

parint_1yr_results = data.frame(cbind(region=c("bankssts","caudal anterior cingulate","caudal middle frontal","cuneus","entorhinal",
                                               "fusiform","inferior parietal","inferior temporal","isthmus cingulate","lateral occipital",
                                               "lateral orbitofrontal","lingual","medial orbitofrontal","middle temporal","parahippocampal",
                                               "paracentral","pars opercularis","pars orbitalis","pars triangularis","pericalcarine",
                                               "postcentral","posterior cingulate","precentral","precuneus","rostral anterior cingulate",
                                               "rostral middle frontal","superior frontal","superior parietal","superior temporal",
                                               "supramarginal","frontal pole","temporal pole","transverse temporal","insula",
                                               "bankssts","caudal anterior cingulate","caudal middle frontal","cuneus","entorhinal",
                                               "fusiform","inferior parietal","inferior temporal","isthmus cingulate","lateral occipital",
                                               "lateral orbitofrontal","lingual","medial orbitofrontal","middle temporal","parahippocampal",
                                               "paracentral","pars opercularis","pars orbitalis","pars triangularis","pericalcarine",
                                               "postcentral","posterior cingulate","precentral","precuneus","rostral anterior cingulate",
                                               "rostral middle frontal","superior frontal","superior parietal","superior temporal",
                                               "supramarginal","frontal pole","temporal pole","transverse temporal","insula"),
                                      stdb=c(-0.009145275,0.03054709,-0.05736047,-0.02476922,0.01623436,-0.04874999,-0.01266469,-0.06930724,
                                             -0.01526762,-0.002327941,0.03504321,-0.003438199,-0.02455862,-0.001404969,-0.002173507,0.004893845,
                                             -0.08252111,-0.02543233,-0.04865577,0.01825710,-0.05104101,0.02120110,-0.03441587,-0.01057619,
                                             0.08912677,-0.05113568,-0.03917462,-0.04567782,-0.06623771,-0.06058244,-0.07944093,-0.1606610569,
                                             -0.03582645,-0.1658142836,
                                             -0.04566405,-0.006751807,0.02478571,-0.02899546,-0.03855572,-0.05870650,-0.01629289,-0.0544577,
                                             0.008218973,-0.009215716,-0.03510163,-0.03356963,0.004279815,-0.07306539,0.05779021,-0.10130549,
                                             -0.02148060,-0.03920716,-0.01999748,-0.02988510,-0.04930760,-0.01895376,-0.04109939,-0.0008455455,
                                             -0.04411341,-0.04060521,-0.03815106,-0.02778063,-0.06891899,-0.05309892,0.01982611,-0.1452740354,
                                             -0.06673085,-0.07127348),
                                      hemi=c("left","left","left","left","left","left","left","left","left","left","left","left","left","left",
                                             "left","left","left","left","left","left","left","left","left","left","left","left","left","left",
                                             "left","left","left","left","left","left","right","right","right","right","right","right","right",
                                             "right","right","right","right","right","right","right","right","right","right","right","right","right",
                                             "right","right","right","right","right","right","right","right","right","right","right","right","right",
                                             "right")),
                                stringsAsFactors=F)

parint_1yr_results %>% 
  ggseg(mapping=aes(fill=as.numeric(stdb)), position="stacked", 
        colour="black",size=.1) + theme_classic() +
  scale_fill_gradient(limits=c(-0.17, 0.10), low = "yellow",
                      high = "red") +
  labs(fill = "Std. B") + guides(fill = guide_colorbar(ticks = FALSE)) +
  ggtitle("1-Year Change in Parent-Reported INT") 

ggsave("figure_parint_1yrchange_classic.tiff", dpi=600)
dev.off()

#Figure 2B
patch1 <- ggplot(model_lh_temppole_1yrchange, aes(x=lh_temporalpole_thickness_scale, y=par_int3_scale)) + 
  xlab("Left Temporal Pole CT") +
  ylab("1-Year Change in 
       Parent-Reported INT") +
  geom_point()+
  geom_smooth(method=lm) +
  theme_classic() 

patch2 <- ggplot(model_rh_temppole_1yrchange, aes(x=rh_temporalpole_thickness_scale, y=par_int3_scale)) + 
  xlab("Right Temporal Pole CT") +
  ylab("1-Year Change in 
       Parent-Reported INT") +
  geom_point()+
  geom_smooth(method=lm) +
  theme_classic() 

patch3 <- ggplot(model_lh_insula_1yrchange, aes(x=lh_insula_thickness_scale, y=par_int3_scale)) + 
  xlab("Left Insula CT") +
  ylab("1-Year Change in 
       Parent-Reported INT") +
  geom_point()+
  geom_smooth(method=lm) +
  theme_classic() 

patch1 / patch2 / patch3

ggsave("figure_parint_1yrchange_scatterplots.tiff", dpi=600)
dev.off()

#Figure 3A
interact_plot(model_ltpole_chronic_stress, pred = lh_temporalpole_thickness_scale, modx = chronic_total_scale, #plot.points = TRUE, 
              modx.values=c(-1,0,1),
              modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
              legend.main = "Total Chronic LES",
              x.label = "Left Temporal Pole CT", y.label = "1-Year Change in Parent-Reported INT")
ggsave("figure_ltemppole_chronic_stress_parint_1yrchange_nopts.tiff", dpi=600)
dev.off()

#Figure 3B
int_data <- data.frame(Parcel=rep(c("Left Temporal Pole", "Right Temporal Pole", "Left Insula"),each=14),
                       stress=rep(c("Housing", "Education", "Treatment/Health", "Marital/Partner", "Financial","Legal/Crime","Other Relationships",
                                    "Parent/Guardian","Death","Interpersonal Loss","Physical Danger","Humiliation","Entrapment","Role Change/Disruption"),3),
                       beta=c(-0.126625857,-0.07388508,-0.04913949,-0.04189460,-0.1549618722,-0.09443519,-0.10764024,
                              -0.08800623,0.01221941,-0.09586548,-0.110636657,-0.08783461,-0.112655516,-0.10017984,
                              -0.121970649,-0.05680808,-0.007558997,-0.001272289,-0.02082826,-0.07629157,-0.139387841,
                              -0.04269630,0.08740576,-0.02713091,-0.126077540,-0.07496086,-0.08740610,-0.04111529,
                              -0.1440369966,-0.118434625,-0.04366198,0.01493252,-0.06145363,-0.03322662,-0.05333344,
                              0.006942765,-0.01238040,-0.02850090,-0.10756291,-0.07013010,-0.08446415,-0.06556843))

ggplot(int_data, aes(x=stress, y=beta, fill=Parcel)) + 
  geom_bar(stat="identity", colour="black", width=0.75, position=position_dodge()) + coord_flip() + theme_classic() + theme(legend.position="top") +
  xlab("Chronic Stressor Type") + ylab("Standardized Beta") +
  scale_fill_manual(values=c("white","grey","black")) +
  scale_x_discrete(limits=c("Role Change/Disruption","Entrapment","Humiliation","Physical Danger","Interpersonal Loss","Death","Parent/Guardian","Other Relationships",
                            "Legal/Crime","Financial","Marital/Partner","Treatment/Health", "Education", "Housing"))
ggsave("figure_barchart_stress_CT_interactions_parint_final.tiff", dpi=600)
dev.off()

#Supplemental Figure 1
interact_plot(model_rtpole_chronic_stress, pred = rh_temporalpole_thickness_scale, modx = chronic_total_scale, #plot.points = TRUE, 
              modx.values=c(-1,0,1),
              modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
              legend.main = "Total Chronic LES",
              x.label = "Right Temporal Pole CT", y.label = "1-Year Change in Parent-Reported INT")
ggsave("figure_rtemppole_chronic_stress_parint_1yrchange_nopts.tiff", dpi=600)
dev.off()

interact_plot(model_linsula_chronic_stress, pred = lh_insula_thickness_scale, modx = chronic_total_scale, #plot.points = TRUE, 
              modx.values=c(-1,0,1),
              modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
              legend.main = "Total Chronic LES",
              x.label = "Left Insula CT", y.label = "1-Year Change in Parent-Reported INT")
ggsave("figure_linsula_chronic_stress_parint_1yrchange_nopts.tiff", dpi=600)
dev.off()
