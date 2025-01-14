
# Simulation to Support Nakagawa et al. A practal and readily implementable method for handling missing standard deviation in the meta-analysis of response ratios.

# This script was used to aggregate multiple instances from the array job on the HPC. It also then calculates the bias, coverage and aggregate statsitics used in the plotting

# Clean up the R Environment
rm(list=ls())

# Set the working directory
wd<-"/Users/senghok/Documents/R/Simulation_ID/MLMA_RData" # Note this directory is not in the git repo. Contains a massive amount of raw data. Hence this file will not run from the repo.
setwd(wd)

# Load the relevant libraries, and header
library(metafor)
library(plyr)

# Check the files
#files<-dir("R")[-c(1:5)]
files<-dir()

load(paste0(files[2]))

#load(paste0("R/", files[1]))


res_unlist<-results
for(i in 2:length(files)){
	load(paste0(files[i]))
	for(j in 1:length(res_unlist)){
		res_unlist[[j]]<-rbind(res_unlist[[j]], results[[j]])
	}
}
results<-res_unlist

# Load the parameters
load("Parameters_MLMA_CV_ID.Rdata")

# Long format all the results
for(i in 1:length(results)){
	long_i<-cbind(results[[i]], parameters[i,])
	if(i == 1){
		long<-long_i
	}else{
		long<-rbind(long, long_i)
	}
}
long_mat<-as.matrix(long)

# # Reformat from long to wide for the methods
# long_full_CV<-as.data.frame(rbind(long_mat[,-c(5:20)], long_mat[,-c(1:4,9:20)], long_mat[,-c(1:8,13:20)], long_mat[,-c(1:12,17:20)], long_mat[,-c(1:16)]))
# long_full_CV$Method_CV<-c(rep("Complete Data CV", dim(long_mat)[1]), rep("Method 1.1 CV", dim(long_mat)[1]), rep("Method 1.2 CV", dim(long_mat)[1]), rep("Method 2 CV", dim(long_mat)[1]), rep("Method 3 CV", dim(long_mat)[1]))

# Reformat from long to wide for the methods
long_full_ID<-as.data.frame(rbind(long_mat[,-c(5:20)], long_mat[,-c(1:4,9:20)], long_mat[,-c(1:8,13:20)], long_mat[,-c(1:12,17:20)], long_mat[,-c(1:16)]))
long_full_ID$Method_ID<-c(rep("Complete Data ID", dim(long_mat)[1]), rep("Method 1.1 ID", dim(long_mat)[1]), rep("Method 1.2 ID", dim(long_mat)[1]), rep("Method 2 ID", dim(long_mat)[1]), rep("Method 3 ID", dim(long_mat)[1]))


# # Calculate the bias and coverage pour CV
# long_full_CV$bias_CV<-long_full_CV$whole_ests_CV - long_full_CV$lnRR
# dfs<-long_full_CV$k_study-1
# tag<-which(long_full_CV$icc_study_CV == 0)
# dfs[tag]<-long_full_CV$k_study[tag] * long_full_CV$k_effect_mu[tag] - 1
# ts<-qt(0.975, df=dfs)
# long_full_CV$coverage_CV<-(abs(long_full_CV$bias_CV) - ts*long_full_CV$whole_SE_CV) <= 0
# long_full_CV$bias_tau2_CV<-long_full_CV$whole_Tau2_CV - long_full_CV$tau2
# long_full_CV$bias_tau2_lnR_CV<-log(long_full_CV$whole_Tau2_CV / long_full_CV$tau2)
# long_full_CV$bias_ICC_CV<-long_full_CV$whole_ICC_CV - long_full_CV$icc_study
# 


# Calculate the bias and coverage pour ID

long_full_ID$bias_ID<-long_full_ID$whole_ests_ID - long_full_ID$lnRR
dfs<-long_full_ID$k_study-1
tag<-which(long_full_ID$icc_study == 0)
dfs[tag]<-long_full_ID$k_study[tag] * long_full_ID$k_effect_mu[tag] - 1
ts<-qt(0.975, df=dfs)
long_full_ID$coverage_ID<-(abs(long_full_ID$bias_ID) - ts*long_full_ID$whole_SE_ID) <= 0
long_full_ID$bias_tau2_ID<-long_full_ID$whole_Tau2_ID - long_full_ID$tau2
long_full_ID$bias_tau2_lnR_ID<-log(long_full_ID$whole_Tau2_ID / long_full_ID$tau2)
long_full_ID$bias_ICC_ID<-long_full_ID$whole_ICC_ID - long_full_ID$icc_study

# # Add a unique code for each parameter set pour CV
# long_full_CV$code_CV<-paste0(long_full_CV[,5], "_", long_full_CV[,6])
# for(i in 7:16){
# 	long_full_CV$code_CV<-paste0(long_full_CV$code_CV, "_", long_full_CV[,i])
# 	print(i)
# }

# Add a unique code for each parameter set pour ID
long_full_ID$code_ID<-paste0(long_full_ID[,5], "_", long_full_ID[,6])
for(i in 7:16){
  long_full_ID$code_ID<-paste0(long_full_ID$code_ID, "_", long_full_ID[,i])
  print(i)
}


# # Save that pour CV
# save(long_full_CV, file="long_MLMA_CV.Rdata")

# Save that
save(long_full_ID, file="long_MLMA_ID.Rdata")



# # Now get the aggregate stats pour CV
# long_full_CV$code2_CV<-paste0(long_full_CV$code_CV, long_full_CV$Method_CV)
# agg_stats_CV<-ddply(long_full_CV, .(code2_CV), summarise, code_CV=code_CV[1], Method_CV=Method[1], mean_bias_CV=mean(bias_CV), 
#                  median_bias_CV=median(bias_CV), min_bias_CV=min(bias_CV), max_bias_CV=max(bias_CV), q10_bias_CV=quantile(bias_CV, 0.1), 
#                  q90_bias_CV=quantile(bias_CV, 0.9), mean_bias_tau2_CV=mean(bias_tau2_CV), median_bias_tau2_CV=median(bias_tau2_CV), 
#                  min_bias_tau2_CV=min(bias_tau2_CV), max_bias_tau2_CV=max(bias_tau2_CV), q10_bias_tau2_CV=quantile(bias_tau2_CV, 0.1), 
#                  q90_bias_tau2_CV=quantile(bias_tau2_CV, 0.9), mean_bias_tau2_lnR_CV=mean(bias_tau2_lnR_CV), median_bias_tau2_lnR_CV=median(bias_tau2_lnR_CV), 
#                  min_bias_tau2_lnR_CV=min(bias_tau2_lnR_CV), max_bias_tau2_lnR_CV=max(bias_tau2_lnR_CV), q10_bias_tau2_lnR_CV=quantile(bias_tau2_lnR_CV, 0.1), 
#                  q90_bias_tau2_lnR_CV=quantile(bias_tau2_lnR_CV, 0.9),  mean_bias_ICC_CV=mean(bias_ICC_CV), median_ICC_CV=median(bias_ICC_CV), 
#                  min_bias_ICC_CV=min(bias_ICC_CV), max_ICC_CV=max(bias_ICC_CV), q10_bias_ICC_CV=quantile(bias_ICC_CV, 0.1), q90_bias_ICC_CV=quantile(bias_ICC_CV, 0.9), 
#                  coverage_CV=mean(coverage_CV))


# Now get the aggregate stats pour ID
long_full_ID$code2_ID<-paste0(long_full_ID$code_ID, long_full_ID$Method_ID)
agg_stats_ID<-ddply(long_full_ID, .(code2_ID), summarise, code_ID=code_ID[1], Method_ID=Method_ID[1], mean_bias_ID=mean(bias_ID), 
                    median_bias_ID=median(bias_ID), min_bias_ID=min(bias_ID), max_bias_ID=max(bias_ID), q10_bias_ID=quantile(bias_ID, 0.1), 
                    q90_bias_ID=quantile(bias_ID, 0.9), mean_bias_tau2_ID=mean(bias_tau2_ID), median_bias_tau2_ID=median(bias_tau2_ID), 
                    min_bias_tau2_ID=min(bias_tau2_ID), max_bias_tau2_ID=max(bias_tau2_ID), q10_bias_tau2_ID=quantile(bias_tau2_ID, 0.1), 
                    q90_bias_tau2_ID=quantile(bias_tau2_ID, 0.9), mean_bias_tau2_lnR_ID=mean(bias_tau2_lnR_ID), median_bias_tau2_lnR_ID=median(bias_tau2_lnR_ID), 
                    min_bias_tau2_lnR_ID=min(bias_tau2_lnR_ID), max_bias_tau2_lnR_ID=max(bias_tau2_lnR_ID), q10_bias_tau2_lnR_ID=quantile(bias_tau2_lnR_ID, 0.1), 
                    q90_bias_tau2_lnR_ID=quantile(bias_tau2_lnR_ID, 0.9),  mean_bias_ICC_ID=mean(bias_ICC_ID), median_ICC_ID=median(bias_ICC_ID), 
                    min_bias_ICC_ID=min(bias_ICC_ID), max_ICC_ID=max(bias_ICC_ID), q10_bias_ICC_ID=quantile(bias_ICC_ID, 0.1), q90_bias_ICC_ID=quantile(bias_ICC_ID, 0.9), 
                    coverage_ID=mean(coverage_ID))

# # Combine with the parameter set by code pour CV
# parameters$code_CV<-paste0(parameters[,1], "_", parameters[,2])
# for(i in 3:12){
# 	parameters$code_CV<-paste0(parameters$code_CV, "_", parameters[,i])
# 	print(i)
# }
# agg_stats_CV<-cbind(agg_stats_CV, parameters[match(agg_stats_CV$code_CV, parameters$code_CV),])
# agg_results_CV<-agg_stats_CV[,-c(1:2)]



# Combine with the parameter set by code pour ID
parameters$code_ID<-paste0(parameters[,1], "_", parameters[,2])
for(i in 3:12){
  parameters$code_ID<-paste0(parameters$code_ID, "_", parameters[,i])
  print(i)
}
agg_stats_ID<-cbind(agg_stats_ID, parameters[match(agg_stats_ID$code_ID, parameters$code_ID),])
agg_results_ID<-agg_stats_ID[,-c(1:2)]

# # Save that to the github repo pour CV
# save(agg_results_CV, file="agg_results_mlma_CV.Rdata")


# Save that to the github repo pour ID
save(agg_results_ID, file="agg_results_mlma_ID.Rdata")


