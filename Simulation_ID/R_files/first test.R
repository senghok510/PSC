
# Simulation to Support Nakagawa et al. A practical and readily implementable method for handling missing standard deviation in the meta-analysis of response ratios.

# This script runs the simulation for the MLMA (i.e. multilevel meta-analysis). It is designed to run as an array job on the HPC and Sydney UNI. There are only 10 reps in here, but I ran on an array 0-999

# Clean up the R Environment
rm(list=ls())

# Incoming arguments from bash
args<-commandArgs()

# Where are we working
directory<-"/Users/senghok/Documents/R/Simulation_ID/R_files"
setwd(directory)

# Load the relevant libraries, and header
library(metafor)
library(plyr)
source("Header_ID_CV.R")

# Get the incoming info on the PBS array index
index<-args[6]

# In this simulation I will include non-independence. The the analysis will be MLMA.

###################################################
################## Parameters #####################
###################################################

# Specify the overall effect magnitude - lets assume a lnRR of 0.3 - this is effectively a 35% increase in the mean
lnRR<-0.3

# Specify the number of effect sizes per study as 3 and 2.4 - based on Macdonald et al. I checked and the distributions look pretty good
k_effect_mu<-3
k_effect_sd<-2.4

proportion_lost<- 0.25

# Note I assume the mean in the control group is 100 - to give +ve means for lnRR
mu_control<-100


n_reps<-2
# But I think we need to test a few degrees of heterogeneity in the variation
sd_study_mu <- mu_control*0.15
sd_study_sd<-sd_study_mu * 0.5

# Set the simplified parameter values 
k_study <- 5 # Number of studies 
k_effect_mu <- 2 # Average number of effects per study 
k_effect_sd <- 0.5 # Standard deviation of effects per study 
tau2 <- 0.001 # Total heterogeneity 
icc_study <- 0.1 # Proportion of heterogeneity at the studylevel 
n_study_mu <- 10 # Mean size of studies 
n_study_sd <- 3 # Standard deviation of the size of studies


# Get all the parameter combinations to run
parameters<-expand.grid(lnRR=lnRR, k_effect_mu=k_effect_mu, k_effect_sd=k_effect_sd, tau2=tau2, icc_study=icc_study, 
                        mu_control=mu_control, n_study_mu=n_study_mu, n_study_sd=n_study_sd, sd_study_mu=sd_study_mu, sd_study_sd=sd_study_sd, k_study=k_study, proportion_lost=proportion_lost)

# Remove the parameters with the combinations of low sd n and high mean sd n etc
# drop<-c(which(parameters$n_study_mu == 10& parameters$n_study_sd == 3))
# parameters<-parameters[-drop,]
rownames(parameters)<-seq(nrow(parameters))

# Create a template to hold the simulation results. For each of the replications, we will perform the analysis by the three methods and record the est, the SE and the tau, then compare to above
template<-data.frame(whole_ests_ID=rep(NA, n_reps), whole_SE_ID=NA, whole_Tau2_ID=NA, whole_ICC_ID=NA, m1.1_ests_ID=NA, m1.1_SE_ID=NA, m1.1_Tau2_ID=NA, m1.1_ICC_ID=NA, m1.2_ests_ID=NA, m1.2_SE_ID=NA, m1.2_Tau2_ID=NA, m1.2_ICC_ID=NA, m2_ests_ID=NA, m2_SE_ID=NA, m2_Tau2_ID=NA, m2_ICC_ID=NA, m3_ests_ID=NA, m3_SE_ID=NA, m3_Tau2_ID=NA, m3_ICC_ID=NA)

# Output list to fill in there will be an object for each set of parameters in the batch - that object will be the filled in template
results<-list()

# Loop for the each parameter set
for(p in 1:nrow(parameters)){
  
  # Get the pth set of parameters
  parameters_p<-parameters[p,]
  
  
  # Create a copy of the template to fill in for this set of parameters
  template_p<-template
  
  # We will do it reps times, each time deleting n numbers_lost of the SDs and reanalysing by the 3 methods
  for(n in 1:n_reps){
    
    # Keep doing until we get the whole nth row of template_p filled in - repeats simulation in the event that any model has convergence issues
    while(sum(is.na(template_p[n,])) > 0){
      
      # Create an nth data set
      data_n<-sim_data(lnRR=parameters_p$lnRR, k_study=parameters_p$k_study, k_effect_mu=parameters_p$k_effect_mu, k_effect_sd=parameters_p$k_effect_sd, tau2=parameters_p$tau2, icc_study=parameters_p$icc_study, n_study_mu=parameters_p$n_study_mu, n_study_sd=parameters_p$n_study_sd, sd_study_mu=parameters_p$sd_study_mu, sd_study_sd=parameters_p$sd_study_sd)
      
      
      data_n_CV<- data_n
      data_n_ID <- data_n
      
      # Calculate the CVs and effect sizes for the complete data and analyse
      data_n_CV$Control.CV<-data_n_CV$Control.SD / data_n_CV$Control.Mean
      data_n_CV$Treatment.CV<-data_n_CV$Treatment.SD / data_n_CV$Treatment.Mean
      data_n_CV<-cbind(data_n_CV, my_lnRR_CV(Control.Mean=data_n_CV$Control.Mean, Treatment.Mean=data_n_CV$Treatment.Mean, Control.CV=data_n_CV$Control.CV, Treatment.CV=data_n_CV$Treatment.CV,
                                             Control.n=data_n_CV$Control.n, Treatment.n=data_n_CV$Treatment.n))


      
      # Calculate the IDs and effect sizes for the complete data and analyse
      data_n_ID$Control.ID<-(data_n_ID$Control.SD)**2 / data_n_ID$Control.Mean
      data_n_ID$Treatment.ID<-(data_n_ID$Treatment.SD)**2 / data_n_ID$Treatment.Mean
      data_n_ID<-cbind(data_n_ID, my_lnRR_ID(Control.Mean=data_n_ID$Control.Mean, Treatment.Mean=data_n_ID$Treatment.Mean, 
                                             Control.ID=data_n_ID$Control.ID, Treatment.ID=data_n_ID$Treatment.ID, 
                                             Control.n=data_n_ID$Control.n, Treatment.n=data_n_ID$Treatment.n))
      
      
      # Note du groupe PSC : Full data method with CV
      
      # Fit the model to the whole data 
      # model_w_CV<-try(rma.mv(yi = yi_CV, V = vi_CV, random=list(~1|Study, ~1|Effect), data=data_n_CV), silent=T)
      # 
      # # Save the results if the model fitted
      # if(class(model_w_CV)[1] == "rma.mv"){
      #   template_p$whole_ests_CV[n]<-model_w_CV$b
      #   template_p$whole_SE_CV[n]<-model_w_CV$se
      #   template_p$whole_Tau2_CV[n]<-sum(model_w_CV$sigma2[c(1:2)])
      #   template_p$whole_ICC_CV[n]<-model_w_CV$sigma2[1] / sum(model_w_CV$sigma2[c(1:2)])
      # }
      
      # Note du groupe PSC : Full data method with ID
      # Full data method
      
      # Fit the model to the whole data 
      model_w_ID<-try(rma.mv(yi = yi_ID, V = vi_ID, random=list(~1|Study, ~1|Effect), data=data_n_ID,control=list(rel.tol=1e-7)), silent=T)
      
      # Save the results if the model fitted
      if(class(model_w_ID)[1] == "rma.mv"){
        template_p$whole_ests_ID[n]<-model_w_ID$b
        template_p$whole_SE_ID[n]<-model_w_ID$se
        template_p$whole_Tau2_ID[n]<-sum(model_w_ID$sigma2[c(1:2)])
        template_p$whole_ICC_ID[n]<-model_w_ID$sigma2[1] / sum(model_w_ID$sigma2[c(1:2)])
      }
      
      # # Find the row IDs you want to have missing SDs and set SD, CV and effect sizes as NA
      # drop<-sample(seq(1, nrow(data_n), 1), round(nrow(data_n_CV) * parameters_p$proportion_lost))
      # data_n_CV$Control.SD[drop]<-NA
      # data_n_CV$Treatment.SD[drop]<-NA
      # data_n_CV$Control.CV[drop]<-NA
      # data_n_CV$Treatment.CV[drop]<-NA
      # data_n_CV$yi_CV[drop]<-NA
      # data_n_CV$vi_CV[drop]<-NA
      
      
      # Find the row IDs you want to have missing SDs and set SD, ID and effect sizes as NA
      drop<-sample(seq(1, nrow(data_n_ID), 1), round(nrow(data_n_ID) * parameters_p$proportion_lost))
      data_n_ID$Control.SD[drop]<-NA
      data_n_ID$Treatment.SD[drop]<-NA
      data_n_ID$Control.ID[drop]<-NA
      data_n_ID$Treatment.ID[drop]<-NA
      data_n_ID$yi_ID[drop]<-NA
      data_n_ID$vi_ID[drop]<-NA
      
      
      
      # # Here we calculate the weighted mean CV from those data available - this is pooled by study first
      # pooled_CV<-ddply(data_n_CV, .(Study), summarise, mean_Control.CV=mean(Control.CV, na.rm=T), 
      #                  mean_Treatment.CV=mean(Treatment.CV, na.rm=T), mean_Control.n=mean(Control.n), mean_Treatment.n=mean(Treatment.n))
      # mcv1<-weighted.mean(pooled_CV$mean_Treatment.CV, pooled_CV$mean_Treatment.n, na.rm=TRUE)
      # mcv2<-weighted.mean(pooled_CV$mean_Control.CV, pooled_CV$mean_Control.n, na.rm=TRUE)
      # 
      # 
      
      # Here we calculate the weighted mean ID from those data available - this is pooled by study first
      pooled_ID<-ddply(data_n_ID, .(Study), summarise, mean_Control.ID=mean(Control.ID, na.rm=T), 
                       mean_Treatment.ID=mean(Treatment.ID, na.rm=T), mean_Control.n=mean(Control.n), mean_Treatment.n=mean(Treatment.n))
      mid1<-weighted.mean(pooled_ID$mean_Treatment.ID, pooled_ID$mean_Treatment.n, na.rm=TRUE)
      mid2<-weighted.mean(pooled_ID$mean_Control.ID, pooled_ID$mean_Control.n, na.rm=TRUE)
      
      
      # Note du groupe PSC: Missing cases
      
      # Method 1.1
      # Use the weighted mean cv where SDs are missing.
      # Then proceed to fit a meta-analysis as normal.
      #Note du groupe PSC : equation 8 & 9 for ID and equation 6 & 7 for CV in our First draft_PSC.pdf document in the cloud
      
      
      # # Now estimate a new yi and vi using the average CV where data are missing
      # data_n_CV$yi_CV[drop]<-log(data_n_CV$Treatment.Mean[drop] / data_n_CV$Control.Mean[drop]) + 0.5 * (mcv1^2 / data_n_CV$Treatment.n[drop] - mcv2^2 / data_n_CV$Control.n[drop])
      # data_n_CV$vi_CV[drop]<-mcv1^2/data_n_CV$Treatment.n[drop] + mcv2^2/data_n_CV$Control.n[drop] + mcv1^4/(2 * data_n_CV$Treatment.n[drop]^2) + mcv2^4/(2 * data_n_CV$Control.n[drop]^2)
      # 
      
      # Now estimate a new yi and vi using the average ID where data are missing
      data_n_ID$yi_ID[drop]<-log(data_n_ID$Treatment.Mean[drop] / data_n_ID$Control.Mean[drop]) + 0.5*((mid1/(data_n_ID$Treatment.Mean[drop]*data_n_ID$Treatment.n[drop])) - (mid2/(data_n_ID$Control.Mean[drop]*data_n_ID$Control.n[drop]))) ###equation 4 ou 6 Nakagawa et al.(2023)
      
      data_n_ID$vi_ID[drop]<-((data_n_ID$Treatment.ID[drop]) /data_n_ID$Treatment.Mean[drop]*data_n_ID$Treatment.n[drop]) + ((data_n_ID$Control.ID[drop]) / data_n_ID$Control.Mean[drop]*data_n_ID$Control.n[drop]) +
        ((data_n_ID$Treatment.ID[drop]/data_n_ID$Treatment.Mean[drop])^2 / (2*data_n_ID$Treatment.n[drop]^2)) + ((data_n_ID$Control.ID[drop]/data_n_ID$Control.Mean[drop])^2 / (2*data_n_ID$Control.n[drop]^2))
      # Fit the model
      
      # model1.1_CV<-try(rma.mv(yi = yi_CV, V = vi_CV, random=list(~1|Study, ~1|Effect), data=data_n_CV), silent=T)
      # 
      # # Save the results if the model fitted
      # if(class(model1.1_CV)[1] == "rma.mv"){
      #   template_p$m1.1_ests_CV[n]<-model1.1_CV$b
      #   template_p$m1.1_SE_CV[n]<-model1.1_CV$se
      #   template_p$m1.1_Tau2_CV[n]<-sum(model1.1_CV$sigma2[c(1:2)])
      #   template_p$m1.1_ICC_CV[n]<-model1.1_CV$sigma2[1] / sum(model1.1_CV$sigma2[c(1:2)])
      # }
      
      model1.1_ID<-try(rma.mv(yi = yi_ID, V = vi_ID, random=list(~1|Study, ~1|Effect), data=data_n_ID), silent=T)
      
      # Save the results if the model fitted
      if(class(model1.1_ID)[1] == "rma.mv"){
        template_p$m1.1_ests_ID[n]<-model1.1_ID$b
        template_p$m1.1_SE_ID[n]<-model1.1_ID$se
        template_p$m1.1_Tau2_ID[n]<-sum(model1.1_ID$sigma2[c(1:2)])
        template_p$m1.1_ICC_ID[n]<-model1.1_ID$sigma2[1] / sum(model1.1_ID$sigma2[c(1:2)])
      }
      
      # Method 1.2
      # Note du groupe PSC: All case
      
      # Use the weighted mean cv everywhere
      # Then proceed to fit a meta-analysis as normal.
      
      # Now estimate a new vi using the average CVs everywhere - note for consistency yi is used throughout
      # data_n_CV$vi_2_CV<-mcv1^2/data_n$Treatment.n + mcv2^2/data_n$Control.n + mcv1^4/(2 * data_n$Treatment.n^2) + mcv2^4/(2 * data_n$Control.n^2)
      # 
      # # Fit the model
      # model1.2_CV<-try(rma.mv(yi = yi_CV, V = vi_2_CV, random=list(~1|Study, ~1|Effect), data=data_n_CV,control=list(rel.tol=1e-8)), silent=T)
      # 
      
      # Now estimate a new vi using the average IDs everywhere - note for consistency yi is used throughout
      data_n_ID$vi_2_ID<-((data_n_ID$Treatment.ID) /data_n_ID$Treatment.Mean*data_n_ID$Treatment.n) + ((data_n_ID$Control.ID) / data_n_ID$Control.Mean*data_n_ID$Control.n) +
        ((data_n_ID$Treatment.ID/data_n_ID$Treatment.Mean)^2 / (2*data_n_ID$Treatment.n^2)) + ((data_n_ID$Control.ID/data_n_ID$Control.Mean)^2 / (2*data_n_ID$Control.n^2))
      # Fit the model
      model1.2_ID<-try(rma.mv(yi = yi_ID, V = vi_2_ID, random=list(~1|Study, ~1|Effect), data=data_n_ID), silent=T)
      
      
      # # Save the results CV if the model fitted
      # if(class(model1.2_CV)[1] == "rma.mv"){
      #   template_p$m1.2_ests_CV[n]<-model1.2_CV$b
      #   template_p$m1.2_SE_CV[n]<-model1.2_CV$se
      #   template_p$m1.2_Tau2_CV[n]<-sum(model1.2_CV$sigma2[c(1:2)])
      #   template_p$m1.2_ICC_CV[n]<-model1.2_CV$sigma2[1] / sum(model1.2_CV$sigma2[c(1:2)])
      # }
      
      
      # Save the results ID if the model fitted
      if(class(model1.2_ID)[1] == "rma.mv"){
        template_p$m1.2_ests_ID[n]<-model1.2_ID$b
        template_p$m1.2_SE_ID[n]<-model1.2_ID$se
        template_p$m1.2_Tau2_ID[n]<-sum(model1.2_ID$sigma2[c(1:2)])
        template_p$m1.2_ICC_ID[n]<-model1.2_ID$sigma2[1] / sum(model1.2_ID$sigma2[c(1:2)])
      }
      
      # Method 2
      # Note du groupe PSC : Multiplicative method 
      # Now we fit a weighted regression(ish) rather than a meta-analysis in metafor
      
      # Create a matrix with v_tilda in the diagonal (note this is NOT the mix v_tilda and v_i)
      # Note du groupe PSC : equation 7 to calculate effect-size's variance for matrix coefficients
      # Vf_CV<-diag(mcv1^2/data_n_CV$Treatment.n + mcv2^2/data_n_CV$Control.n + mcv1^4/(2 * data_n_CV$Treatment.n^2) + mcv2^4/(2 * data_n_CV$Control.n^2))
      # row.names(Vf_CV)<-data_n_CV$Effect
      # colnames(Vf_CV)<-data_n_CV$Effect
      # 
      # 
      # # Also apparently need to copy the IDs across to second predictor
      # data_n_CV$ID2<-data_n_CV$Effect
      # 
      # Note du groupe PSC : equation 9 to calculate effect-size's variance for matrix coefficients
      # Create a matrix with v_tilda in the diagonal (note this is NOT the mix v_tilda and v_i)
   
      Vf_ID <- diag(mid1/(data_n_ID$Treatment.n * data_n_ID$Treatment.Mean)+ mid2/(data_n_ID$Control.n * data_n_ID$Control.Mean) + mid1^2/(2 * data_n_ID$Treatment.n^2 * data_n_ID$Treatment.Mean^2) + mid2^2/(2 * data_n_ID$Control.n^2 * data_n_ID$Control.Mean^2))
      row.names(Vf_ID)<-data_n_ID$Effect
      colnames(Vf_ID)<-data_n_ID$Effect
      
      # Also apparently need to copy the IDs across to second predictor
      data_n_ID$ID2<-data_n_ID$Effect
      
      # Now fit a weighted regression for CV methods
      # model2_CV<-try(rma.mv(yi = yi_CV, V = 0, random=list(~1|Study, ~1|Effect, ~1|ID2), data=data_n_CV,control=list(rel.tol=1e-8), R=list(ID2=Vf_CV), Rscale=F), silent=T)
      # 
      # # Save the results if the model fitted
      # if(class(model2_CV)[1] == "rma.mv"){
      #   template_p$m2_ests_CV[n]<-model2_CV$b
      #   template_p$m2_SE_CV[n]<-model2_CV$se
      #   template_p$m2_Tau2_CV[n]<-sum(model2_CV$sigma2[c(1:2)])
      #   template_p$m2_ICC_CV[n]<-model2_CV$sigma2[1] / sum(model2_CV$sigma2[c(1:2)])
      # }
      # 
      # 
      # Now fit a weighted regression for ID methods
      model2_ID<-try(rma.mv(yi = yi_ID, V = 0, random=list(~1|Study, ~1|Effect, ~1|ID2), data=data_n_ID,control=list(rel.tol=1e-7), R=list(ID2=Vf_ID), Rscale=F), silent=T)
      
      # Save the results if the model fitted
      if(class(model2_ID)[1] == "rma.mv"){
        template_p$m2_ests_ID[n]<-model2_ID$b
        template_p$m2_SE_ID[n]<-model2_ID$se
        template_p$m2_Tau2_ID[n]<-sum(model2_ID$sigma2[c(1:2)])
        template_p$m2_ICC_ID[n]<-model2_ID$sigma2[1] / sum(model2_ID$sigma2[c(1:2)])
      }
      
      # Method 3
      # Note du groupe PSC : Hybride method 
      # OK now to combine the methods - we set vi to 0 for missing SDs, create a new V matrix of 0s where data are known and the vi from the mean CV for missings, then fit together in one model
      
      # Switch the inverse n in the matrix with the vi - note this is v.tilda where SDs are missing and vi where they are not, as per method 1
      # diag(Vf_CV)<-data_n_CV$vi_CV
      # 
      # # Add 0s to V matrix where SDs are known
      # diag(Vf_CV)[-drop]<-0
      # 
      # # Now add 0s back to the vi where SDs are missing to remove v.tilda from where SD is missing
      # data_n_CV$vi_CV[drop]<-0
      # 
      # 
      
      # Switch the inverse n in the matrix with the vi - note this is v.tilda where SDs are missing and vi where they are not, as per method 1
      diag(Vf_ID)<-data_n_ID$vi_ID
      Vf_ID[is.na(Vf_ID)] <- 0
      
      # Add 0s to V matrix where SDs are known
      diag(Vf_ID)[-drop]<-0
      
      # Now add 0s back to the vi where SDs are missing to remove v.tilda from where SD is missing
      data_n_ID$vi_ID[drop]<-0
      
      
      # Try fit the model
      # model3_CV<-try(rma.mv(yi = yi_CV, V = vi_CV, random=list(~1|Study, ~1|Effect, ~1|ID2), data=data_n_CV,control=list(rel.tol=1e-7), R=list(ID2=Vf_CV), Rscale=F), silent=T)
      # 
      # # Save the results if the model fitted
      # if(class(model3_CV)[1] == "rma.mv"){
      #   template_p$m3_ests_CV[n]<-model3_CV$b
      #   template_p$m3_SE_CV[n]<-model3_CV$se
      #   template_p$m3_Tau2_CV[n]<-sum(model3_CV$sigma2[c(1:2)])
      #   template_p$m3_ICC_CV[n]<-model3_CV$sigma2[1] / sum(model3_CV$sigma2[c(1:2)])
      # }
      

      # Try fit the model
      model3_ID<-try(rma.mv(yi = yi_ID, V = vi_ID, random=list(~1|Study, ~1|Effect, ~1|ID2), data=data_n_ID, control=list(rel.tol=1e-8),R=list(ID2=Vf_ID), Rscale=F), silent=T)

      # Save the results if the model fitted
      if(class(model3_ID)[1] == "rma.mv"){
        template_p$m3_ests_ID[n]<-model3_ID$b
        template_p$m3_SE_ID[n]<-model3_ID$se
        template_p$m3_Tau2_ID[n]<-sum(model3_ID$sigma2[c(1:2)])
        template_p$m3_ICC_ID[n]<-model3_ID$sigma2[1] / sum(model3_ID$sigma2[c(1:2)])
      }
      
    }# Closing the while loop
    
  }# Closing the reps
  
  # Save the results from this n_reps set of parameters
  results[[p]]<-template_p
  
}# Closing the parameters loop

# Save the results from the simulation
save(results, file=paste0("/Users/senghok/Documents/R/Simulation_ID/MLMA_RData/Simulation_MLMA_CV_ID", 1, ".Rdata"))

save(parameters, file="/Users/senghok/Documents/R/Simulation_ID/MLMA_RData/Parameters_MLMA_CV_ID.Rdata")


