
# Simulation to Support Nakagawa et al. A practical and readily implementable method for handling missing standard deviation in the meta-analysis of response ratios.

# This script contains 2 functions for the simulation

# First written by AM Senior @ The University of Sydney, 25/06/2021.

############################################
############### sim_data ###################
############################################

# The function is designed to simulate data at a multi-level with study and within-study
# The same function will all for a more standard design by setting k_study as 1, and k_effects the desired nu,ber of studies, and icc_study = 0, giving all the heterogeneity at the effect size level

# lnRR = the overall mean effect size
# k_study = the number of studies

# k_effect_mu = the mean number of effect sizes per study

#Hok: Mean number of effect sizes : ex : L1 , L2 => k_effect_mu = L1+L2/2

# k_effect_sd = the sd in number of effect sizes per study
# tau2 = the total heterogeneity
# icc_study = the proportion (intraclass correlation) of tau2 coming from the study level effect
# n_study_mu = the mean sample size studies in the dataset
#Hok:
# n_study_sd = the sd of sample size of studies in the dataset

# Hok: random sample size n1 ,n2 of studies  => standard deviation = ...
# sd_study_mu is the mean sd of data within effect size for each study
# Hok: for each study : sd_study_1 , sd_study_2: sd_study_mu = sd_study_1+sd_study_2/2

# sd_study_sd is the sd of sd of data within effect size for each study
# return_true_effects allows the user to see the simulated effect at the level of each study and effect size in the outputted data

# NOTE SDs and ns vary at the level of study rather than effect size (or treatment group). In my experiuence this is the most realistic scenario

sim_data<-function(lnRR, k_study, k_effect_mu, k_effect_sd, tau2, icc_study, n_study_mu,
                   n_study_sd, sd_study_mu, sd_study_sd, return_true_effects=F){

	# Load the packages
	require(gamlss.dist)

	# A bit of reparameterization
	# Make sure the various sds are not <= 0: for the gamma and DPO distributions
  #Hok:
	if(sd_study_sd <= 0){
		sd_study_sd<-10^-8
	}
	if(k_effect_sd <= 0){
		k_effect_sd<-10^-8
	}
	if(n_study_sd <= 0){
		n_study_sd<-10^-8
	}

	# Calculate the between and within-study heterogeneity from the tau2 and icc study
	study_sigma2<-tau2 * icc_study #between sd
	resid_sigma2<-tau2 * (1 - icc_study) #within sd

	# For the gamma distirbution for the variance the a (shape) and b (scale) are approximated as

	#Hok: to draw the sd of a study i from the gamma distribution
	#parameters
	a<-sd_study_mu^2 / sd_study_sd^2
	b<-sd_study_sd^2 / sd_study_mu

	# For the double poisson distirbution the sigma is parameterised as - I will specify the mean as n_study_mu - 3, then add 3 to avoid 0 sample sizes - same for k_effects. Note the addition of 0.0001 avoids 0 means
	#Hok: To draw a random variable sample size of studies , number of effect size per studies L
	#parameters
	sigma_n<-n_study_sd^2 / (n_study_mu - 3 + 10^-8)


	#
	sigma_k<-k_effect_sd^2 / (k_effect_mu - 1 + 10^-8)

 	# Loop for the k studies

	#Hok: loop in the k_study
	for(i in 1:k_study){

	  #Action:
	  #- select a random variable \theta_i(Estimator) from the normal distribution
	  #- select a random number of effect size per study from the poisson distribution
	  #- select a random sample size of study from the poisson distributional
	  #- select the SD for study i was drawn from a random Gamma distribution - standard deviation of the observations







		# Get the study specific-effect for study i
	  #randomly generate a variable effect_size for study i
		ES_i<-rnorm(1, lnRR, sqrt(study_sigma2))

		# Find study-specific number of effect sizes, sample size and variance for study i
	  # Number Effect size get from the poisson distribution
		k_effect_i<-rDPO(1, mu=(k_effect_mu - 1 + 10^-8), sigma=sigma_k) + 1
		# number of observations get from the poisson distribution
		n_i<-rDPO(1, mu=(n_study_mu - 3 + 10^-8), sigma=sigma_n) + 3
		sd_i<-rgamma(1, shape=a, scale=b)

		# Loop for the k effects

		#Actions:
		#- select a random effect size j from study i
		#- corresponding to the effect size , the constrol gorup and the experimental group are drwand from the normal distribution


		for(j in 1:k_effect_i){

			# Get the within-study effects for study i effect size j
			ES_ij<-rnorm(1, ES_i, sqrt(resid_sigma2))

			# Simulate the control group for study i, effect size j
			control_ij<-rnorm(n=n_i, mean=mu_control, sd=sd_i)

			# Simulate the treatment group for study i, effect size j
			treatment_ij<-rnorm(n_i, mean(mu_control * exp(ES_ij)), sd=sd_i)

			# Now calculate the mean, sd and give n in a nice way for downstream code
			data_ij<-data.frame(Effect = paste(i, j, sep="_"), Study = i,
			                    Control.Mean = mean(control_ij), Control.SD = sd(control_ij),
			                    Control.n = length(control_ij), Treatment.Mean = mean(treatment_ij),
			                    Treatment.SD = sd(treatment_ij), Treatment.n = length(treatment_ij))

			# If you want the true simulated effects returned, add those in
			if(return_true_effects){
				data_ij$ES_i<-ES_i
				data_ij$ES_ij<-ES_ij
				data_ij$sd_i<-sd_i
			}

			# Keep a dataframe of all the data
			if(i == 1 & j == 1){
				data<-data_ij
			}else{
				data<-rbind(data, data_ij)
			}
		}
	}

	# Return the data
	return(data)

}
#For example: the result might look like:
data <- data.frame(
  Effect = c("1_1", "1_2", "2_1", "2_2"),
  Study = c(1, 1, 2, 2),
  Control.Mean = c(0.5, 0.5, 0.45, 0.48),
  Control.SD = c(0.1, 0.1, 0.12, 0.11),
  Control.n = c(30, 30, 25, 25),
  Treatment.Mean = c(0.6, 0.55, 0.52, 0.58),
  Treatment.SD = c(0.15, 0.14, 0.13, 0.14),
  Treatment.n = c(30, 30, 25, 25),
  ES_i = c(NA, NA, NA, NA),  # Replace NA with actual values if available
  ES_ij = c(NA, NA, NA, NA), # Replace NA with actual values if available
  sd_i = c(NA, NA, NA, NA)   # Replace NA with actual values if available
)



############################################
################ my_lnRR ###################
############################################

# Function to calcualte the lnRR based on CV as given in the manuscript

my_lnRR_CV<-function(Control.Mean, Treatment.Mean, Control.CV, Treatment.CV, Control.n, Treatment.n){

	# The Effect size
	yi_CV<-log(Treatment.Mean / Control.Mean) + 0.5 * (Treatment.CV^2 / Treatment.n - Control.CV^2 / Control.n)

	# The sampling variance
	vi_CV<-(Treatment.CV^2 / Treatment.n) + (Control.CV^2 / Control.n) +
	  (Treatment.CV^4 / (2 * Treatment.n^2)) + (Control.CV^4 / (2 * Control.n^2))

	# Return the data
	return(data.frame(yi_CV=yi_CV, vi_CV=vi_CV))

}

my_lnRR_ID <- function(Control.Mean, Treatment.Mean,Control.ID,Treatment.ID, Control.n, Treatment.n){

  yi_ID <- log(Treatment.Mean / Control.Mean) + 0.5*((Treatment.ID /(Treatment.Mean*Treatment.n)) - (Control.ID /(Control.Mean*Control.n))) ###equation 4 ou 6 Nakagawa et al.(2023)

  vi_ID <- ((Treatment.ID) /Treatment.Mean*Treatment.n) + ((Control.ID) / Control.Mean*Control.n) +
    ((Treatment.ID/Treatment.Mean)^2 / (2*Treatment.n^2)) + ((Control.ID/Control.Mean)^2 / (2*Control.n^2))
  
  
  # Return the data
  return(data.frame(yi_ID=yi_ID, vi_ID=vi_ID))
  

}



