#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################  FUNCTION TO PRODUCE THE VALUE OF THE FIT MEASURE TO BE BOOTSTRAPPED  ##################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function called by boot() for a given number of replicates to help define a confidence   
#          interval for a given fit measure
#          
# Inputs: data - dataframe containing the demographics, physiological indicators for each participant
#         fit - model object of the 10-fold cross-validated regression model
#         indices - numeric vector that is used to extract a sample
#         observations - dataframe of mortality status and time to death
#         fit_stat_name - string indicating the name of the fit measure of interest
#
# Outputs: return_this - value of the fit measure of interest

bootstrap_on_fit_measures <- function(data
                                      , fit
                                      , indices
                                      , observations
                                      , fit_stat_name)
{
  # Define a subset of the data of predictors
  d <- data[indices,] 
  
  # Define a subset of the outcome variable 
  true_reponses <- observations[indices,]
  
  # Calculate the predicted outcome variable from the subset while ensuring no penalization 
  predictions <- predict(fit
                         , d
                         , type = "response"
                         , s = 0)
  
  # Run a cox regression model to assess the fit of the predicted outcome variable and the observed outcome 
  # variables 
  boot_fit <- coxph(true_reponses ~ predictions)
  
  # For a given fit measure, extract the value of that fit measure for the previous cox model 
  if(fit_stat_name == "rsq")
  {
    return_this <- summary(boot_fit)$rsq
    # print(return_this)
    
  } else if(fit_stat_name == "AIC") {
    
    return_this <- glance(boot_fit)$AIC
    # print(return_this)
    
  } else if(fit_stat_name == "concordance") {
    
     return_this <- glance(boot_fit)$concordance
    # print(return_this)
    
  } 
  
  return(return_this)
}