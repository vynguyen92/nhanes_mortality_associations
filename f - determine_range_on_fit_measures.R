#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############  FUNCTION TO CALCULATE THE BOOTSTRAPPED CONFIDENCE INTERVAL OF THE FIT MEASURES  ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function conducts bootstrapping for a given number of replicates to define the confidence 
#          interval and standard error on the fit measures
#          
# Inputs: input_data - dataframe of the predictors
#         output_data - dataframe of the mortality status and time to death variables
#         fit_of_cv_model - model object of the cross-validated regression model
#         fit_stats - string indicating the name of the fit measures
#         codename - string indicating the name of the population type ("all" or "[insert code name of  
#                    physiological indicator]") or the codename of the physiological indicator
#         treatment_on_contin - string indictating the treatment on the continous variable, i.e. "linear", 
#                               "novemtiles", or "splines"
#         number_of_replicates - number of bootstrap replicates 
#         sensitivity_range - string indicating the percentiles of the transformed variable on which 
#                             participants to include in the regression models 
#         model_types - string indicating whether the transformation was applied on age or the physiological 
#                       indicator
#         dimension_of_matrix - the number or rows (participants) and columns (predictors) of the matrix fed 
#                               into the cross-validation
#
# Outputs: df_fits - a dataframe of mean, confidence interval, and standard error for the fit measures

determine_range_on_fit_measures <- function(input_data
                                            , output_data
                                            , fit_of_cv_model
                                            , fit_stats
                                            , codename
                                            , treatment_on_contin
                                            , number_of_replicates
                                            , sensitivity_range
                                            , model_types
                                            , dimension_of_matrix)
{
  # Calculate the confidence interval by running 1000 bootstrap replicates for each fit measure
  for(f in seq(length(fit_stats)))
  {
    # Determine the ith fit measure
    fit_stats_f <- fit_stats[f]
    # print(fit_stats_f)
     
    # Run 1000 bootstrap replicates for the ith fit measure
    boot_fit_stat <- boot(data = input_data
                          , statistic = bootstrap_on_fit_measures
                          , R = number_of_replicates
                          , fit = fit_of_cv_model
                          , observations = output_data
                          , fit_stat_name = fit_stats_f
                          , parallel = "snow")
    
    # Calculate the mean value of the fit measure
    if(fit_stats_f == "rsq")
    {
      # The Nagelkerke R2 is calculated based on the Cox & Snell R2 and the maximum R2
      fit_stats_mean <- unname(boot_fit_stat$t0[1]/boot_fit_stat$t0[2])
      
    } else {
      # There is only one value to extract for the AIC or concordance index
      fit_stats_mean <- unname(boot_fit_stat$t0[1])
    }
    
    # Calculate the bootstrapped standard error of the fit measure
    fit_stats_std_error <- sd(boot_fit_stat$t[,1])
      
    # Calclate the bootstrapped low bound of the confidence interval on the fit measure
    fit_stats_ci_low <- fit_stats_mean - 1.96*fit_stats_std_error
    
    # Some calculations can lead to a negative value of the fit measure, so that minimum is capped at 0
    if(fit_stats_ci_low < 0)
    {
      fit_stats_ci_low <- 0
    }
    
    # Calclate the bootstrapped high bound of the confidence interval on the fit measure
    fit_stats_ci_high <- fit_stats_mean + 1.96*fit_stats_std_error
    
    # Define a dataframe of the mean, confidence intervals, and the standard error
    df_fits_f <- data.frame(fit_stats_mean
                            , fit_stats_ci_low
                            , fit_stats_ci_high
                            , fit_stats_std_error
                            , stringsAsFactors = FALSE)
    
    # Change the column names of the statistics of the fit measure for legibility
    colnames(df_fits_f) <- gsub("fit_stats"
                                , fit_stats_f
                                , colnames(df_fits_f))
    
    # Combine the statistics for the different fit measures togther while also including pertinent 
    # information such as physiological indicator or population type, treatment on the continuous variable
    # and sensitivity ranges 
    if(f == 1)
    {
      # For analyses performed on the physiological indicator
      if(model_types == "pi")
      {
        df_fits <- data.frame(pi = codename
                              , treatment_on_pi = treatment_on_contin 
                              , sensitivity_range
                              , df_fits_f
                              , stringsAsFactors = FALSE)
        
      # For analyses performed on age
      } else if(model_types == "demo") {
        
        df_fits <- data.frame(population_type = codename
                              , treatment_on_demo = treatment_on_contin 
                              , sensitivity_range
                              , df_fits_f
                              , stringsAsFactors = FALSE)
      }
      
      
    } else {
      df_fits <- data.frame(df_fits
                            , df_fits_f
                            , stringsAsFactors = FALSE)
    }
    
    
  }
  
  # Perform the predictions from the cross-validated models
  predictions <- predict(fit_of_cv_model
                           , input_data
                           , type = "response"
                           , s = 0)

  # Perform this regression model to assess the fit of the cross-validated models
  boot_fit <- coxph(output_data ~ predictions)
  
  # Determine the number of participants and predictors for calculations of other fit measures
  num_participants <- dimension_of_matrix[1]
  num_predictors <- dimension_of_matrix[2]
  
  # Calculate the different penalties to calculate a corrected AIC
  penaltyN <- 2*(num_predictors)*(num_predictors + 1)
  penaltyD <- num_participants - num_predictors - 1  
  penalty <- penaltyN/penaltyD
  
  # Calculated the corrected AIC
  AIC_corrected <- glance(boot_fit)$AIC + penalty
  
  # Determine the fit of a null model
  null_model <- coxph(output_data ~ 1)
  
  # Extract the log-likelihood of the null model
  log_likelihood_null <- glance(null_model)$logLik
  
  # Extract the log-likelihood of the full model
  log_likelihood_full <- glance(boot_fit)$logLik
  
  # Calculate the McFadden R2
  mcfadden_rsq <- 1 - (log_likelihood_full/log_likelihood_null)
  
  # Calculate the corrected McFadden R2
  mcfaddenadj_rsq <- 1 - ((log_likelihood_full - num_predictors)/log_likelihood_null)

  # Put the other fit measures into a dataframe
  df_other_fit_measures <- data.frame(AIC_corrected 
                                      , mcfadden_rsq 
                                      , mcfaddenadj_rsq 
                                      )
  
  # Combine all fit measures together with the other statistics for the regression models
  df_fits <- df_fits %>%
    cbind(.
           , glance(boot_fit)) %>%
    cbind(.
          , df_other_fit_measures)
  
  return(df_fits)
}