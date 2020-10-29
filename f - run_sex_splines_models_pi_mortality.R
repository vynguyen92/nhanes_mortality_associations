#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##########  FUNCTION TO PERFORM SPLINES MODEL BETWEEN MORTALITY AND PHYSIOLOGICAL INDICATOR BY SEX  ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function run a series of spline models between transformed physiological indicator and  
#          mortality by sex
#          
# Inputs: dataset_long - long-formatted dataset based on the physiological indictors
#         list_dataset_ref - a list of two datasets (one for the unweighted and the other for the weighted 
#                            analysis) containing the reference points specific for each sex
#
# Outputs: regression_results - a list of two lists (one for the unweighted and the other for the weighted 
#                               analysis) with each list containing a dataset predicted risk

run_sex_splines_models_pi_mortality <- function(dataset_long
                                                , list_dataset_ref = NA)
{
  # Determine the codename for all physiologica indicators 
  pi_include <- unique(dataset_long$pi)
  
  # Define an index for given physiological indicators. This aspect helps with debugging
  index_pi <- which(pi_include == "BMXBMI")
  # Include all physiological indicators or a subset for debugging
  pi_include <- pi_include#[index_pi]
  # Determine the number of physiological indicators
  num_pi <- length(pi_include)
  
  # Define the analysis types 
  analysis_type <- c("unweighted"
                     , "weighted"
                     )
  # Determine the number of analysis types
  num_analysis_types <-  length(analysis_type)
  
  # Specify the restrictions on the physiological measurements
  types_sensitivity_analyses <- c("00_100"
                                  , "01_99"
                                  , "05_95"
                                  , "10_90"
                                  )
  # Determine the number of sensitivity analyses
  num_sensitivity_analyses <- length(types_sensitivity_analyses)
  
  # Initialize the list to hold the regression results
  regression_results <- list()
  
  # Run a series of regression models for each type of analysis.
  for(j in seq(num_analysis_types))
  {
    # Determine the type of analysis
    analysis_j <- analysis_type[j]
    
    # Run a series of regression models for each sensitivity analysis
    for(k in seq(num_sensitivity_analyses))
    {
      # Determine the sensitivity analysis
      sensitivity_k <- types_sensitivity_analyses[k]
      
      # Use the information of the sensitivity analysis to determine the minimum and maximum percentiles
      range_percentiles <- strsplit(sensitivity_k, "_") %>%
        unlist(.) %>%
        as.numeric(.)
      
      # Determine the minimum percentile for the sensitivity analyses
      min_perc <- range_percentiles[1]
      
      # Determine the maximum percentile for the sensitivity analysis
      max_perc <- range_percentiles[2]
      
      # Run a spline regression model for a given physiological indicator
      for(i in seq(num_pi))
      {
        # Determine the codename of the physiological indicator 
         pi_i <- pi_include[i]
        
        # Print out the analysis type, the sensitivity analysis, and physiological indicator 
        print(paste(analysis_j
                    , sensitivity_k
                    , pi_i
                    , sep = " - "))
        
        # Extract the dataset corresponding to participants with measurements for this physiological indicator
        dataset_i <- dataset_long %>%
          filter(pi == pi_i) %>%
          na.omit(.)
        
        # Determine the physiological measurement corresponding to the minimum percentile for the sensitivity 
        # analysis
        min_pi_perc_k <- quantile(dataset_i$pi_value
                                  , probs = min_perc/100
                                  , names = FALSE)
        
        # Determine the physiological measurement corresponding to the maximum percentile for the sensitivity 
        # analysis
        max_pi_perc_k <- quantile(dataset_i$pi_value
                                  , probs = max_perc/100
                                  , names = FALSE)
        
        # Determine the dataset to include participants who are with these percentiles for analysis
        dataset_i <- dataset_i %>%
          filter(pi_value >= min_pi_perc_k &
                   pi_value <= max_pi_perc_k)
        
        # Define a vector of all physiological measurements 
        all_pi_values <- dataset_i$pi_value
        
        # Extract the reference points and novemtiles for a given physiological indicator and sensitivity 
        # analysis
        if(anyNA(list_dataset_ref) == TRUE)
        {
          ref_dataset_temp <- NA
        } else {
          
          # Extract the list of dataframe containing the reference points
          dataset_ref_j <- list_dataset_ref[[analysis_j]]
          
          
          ref_dataset_temp <- dataset_ref_j %>%
            filter(pi == pi_i) %>%
            filter(sensitivity_range == sensitivity_k) 
          
        }
        
        # Perform the transformation on the physiological indicator and determine the predicted risk from the 
        # splines models for each gender
        regression_results_temp <- dataset_i %>%
          group_by(gender) %>%
          do(treat_pi_and_splines(.
                                  , analysis_j
                                  , sensitivity_k
                                  , all_pi_values
                                  , ref_dataset_temp)) %>%
          ungroup(.)
        
        # Combine the dataset of predicted risk for all measurements for both gender across all physiological 
        # indicators
        if(i == 1 & k == 1)
        {
          regression_predicted_risk <- regression_results_temp
          
        } else {
         
          regression_predicted_risk <- regression_predicted_risk %>%
            full_join(.
                      , regression_results_temp
                      , by = colnames(.))
        }
        
        
      }
    }
    
    # Store the combined dataset of predicted risk for a given analysis type in a list
    regression_results[[analysis_j]] <- regression_predicted_risk
  }
  
  return(regression_results)
}