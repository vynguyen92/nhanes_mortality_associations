#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######  FUNCTION TO EXTRACT THE MEASUREMENTS OF THE PHYSIOLOGICAL INDICATOR FOR A SENSITIVITY ANALYSIS  #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extract all measurements of a physiological indicator that were used in a given   
#          sensitivity analysis. The measurements are extracted from the original dataset. 
#          
# Inputs: x - dataframe of the predicted risks of the measurements of a physiological indicator and a 
#             sensitivity analysis
#         long_dataset - long-formatted dataframe based on the physiological indicators
#         motif_demo - string indicating whether the analysis was run on the NHANES population ("all") or 
#                      performed on sex ("gender")
#
# Outputs: df_distribution - dataframe of all measurements for a physiological indicator used in a given
#                            sensitivity analysis
#                 

define_distribution <- function(x
                                , long_dataset
                                , motif_demo)
{
  # Determine the sensitivity analysis of interest
  sensitivity_range_i <- unique(x$sensitivity_range)
  
  # Determine the codename of the physiological indicator
  pi_i <- unique(x$pi)
  
  # Determine the minimum value of the physiological indicator
  min_pi_perc_k <- min(x$pi_value)
  
  # Determine the maximum value of the physiological indicator
  max_pi_perc_k <- max(x$pi_value)
  
  # Extract the measurements pertaining to the physiological indicator from the original dataset
  long_dataset <- long_dataset %>%
    filter(pi == pi_i)
  
  # For sex-stratified analyses, extract the measurements pertaining to a given gender
  if(motif_demo == "gender")
  {
    # Determine the gender of interest
    gender_i <- unique(x$gender)
    
    # Filter the dataset to contain measurements for a given gender
    long_dataset <- long_dataset %>%
      filter(gender == gender_i)
    
  } else {
    # No changes needed for analyses applied on the NHANES population
    long_dataset <- long_dataset
  }
  
  # Extract that measurements that were used in a given sensitivity analysis
  pi_values_i <- long_dataset %>%
    filter(pi_value >= min_pi_perc_k &
             pi_value <= max_pi_perc_k) %>%
    dplyr::select(pi_value)
  # print(pi_values_i)
  
  # Form a dataframe with sensitivity analysis and the measurements of the physiological indicator used in 
  # the analysis
  df_distribution <- data.frame(sensitivity_range = sensitivity_range_i
                                , pi_value = pi_values_i
                                , stringsAsFactors = FALSE)
  
  return(df_distribution)
}
