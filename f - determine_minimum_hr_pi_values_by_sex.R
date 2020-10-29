#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########  FUNCTION TO DETERMINE THE MEASUREMENT CORRESPONDING TO THE MINIMUM HAZARD RATIO BY SEX ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determine the value the physiological indicators that corresponds to the minimum hazard 
#          ratio to be used as new reference points for each sex
#          
# Inputs: list_stats - list of dataframe containing regression statistics from the cross-validated models 
#         dataset_long - long-formatted dataframe based on the physiological indictors
#
# Outputs: list_refs - list of two dataframes of the new reference points by sex (one for the unweighted and 
#                      another for the weighted analysis)

determine_minimum_hr_pi_values_by_sex <- function(list_stats
                                                  , dataset_long)
{
  # Determine the analysis types
  analysis_types <- names(list_stats)
  # Determine the number of analysis types
  num_analysis_types <- length(analysis_types)
  
  # Initialize an empty list
  list_refs <- list()
  
  # Determine the new reference points for each analysis type
  for(i in seq(num_analysis_types))
  {
    # Determine a given analysis type
    analysis_type_i <- analysis_types[i]
    print(analysis_type_i)
    
    # Extract the dataset of predicted risk for a given analysis type
    df_stats_i <- list_stats[[analysis_type_i]]
    
    # Extract all the genders
    genders <- unique(df_stats_i$gender) %>%
      as.character(.)
    # Determine the number of genders
    num_gender <- length(genders)
    
    for(j in seq(num_gender))
    {
      # Determine the gender
      gender_j <- genders[j]
      print(gender_j)
      
      # Extract the measurements for a given gender
      dataset_long_j <- nhanes_long_dataset %>%
        filter(gender == gender_j)
      
      # Determine the measurement with the lowest hazard ratio and is also a critcal point for each gender
      dataset_ref_i_j <- df_stats_i %>%
        # Include the statistics for a given gender
        filter(gender == gender_j) %>%
        # Determine the new reference point
        determine_minimum_hr_pi_values(.
                                       , dataset_long_j
                                       , "gender"
                                       , analysis_type_i) %>%
        # Define a new column with the gender name
        mutate(gender = rep(gender_j, nrow(.)))
      
      # Combine the dataframe of new reference points across all gender, physiological indicators, and 
      # sensitivity analyses
      if(j == 1)
      {
        dataset_ref_i <- dataset_ref_i_j 
      } else {
        dataset_ref_i <- dataset_ref_i %>%
          full_join(.
                    , dataset_ref_i_j
                    , by = colnames(.))
      }
      
    }
    # Store the dataframe of new reference points for a given analysis type into a list
    list_refs[[analysis_type_i]] <- dataset_ref_i
    # print(dataset_ref_i)
      
    
  }
  return(list_refs)
}