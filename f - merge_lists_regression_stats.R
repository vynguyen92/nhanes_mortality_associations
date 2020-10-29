#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################### FUNCTION TO MERGE THE CORRESPONDING DATASET TOGETHER FROM TWO LISTS ######################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function merge the corresponding dataset from the list of the unadjusted regression results  
#          with that from the list of adjusted regression results
#
# Inputs: list_stats_unadjusted - a list of dataframe of the unadjusted regression results. A dataframe is    
#                                 available each to contain the coefficients, prediction performance, and 
#                                 predicted risk
#         list_stats_adjusted - a list of dataframe of the adjusted regression results.
#
# Outputs: list_final - a list containing merged dataset each to contain the coefficients, prediction 
#                       performance, and predicted risk
#

merge_lists_regression_stats <- function(list_stats_unadjusted
                                         , list_stats_adjusted)
{
  # Determine the different dataset in the list
  names_datasets <- names(list_stats_unadjusted)
  # Determine the number of datasets
  num_datasets <- length(names_datasets)
  
  # Initialize an empty list to store the combined list
  list_final <- list()
  
  # For each type of dataset, merge the the results together
  for(d in seq(num_datasets))
  {
    # Determine the name of the dataset
    name_dataset_d <- names_datasets[d]
    
    # Extract that dataset from the unadjusted list
    dataset_d_un <- list_stats_unadjusted[[name_dataset_d]] %>%
      # Add a column to specify the unadjusted regression results
      mutate(adjustment = rep("unadjusted", nrow(.)))
    
    # Extract that dataset from the adjusted list
    dataset_d_adjusted <- list_stats_adjusted[[name_dataset_d]] %>%
      # Add a column to specify the adjusted regression results 
      mutate(adjustment = rep("adjusted", nrow(.)))
    
    # Merge the two dataset together and store it into the list
    list_final[[name_dataset_d]] <- dataset_d_un %>%
      full_join(.
                , dataset_d_adjusted
                , by = colnames(.)) %>%
      # Ensure that the naming of the sensitivity analyses are consistent
      mutate(sensitivity_range = case_when(sensitivity_range == "0_100" ~ "00_100"
                                           , sensitivity_range == "00_100" ~ "00_100"
                                           , sensitivity_range == "1_99" ~ "01_99"
                                           , sensitivity_range == "01_99" ~ "01_99"
                                           , sensitivity_range == "05_95" ~ "05_95"
                                           , sensitivity_range == "5_95" ~ "05_95"
                                           , sensitivity_range == "10_90" ~ "10_90"))
    
  }
  
  return(list_final)
}