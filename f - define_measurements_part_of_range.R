#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####################  FUNCTION TO DETERMINE MEASUREMENTS TO DEFINE AS PART OF A RANGE #########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function deterine whether the measurements with hazard ratio at 1.1 can be used to define ranges
#          for the association-based thresholds
#
# Inputs: dataset_associations_thresholds - dataset containing the measurements that have a hazard ratio of 1.1
#                                           for a given physiological indicator and sensitivity analysis.
#         df_predicted_risk - dataset containing the predicted mortality risk for a given physiological  
#                             indicator and sensitivity analysis
#
# Outputs: dataset_comparison - dataframe of measurements with hazard ratio of 1.1 that can be used to define 
#                               ranges for the association-based thresholds

define_measurements_part_of_range <- function(dataset_associations_thresholds
                                              , df_predicted_risk)
{
  # Determin the number of measurements pertaining to the same bound
  counts_points_by_bounds <- dataset_associations_thresholds %>%
    group_by(direction_final) %>%
    summarise(count = length(pi))
  # print(counts_points_by_bounds)
  
  # Determine whether the upper bound or lower bound or both have multiple measurements
  bounds_with_multiple_values <- counts_points_by_bounds %>%
    filter(count >= 2) %>%
    dplyr::select(direction_final) %>%
    unlist(., use.names = FALSE)
  # print(bounds_with_multiple_values)
  
  # Determine the number of bounds with multiple measurements
  num_directions_with_multiple_values <- length(bounds_with_multiple_values)
  # print(num_directions_with_multiple_values)
  
  # Define a new column with the percentage of points hold to a hazard ratio of 1.1 to be 100
  # This will include the measurements that are closest to the reference to be included 
  dataset_associations_thresholds <- dataset_associations_thresholds %>%
    mutate(perc_close_to_1.1 = rep(100, nrow(.)))
  
  # If no bounds have multiple measurements, then the dataset will remain the same
  if(num_directions_with_multiple_values == 0)
  {
    dataset_associations_thresholds <- dataset_associations_thresholds
    
  # If there are bounds with multiple measurements, then determine whether that measurements should be defined  
  # as a range
  } else {
    
    for(i in seq(num_directions_with_multiple_values))
    {
      bounds_i <- bounds_with_multiple_values[i]
      # print(bounds_i)
      
      # Subset the dataset to containing a given bound
      subset_assoc_thresholds_i <- dataset_associations_thresholds %>%
        filter(direction_final == bounds_i)
      # print(subset_assoc_thresholds_i)
      
      # Define the measurement closest to the reference point
      if(bounds_i == "upper threshold")
      {
        include_value <- min(subset_assoc_thresholds_i$x)
      } else if(bounds_i == "lower threshold") {
        include_value <- max(subset_assoc_thresholds_i$x)
      }
      
      # Add a column of the measurement closest to the reference point and exclude rows that pertain to this 
      # measurement
      subset_assoc_thresholds_i <- subset_assoc_thresholds_i %>%
        mutate(include_value = rep(include_value, nrow(.))) %>%
        filter(x != include_value)
      # print(subset_assoc_thresholds_i)
      
      # Determine measurements that could be candidate for the range
      unique_values_i <- unique(subset_assoc_thresholds_i$x)
      
      # Determine number of candidate measurements that may define a range
      num_values_i <- nrow(subset_assoc_thresholds_i)
      
      for(j in seq(num_values_i))
      {
        # Determine a given candidate measurement
        unique_values_i_j <- unique_values_i[j]
        # print(unique_values_i_j)
        
        # Extract the candidate measurement and all pertinent information
        subset_assoc_thresholds_i_j <- subset_assoc_thresholds_i %>%
          filter(x == unique_values_i_j)
        # print(subset_assoc_thresholds_i_j)
        
        # Determine the minimum and maximum measurement within the subset dataset
        min_pi_value <- min(subset_assoc_thresholds_i_j$include_value, subset_assoc_thresholds_i_j$x)
        max_pi_value <- max(subset_assoc_thresholds_i_j$include_value, subset_assoc_thresholds_i_j$x)
        
        # Define a fraction to define the buffer zone around hazard ratio = 1.1
        range_hr <- abs(diff(range(df_predicted_risk$hazard_ratio)))
        frac_hr <- 0.03*range_hr
        
        # Determine whether measurements are within a buffer zone around 1.1
        df_predicted_risk_i_j <- df_predicted_risk %>%
          # Include measurements that are within the included association threshold and the candidate measurement
          filter(pi_value >= min_pi_value & pi_value <= max_pi_value) %>%
          # Determine whether each measurements has hazard ratio that are within a buffer around HR = 1.1
          mutate(close_to_1.1 = if_else(hazard_ratio >= (1.1 - frac_hr) & hazard_ratio <= (1.1 + frac_hr), 1, 0))
        
        # Determine the percentage of points within the buffer zone
        perc_close_to_1.1 <- sum(df_predicted_risk_i_j$close_to_1.1)/nrow(df_predicted_risk_i_j)*100
        # print(perc_close_to_1.1)
        
        # Determine the index pertaining to the candidate measurement
        index_x <- which(dataset_associations_thresholds$x == unique_values_i_j)
        
        # Assign the percentage in the dataset of association thresholds
        dataset_associations_thresholds[index_x,"perc_close_to_1.1"] <- perc_close_to_1.1
        
      }
      # View(dataset_associations_thresholds)
      
      # Filter our candidate measurements with percentage of points within the buffer zone to be less than 90%
      dataset_associations_thresholds <- dataset_associations_thresholds %>%
        mutate(include = ifelse(perc_close_to_1.1 >= 90, 1, 0)) %>%
        filter(include == 1)
    
    }
    # print(dataset_associations_thresholds)
    
    return(dataset_associations_thresholds)
  }
}