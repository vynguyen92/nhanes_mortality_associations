#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############  FUNCTION TO COMPARE CLINICAL WITH MEASUREMENTS WITH HAZARD RATIO AT 1.1 BY SEX ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function compare the clinical thresholds with the measurements with hazard ratio at 1.1 by sex
#
# Inputs: df_stats_sex_pi - dataset containing the predicted mortality risk for each sex.
#         pi_i - string of the codename of the physiolgical indicator
#         df_ref_sex_pi - dataset containing the median measurement or the reference novemtile for a given 
#                         physiological indicator.
#         sensitivity_j - string of the sensitivity range
#         analysis_type_i - string indicating whether the results are for the "unweighted" or "weighted" results
#
# Outputs: dataset_comparison - dataframe of measurements and clinical thresholds by sex for a given physiological
#                               indicator and sensitivity range

compare_threshold_sex <- function(df_stats_sex_pi
                                  , pi_i
                                  , df_ref_sex_pi
                                  , sensitivity_j
                                  , analysis_type_i)
{
  # Determine the genders
  gender <- unique(df_stats_sex_pi$gender)
  # Determine the number of genders
  num_gender <- length(gender)
  
  # Dataset manually developed to contain the measurements of when the harzard ratio is 1.1. These measurements 
  # may be missing from the automatic process of detecting these points.
  df_extra_measurements <- read_excel("NHANES - Dataset of Physiological Measurements with Hazard Ratio at 1.1.xlsx"
                                      , sheet = "gender")
  
  for(g in seq(num_gender))
  {
    # Determine the gender
    gender_g <- gender[g] %>%
      as.character(.)
    # print(gender_g)

    # Extract the predicted mortality risk for a given gender
    df_predicted_risk_g <- df_stats_sex_pi %>%
      filter(gender == gender_g)
    # print(df_predicted_risk_g)

    # Extract the measurements that have a hazard ratio of 1.1
    list_of_features_i_g <- define_hr_and_measurements_at_increased_risk(df_predicted_risk_g)
    # print(list_of_features_i_g)

    # Determine whether there are case when there are no measurements with a hazard ratio of 1.1
    no_measurements_with_hr_1.1 <- any(is.na(list_of_features_i_g$df_measurements))
    # print(no_measurements_with_hr_1.1)
    
    # Determine whether the physiological indicator is in a dataset contain the measurements of when 
    # the harzard ratio is 1.1 
    pi_in_df_extra <- pi_i %in% df_extra_measurements$pi 
    # print(pi_in_df_extra)
    
    if(pi_in_df_extra == TRUE)
    {
      # Define a pattern for gender to use it to label the measurements
      gender_pattern <- paste("adult"
                              , " "
                              , gsub("_", "", gender_g)
                              , "s"
                              , sep = "")
      
      # Extract these measurements from the manually developed dataset as store it as the dataset containing 
      # the measurements with harzard ratios at 1.1
      df_extra_measurements_i_g <- df_extra_measurements %>%
        filter(pi == pi_i) %>%
        filter(sensitivity_range == sensitivity_j) %>%
        filter(analysis_type == analysis_type_i) %>%
        filter(gender == gender_pattern) %>%
        dplyr::select(x, y) %>%
        mutate(pi = rep(pi_i, nrow(.))) %>%
        mutate(population = rep(gender_g, nrow(.)))
      # print(df_extra_measurements_i_g)
      
      # If the physiological indicator doesn't have measurements, then use the subset of extra measurements
      if(no_measurements_with_hr_1.1 == TRUE)
      {
        dataset_association_thresholds_i_g <- df_extra_measurements_i_g
      
      # If the physiological indicator does have measurements, then merge the subset of extra measurements 
      # with the measurements
      } else {
        
        dataset_association_thresholds_i_g <- list_of_features_i_g[["df_measurements"]] %>%
          mutate(pi = rep(pi_i, nrow(.))) %>%
          mutate(population = rep(gender_g, nrow(.)))
        
        dataset_association_thresholds_i_g <- dataset_association_thresholds_i_g %>%
          full_join(.
                    , df_extra_measurements_i_g
                    , by = colnames(.))
        
      }
      
      # For physiological indicators that aren't missing any measurments when the harzard ratio is 1.1, add a
      #  new column for the codename of the indicator
    } else {
      
      dataset_association_thresholds_i_g <- list_of_features_i_g[["df_measurements"]] %>%
        mutate(pi = rep(pi_i, nrow(.))) %>%
        mutate(population = rep(gender_g, nrow(.)))
    }
    # print(dataset_association_thresholds_i_g)
    
    # Extract the referencen point for a given sex
    ref_point_sex_g <- df_ref_sex_pi %>%
      filter(gender == gender_g) %>%
      dplyr::select(ref) %>%
      unlist(., use.names = FALSE)
    # print(ref_point_sex_g)

    # Automatically determine whether the thresholds are lower or upper bound by determine the slope from the 
    # reference pointto the measurement at harzard ratio is 1.1
    dataset_association_thresholds_i_g <- dataset_association_thresholds_i_g %>%
      mutate(ref = rep(ref_point_sex_g, nrow(.))) %>%
      # Determine slope from the reference point to the measurement at harzard ratio is 1.1
      mutate(slope = (y-1)/(x-ref)) %>%
      # If the slope is negative, then the thresholds is a lower point, and positive slope implies that its an 
      # upper bound
      mutate(direction_final = case_when(slope < 0 ~ "lower threshold"
                                         , slope > 0 ~ "upper threshold")) 
    
    # Define whether there should be a range for measurements with hazard ratios equal to 1.1
    dataset_association_thresholds_i_g <- define_measurements_part_of_range(dataset_association_thresholds_i_g
                                                                          , df_predicted_risk_g) %>%
      # Select pertinent columns
      dplyr::select(x, y, pi, population, direction_final) %>%
      # Round the measurements to two digits for ease of reading in the manuscript
      mutate(x = round(x, digits = 2)) %>%
      # A given bound may have a range, so calculate the range for the lower and upper bound
      mutate(min_threshold = case_when(direction_final == "lower threshold" ~ min(subset(.
                                                                                         , direction_final == "lower threshold")$x)
                                       , direction_final == "upper threshold" ~ min(subset(.
                                                                                           , direction_final == "upper threshold")$x))) %>%
      mutate(max_threshold = case_when(direction_final == "lower threshold" ~ max(subset(.
                                                                                         , direction_final == "lower threshold")$x)
                                       , direction_final == "upper threshold" ~ max(subset(.
                                                                                           , direction_final == "upper threshold")$x)))  %>%
      dplyr::select(pi, direction_final, population, min_threshold, max_threshold) %>%
      unique(.) %>%
      mutate(population = ifelse(population == "_male", "adult males", "adult females"))
    # print(dataset_association_thresholds_i_g)

    # Merge the dataset specific for each sex together
    if(g == 1)
    {
      dataset_association_thresholds_g <- dataset_association_thresholds_i_g
    } else {
      dataset_association_thresholds_g <- dataset_association_thresholds_g %>%
        full_join(.
                  , dataset_association_thresholds_i_g
                  , by = colnames(.))
    }

  }
  return(dataset_association_thresholds_g)
}