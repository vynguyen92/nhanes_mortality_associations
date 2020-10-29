#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  FUNCTION TO DETERMINE THE MEASUREMENT CORRESPONDING TO THE MINIMUM HAZARD RATIO FROM SPLINE MODEL  #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determine the value of a physiological indicator that corresponds to the minimum hazard 
#          ratio from the splines model
#          
# Inputs: list_stats - numeric vector of the critical points (the physiological measurements)
#         datasest_splines - dataframe of the predicted risk of all measurements for a physiological indicator
#         dataset_novemtiles - dataframe of regression statistics of the novemtile with the minimum hazard ratio
#         dataset_long - long-formatted dataframe based on the physiological indictors
#                      or sex-stratified ("gender"). 
#
# Outputs: min_pi - numeric value of the measurement that is a critical point and has the minimum hazard ratio
#                   from the spline model

determine_pi_value_with_min_hr_splines <- function(critical_points
                                                   , datasest_splines
                                                   , dataset_novemtiles = NA
                                                   , dataset_long = NA
                                                   , analysis_type
                                                   )
{
  
  # Read in a dataset that helps to manually determine the reference point when the automation does not work
  df_ref_problematics <- read_excel("NHANES - Reference Points for Problematic.xlsx")
  
  # Determine the codename of the physiological indicator
  physiological_indicator <- unique(datasest_splines$pi)
  
  # Determine the sensitivity range
  sensitivity_range <- unique(datasest_splines$sensitivity_range)
  
  # If we're determining the reference points for the sex-stratified analyses
  if(is.na(dataset_novemtiles) == TRUE)
  {
    # If the physiological indicator for a given sensitivity range, analysis type, and sex is in the 
    # dataset of problematics, then present will be 1 or else it will be 0
    present <- which(df_ref_problematics$pi == physiological_indicator 
                     & df_ref_problematics$sensitivity_range == sensitivity_range 
                     & df_ref_problematics$analysis_type == analysis_type 
                     & df_ref_problematics$population == unique(datasest_splines$gender)) %>% 
      length(.)
  
  # Else if we're determining the reference points for the entire NHANES population
  } else {
    
    # If the physiological indicator for a given sensitivity range, analysis type, and NHANES population is 
    # in the dataset of problematics, then present will be 1 or else it will be 0
    present <- which(df_ref_problematics$pi == physiological_indicator  &
                       df_ref_problematics$sensitivity_range == sensitivity_range  &
                       df_ref_problematics$analysis_type == analysis_type  &
                       df_ref_problematics$population == "all" ) %>% 
      length(.)
  }
  
  # If we're determining the reference points for the sex-stratified analyses
  if(is.na(dataset_novemtiles) == TRUE)
  {
    # If the case is present in the dataset of problematics, then extract the row pertaining to the case
    if(present == 1)
    {
      df_ref_problematics_case <- df_ref_problematics %>%
        filter(pi == physiological_indicator) %>%
        filter(sensitivity_range == sensitivity_range) %>%
        filter(population == unique(datasest_splines$gender)) %>%
        filter(analysis_type == analysis_type)

    # If the case is NOT present in the dataset of problematics, then the dataset will be NA
    } else {
      df_ref_problematics_case <- NA
    }
    
  # Else if we're determining the reference points for the entire NHANES population
  } else {

    # If the case is present in the dataset of problematics, then extract the row pertaining to the case
    if(present == 1)
    {
      df_ref_problematics_case <- df_ref_problematics %>%
        filter(pi == physiological_indicator) %>%
        filter(sensitivity_range == sensitivity_range) %>%
        filter(analysis_type == analysis_type) %>%
        filter(population == "all")
      
    # If the case is NOT present in the dataset of problematics, then the dataset will be NA
    } else {
      df_ref_problematics_case <- NA
    }

  }
  # View(df_ref_problematics)
  # View(datasest_splines)
  
  # Determine the outliers
  outliers_pi_values <- boxplot.stats(datasest_splines$pi_value)$out
  # print(outliers_pi_values)
  
  # Eliminate the outliers to prevent the outliers from being selected for having a slope of 0
  # Outliers may actually have a non-zero slope, but due to lack of adjacent values, the calculated slope
  # can be close to 0
  subset_splines_for_threshold_slope <- datasest_splines %>%
    filter(!(pi_value %in% outliers_pi_values)) 
  # View(subset_splines_for_threshold_slope)
  
  # Determine the second smallest order of magnitude of the slopes
  threshold_slope <- subset_splines_for_threshold_slope %>%
    dplyr::select(slope) %>%
    unlist(., use.names = FALSE) %>%
    log10(.) %>%
    .[is.finite(.)] %>%
    round(., digits = 0) %>% 
    unique(.) %>%
    sort(.) 
  
  if(length(threshold_slope) == 1)
  {
    threshold_slope <- threshold_slope
  } else {
    threshold_slope <- threshold_slope[2]
  }
  # print(threshold_slope)
  
  # Manually change the threshold of the slope to be -3 if the threshold slope is -2
  if(threshold_slope %in% c(-2) & sensitivity_range != "10_90")
  {
    threshold_slope <- -3
  }
  
  # Include only physiological measurements with a slope less than the threshold
  subset_splines <- datasest_splines %>%
    filter(log10(slope) <= threshold_slope)
  # View(subset_splines)
    
  # If the physiological indicator is HOMA-IR for sensitivity analysis on all participants and for
  # the weighted analysis, include measurements which have miniscule changes in these slope from a
  # measurement to the next.
  if(physiological_indicator == "VNHOMAIR" 
     & sensitivity_range == "00_100"
     & analysis_type == "weighted")
  {
    subset_splines <- subset_splines %>%
      filter(neighborhood_diff <= 0.01)
  }
  # View(subset_splines)
  
  # Extract other critical points that were not captured by the function feature()
  potential_other_cpts <-  subset_splines$pi_value %>% 
    round(., digits = 2) %>%
    unique(.)
  # print(potential_other_cpts)
  
  # Concatenate the critical points determined by features() with those manually determined by the programmer
  critical_points <- c(critical_points
                       , potential_other_cpts
                       ) %>%
    unique(.) %>%
    .[!is.na(.)]
  # print(critical_points)
  
  # For problematic cases, extract the reference points from the dataset of manually determined reference
  # points
  if(anyNA(df_ref_problematics_case) == FALSE)
  {
    critical_points <- df_ref_problematics_case %>%
      dplyr::select(critical_points) %>%
      unlist(., use.names = FALSE)
  }
  # print(critical_points)
  
  # Determine the number of critical points
  num_cpts <- length(critical_points)
  
  # Determine the measurement corresponding to the minimum hazard ratio
  if(anyNA(critical_points) == TRUE)
  {
    if(anyNA(dataset_novemtiles) == FALSE)
    {
      # If there are no critical points and if the novemtiles dataset is not NA,
      # then use the mean value of the physiological indicator of the novemtile with the
      # minimum hazard ratio
      min_pi <- dataset_novemtiles$mean_within_ntile
      
    } else {
      
      # If there are no critical points and if the novemtiles dataset is NA, 
      # then use the measurements of physiological indicator that has a hazard ratio closest to 1 
      min_pi <- datasest_splines %>% 
        mutate(hr_diff = abs(hazard_ratio - 1)) %>%
        filter(hr_diff == min(hr_diff)) %>%
        select(pi_value) %>%
        unlist(., use.names = FALSE)
        
    }
    # print(min_pi)
  # If there are several critical points, then find the local minimum   
  } else {
    
    for(c in seq(num_cpts))
    {
      # Extract a given critical point
      cpt_c <- critical_points[c]
      # print(cpt_c)
      
      # Add a column that is the absolute value between the pi_value and the given critical point
      temp_dataset_c <- datasest_splines %>%
        mutate(diff = abs(pi_value - cpt_c)) #%>%
        # filter(slope <= threshold_slope)
      # View(temp_dataset_c)
      
      # Define a dataset that include the measurement that is closest to the critical point and has the minimum hazard ratio
      info_cpt_c <- temp_dataset_c %>%
        # Include measurements that are closest to the critical point
        filter(diff == min(diff)) %>%
        # Include the measurements that has the minimum hazard ratio
        filter(hazard_ratio == min(hazard_ratio))
      # print(info_cpt_c)
      
      # Select the minimum hazard ratio corresponding to the measurement that is closest to the critical point
      temp_min <- info_cpt_c %>%
        dplyr::select(hazard_ratio) %>%
        unlist(.
               , use.names = FALSE)
      
      # If there are multiple critical points, find the minimum hazard ratio
      if(c == 1)
      {
        # For the first critical point, set the hazard ratio corresponding to the measurements closest to the critical point
        min_hr <- temp_min
        dataset_cpt_c <- info_cpt_c
        
      } else {
        
        # If the sequential critical point has a lower hazard ratio than previous, then this is the new minimum hazard ratio
        if(temp_min < min_hr)
        {
          min_hr <- temp_min
          dataset_cpt_c <- info_cpt_c
          
        # If the sequential critical point has a higher hazard ratio than the previous, then the minimum hazard ratio remains unchanged
        } else {
          
          min_hr <- min_hr
          dataset_cpt_c <- dataset_cpt_c
          
        }
      }
      
    }
    
    min_pi <- dataset_cpt_c$pi_value
    # print(min_pi)
  }
  
  # Check if the measurement of the critical point is within the twice the range of a novemtile
  if(anyNA(dataset_novemtiles) == FALSE)
  {
    # Determine the lower bound corresponding to twice the range of the novemtile
    pi_value_percentage_away_min <- dataset_novemtiles$lower_bounds*1
    new_lower_bound <- dataset_novemtiles$lower_bounds - pi_value_percentage_away_min
    # print(new_lower_bound)
    
    # Determine the upper bound corresponding to twice the range of the novemtile
    pi_value_percentage_away_max <- dataset_novemtiles$upper_bounds*1
    new_upper_bound <- dataset_novemtiles$upper_bounds + pi_value_percentage_away_max
    # print(new_upper_bound)
    
    # If the measurement is outside the twice the range of the novemtile, then use the mean
    # measurement of the novemtile as the reference point
    if((min_pi < new_lower_bound) | (min_pi > new_upper_bound))
    {
      min_pi <- dataset_novemtiles$mean_within_ntile
      
    # If the measurement is inside twice of the range of the novemtile, then use this measurement
    } else {
      
      min_pi <- min_pi
      
    }
  
  # If a dataset of the novemtiles statistics is not available, then use this measurement
  } else {
    
    min_pi <- min_pi
  }
  # print(min_pi)
  return(min_pi)
}