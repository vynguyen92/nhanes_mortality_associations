#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######  FUNCTION TO DETERMINE THE MEASUREMENTS AND HAZARD RATIO SHOW A 10% INCREASE FROM MINIMUM RISK  #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determine the measurements and hazard ratios that show a 10% increase from the minimum 
#          risk (the local minimum) from the splines model
#          
# Inputs: dataset_risk_splines - dataframe of the predicted risk of all measurements for a physiological 
#                                indicator
#
# Outputs: list_features - a list containing 3 objects
#                          "hr_splines_1.1" is the hazard ratio showing a 10% increase from the minimum risk
#                          "df_measurements" are the measurements corresponding to this increase in risk
#                          "to_plot_intersects" is a boolean to indicate whether there are measurements that 
#                          show this increase risk
#

define_hr_and_measurements_at_increased_risk <- function(dataset_risk_splines)
{
  # Determine the maximum hazard ratio from the spline model
  max_hr <- max(dataset_risk_splines$hazard_ratio)
  
  # Determine the minimum hazard ratio from the spline model
  minimum_hr <- min(dataset_risk_splines$hazard_ratio)
  
  # Apply to determine the critical points of the physiological indicator to help identify the 
  # measurement pertaining to the minimum hazard ratio 
  features_splines <- features(x = dataset_risk_splines$pi_value
                               , y = dataset_risk_splines$predicted_risk) 
  
  # Extract the critical points 
  critical_points_x <- features_splines[["cpts"]]
  
  # Determine the number of critical points
  num_cpts <- length(critical_points_x)
  
  # If there are no critical points, then find the measurement corresponding to the global minimum
  if(anyNA(critical_points_x) == TRUE)
  {
    # Define a boolean to be true to indicate that the 1.1x the minimum hazard ratio intersects with the spline
    # model
    plot_intersects <- TRUE
    
    # Determine the minimum hazard ratio of the splines model
    min_hr <- dataset_risk_splines %>%
      filter(hazard_ratio == min(hazard_ratio)) %>%
      dplyr::select(hazard_ratio) %>%
      unlist(.
             , use.names = FALSE)
    
    # Determine the hazard ratio that is 1.1 times the minimum hazard ratio
    hr_splines_1.1 <- min_hr*1.1
    
    # Include a new column of the difference between the hazard ratios and that when it's 1.1 times the 
    # minimum hazard ratio
    measurements_splines_top <- dataset_risk_splines %>%
      mutate(diff = abs(hazard_ratio - hr_splines_1.1)) %>%
      arrange(diff)
    
    # Determine the measurements when the hazard ratios that are 1.1 times the minimum hazard ratio
    measurements_splines_1.1 <- measurements_splines_top %>%
      filter(diff == min(diff)) %>%
      dplyr::select(pi_value) %>%
      unlist(.
             , use.names = FALSE)
    
    # Determine the number of intersections between the splines model and when the hazard ratio is 1.1 times  
    # the minimum hazard ratio
    num_intersects <- length(measurements_splines_1.1)
    
    # Define a dataset of the measurements and hazard ratios that are 1.1 times the minimum hazard ratio
    df_measurements_splines_1.1 <- data.frame(x = measurements_splines_1.1
                                              , y = rep(hr_splines_1.1, num_intersects)
                                              , stringsAsFactors = FALSE)
    
  # If there are several critical points, then find the local minimum 
  } else {
    
    for(c in seq(num_cpts))
    {
      # Extract a given critical point
      cpt_c <- critical_points_x[c]
      # print(cpt_c)
      
      # Add a column that is the absolute value between the pi_value and the given critical point
      temp_dataset_c <- dataset_risk_splines %>%
        mutate(diff = abs(pi_value - cpt_c))
      
      # Define a dataset that include the measurement that is closest to the critical point and has 
      # the minimum hazard ratio
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
      # print(temp_min)
      
      temp_diff_min_from_1 <- abs(temp_min - 1)
      # print(temp_diff_min_from_1)
      
      # If there are multiple critical points, find the minimum hazard ratio
      if(c == 1)
      {
        min_hr <- temp_min
        diff_min_from_1 <- temp_diff_min_from_1
      } else {
        # If the sequential critical point has a lower hazard ratio than previous, then this is the new 
        # minimum hazard ratio
        if(temp_min < min_hr & temp_diff_min_from_1 < diff_min_from_1)
        {
          min_hr <- temp_min
          diff_min_from_1 <- temp_diff_min_from_1
        # If the sequential critical point has a higher hazard ratio than the previous, then the minimum hazard 
        # ratio remains unchanged
        } else {
          min_hr <- min_hr
        }
      }
      # print(min_hr)
      
    }
    
    # print(min_hr)
    
    # Determine the hazard ratio that is 1.1 times the minimum hazard ratio
    
    if(min_hr > 1.3)
    {
      hr_splines_1.1 <- 1.1
    } else {
      hr_splines_1.1 <- min_hr*1.1
    }
    
    
    # print(hr_splines_1.1)
    
    # Define a column of the absolute difference between the hazard ratio and when it's 1.1 times the minimum 
    # hazard ratio
    measurements_splines_with_diff <- dataset_risk_splines %>%
      mutate(diff = abs(hazard_ratio - hr_splines_1.1))
    # View(measurements_splines_with_diff)
    
    # If the 1.1 times the minimum hazard ratio is higher than the maximum hazard ratio of the splines models or
    # if the minimum difference between the hazard ratio and 1.1 times the minimum hazard ratio is greater than 
    # 0.01, then there is no intersection and thus there is no measurement corresponding to when the hazard 
    # ratio is 1.1 times the minimum hazard ratio
    if(hr_splines_1.1 > max_hr | min(measurements_splines_with_diff$diff) >= 0.01)
    {
      plot_intersects <- FALSE
      
      df_measurements_splines_1.1 <- NA
      
    # If there are intersections between the splines model and 1.1 times the minimum hazard ratio, then 
    # determine the measurements and hazard ratios of these intersections. As there may be more than 1 
    # intersections, we need to ensure the identification of all these intersections. 
    } else {
      
      plot_intersects <- TRUE
      
      # Extract a dataset of predicted risk from the splines model that has a difference of less than 0.02 
      # from the 1.1 times the minimum hazard ratio.
      # There are many measurements that are similiar and have similar hazard ratios, so we need to detect 
      # the different measurements that have similar hazard ratios.
      measurements_splines_top <- measurements_splines_with_diff  %>%
        filter(diff < 0.02) %>%
        arrange(pi_value)
      # print(measurements_splines_top$pi_value)
      
      # Form a numeric vector of measurements starting from the 2nd index to the last with the last repeated 
      # twice. This is used to formed a shift in the measurements to help calculate the difference between a 
      # measurements and the adjacent measurement.
      shifted_pi_values_top <- append(measurements_splines_top$pi_value[2:nrow(measurements_splines_top)]
                                      , measurements_splines_top$pi_value[nrow(measurements_splines_top)])
      # print(shifted_pi_values_top)

      # Calculate the changes between a given measurement and its adjacent measurement
      changes <- abs(measurements_splines_top$pi_value - shifted_pi_values_top)/
        measurements_splines_top$pi_value
      # print(sort(log10(changes)))
      
      # Determine the indices of where the changes are greater than 0.1
      index_large_changes <- which(log10(changes) > -1)
      # print(index_large_changes)
      
      # Define a function to split a vector at a given position
      splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
      
  
      # Split the vector of measurements at the position shown to have the largest change. 
      # Splitting the measurements into groups will help with better identifying all intersections. 
      splited_pi_values <- splitAt(measurements_splines_top$pi_value
                                   , (index_large_changes + 1))
      # print(splited_pi_values)
      # Determine the number of groups based on the splits
      num_groups_from_split <- length(splited_pi_values)
      
      
      for(s in seq(num_groups_from_split))
      {
        # Determine the measurements within a given group
        splited_pi_values_s <- splited_pi_values[[s]]
        
        # Determine the measurement within a group that has a hazard ratio that is closest to the hazard ratio 
        # at 1.1 times that of the local minimum
        measurements_splines_s <- measurements_splines_top %>%
          filter(pi_value %in% splited_pi_values_s) %>%
          filter(diff == min(diff))  %>%
          dplyr::select(pi_value) %>%
          unlist(.
                 , use.names = FALSE)
        
        # Gather all the measurements across the splitted groups
        if(s == 1)
        {
          measurements_splines_1.1 <- measurements_splines_s
        } else {
          measurements_splines_1.1 <- append(measurements_splines_1.1
                                             , measurements_splines_s)
        }
      }
      
      # Determine the number of measurements with hazard ratios that are closest to the hazard ratio at 1.1 
      # times that of the local minimum
      num_intersects <- length(measurements_splines_1.1)
      
      # Define a dataset containing the measurements and the hazard ratio at 1.1 times that of the local minimum
      df_measurements_splines_1.1 <- data.frame(x = measurements_splines_1.1
                                                , y = rep(hr_splines_1.1, num_intersects)
                                                , stringsAsFactors = FALSE)
      
    }
  }
  
  # Initialize an empty list to store the pertinent results to draw the intersection between the splines model 
  # and when the hazard ratio is 1.1 times that of the local minimum
  list_features <- list()
  
  # Store the value of the hazard ratio that is 1.1 times that of the local minimum
  list_features[["hr_splines_1.1"]] <- hr_splines_1.1
  
  # Store the measurements having hazard ratios that are closest to that hazard ratio at 1.1 times that of the 
  # local minimum
  list_features[["df_measurements"]] <- df_measurements_splines_1.1
  
  # Store the boolean indicating that there at intersections between the splines model and when the hazard ratio 
  # is 1.1 times that of the local minimum
  list_features[["to_plot_intersects"]] <- plot_intersects

  return(list_features)
  
}