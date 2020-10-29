#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######  FUNCTION TO DETERMINE THE PHYSIOLOGICAL MEASUREMENT CORRESPONDING TO THE MINIMUM HAZARD RATIO  #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determine the value or novemtile of the physiological indicators that corresponds to  
#          the minimum hazard ratios to be used as new reference points or group
#          
# Inputs: list_stats - list of dataframes containing regression statistics from the cross-validated models 
#         dataset_long - long-formatted dataframe based on the physiological indictors
#         name_motif - string indicating whether the analyses were applied on the NHANES population ("all") or 
#                      or sex-stratified ("gender"). 
#
# Outputs: dataset_merged_ref - dataframe of the value or novemtile of the physiological indicators that
#                               corresponds to the minimum hazard ratios. 

determine_minimum_hr_pi_values <- function(list_stats
                                           , dataset_long
                                           , name_motif = "all"
                                           , analysis_type)
{
  # Determine the physiological indicators and the sensitivity ranges 
  if(name_motif == "all")
  {
    unique_pi <- unique(list_stats[["predicted_risk"]]$pi)
    
    sensitivity_ranges <- unique(list_stats[["predicted_risk"]]$sensitivity_range)
    
  } else if(name_motif == "gender") {
    
    unique_pi <- unique(list_stats$pi)
    
    sensitivity_ranges <- unique(list_stats$sensitivity_range)
  }
  
  # Determine the index of a physiological indicator of interest for debugging
  # index_one <- which(unique_pi == c(#"VNRFPI"
  #                                   # "LBXSTR"
  #                                   # "VNTOTHDRATIO"
  #                                   # "LBXCRP"
  #                                   # "LBXWBCSI"
  #                                   # "LBXGLU"
  #                                   # "LBXSAPSI"
  #                                   # "VNHOMAIR"
  #                                   # "VNINGLURATIO"
  #                                   # "LBXSCR"
  #                                   # "BMXHT"
  #                                   # "VNLDHDLRATIO"
  #                                   # "LBXCRP"
  #                                   # "LBXWBCSI"
  #                                   # "LBXGLU"
  #                                   # "LBXSAPSI"
  #                                   # "LBXSCR"
  #                                   # "VNEGFR"
  #                                   ))

  # Subset the physiological indicators for debugging
  unique_pi <- unique_pi#[index_one]
  
  # Determine the number of physiologial indicators 
  num_pi <- length(unique_pi)
  
  # Determine the number of sensitivity analyses
  num_sensitivity_range <- length(sensitivity_ranges)
  
  # Initialize an empty list
  list_ref <- list()
  
  # Initialize a counter 
  counter <- 1
  
  # Determine the value of a given physiological indicator where the hazard ratio of a model is the minimum
  # for a given sensitivity analysis
  
  
  for(i in seq(num_pi))
  {
    # Determine the physiological indicator
    pi_i <- unique_pi[i]
    print(pi_i)
    
    for(j in seq(num_sensitivity_range))
    {
      # Determine the sensitivity analysis
      sensitivity_range_j <- sensitivity_ranges[j]
      print(sensitivity_range_j)
      
      # Define the dataset containing the predicted risk
      if(name_motif == "all")
      {
        dataset_stats <- list_stats[["predicted_risk"]]
        
      } else if(name_motif == "gender") {
        
        dataset_stats <- list_stats
      }
      
      # Define a dataset of the values of the physiological indicator for a given sensitivity analysis
      dataset_distributions_i_j <- dataset_stats %>%
        # Include statistics for a given physiological indicator
        filter(pi == pi_i) %>%
        # Include statistics for a given sensitivity analysis
        filter(sensitivity_range == sensitivity_range_j) %>%
        # Extract all values of the physiological indicator from the original dataset
        define_distribution(.
                            , dataset_long
                            , "pi") %>%
        ungroup(.)
      # print(str(dataset_distributions_i_j))
      
      # For analyses applied on the NHANES population, determine the novemtile with the minimum hazard ratio
      if(name_motif == "all")
      {
        # Extract the regression coefficients for the physiological indicator for a given sensitivity analysis 
        dataset_coeff_i_j <- list_stats[["coefficients"]] %>%
          # Include statistics for a given physiological indicator 
          filter(pi == pi_i) %>%
          # Include statistics for a given sensitivity analysis
          filter(sensitivity_range == sensitivity_range_j) #%>%
        
        # Define the boundaries of the values of the physiological indicator for each novemtile
        dataset_novemtiles_i_j <- dataset_distributions_i_j %>%
          define_range_of_novemtile(.
                                    , dataset_coeff_i_j
                                    , "pi") %>%
          # Include statistics pertaining to the novemtile with the minimum hazard ratio
          filter(estimate == min(estimate))
        
        # Define dataset that contains the statistics for the novemtile with the minimum hazard ratio  
        dataset_novemtiles_ref <- dataset_novemtiles_i_j %>%
          # Select the pertinent columns
          dplyr::select(pi, sensitivity_range, treatment_on_pi, novemtile_name) %>%
          # Redefine the novmetile_name column as "ref" to help with mergining with that of the splines dataset
          mutate(ref = as.numeric(novemtile_name)) %>%
          # Exclude the novemtile_name column 
          dplyr::select(pi, sensitivity_range, treatment_on_pi, ref)
      }

      # Define a dataset of predicted risk from the spline model for a physiological indicator and a given 
      # sensitivity analysis
      dataset_splines_i_j <- dataset_stats %>%
        # Include statistics for a given physiological indicator 
        filter(pi == pi_i) %>%
        # Include statistics for a given sensitivity analysis
        filter(sensitivity_range == sensitivity_range_j) %>%
        # # Include statistics for the splines model
        filter(treatment_on_pi == "splines")
      # View(dataset_splines_i_j)

      # Apply to determine the critical points of the physiological indicator to help identify the 
      # measurement pertaining to the minimum hazard ratio
      features_splines <- features(x = dataset_splines_i_j$pi_value
                                   , y = dataset_splines_i_j$hazard_ratio) 
      # print(features_splines)
      # print(nrow(dataset_splines_i_j))

      # If the sensitivity range include all participants, then eliminate the outliers from the dataset 
      # of harzard for each measurement
      if(sensitivity_range_j == "00_100")
      {
        # Determine the outliers
        outliers_pi_values <- boxplot.stats(dataset_splines_i_j$pi_value)$out

        # Exclude the outliers
        dataset_splines_i_j <- dataset_splines_i_j %>%
          filter(!(pi_value %in% outliers_pi_values))
      }

      # Determine slope at each measurement using the values of a measurement that is adjacent
      deriv_first <- diff(dataset_splines_i_j$hazard_ratio)/diff(dataset_splines_i_j$pi_value)
      
      # Include a column for the slope and for the how much the slope changes with each measurement
      dataset_splines_i_j <- dataset_splines_i_j %>%
        arrange(pi_value) %>%
        unique(.) %>%
        mutate(slope = abs(c(NA, deriv_first) - 0)) %>%
        mutate(neighborhood_diff = abs(c(diff(.$slope), NA))) %>%
        na.omit(.)
      # View(dataset_splines_i_j )
      
      # Extract the critical points 
      critical_points_x <- features_splines[["cpts"]]
      # print(critical_points_x)
      
      # Determine the number of critical points
      num_cpts <- length(critical_points_x)

      # Determine the measurements of physiological indicator corresponding to the minimum hazard ratio
      if(name_motif == "all")
      {
        # Determine these measurements for the splines and use the novemtiles results to assess the alignment
        # between the measurements
        min_pi_splines <- determine_pi_value_with_min_hr_splines(critical_points_x
                                                                 , dataset_splines_i_j
                                                                 , dataset_novemtiles_i_j
                                                                 , NA
                                                                 , analysis_type)

      } else if(name_motif == "gender") {

        # Determine these measurements for the splines model
        min_pi_splines <- determine_pi_value_with_min_hr_splines(critical_points_x
                                                                 , dataset_splines_i_j
                                                                 , NA
                                                                 , NA
                                                                 , analysis_type
                                                                 )
        

      }
      # print(min_pi_splines)

      # Define a dataframe containing the codename of the physiological indicator, the sensitivity analysis,
      # the treatment on the physiological indicator, and the measurements corresponding to the minimum hazard ratio
      dataset_splines_linear_ref <- data.frame(pi = pi_i
                                               , sensitivity_range = sensitivity_range_j
                                               , treatment_on_pi = c("splines", "linear")
                                               , ref = rep(min_pi_splines, 2))
      # print(dataset_splines_linear_ref)

      
      if(name_motif == "all")
      {
        # Combine the measurements from the splines and novemtiles models together if the analysis is applied on the 
        # NHANES population
        dataset_ref <- full_join(dataset_splines_linear_ref
                                 , dataset_novemtiles_ref
                                 , by = colnames(dataset_splines_linear_ref))

      } else if(name_motif == "gender") {

        # No changes needed since the data from the novemtiles models are not available for the sex-stratified analyses
        dataset_ref <- dataset_splines_linear_ref

      }
      # print(dataset_ref)
      
      # Store all the datasets with the new reference points in a list
      list_ref[[counter]] <- dataset_ref

      # Increment the counter
      counter <- counter + 1
      
    }
    
  }
  
  # Define a function that joins the datasets together by column names 
  joining_by_cols <- function(x, y) full_join(x, y, by = colnames(x))

  # Merge all the datasets together 
  dataset_merged_ref <- list_ref %>%
    reduce(joining_by_cols)

  return(dataset_merged_ref)
  
}