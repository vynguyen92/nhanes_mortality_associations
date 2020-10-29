#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############################  FUNCTION TO DETERMINE THE RANGES OF THE NOVEMTILE  #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determine the ranges of the novemtiles and incorporate that information into the 
#          dataset of regression statistics for the novemtiles model
#          
# Inputs: dataset_risk_splines - dataframe of measurements for a given physiological indicator
#         dataset_coefficients - dataset of regression statistics for linear, novemtiles, and splines model
#         name_motif - string to differentiate the results derived from analyses on the physiological indicators 
#                      vs age
#         references_dataset - dataframe of reference points. Defaults to NA.
#         type_analysis - string indicated whether the survey weights were accounted (weighted) or not 
#                         (unweighted)
#
# Outputs: dataset_novemtiles_updated - a dataframe of the regression coefficients and ranges of the novemtiles
#

define_range_of_novemtile <- function(x
                                      , dataset_coefficients
                                      , name_motif
                                      , type_analysis
                                      , references_dataset = NA)
{
  
  
  
  # Extract the coefficients pertaining to the physiological indicator
  if(name_motif == "pi")
  {
    
    # Determine the given sensitivity range 
    sensitivity_range_j <- unique(x$sensitivity_range)
    
    # Determine the given physiological indicator
    indicator_i <- unique(dataset_coefficients$pi)
    
    dataset_coefficients_i <- dataset_coefficients %>%
      filter(sensitivity_range == sensitivity_range_j) %>%
      filter(pi == indicator_i) %>%
      filter(grepl("^novemtiles", term) == TRUE)
    # print(dataset_coefficients_i)
    
    # Determine the ranges of each novemtile
    ntiles <- quantile(x$pi_value
                       , na.rm = TRUE
                       , probs = seq(0, 1, by = 1/9))
    
    
  # Extract the coefficients pertaining to age  
  } else if(name_motif == "demo") {
    
    # Determine the given sensitivity range 
    sensitivity_range_j <- unique(dataset_coefficients$sensitivity_range)
    
    indicator_i <- unique(dataset_coefficients$population_type)
    
    dataset_coefficients_i <- dataset_coefficients %>%
      filter(grepl("^novemtiles", term) == TRUE)
    
    # Determine the ranges of each novemtile
    ntiles <- quantile(x$age
                       , na.rm = TRUE
                       , probs = seq(0, 1, by = 1/9))
    
  }
  
  # Ensure that there are any novemtiles with the same lower and upper bound
  ntiles <- unique(ntiles)
  # print(ntiles)

  # If there isn't a reference dataset, then use the 5th novemtile as the reference
  if(anyNA(references_dataset) == TRUE)
  {
    middle_index <- ceiling(length(ntiles)/2)

  # Else, extract the number of the reference group from the datasets of reference points
  } else {

    middle_index <- references_dataset %>%
      filter(pi == indicator_i) %>%
      filter(sensitivity_range == sensitivity_range_j) %>%
      filter(treatment_on_pi %in% "novemtiles") %>%
      dplyr::select(ref) %>%
      unlist(., use.names = FALSE) %>%
      as.numeric(.)
  }
  # print(middle_index)

  # Determine the lower and upper bounds of the novemtiles
  lower_bounds_of_n_tiles <- ntiles[1:(length(ntiles)-1)]
  upper_bounds_of_n_tiles <- ntiles[2:length(ntiles)]
  
  # Determine the lower and upper bound of the reference novemtile
  middle_lower_bound <- lower_bounds_of_n_tiles[middle_index]
  middle_upper_bound <- upper_bounds_of_n_tiles[middle_index]
  # Determine the mean of the bounds of the reference novemtile
  middle_mean <- mean(c(middle_lower_bound, middle_upper_bound))

  # Exclude the bounds of the reference novemtile for ease of mergining with the dataset of coefficients,
  # which does not have results for the reference novemtile
  lower_bounds_of_n_tiles <- lower_bounds_of_n_tiles[-middle_index]
  upper_bounds_of_n_tiles <- upper_bounds_of_n_tiles[-middle_index]


  dataset_novemtiles_updated <- dataset_coefficients_i %>%
    # Incorporate the lower and upper bounds of the novemtiles
    mutate(lower_bounds = lower_bounds_of_n_tiles
           , upper_bounds = upper_bounds_of_n_tiles) %>%
    # Calculate the mean measurement of the novemtiles
    mutate(mean_within_ntile = (lower_bounds_of_n_tiles+upper_bounds_of_n_tiles)/2) %>%
    # Include a column containing the number of the novemtile
    mutate(novemtile_name = gsub("novemtiles_pi_"
                                 , ""
                                 , term))
  # print(colnames(dataset_novemtiles_updated))
  
  # Add a row containing the information for the reference novemtile
  if(name_motif == "pi")
  {
    if(type_analysis == "weighted")
    {
      dataset_novemtiles_updated <- dataset_novemtiles_updated %>%
        add_row(pi = indicator_i
                , treatment_on_pi = "novemtiles"
                , sensitivity_range = sensitivity_range_j
                , term = paste("novemtiles_pi_", middle_index, sep = "")
                , estimate = 0
                , std.error = NA
                , robust.se = NA
                , statistic = NA
                , p.value = NA
                , conf.low = NA
                , conf.high = NA
                , lower_bounds = middle_lower_bound
                , upper_bounds = middle_upper_bound
                , mean_within_ntile = middle_mean
                , novemtile_name = toString(middle_index))

    } else if(type_analysis == "unweighted") {

      dataset_novemtiles_updated <- dataset_novemtiles_updated %>%
        add_row(pi = indicator_i
                , treatment_on_pi = "novemtiles"
                , sensitivity_range = sensitivity_range_j
                , term = paste("novemtiles_pi_", middle_index, sep = "")
                , estimate = 0
                , std.error = NA
                , statistic = NA
                , p.value = NA
                , conf.low = NA
                , conf.high = NA
                , lower_bounds = middle_lower_bound
                , upper_bounds = middle_upper_bound
                , mean_within_ntile = middle_mean
                , novemtile_name = toString(middle_index))
    }
  } else if(name_motif == "demo") {
    
    if(type_analysis == "weighted")
    {

      dataset_novemtiles_updated <- dataset_novemtiles_updated %>%
        add_row(population_type = indicator_i
                , treatment_on_demo = "novemtiles"
                , sensitivity_range = sensitivity_range_j
                , term = paste("novemtiles_age_", middle_index, sep = "")
                , estimate = 0
                , std.error = NA
                , robust.se = NA
                , statistic = NA
                , p.value = NA
                , conf.low = NA
                , conf.high = NA
                , lower_bounds = middle_lower_bound
                , upper_bounds = middle_upper_bound
                , mean_within_ntile = middle_mean
                , novemtile_name = toString(middle_index))

    } else if(type_analysis == "unweighted") {

      dataset_novemtiles_updated <- dataset_novemtiles_updated %>%
        add_row(population_type = indicator_i
                , treatment_on_demo = "novemtiles"
                , sensitivity_range = sensitivity_range_j
                , term = paste("novemtiles_age_", middle_index, sep = "")
                , estimate = 0
                , std.error = NA
                , statistic = NA
                , p.value = NA
                , conf.low = NA
                , conf.high = NA
                , lower_bounds = middle_lower_bound
                , upper_bounds = middle_upper_bound
                , mean_within_ntile = middle_mean
                , novemtile_name = toString(middle_index))
    }

  }
  
    

  return(dataset_novemtiles_updated)
}