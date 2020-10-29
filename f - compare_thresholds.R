#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##################  FUNCTION TO COMPARE CLINICAL WITH MEASUREMENTS WITH HAZARD RATIO AT 1.1 ###################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function compare the clinical thresholds with with the measurements with hazard ratio at 1.1 
#          for the entire population and each sex
#
# Inputs: dataset_thresholds - dataset containing the range of the clinical thresholds for every physiological
#                              indicator.
#         list_regression_stats - list containing the regression results for the entire population
#         dataset_references - dataset containing the median measurement or the reference novemtile for every 
#                              physiological indicator.
#         list_regression_stats_sex - list containing the sex-specific regression results 
#         list_dataset_referencs_sex - list containing the median measurement for every sex and every 
#                                      physiological indicator.
#         dataset_body_systems - dataset showing the correspondence between the codename, name, and body system
#                                for every physiological indicator
#         analysis_type - string indicating whether the results are for the "unweighted" or "weighted" results
#
# Outputs: dataset_comparison - dataframe of measurements and clinical thresholds for all physiological
#                               indicators and sensitivity ranges

compare_thresholds <- function(dataset_thresholds
                               , list_regression_stats
                               , dataset_references
                               , list_regression_stats_sex
                               , list_dataset_referencs_sex
                               , dataset_body_systems
                               , analysis_type)
{
  # Extract the dataset of regression results for a given analysis type
  df_predicted_risk_sex <- list_regression_stats_sex[[analysis_type]]
  # print(str(df_predicted_risk_sex))
  
  # Extract the reference points for every physiological indicator 
  df_references_sex <- list_dataset_referencs_sex[[analysis_type]]
  # print(str(df_references_sex))
  
  # Define a dataset showing the range for the lower and upper bound of the thresholds for every population 
  dataset_thresholds_updated <- dataset_thresholds %>%
    # Define a new column containing the range of thresholds for the lower and upper bounds
    mutate(clinical_thresholds = paste(min_threshold
                                       , "-"
                                       , max_threshold
                                       , sep = " ")) %>%
    # Ensure that the clinical thresholds that are NA show up as blank
    mutate(clinical_thresholds = case_when(is.na(min_threshold) == TRUE ~ ""
                                           , min_threshold == max_threshold ~ gsub("-[0-9.]+$"
                                                                                   , ""
                                                                                   , clinical_thresholds)
                                           , min_threshold != max_threshold ~ clinical_thresholds
                                           )) %>%
    # Select the pertinent columns
    dplyr::select("pi", "direction_final", "population", "clinical_thresholds") %>%
    ungroup(.) %>%
    mutate(direction_final = gsub("\\s", "_", direction_final) %>%
             paste("clinical", ., sep = "_")) %>%
    # Format from long to wide 
    spread(., direction_final, clinical_thresholds) %>%
    # Select columns that don't contain only NA's or missingness 
    dplyr::select("pi", "population", "clinical_lower_threshold", "clinical_upper_threshold")
  # View(dataset_thresholds_updated)

  # Extract dataset of predicted mortality risk from analysis on entire population
  dataset_predicted_risk <- list_regression_stats[["predicted_risk"]]

  # Determine codenames of the selected physiological indicators
  codenames_pi <- dataset_predicted_risk %>%
    dplyr::select(pi) %>%
    unique(.) %>%
    unlist(.
           , use.names = FALSE)
  
  # # Define a vector to hold the indices of some physiological indicator for debugging
  # index_one <- which(codenames_pi %in% c(#"BPXPLS"
  #                                        "LBXCRP"
  #                                        , "BMXBMI"
  #                                        , "VNAVEBPXDI"
  #                                        ))
  # 
  # codenames_pi <- codenames_pi[index_one]
  
  # Determine the number of selected physiological indicators
  num_pi <- length(codenames_pi)
  
  sensitivity_ranges <- dataset_predicted_risk %>%
    dplyr::select(sensitivity_range) %>%
    unique(.) %>%
    unlist(.
           , use.names = FALSE)
  
  num_sensitivity_analyses <- length(sensitivity_ranges)

  # Initialize an empty list
  list_associations_thresholds <- list()

  counter <- 1
  
  # Dataset manually developed to contain the measurements of when the harzard ratio is 1.1. These measurements 
  # may be missing from the automatic process of detecting these points.
  df_extra_measurements <- read_excel("NHANES - Dataset of Physiological Measurements with Hazard Ratio at 1.1.xlsx"
                                      , sheet = analysis_type)
  
  seq(num_sensitivity_analyses)
  for(j in 2)
  {
    sensitivity_j <- sensitivity_ranges[j]
    print(sensitivity_j)
    
    for(i in seq(num_pi))
    {
      # Determine the codename of the selected physiological indicator
      pi_i <- codenames_pi[i]
      print(pi_i)
      
      # Define a dataset of predicted mortality risk for the sex-stratified analysis using participants with 
      # measurements within the 1st and 99th percentiles
      df_stats_sex_i <- df_predicted_risk_sex %>%
        filter(sensitivity_range == sensitivity_j) %>%
        filter(treatment_on_pi == "splines") %>%
        filter(pi == pi_i)
      # print(str(df_stats_sex_i))
      
      # Define a dataset of the reference points for the sex-stratified analysis using participants with 
      # measurements within the 1st and 99th percentiles
      df_ref_sex_i <- df_references_sex %>%
        filter(sensitivity_range == sensitivity_j) %>%
        filter(treatment_on_pi == "splines") %>%
        filter(pi == pi_i)
      # print(str(df_ref_sex_i))
      
      # Define a dataset of the reference point for the spline model for the entire population using  
      # participants with measurements within the 1st and 99th percentiles
      df_refs_i <- dataset_references %>%
        filter(sensitivity_range == sensitivity_j) %>%
        filter(treatment_on_pi == "splines") %>%
        filter(pi == pi_i)
      # print(df_refs_i)
      
      # Extract the reference point for a given physiological indicator
      ref_point_i <- df_refs_i %>%
        dplyr::select(ref) %>%
        unlist(., use.names = FALSE)
      # print(ref_point_i)
      
      predicted_risk_i_j <- list_regression_stats[["predicted_risk"]] %>%
        filter(sensitivity_range == sensitivity_j) %>%
        filter(treatment_on_pi == "splines") %>%
        filter(pi == pi_i)
      
      # Extract the measurements that have a hazard ratio of 1.1
      list_of_features_i <- predicted_risk_i_j %>%
        define_hr_and_measurements_at_increased_risk(.)
      # print(list_of_features_i)
    
      no_measurements_with_hr_1.1 <- any(is.na(list_of_features_i$df_measurements))
      # print(no_measurements_with_hr_1.1)
      
      pi_in_df_extra <- pi_i %in% df_extra_measurements$pi 
      # print(pi_in_df_extra)
      
      if(pi_in_df_extra == TRUE)
      {

        # Extract these measurements from the manually developed dataset as store it as the dataset containing 
        # the measurements with harzard ratios at 1.1
        df_extra_measurements_i <- df_extra_measurements %>%
          filter(pi == pi_i) %>%
          filter(sensitivity_range == sensitivity_j) %>%
          dplyr::select(x, y) %>%
          mutate(pi = rep(pi_i, nrow(.)))
        # print(df_extra_measurements_i)
        
        # If the physiological indicator doesn't have measurements, then use the subset of extra measurements
        if(no_measurements_with_hr_1.1 == TRUE)
        {
          dataset_association_thresholds_i <- df_extra_measurements_i
          
        # If the physiological indicator does have measurements, then merge the subset of extra measurements 
        # with the measurements
        } else {
          
          dataset_association_thresholds_i <- list_of_features_i[["df_measurements"]] %>%
            mutate(pi = rep(pi_i, nrow(.))) 
          
          dataset_association_thresholds_i <- dataset_association_thresholds_i %>%
            full_join(.
                      , df_extra_measurements_i
                      , by = colnames(.))
          
        }
        

        
        # For physiological indicators that aren't missing any measurments when the harzard ratio is 1.1, add 
        # a new column for the codename of the indicator
      } else {
        
        dataset_association_thresholds_i <- list_of_features_i[["df_measurements"]] %>%
          mutate(pi = rep(pi_i, nrow(.)))
      }
      # print(dataset_association_thresholds_i)
      
      num_rows_df_association_thresholds_i <- dim(dataset_association_thresholds_i)[1]
      # print(num_rows_df_association_thresholds_i)
      
      if(num_rows_df_association_thresholds_i == 0)
      {
        dataset_association_thresholds_i <- data.frame(x = NA
                                                       , y = NA
                                                       , pi = pi_i
                                                       , stringsAsFactors = FALSE)
      } else {
        dataset_association_thresholds_i <- dataset_association_thresholds_i
      }
      
      # Automatically determine whether the thresholds are lower or upper bound by determine the slope from the 
      # reference point to the measurement at harzard ratio is 1.1
      dataset_association_thresholds_i <- dataset_association_thresholds_i %>%
        mutate(ref = rep(ref_point_i, nrow(.))) %>%
        # Determine slope from the reference point to the measurement at harzard ratio is 1.1
        mutate(slope = (y-1)/(x-ref)) %>%
        # If the slope is negative, then the thresholds is a lower point, and positive slope implies that its 
        # an upper bound
        mutate(direction_final = case_when(slope < 0 ~ "lower threshold"
                                           , slope > 0 ~ "upper threshold")) #%>%
        
      # print(dataset_association_thresholds_i)
      
      # Define whether there should be a range for measurements with hazard ratios equal to 1.1
      dataset_association_thresholds_i <- define_measurements_part_of_range(dataset_association_thresholds_i
                                                                            , predicted_risk_i_j) %>%
      # Select pertinent columns
      dplyr::select(x, y, pi, direction_final) %>%
      # Round the measurements to two digits for ease of reading in the manuscript
      mutate(x = round(x, digits = 2))
      # print(dataset_association_thresholds_i)
      
      # Determine whether a physiological indicator has an upper, lower, or both bounds for threshold
      unique_types_thresholds <- dataset_association_thresholds_i$direction_final %>%
        unique(.) %>%
        unlist(., use.names = FALSE)
      # print(unique_types_thresholds)

      # Determine the number of bounds for a threshold
      num_types_thresholds <- length(unique_types_thresholds)

      # A given bound may have a range, so calculate the range for the lower and upper bound
      if(num_types_thresholds == 2)
      {
        dataset_association_thresholds_i <- dataset_association_thresholds_i %>%
          mutate(min_threshold = case_when(direction_final == "lower threshold" ~ min(subset(.
                                                                                             , direction_final == "lower threshold")$x)
                                           , direction_final == "upper threshold" ~ min(subset(.
                                                                                               , direction_final == "upper threshold")$x))) %>%
          mutate(max_threshold = case_when(direction_final == "lower threshold" ~ max(subset(.
                                                                                             , direction_final == "lower threshold")$x)
                                           , direction_final == "upper threshold" ~ max(subset(.
                                                                                               , direction_final == "upper threshold")$x)))
        # View(dataset_association_thresholds_i)

      } else if(num_types_thresholds == 1) {

        dataset_association_thresholds_i <- dataset_association_thresholds_i %>%
          mutate(min_threshold = min(x)) %>%
          mutate(max_threshold = max(x))

      }
      # print(dataset_association_thresholds_i)

      # Define a dataset of the association-based thresholds for the entire NHANES population
      dataset_association_thresholds_i <- dataset_association_thresholds_i  %>%
        # Select pertinent column
        dplyr::select(pi, direction_final, min_threshold, max_threshold) %>%
        # Do this to eliminate rows with duplicate data
        unique(.) %>%
        # Add a column indicate that these thresholds pertain the entire NHANES population
        mutate(population = rep("all", nrow(.)))
      # print(dataset_association_thresholds_i)

      dataset_association_thresholds_sex <- compare_threshold_sex(df_stats_sex_i
                                                                  , pi_i
                                                                  , df_ref_sex_i
                                                                  , sensitivity_j
                                                                  , analysis_type)
      # print(dataset_association_thresholds_sex)

      dataset_association_thresholds_i <- dataset_association_thresholds_i %>%
        full_join(.
                  , dataset_association_thresholds_sex
                  , by = colnames(.)) %>%
        mutate(sensitivity_range = rep(sensitivity_j, nrow(.)))
      # print(dataset_association_thresholds_i)
      
      dataset_clinical_thresholds_i <- dataset_thresholds %>%
        filter(pi == pi_i) 
      
      index_colnames_thresholds <- grepl("threshold", colnames(dataset_clinical_thresholds_i))
      col_names_df_c_thresholds <- colnames(dataset_clinical_thresholds_i)
      colnames(dataset_clinical_thresholds_i)[index_colnames_thresholds] <- paste("clinical"
                                                                                  , col_names_df_c_thresholds[index_colnames_thresholds]
                                                                                  , sep = "_")
      # print(dataset_clinical_thresholds_i)
      
      dataset_thresholds_i <- dataset_association_thresholds_i %>%
        left_join(.
                  , dataset_clinical_thresholds_i
                  , by = c("pi", "direction_final", "population")) %>%
        mutate(min_diff = (min_threshold - clinical_max_threshold)) %>%
        mutate(max_diff = (max_threshold - clinical_min_threshold)) #%>%
        # mutate(min_diff_corrected = ifelse(abs(min_diff) < abs(max_diff), min_diff, max_diff)) %>%
        # mutate(max_diff_corrected = ifelse(abs(min_diff) > abs(max_diff), min_diff, max_diff)) 
      # View(dataset_thresholds_i)

      list_associations_thresholds[[counter]] <- dataset_thresholds_i

      counter <- counter + 1
      
    }
  }
  # Define a function to join the dataset by the column names
  joining_by_colnames <- function(x, y) full_join(x, y, by = colnames(x))

  # Merge all the datasets together
  dataset_association_thresholds <- list_associations_thresholds %>%
    reduce(joining_by_colnames)  %>%
    # Define the rane of the thresholds
    mutate(associations_thresholds = paste(min_threshold
                                       , "-"
                                       , max_threshold
                                       , sep = " ")) %>%
    mutate(differences = paste(min_diff %>% round(., digits = 2)
                               , "-"
                               , max_diff  %>% round(., digits = 2)
                               , sep = " ")) %>%
    mutate(associations_thresholds = case_when(is.na(min_threshold) == TRUE ~ ""
                                           , min_threshold == max_threshold ~ gsub("\\s-\\s[0-9.]+$","", associations_thresholds)
                                           , min_threshold != max_threshold ~ associations_thresholds)) %>%
    mutate(differences = case_when(is.na(min_diff) == TRUE ~ ""
                                   , min_diff == max_diff ~ gsub("\\s-\\s[0-9.]+$","", differences)
                                   , min_diff != max_diff ~ differences)) %>%
    dplyr::select("pi", "sensitivity_range", "direction_final", "population", "associations_thresholds", "differences") %>%
    gather(., threshold_type, value, associations_thresholds:differences) %>%
    mutate(thres_type_bound = gsub("\\s", "_", paste(threshold_type, direction_final, sep = "_"))) %>%
    dplyr::select("pi", "sensitivity_range","population", "thres_type_bound", "value") %>%
    spread(., thres_type_bound, value)
  # View(dataset_association_thresholds)


  # Determine the row where the population is not labeled
  index_na_clinical <- which(is.na(dataset_thresholds_updated$population) == TRUE)

  # Label the population for these row as "all"
  dataset_thresholds_updated[index_na_clinical,"population"] <- "all"

  # Merge the clinical thresholds with the measurements with hazard ratios at 1.1
  dataset_comparison <- full_join(dataset_thresholds_updated
                                  , dataset_association_thresholds
                                  , by = c("pi", "population"))
  # print(colnames(dataset_comparison))


  # Merge in the name and body system of the physiological indicators
  dataset_comparison <- right_join(dataset_comparison
                                   , dataset_body_systems
                                   , by = "pi")
  
  # View(dataset_comparison[,c("clinical_lower_threshold"
  #                            , "clinical_upper_threshold"
  #                            , "associations_thresholds_lower_threshold"
  #                            , "associations_thresholds_upper_threshold"
  #                            , "differences_lower_threshold"
  #                            , "differences_upper_threshold"
  #                            , "pi_names")])

  write.csv(dataset_comparison
            , file = "NHANES - Comparison of Association-based and Clinical Thresholds 1b.csv")

  return(dataset_comparison)
  
}