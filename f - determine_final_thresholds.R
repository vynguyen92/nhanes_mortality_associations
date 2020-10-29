#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###################  FUNCTION TO FINALIZE THE THRESHOLDS FOR EACH PHYSIOLOGICAL INDICATOR  ####################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determines the ranges of the lower and upper bounds of the clinical thresholds for  
#          all physiological indicators
#          
# Inputs: pi_thresholds - dataframe of clinical thresholds for each physiological indicator 
#
# Outputs: all_pi_thresholds - dataframe of the ranges of each bound of the clinical thresholds

determine_final_thresholds <- function(pi_thresholds)
{
  
  # Consolidate the direction to indicate whether the thresholds are upper or lower bounds
  pi_thresholds <- pi_thresholds %>%
    mutate(direction_final = case_when(direction %in% c(">", ">=") ~ "upper threshold"
                                       , direction %in% c("<", "<=") ~ "lower threshold"))

  # Determine the number of physiological indicators
  unique_pi <- unique(pi_thresholds$codename_pi)
  num_pi <- length(unique_pi)
  
  
  for(i in seq(num_pi))
  {
    # Determine the codename of the physiological indicator
    pi_codename <- unique_pi[i]
    # print(pi_codename)

    # Define a dataset to include the thresholds pertinent to the physiological indicator
    pi_thresholds_subset <- pi_thresholds %>%
      filter(codename_pi == pi_codename)

    # print(pi_thresholds_subset)

    # Determine the number of populations 
    # Population can include adult males, adult females, and NA (implying all)
    num_unique_populations <- pi_thresholds_subset %>%
      dplyr::select(population) %>%
      unique() %>%
      unlist(., use.names = FALSE) %>%
      length()

    # If sex-specific thresholds are available, then do not use the thresholds applicable to all
    if(num_unique_populations >= 3)
    {
      # pi_thresholds_female <- pi_thresholds_subset %>%
      #   filter(is.na(population)) %>%
      #   mutate(population = "adult females")
      #
      # pi_thresholds_male <- pi_thresholds_subset %>%
      #   filter(is.na(population)) %>%
      #   mutate(population = "adult males")
      #
      # pi_thresholds_subset <- pi_thresholds_subset %>%
      #   filter(!is.na(population)) %>%
      #   full_join(.
      #             , pi_thresholds_female
      #             , by = colnames(pi_thresholds_subset)) %>%
      #   full_join(.
      #             , pi_thresholds_male
      #             , by = colnames(pi_thresholds_subset))

      pi_thresholds_subset <- pi_thresholds_subset %>%
        filter(!is.na(population))

    }

    # Define a dataset of the finalized thresholds by determine the minimum and maximum of each lower or upper
    # bound
    pi_thresholds_final <- pi_thresholds_subset %>%
      group_by(direction_final
               , population) %>%
      # Determine the minimum and maximum of each lower or upper bound
      summarise(min_threshold = min(clinical_threshold, na.rm = TRUE)
                , max_threshold = max(clinical_threshold, na.rm = TRUE)) %>%
      # Replace infinite values with NA
      mutate(min_threshold = replace(min_threshold, is.infinite(min_threshold), NA)
             , max_threshold = replace(max_threshold, is.infinite(max_threshold), NA)) %>%
      # mutate(final_threshold = case_when(direction_final == "lower threshold" ~ max_threshold
      #                                    , direction_final == "upper threshold" ~ min_threshold)) #%>%
      # Change the column name of the codenames for ease of merging downstream
      mutate(pi = pi_codename) %>%
      # Select the pertinent columns 
      dplyr::select(pi, direction_final, population, min_threshold, max_threshold)

    
    # If population label is empty, then the threshold is applicable to all, so label accordingly
    if(unique(is.na(pi_thresholds_final$population)) == TRUE &
       unique(!is.na(pi_thresholds_final$direction_final == TRUE)))
    {
      pi_thresholds_final <- pi_thresholds_final %>%
        mutate(population = "all")
    }

    # View(pi_thresholds_final)

    # Merge the datasets of finalized thresholds across all physiological indicators
    if(i == 1)
    {
      all_pi_thresholds <- pi_thresholds_final

    } else {
      all_pi_thresholds <- all_pi_thresholds %>%
        full_join(.
                  , pi_thresholds_final
                  , by = colnames(pi_thresholds_final))
    }


  }

  return(all_pi_thresholds)
}