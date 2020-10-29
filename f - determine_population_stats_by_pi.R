#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############  FUNCTION TO DETERMINE STATISTICS TO INCLUDE PHYSIOLOGICAL INDICATORS IN OUR ANALYSIS  ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function merge all NHANES dataset together and include only variables that are need
#          
# Inputs: dataset_merged - data frame containing the demographics, physiological indicators, mortality info for 
#                          each participant 
#         dataset_responses - data frame containing measurements for the physiological indicators
#
# Outputs: stats_on_pi - a dataset of statistics (number of participants, number of cycles, number of unique 
#                        physiological measurements) by physiological indicator to determine which indicators to 
#                        include our analyses


determine_population_stats_by_pi <- function(dataset_merged
                                             , dataset_responses)
{
  # Determine the codenames pertaining to the physiological indicators
  codenames_pi <- intersect(colnames(dataset_merged)
                            , colnames(dataset_responses))
  codenames_pi <- codenames_pi[which(codenames_pi != "SEQN")]
   
  # Define a vector of codenames for the  demographic, outcome, and sampling design variables 
  variables_core <- c("MORTSTAT"
                      , "PERMTH_INT"
                      , "WTINT2YR"
                      , "SDMVPSU"      
                      , "SDMVSTRA"
                      , "RIDRETH1"
                      , "RIAGENDR"
                      , "RIDAGEYR"
                      , "SDDSRVYR")
  
  # Convert from wide to a long-formatted dataset
  # Select pertinent variables 
  dataset_long <- dataset_merged[,c(variables_core, codenames_pi)] %>%
    # Convert to long format based on the physiological indicators
    gather(., pi, pi_value, codenames_pi) %>%
    # Remove any participants with missing information
    na.omit(.) %>%
    # Remove participants with no follow-up data
    filter(PERMTH_INT != 0)
  
  # Define a dataset with the colnames and names of the physiological indicators
  labels_dataset_merged <- data.frame(pi = colnames(dataset_merged)
                                      , pi_names = get_label(dataset_merged))
  
  # Determine for each physiological indicator: the number of participants, number of study years, and number of 
  # unique measurements
  stats_on_pi <- dataset_long %>%
    group_by(pi) %>%
    # Determine the statistics for each physiological indicator
    summarise(num_participants = length(WTINT2YR)
              , cycle_length = length(unique(SDDSRVYR))
              , unique_cycle = toString(unique(SDDSRVYR))
              , num_unique_values = length(unique(pi_value))) %>%
    # Incorporate the name of the physiological indicators into the dataset
    left_join(.
              , labels_dataset_merged
              , by = "pi") %>%
    # Determine which indicators to include or exclude 
    mutate(include = if_else(num_participants >= 10000 
                               & cycle_length >= 6
                               & num_unique_values > 3, "Yes", "No"))
  
  return(stats_on_pi)
}