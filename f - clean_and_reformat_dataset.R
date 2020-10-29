#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####################  FUNCTION TO CLEAN AND REFORMAT THE NHANES DATASET INTO LONG FORMAT  #####################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function exclude any extraneous variables and then reformat the dataset into a long format based 
#          on the physiological indicators. A column for cycle-adjusted survey weights is included. 
#          
# Inputs: dataset_merged - data frame containing the demographics, physiological indicators, mortality info for 
#                          each participant 
#         dataset_stats - data frame containing the statistics and include criteria for each physiological 
#                         indicator
#
# Outputs: dataset_long - a long-formatted dataset of the pertinent demographics and mortality data by the 
#                         physiological indicator

clean_and_reformat_dataset <- function(dataset_merged
                                       , dataset_stats
                                       , include_additional_variables = "")
{
  # Determine the vector of codenames for which physiological indicators to include our analysis
  pi_include <- dataset_stats %>%
    filter(include == "Yes") %>%
    select(pi) %>%
    unlist(., use.names = FALSE)
  
  # Change the column name of pertinent variables for legibility 
  dataset_merged <- dataset_merged %>%
    mutate(mortality_status = MORTSTAT
           , time_to_death = PERMTH_INT
           , weights = WTINT2YR
           , cluster = paste(dataset_merged$SDMVPSU, dataset_merged$SDMVSTRA)
           , cycles = SDDSRVYR
           , age = RIDAGEYR
           , gender = relevel(factor(if_else(RIAGENDR == 1, "_male", "_female"))
                              , ref = "_male")
           , race = relevel(factor(case_when(RIDRETH1 == 1 ~ "_mexican_american"
                                             , RIDRETH1 == 2 ~ "_other_hispanic"
                                             , RIDRETH1 == 3 ~ "_whites"
                                             , RIDRETH1 == 4 ~ "_blacks"
                                             , RIDRETH1 == 5 ~ "_other" ))
                            , ref = "_whites")
           )
  
  # Define a vector of the new column names
  variables_core <- c("SEQN"
                      , "mortality_status"
                      , "time_to_death"
                      , "weights"
                      , "cluster"
                      , "cycles"
                      , "age"
                      , "gender"
                      , "race")
  
  # For analyses that involved smoking, define a column vector for smoking and include "smoking"
  # as pertinent variables to reformat the dataset from wide to long
  if(include_additional_variables == "smoking")
  {
    # Define a column vector with the column name of "smoking"
    dataset_merged <- dataset_merged %>%
      mutate(smoking = log10(LBXCOT))
    
    # Include smoking as a pertinent variable for reformatting the dataset
    variables_core <- variables_core %>%
      append(., "smoking")
  }

  # Convert from wide to long format
  # Select the pertinent variables
  dataset_long <- dataset_merged[,c(variables_core, pi_include)] %>%
    # Format long based on the physiological indicators
    gather(., pi, pi_value, pi_include) %>%
    # Remove participants with missing data
    na.omit(.) %>%
    # Remove participants with no follow-up data
    filter(time_to_death > 0) %>%
    # Include a column with the total number of cycles with data available for each physiological indicator
    left_join(.
              , dataset_stats[,c("pi", "cycle_length")]
              , by = "pi") %>%
    # Define a column for the cycle-adjusted survey weights
    mutate(adjusted_weights = (1/cycle_length)*weights)
  
  return(dataset_long)

}