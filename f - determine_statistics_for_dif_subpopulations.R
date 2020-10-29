#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##########  FUNCTION TO DETERMINE POPULATION AND DISTRIBUTION STATISTICS BY DIFFERENT SUBPOPULATION  ##########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determines the population statistics for age, sex, race, and mortality status along  
#          with the distribution statistics for each physiological indicator on subpopulation specific to each 
#          participants with measurements for a given indicator.
#          
# Inputs: dataset_merged - dataframe containing the demographics, physiological indicators, mortality info for 
#                          each participant 
#         dataset_long - long-formatted dataframe based on the physiological indictors
#         dataset_body_systems - dataframe containing the name and body system of the physiological indicator
#
# Outputs: none - population statistics and distribution statistics are printed to the console, viewed in 
#                 another window, or outputted as a csv file
#                 

determine_statistics_for_dif_subpopulations <- function(dataset_merged
                                                        , dataset_long
                                                        , dataset_body_systems)
{
  # Determine the number of participants by gender for each physiological indicator
  dataset_stats_gender <- dataset_long %>%
    group_by(pi, gender) %>%
    summarise(counts = length(gender)) %>%
    ungroup(.) %>%
    mutate(cats = gender)

  # Determine the number of participants by race for each physiological indicator
  dataset_stats_race <- dataset_long %>%
    group_by(pi, race) %>%
    summarise(counts = length(race)) %>%
    ungroup(.) %>%
    mutate(cats = race)

  # Determine the number of participants by mortality status for each physiological indicator
  dataset_stats_mort <- dataset_long %>%
    group_by(pi, mortality_status) %>%
    summarise(counts = length(mortality_status)) %>%
    ungroup(.) %>%
    mutate(cats = case_when(mortality_status == 1 ~ "deceased"
                            , mortality_status == 0 ~ "alive"))

  # View(dataset_stats_mort)

  # Determine the number of participants with measurements for each physiological indicator
  dataset_sample_size <- dataset_long %>%
    group_by(pi) %>%
    summarise(sample_size = length(pi))


  # Form a dataset with the number and percentages of participants by category for each categorical variable
  # and for each physiological indicator
  dataset_cats <- dataset_stats_gender %>%
    # Join the datasets of sample size for gender and race
    full_join(.
              , dataset_stats_race
              , by = NULL) %>%
    # Join the datasets of sample sizes for gender, race, and mortality status 
    full_join(.
              , dataset_stats_mort
              , by = NULL) %>%
    # Select the pertinent column variables
    dplyr::select(pi, cats, counts) %>%
    # Include the total sample size
    full_join(.
              , dataset_sample_size
              , by = "pi") %>%
    # Calculate the percentages of participants by category for each categorical variable 
    mutate(percents = round(counts/sample_size*100, digits = 2)) %>%
    # Select the pertinent column variables 
    dplyr::select(pi, cats, counts, percents) %>%
    # Define a string column variable containing the sample size and percentages in one cell
    mutate(labels = paste(counts
                          , " ("
                          , percents
                          , ")"
                          , sep = "")) %>%
    # Select the pertinent column variables 
    dplyr::select(pi, cats, labels) %>%
    # Format to wide version to show the number and percentages of participants by the physiological indicators
    spread(., cats, labels) %>%
    # Select the variables to help rearrange the column variables 
    dplyr::select("pi"
                  , "alive"
                  , "deceased"
                  , "_female"
                  , "_male"
                  , "_mexican_american"
                  , "_other_hispanic"
                  , "_whites"
                  , "_blacks"
                  , "_other") %>%
    # Include information on the name of the physiological indicators
    left_join(.
              , dataset_body_systems
              , by = "pi")
  
  # View(dataset_cats)

  # Output the dataset of sample size and percentages by physiological indicator into a csv file
  write.csv(x = dataset_cats
            , file = "NHANES - Sample Size by Categorical Variables for Subpopulations.csv")

  # Define a dataset with participants who have complete data for age, sex, and race
  dataset_all_nhanes <- dataset_merged %>%
    select(RIAGENDR, RIDAGEYR, RIDRETH1) %>%
    na.omit(.)

  # Determine the number of participants by age, sex, and race
  list_all_nhanes <- sapply(dataset_all_nhanes, summary.factor)
  print(list_all_nhanes)

  # Determine the percentage of participants by sex
  percents_gender <- round(list_all_nhanes$RIAGENDR/dim(dataset_all_nhanes)[1]*100
                           , digits = 2)
  print(percents_gender)

  # Determine the percentage of participants by race
  percents_race <- round(list_all_nhanes$RIDRETH1/dim(dataset_all_nhanes)[1]*100
                         , digits = 2)
  print(percents_race)

  # Determine the percentiles of age for each subpopulation specific to the physiological indicators
  dataset_sub_age <- dataset_long %>%
    group_by(pi) %>%
    summarise(min = min(age)
              , perc_01 = quantile(age, probs = 0.01)
              , perc_05 = quantile(age, probs = 0.05)
              , perc_10 = quantile(age, probs = 0.10)
              , median = quantile(age, probs = 0.5)
              , mean = mean(age)
              , perc_90 = quantile(age, probs = 0.90)
              , perc_95 = quantile(age, probs = 0.95)
              , perc_99 = quantile(age, probs = 0.99)
              , max = max(age)) %>%
    # Include information on the name of the physiological indicators
    left_join(.
              , dataset_body_systems
              , by = "pi")

  write.csv(x = dataset_sub_age
            , file = "NHANES - Percentiles of Age for Subpopulations.csv")

  # Determine the percentiles of age for the NHANES population
  dataset_nhanes_age <- dataset_all_nhanes %>%
    summarise(min = min(RIDAGEYR)
              , perc_01 = quantile(RIDAGEYR, probs = 0.01)
              , perc_05 = quantile(RIDAGEYR, probs = 0.05)
              , perc_10 = quantile(RIDAGEYR, probs = 0.10)
              , median = quantile(RIDAGEYR, probs = 0.5)
              , mean = mean(RIDAGEYR)
              , perc_90 = quantile(RIDAGEYR, probs = 0.90)
              , perc_95 = quantile(RIDAGEYR, probs = 0.95)
              , perc_99 = quantile(RIDAGEYR, probs = 0.99)
              , max = max(RIDAGEYR))

  # Determine the percentiles of follow-up time for each subpopulation specific to the physiological indicators
  dataset_sub_time_to_death <- dataset_long %>%
    group_by(pi) %>%
    summarise(min = min(time_to_death)
              , perc_01 = quantile(time_to_death, probs = 0.01)
              , perc_05 = quantile(time_to_death, probs = 0.05)
              , perc_10 = quantile(time_to_death, probs = 0.10)
              , median = quantile(time_to_death, probs = 0.5)
              , mean = mean(time_to_death)
              , perc_90 = quantile(time_to_death, probs = 0.90)
              , perc_95 = quantile(time_to_death, probs = 0.95)
              , perc_99 = quantile(time_to_death, probs = 0.99)
              , max = max(time_to_death)) %>%
    # Include information on the name of the physiological indicators
    left_join(.
              , dataset_body_systems
              , by = "pi")

  write.csv(x = dataset_sub_time_to_death
            , file = "NHANES - Percentiles of Time to Death for Subpopulations.csv")

  View(dataset_nhanes_age)
}