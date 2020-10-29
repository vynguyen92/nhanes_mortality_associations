#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################  FUNCTION TO DEFINE A DATASET TO INDICATE THE POSITION OF Y-AXIS TICK MARKS  ################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a dataframe indicating the values to draw a dashed line to separate the 
#          physiological indicators in the alphabet soup plot. Tick marks are defined at very half value, i.e. 
#          1.5, 2.5, etc.
#
# Inputs: dataset_stats - dataset of prediction performance for a given fit measure
#
# Outputs: dataset_unique_body_system_pi - dataframe the codename, body system, and tick marks of the  
#                                          physiological indicators
#


define_interval_ticks <- function(dataset_stats)
{
  # Determine the number of physiological indicators by body system
  counts_by_body_system <- dataset_stats %>%
    dplyr::select(body_system_categories, pi_names) %>%
    unique(.)%>%
    group_by(body_system_categories) %>%
    summarise(counts = length(pi_names)) %>%
    ungroup(.)
  
  # Define a unique dataset of the codename and body systems of the physiological indicators 
  dataset_unique_body_system_pi <- unique(dataset_stats[,c("body_system_categories", "pi")])
  
  # Determine the number of body systems 
  num_body_system <- nrow(counts_by_body_system)
  
  for(i in seq(num_body_system))
  {
    # Determine the body system
    body_system_i <- counts_by_body_system$body_system_categories[i]
    
    # Extract the dataset corresponding to the physiological indicators within a given body system
    dataset_unique_body_system_pi_i <- dataset_unique_body_system_pi %>%
      filter(body_system_categories == body_system_i)
    
    # Determine the number of physiological indicator in a given body system
    num_pi_per_body_system <- nrow(dataset_unique_body_system_pi_i)
    
    # Define a numeric vector of tick marks to help separate the physiological indicators
    # These tick marks are 1.5, ..., n.5, where n is the total number of physiological indicators within a 
    # body system
    interval_ticks_i <- seq(num_pi_per_body_system) + 0.5
    
    # Combine the numeric vector of tick markers together across all physiological indicators
    if(i == 1)
    {
      interval_ticks <- interval_ticks_i
    } else {
      interval_ticks <- append(interval_ticks, interval_ticks_i)
    }
    
  }
  
  # Defnie a dataset of the codename, body system, and tick marks of the physiological indicators
  dataset_unique_body_system_pi <- dataset_unique_body_system_pi %>%
    arrange(body_system_categories) %>%
    mutate(interval_ticks = interval_ticks)
  
  return(dataset_unique_body_system_pi)
}