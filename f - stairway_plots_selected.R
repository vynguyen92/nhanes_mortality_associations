#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# FUNCTION TO MAKE PNAEL STAIRWAY PLOT FOR SELECTED PHYSIOLOGICAL INDICATOR ###################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of the stariway plot of mortality risk across the distribution of 
#          the selected physiological indicators for each sensitivity analysis.
#
# Inputs: list_regression_stats - a list of dataframe of the regression results. A dataframe is available each  
#                                 to contain the coefficients, prediction performance, and predicted risk
#         dataset_thresholds - dataframe of the clinical thresholds
#         dataset_long - long-formatted dataset based on the physiological indictors
#         pi_selected - string vector of the codenames of the selected physiological indicators
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         name_of_folder - string of the name of the new folder to hold the plots
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         df_body_systems - dataframe of the body system of the physiological indicators 
#         dataset_refs - default to NA. If specified, then it's a dataframe of reference groups
#
# Outputs: none - png and pdf versions of the a panel plot of the stariway plots
#

stairway_plots_selected <- function(list_regression_stats
                                    , dataset_thresholds
                                    , dataset_long
                                    , pi_selected
                                    , type_of_analysis
                                    , current_directory
                                    , name_of_folder
                                    , df_body_systems
                                    , dataset_refs = NA)
{
  # Read in dataset to indicate the position of the measurements when the hazard ratio is at a 10% increase
  # from the minimum mortality risk
  df_extra_measurements <- read_excel("NHANES - Dataset of Physiological Measurements with Hazard Ratio at 1.1.xlsx"
                                      , sheet = type_of_analysis)
  
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()

  # Make a new folder if the folder doesn't exist
  if(name_of_folder %in% all_files_in_current_directory)
  {

  } else {
    dir.create(name_of_folder)
  }

  # Define a string for the working directory for the new folder
  new_working_directory <- paste(current_directory
                                 , name_of_folder
                                 , sep = "/")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  # Include the body systems for selected physiological indicators
  df_body_systems_selected <- df_body_systems %>%
    filter(pi %in% pi_selected)
  
  # Determine the names of the selected physiological indicators
  names_pi <- df_body_systems_selected %>%
    dplyr::select(pi_names) %>%
    unlist(.
           , use.names = FALSE)
  
  # Determine codenames of the selected physiological indicators
  codenames_pi <- df_body_systems_selected %>%
    dplyr::select(pi) %>%
    unlist(.
           , use.names = FALSE)
  
  # Determine the sensitivity analyses
  sensitivity_analyses <- list_regression_stats[["fit"]] %>%
    dplyr::select(sensitivity_range) %>%
    unique(.) %>%
    unlist(.
           , use.names = FALSE) 
  
  # Determine a given sensitivity analysis. This is for debugging purposes.
  # sensitivity_analyses <- sensitivity_analyses[2]
  
  # Extract pertinent legend boxes to snitch them together with the panel plot of stairway plots
  legend_boxes <- make_legend_boxes(list_regression_stats
                                    , dataset_thresholds)
  
  # Make a stairway plot for each combination of a physiological indicator and sensitivity analysis
  stairway_plot_each_pi_and_sensitivity_range(sensitivity_analyses
                                              , codenames_pi
                                              , names_pi
                                              , dataset_long
                                              , list_regression_stats
                                              , dataset_thresholds
                                              , dataset_refs
                                              , type_of_analysis
                                              , legend_boxes
                                              , df_extra_measurements
                                              , "panel")
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
  # stairway_plot_i
}