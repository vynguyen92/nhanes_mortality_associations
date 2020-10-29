#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### FUNCTION TO MAKE INDIVIDUAL STAIRWAY PLOT FOR EACH PHYSIOLOGICAL INDICATOR & SENSITIVITY ANALYSIS #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a stariway plot of mortality risk across the distribution of for each
#          combination of a physiological indicators and a sensitivity analysis.
#
# Inputs: list_regression_stats - a list of dataframe of the regression results. A dataframe is available each  
#                                 to contain the coefficients, prediction performance, and predicted risk
#         dataset_thresholds - dataframe of the clinical thresholds
#         dataset_long - long-formatted dataset based on the physiological indictors
#         pi_selected - string vector of the codenames of the selected physiological indicators
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         name_of_folder - string of the name of the new folder to hold the plots
#         df_body_systems - dataframe of the body system of the physiological indicators 
#         dataset_refs - default to NA. If specified, then it's a dataframe of reference groups
#
# Outputs: none - png and pdf versions of the stairway plot
#

stairway_plots_individual <- function(list_regression_stats
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
  
  # Create a folder with the name of the analysis type to separate the series of stairway plots between the
  # weighted and unweighted analyses
  dir.create(type_of_analysis)
  
  # Create the working directory for the new folder of the analysis type
  new_working_directory <- paste(current_directory
                                 , name_of_folder
                                 , type_of_analysis
                                 , sep = "/")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  # Determine codenames of the selected physiological indicators
  codenames_pi <- df_body_systems %>%
    dplyr::select(pi) %>%
    unlist(.
           , use.names = FALSE)
  
  # # Determine the index for a given physiological indicator. This is for debugging purposes.
  # index_one <- which(codenames_pi == "BMXBMI")
  # 
  # # Determine the codename for the given physiological indicator. This is for debugging purposes.
  # codenames_pi <- codenames_pi[index_one]
  
  # Determine the names of the selected physiological indicators
  names_pi <- df_body_systems %>%
    dplyr::select(pi_names) %>%
    unlist(.
           , use.names = FALSE)
  # names_pi <- names_pi[index_one]
  
  # Determine the sensitivity analyses
  sensitivity_analyses <- list_regression_stats[["fit"]] %>%
    dplyr::select(sensitivity_range) %>%
    unique(.) %>%
    unlist(.
           , use.names = FALSE) 
  
  # # Determine a given sensitivity analysis. This is for debugging purposes.
  # sensitivity_analyses <- sensitivity_analyses[2]
  
  # Extract pertinent legend boxes to snitch them together with the panel plot of stairway plots
  legend_boxes <- make_legend_boxes(list_stats = list_regression_stats
                                    , thresholds_dataset = dataset_thresholds
                                    , size_legend_title = 10
                                    , size_legend_text = 10)
  
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
                                              , "individual")
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
}