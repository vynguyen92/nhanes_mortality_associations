#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########  FUNCTION TO MAKE AN ALPHABET SOUP PLOT OF PREDICTION PERFORMANCE FOR EACH FIT MEASURE  ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates an alphabet soup plot to show the prediction performance across the models and
#          physiological indicators for the 3 fit meaures:  Nagelkerke R2, AIC, and Concordance Index. This is
#          faceted by the sensitivity analysis and the body system.
#
# Inputs: list_stats_pi -  a list of dataframe of the regression results. A dataframe is available each to 
#                          contain the coefficients, prediction performance, and predicted risk
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         pi_names_body_syst_cats - dataframe of the body system of the physiological indicators 
#
# Outputs: none - png and pdf versions of the alphabet soup plot of each fit measure are printed to a folder
# 

alphabet_soup_plot_fit_pi <- function(list_stats
                                      , type_of_analysis
                                      , current_directory
                                      , pi_names_body_syst_cats)
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the alphabet soup plots
  name_folder <- "Alphabet Soup Plots - Fit for Physiological Indicators"
  
  # Make a new folder if the folder doesn't exist
  if(name_folder %in% all_files_in_current_directory)
  {
    
  } else {
    dir.create(name_folder)
  }
  
  # Define a string for the working directory for the new folder
  new_working_directory <- paste(current_directory
                                 , name_folder
                                 , sep = "/")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  # Form a long-formated dataset containing a column of prediction performance for all fit measures 
  # for the sensitivity analysis involving th entire NHANES population and those within the 1st and 99th
  # percentiles 
  dataset_stats <- list_stats[["fit"]] %>%
    filter(sensitivity_range %in% c("00_100", "01_99")) %>%
    gather(., fit_stats, value, rsq_mean:concordance_std_error) %>%
    separate(fit_stats, c("fit", "stats_1", "stats_2"), sep = "_") %>%
    unite(., "stats", stats_1:stats_2) %>%
    mutate(stats = gsub("_NA$", "", stats)) %>%
    spread(., stats, value) %>%
    dplyr::select("pi", "treatment_on_pi", "sensitivity_range", "fit", "ci_high", "ci_low", "mean", "std_error") 
  

  # Include info on the body systems
  dataset_stats <- dataset_stats %>%
    full_join(.
              , pi_names_body_syst_cats
              , by = "pi")

  # Determine all fit measures
  fit_measures <- unique(dataset_stats$fit)
  # Determine the number of fit measures
  num_fit_measures <- length(fit_measures)

  # Determine the number of transformations
  treatments <- unique(dataset_stats$treatment_on_pi)
  # Determine the number of transformations
  num_treatments <- length(treatments)

  
  # For each fit measure, make an alphabet soup plot showing the prediction performance across the different
  # models and the physiological indicators
  for(i in seq(num_fit_measures))
  {
    # Determine the given fit measure
    fit_measures_i <- fit_measures[i]
    print(fit_measures_i)

    # Extract the dataset of statistics pertaining to this fit measure
    dataset_i <- dataset_stats %>%
      filter(fit == fit_measures_i)
    

    # Define a more descriptive name and the minimum and maximum value of the x-axis for each fit measure
    if(fit_measures_i == "rsq")
    {
      fit_name <- bquote("Nagelkerke" ~ R^2)
      axis_min <- 0
      axis_max <- max(dataset_i$ci_high)
      
    } else if(fit_measures_i == "concordance") {
      
      fit_name <- "Concordance Index"
      axis_min <- 0.5
      axis_max <- 1
      
      
    } else if(fit_measures_i == "AIC") {
      
      fit_name <- "Akaike Information Criterion"
      axis_min <- min(dataset_i$ci_low)
      axis_max <- max(dataset_i$ci_high)
      
    }
    
    # Convert a character vector of transformation names into a factor to ensure correct order
    dataset_i$treatment_on_pi <- factor(dataset_i$treatment_on_pi
                                        , levels = c("linear"
                                                     , "novemtiles"
                                                     , "splines"))

    # Specify the different type of sensitivity analyses
    dataset_i <- dataset_i %>%
      mutate(sensitivity_range = case_when(sensitivity_range == "00_100" ~ "All Participants"
                                           , sensitivity_range == "01_99" ~ "Participants in 1st and 99th Percentiles"))

    # Make this column vector as a factor to ensure the correct order of the sensitivity analyses
    dataset_i$sensitivity_range <- factor(dataset_i$sensitivity_range
                                          , levels = c("All Participants"
                                                       , "Participants in 1st and 99th Percentiles"))

    
    # Define a dataset of the mean prediction performance of all models for each physiological indicator and 
    # arrange in order of this mean value
    dataset_ordered_stats <- dataset_i %>%
      group_by(pi_names) %>%
      summarise(mean_stat = mean(mean)) %>%
      arrange(mean_stat)

    
    # Extract the codename of the physiological indicators ordered by the mean prediction performance
    ordered_al_comp_names <- dataset_ordered_stats[,"pi_names"] %>%
      unname(.) %>%
      unlist(.)
    
    # For the AIC, reverse the order, since smaller values imply better prediction fit
    if(fit_measures_i  == "AIC")
    {
      ordered_al_comp_names <- rev(ordered_al_comp_names)
    }

    # Refine a character vector of codenames as a factor to ensure the correct order of the physiological 
    # indicators
    dataset_i$pi_names <- factor(dataset_i$pi_names
                                         , levels = ordered_al_comp_names)
    

    # Define a dataset indicating the half mark of the y-axis to draw a dashed line to separate the 
    # physiological indicators
    dataset_intervals_ticks <- define_interval_ticks(dataset_i)

    # Merge the dataset of tick marks into the dataset of prediction performance 
    dataset_i <- dataset_i %>%
      full_join(.
                , dataset_intervals_ticks
                , by = c("pi", "body_system_categories"))

    # Make an alphabet soup plot to show the prediction performance across the models and the physiological 
    # indicators 
    plot_fit <- ggplot(dataset_i
                       , aes(x = pi_names
                             , y = mean
                             , color = treatment_on_pi
                             , shape = treatment_on_pi)) +
      geom_jitter(size = 5
                  , position = position_dodge(0.8)) +
      # Draw dashed line to separate the physiological indicators
      geom_vline(xintercept = unlist(dataset_i$interval_ticks)
                 , size = 0.3
                 , color = "gray"
                 , linetype = "longdash") +
      scale_x_discrete(expand = expand_scale(mult = 0, add = 0.5)) +
      ylab(fit_name) +
      # Plot the confidence interval of each prediction performance for each model 
      geom_errorbar(aes(ymax = ci_high
                         , ymin = ci_low)
                     , position = position_dodge(0.8)) +
      coord_flip() +
      # Facet the plot by body system and sensitivity range
      facet_grid(rows = vars(dataset_i$body_system_categories)
                 , cols = vars(dataset_i$sensitivity_range)
                 , scales = "free_y"
                 , space = "free" ) +
      scale_color_manual(name = "Types of Model"
                         , values = c("#CC0000", "#F0B707", "#000080")) +
      scale_shape_manual(name = "Types of Model"
                         , values = c(76, 78, 83, 32, 33)) +
      theme_bw() +
      theme(legend.position = "top"
            , legend.title = element_text(size = 12)
            , legend.text = element_text(size = 12)
            , strip.text = element_text(size = 12)
            , plot.title = element_text(hjust = 0.5)
            , panel.grid.major = element_blank()
            , panel.grid.minor = element_blank()
            , axis.title.y = element_blank()
            , axis.text.y = element_text(size = 12)
            , axis.text.x = element_text(size = 12)
            , axis.title.x = element_text(size = 12))
    
    # Define file names of the png and pdf versions of the alphabet soup plots
    plot_name.png <- paste("alphabet_soup_plot"
                           , type_of_analysis
                           , fit_measures_i
                           , "pi_mortality.png"
                           , sep = "_")
    plot_name.pdf <- paste("alphabet_soup_plot"
                           , type_of_analysis
                           , fit_measures_i
                           , "pi_mortality.pdf"
                           , sep = "_")

    # Save the alphabet soup plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = plot_fit
           , width = 14
           , height = 19)
    
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = plot_fit
           , width = 14
           , height = 19)

  }

  # Set the working directory back to the main directory 
  setwd(current_directory)
  # plot_fit
}