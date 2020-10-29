#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## FUNCTION TO MAKE A FACETED ALPHABET SOUP PLOT OF PREDICTION PERFORMANCE FOR THE CONCORDANCE INDEX AND R2 ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a faceted alphabet soupt plot to show the prediction performance across the  
#          models and physiological indicators for the Nagelkerke R2 and Concordance Index. This is faceted by
#          the transformations and the body system. The sample size of each physiolgical indicators are also
#          displayed.
#
# Inputs: list_stats_pi - a list of dataframe of the regression results. A dataframe is available each to 
#                         contain the coefficients, prediction performance, and predicted risk
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         pi_names_body_syst_cats - dataframe of the body system of the physiological indicators 
#
# Outputs: none - png and pdf versions of the faceted alphabet soupt plot of each fit measure are printed to a 
#                 folder
#

alphabet_soup_plot_facet_cindex_rsq <- function(list_stats_pi
                                                , type_of_analysis
                                                , current_directory
                                                , pi_names_body_syst_cats)
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the alphabet soup plots
  name_folder <- "Alphabet Soup Plots - Facet Concordance and Nagelkerke R2"
  
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
  
  # Form a long-formated dataset containing a column of prediction performance for R2 and Concordance Index
  # for the sensitivity analysis involving th entire NHANES population and those within the 1st and 99th
  # percentiles 
  dataset_stats <- list_stats_pi[["fit"]] %>%
    filter(sensitivity_range %in% c("00_100", "01_99")) %>%
    gather(., fit_stats, value, rsq_mean:concordance_std_error) %>%
    separate(fit_stats, c("fit", "stats_1", "stats_2"), sep = "_") %>%
    unite(., "stats", stats_1:stats_2) %>%
    mutate(stats = gsub("_NA$", "", stats)) %>%
    spread(., stats, value) %>%
    dplyr::select("pi", 
                  "treatment_on_pi", 
                  "sensitivity_range", 
                  "n", "fit", 
                  "ci_high", 
                  "ci_low", 
                  "mean", 
                  "std_error") %>%
    filter(fit %in% c("rsq", "concordance")) %>%
    filter(sensitivity_range != "00_100" |  fit != "concordance" ) %>%
    mutate(sensitivity_range = case_when(sensitivity_range == "00_100" ~ "All Participants"
                                         , sensitivity_range == "01_99" ~ "Participants in 1st and 99th Percentiles")) %>%
    # Define a more descriptive name for each fit measure
    mutate(fit = case_when(fit == "rsq" ~  "Nagelkerke R2"
                           , fit == "concordance" ~ "Concordance Index")) %>%
    # Make labels of the sample size for each physiological indicator and fit measure
    mutate(n_label = paste("N = "
                           , n
                           , sep = ""))
  
  # Determine the row indices pertaining to the Nagelkerke R2
  index_rsq <- which(dataset_stats$fit == "Nagelkerke R2")
  # Don't have labels of the number of participants for the Nagelerke R2
  dataset_stats$n_label[index_rsq] <- NA

  # Include info on the body systems
  dataset_stats <- dataset_stats %>%
    full_join(.
              , pi_names_body_syst_cats
              , by = "pi")

  # Define a dataset indicating the half mark of the y-axis to draw a dashed line to separate the physiological 
  # indicators
  dataset_intervals_ticks <- define_interval_ticks(dataset_stats)

  # Determine the row indices pertaining to the concordance index
  index_concordance <- which(dataset_stats$fit == "Concordance Index")
  # Determine the smallest concordance index
  min_cindex <- min(dataset_stats$ci_low[index_concordance])
  
  # Merge the dataset of tick marks into the dataset of prediction performance 
  dataset_stats <- dataset_stats %>%
    full_join(.
              , dataset_intervals_ticks
              , by = c("pi", "body_system_categories")) %>%
    # Define a new column to set the minimum value of the x-axis
    mutate(min = case_when(fit == "Nagelkerke R2" ~ 0
                           , fit == "Concordance Index" ~ 0.80))

  # Due to the long-formatted dataset, filter the statistics for a fit measure and a transformation to arrange
  # by the number of participants
  dataset_ordered_stats <- dataset_stats %>%
    filter(fit == "Concordance Index" ) %>%
    filter(treatment_on_pi == "linear" ) %>%
    arrange(n)

  # Extract the codename of the physiological indicators ordered by the number of participants
  ordered_al_comp_names <- dataset_ordered_stats[,"pi_names"] %>%
    unname(.) %>%
    unlist(.)

  # Refine a character vector of codenames as a factor to ensure the correct order of the physiological indicators
  dataset_stats$pi_names <- factor(dataset_stats$pi_names
                               , levels = (ordered_al_comp_names))

  # Make an alphabet soup plot to show the prediction performance across the models and the physiological indicators 
  # for the Concordance Index and the R2
  plot_fit <- ggplot(dataset_stats
                     , aes(x = pi_names
                           , y = mean
                           , color = treatment_on_pi
                           , shape = treatment_on_pi)) +
    geom_point(aes(x = pi_names
                   , y = min)
               , shape = 32) +
    # Write the label of sample size to appear only in the concordance column 
    geom_text(aes(x = pi_names
                  , y = min + 0.005
                  , label = n_label)
              , color = "black"
              , size = 4) +
    geom_jitter(size = 5
                , position = position_dodge(0.8)) +
    # Draw dashed line to separate the physiological indicators
    geom_vline(xintercept = unlist(dataset_stats$interval_ticks)
               , size = 0.3
               , color = "gray"
               , linetype = "longdash") +
    scale_x_discrete(expand = expand_scale(mult = 0, add = 0.5)) +
    # Plot the confidence interval of each prediction performance for each sensitivity analysis 
    geom_errorbar(aes(ymax = ci_high
                      , ymin = ci_low)
                  , position = position_dodge(0.8)) +
    coord_flip() +
    ylab("Values of Fit Measures") +
    # Facet the plot by sensitivity analysis and transformation
    facet_grid(rows = vars(body_system_categories)
               , cols = vars(fit, sensitivity_range)
               , scales = "free"
               , space = "free_y"
               ) +
    scale_color_manual(name = "Types of Model"
                       , values = c("#CC0000", "#F0B707", "#000080")) +
    scale_shape_manual(name = "Types of Model"
                       , values = c(76, 78, 83)) +
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
  plot_name.png <- paste("facet_alphabet_soup_plot"
                         , "_"
                         , type_of_analysis
                         , ".png"
                         , sep = "")
  plot_name.pdf <- paste("facet_alphabet_soup_plot"
                         , "_"
                         , type_of_analysis
                         , ".pdf"
                         , sep = "")
  
  # Save the alphabet soup plots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = plot_fit
         , width = 15
         , height = 19)
  
  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = plot_fit
         , width = 15
         , height = 19)
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
  # plot_fit
}