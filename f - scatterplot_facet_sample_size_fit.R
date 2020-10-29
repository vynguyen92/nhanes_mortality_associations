#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##########  FUNCTION TO CREATE A PANEL PLOT OF SCATTERPLOT OF PARTICIPANTS BY PREDICTION PERFORMANCE  #########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of scatterplot to show the number of participants across the 
#          prediction performance across 3 fit meaures:  Nagelkerke R2, AIC, and Concordance Index
#
# Inputs: list_stats_pi -  a list of dataframe of the regression results. A dataframe is available each to 
#                          contain the coefficients, prediction performance, and predicted risk
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         pi_names_body_syst_cats - dataframe of the body system of the physiological indicators 
#
# Outputs: none - png and pdf versions of the panel plot of scatterplot are printed to a folder

scatterplot_facet_sample_size_fit <- function(list_stats_pi
                                              , type_of_analysis
                                              , current_directory
                                              , pi_names_body_syst_cats)
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the histograms
  name_folder <- "Scatterplots - Facet Sample Size and Fit Measures"
  
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
  
  # Form a long-formated dataset containing a column for the number of participants and another
  # column containing the prediction performance for all fit measures 
  dataset_stats <- list_stats_pi[["fit"]] %>%
    gather(., fit_stats, value, rsq_mean:concordance_std_error) %>%
    separate(fit_stats, c("fit", "stats_1", "stats_2"), sep = "_") %>%
    unite(., "stats", stats_1:stats_2) %>%
    mutate(stats = gsub("_NA$", "", stats)) %>%
    spread(., stats, value) %>%
    dplyr::select("pi"
                  , "treatment_on_pi"
                  , "sensitivity_range"
                  , "n"
                  , "fit"
                  , "ci_high"
                  , "ci_low"
                  , "mean"
                  , "std_error") 

  # Further process the dataset of prediction performance 
  dataset_stats <- dataset_stats %>%
    # Include info on the body system
    full_join(.
              , pi_names_body_syst_cats
              , by = "pi")  %>%
    # Specify the different type of sensitivity analyses
    mutate(sensitivity_range = case_when(sensitivity_range == "00_100" ~ "All Participants"
                                       , sensitivity_range == "01_99" ~ "Participants in 1st and 99th Percentiles"
                                       , sensitivity_range == "05_95" ~ "Participants in 5th and 95th Percentiles"
                                       , sensitivity_range == "10_90" ~ "Participants in 10th and 90th Percentiles")) %>%
    # Make this column vector as a factor to ensure the correct order of the sensitivity analyses
    mutate(sensitivity_range = factor(sensitivity_range
                                    , levels = c("All Participants"
                                                 , "Participants in 1st and 99th Percentiles"
                                                 , "Participants in 5th and 95th Percentiles"
                                                 , "Participants in 10th and 90th Percentiles"))) %>%
    # Define a new column with title labels for each fit measure
    mutate(letter_labels = case_when(fit == "rsq" ~ "C"
                                     , fit == "concordance" ~ "B"
                                     , fit == "AIC" ~ "A")) %>%
    # Define the name of the fit measure for legibility
    mutate(fit = case_when(fit == "rsq" ~ "Nagelkerke R2"
                           , fit == "concordance" ~ "Concordance Index"
                           , fit == "AIC" ~ "Akaike Information Criterion")) 
  # View(dataset_stats)
  
  # Determine the number of body systems
  num_systems <- length(unique(dataset_stats$body_system_categories))
  
  # Define a function to output a vector of hexcodes for the default ggplot color scheme for a given number
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  # Make a panel plot of scatterplot showing the number of participants and the prediction performance
  # across all fit measures
  facet_scatterplot <- ggplot(data = dataset_stats) +
    facet_wrap(vars(letter_labels, fit)
               , scales = "free_y") +
    geom_point(mapping = aes(x = n
                             , y = ci_high
                             , color = body_system_categories
                             , shape = sensitivity_range)
               , size = 4) +
    ylab("Values of Fit Measures") +
    xlab("Sample Size") +
    scale_color_manual(name = "Body Systems"
                       , values = gg_color_hue(num_systems)) +
    scale_shape_manual(name = "Types of Distribution"
                       , values = c(48, 49, 50, 51)) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE, order = 2)
           , color = guide_legend(nrow = 1, order = 1)) +
    theme_bw()  +
    theme(legend.position = "top"
          , legend.title = element_text(size = 12)
          , legend.text = element_text(size = 12)
          , legend.box = "vertical"
          , strip.text = element_text(size = 13)
          , axis.text = element_text(size = 12)
          , axis.title = element_text(size = 12))
  
  # Define file names of the png and pdf versions of the panel of scatterplots
  plot_name.png <- paste("scatterplot_facet"
                         , type_of_analysis
                         , "sample_size.png"
                         , sep = "_")
  plot_name.pdf <- paste("scatterplot_facet"
                         , type_of_analysis
                         , "sample_size.pdf"
                         , sep = "_")
  
  # Save the panel of scatterplots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = facet_scatterplot
         , width = 14
         , height = 9)


  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = facet_scatterplot
         , width = 14
         , height = 9)
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
}