#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########  FUNCTION TO MAKE A SERIES OF VOLCANO PLOT OF THE FIT MEASURE AND FIT OF MODEL SIGNIFICANCE  #########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a series of voclano plot to show the prediction performance (Nagelkerke R2, 
#          AIC, and Concordance Index) with respect to model significance (test statistics and p-values from 
#          the log-likelihood, wald, and scores tests).
#
# Inputs: list_stats_pi -  a list of dataframe of the regression results. A dataframe is available each to 
#                          contain the coefficients, prediction performance, and predicted risk
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted"  
#                            or "unweighted"
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#
# Outputs: none - png and pdf versions of the basic volcano plots, volcano plots faceted by sensitivity 
#                 analysis, and volcano plot faceted by significance tests are printed to a folder
# 

volcano_plot_fit_significance <- function(list_stats
                                          , type_of_analysis
                                          , current_directory)
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the alphabet soup plots
  name_folder <- "Volcano Plots - Fit Measures and Significance"
  
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
  
  # Determine the column names of the dataset of prediction performance that are the test statistics or 
  # p-values from the log-likelihood, wald, and score tests
  col_name_sig <- grepl("^statistic|^p.value", colnames(list_stats[["fit"]])) %>%
    colnames(list_stats[["fit"]])[.]
  
  # Determine the number of measures of model significance
  sig_measures <- gsub("^statistic.|^p.value.", "", col_name_sig) %>%
    unique(.)
  
  # Define a vector of the type of measures to show model significance
  sig_types <- c("statistic", "p.value")
  # Determine the types of measures to show model significance
  num_sig_types <- length(sig_types)
  
  # Form a long-formatted dataset containing a column of the prediction performance for all fit measures 
  # and include the measures of model significance
  dataset_stats <- list_stats[["fit"]] %>%
    gather(., fit_stats, value, rsq_mean:concordance_std_error) %>%
    separate(fit_stats, c("fit", "stats_1", "stats_2"), sep = "_") %>%
    unite(., "stats", stats_1:stats_2) %>%
    mutate(stats = gsub("_NA$", "", stats)) %>%
    spread(., stats, value) %>%
    select("pi"
           , "treatment_on_pi"
           , "sensitivity_range"
           , "fit"
           , "ci_high"
           , "ci_low"
           , "mean"
           , "std_error"
           , col_name_sig) %>%
    # Specify the different type of sensitivity analyses
    mutate(sensitivity_range = case_when(sensitivity_range == "00_100" ~ 
                                           "All Participants"
                                         , sensitivity_range == "01_99" ~ 
                                           "Participants in 1st and 99th Percentiles"
                                         , sensitivity_range == "05_95" ~ 
                                           "Participants in 5th and 95th Percentiles"
                                         , sensitivity_range == "10_90" ~ 
                                           "Participants in 10th and 90th Percentiles")) %>%
    # Make this column vector as a factor to ensure the correct order of the sensitivity analyses
    mutate(sensitivity_range = factor(sensitivity_range
                                      , levels = c("All Participants"
                                                   , "Participants in 1st and 99th Percentiles"
                                                   , "Participants in 5th and 95th Percentiles"
                                                   , "Participants in 10th and 90th Percentiles"))) 
  # View(dataset_stats)
  
  # Determine the unique fit measures
  fit_measures <- unique(dataset_stats$fit)
  # Determine the number of fit measures
  num_fit_measures <- length(fit_measures)
  
  # Make a volcano plot that shows the association between the model significance and prediction performance 
  # for a combination of a significance measures and fit measure
  for(i in seq(num_sig_types))
  {
    # Determine the measure of model significance
    sig_type_i <- sig_types[i]
    # print(sig_type_i)
    
    # Define a more descriptive name for each measure of model significance
    if(sig_type_i == "statistic")
    {
      sig_name <- "Test Statistics"
      
    } else if(sig_type_i == "p.value") {
      
      sig_name <- "-log10(p-value)"
     
    } 
    
    # Determine all column names pertaining to a type of measure of model significance
    colnames_sig_type_i <- grepl(sig_type_i, colnames(dataset_stats)) %>%
      colnames(dataset_stats)[.]
    
    # Format as a long dataset with a column for all the tests of a given measure of model significance 
    # in a column 
    dataset_i <- dataset_stats %>%
      select("pi"
             , "treatment_on_pi"
             , "sensitivity_range"
             , "fit"
             , "ci_high"
             , "ci_low"
             , "mean"
             , "std_error"
             , colnames_sig_type_i) %>%
      gather(., sig_stats, sig_value, colnames_sig_type_i) %>%
      # Define a more descriptive name for each test for a given measure of model significance
      mutate(sig_stats = case_when(grepl(".log$",sig_stats) == TRUE ~ "Likelihood-ratio Test"
                                   , grepl(".sc$",sig_stats) == TRUE ~ "Score Test"
                                   , grepl(".wald$",sig_stats) == TRUE ~ "Wald Test"))
   
    # Run through all the different fit measures: Nagelkerke R2, AIC, and Concordance Index
    for(j in seq(num_fit_measures))
    {
      # Determine the fit measure
      fit_measure_j <- fit_measures[j]
      # print(fit_measure_j)
      
      # Extract the statistics pertaining to this given fit measure
      dataset_i_j <- dataset_i %>%
        filter(fit == fit_measure_j) 
      
      # Define a more descriptive name and the minimum and maximum value of the x-axis for each fit measure
      if(fit_measure_j == "rsq")
      {
        fit_name <- bquote("Nagelkerke" ~ R^2)
        axis_min <- 0
        axis_max <- max(dataset_i_j$ci_high)
        
      } else if(fit_measure_j == "concordance") {
        
        fit_name <- "Concordance Index"
        axis_min <- 0.5
        axis_max <- 1
        
      } else if(fit_measure_j == "AIC") {
        
        fit_name <- "-log10(Akaike Information Criterion)"
        dataset_i_j$mean <- -log10(dataset_i_j$mean)
        axis_min <- min(dataset_i_j$ci_low)
        axis_max <- max(dataset_i_j$ci_high)
        
      }
      
      # log10-transform the p-values
      if(sig_type_i == "p.value")
      {
        dataset_i_j$sig_value <- -log10(p.adjust(dataset_i_j$sig_value))
      }
      
      # Make a volcano plot with the values of the fit measures on the x-axis and 
      # the values of the fit of model significance on the y-axis 
      volcano_plot <- ggplot(data = dataset_i_j
                             , mapping = aes(x = mean
                                             , y = sig_value
                                             , color = treatment_on_pi
                                             , shape = sensitivity_range)) +
        geom_point(size = 3) +
        scale_y_log10() +
        xlab(fit_name) +
        ylab(sig_name) +
        scale_color_manual(name = "Types of Model"
                           , values = c("#CC0000", "#F0B707", "#000080")) +
        scale_shape_manual(name = "Types of Distribution"
                           , values = c(48, 49, 50, 51)) +
        theme_bw() +
        theme(legend.position = "top"
              , legend.box = "vertical"
              , legend.title = element_text(size = 12)
              , legend.text = element_text(size = 12)
              , strip.text = element_text(size = 12)
              , plot.title = element_text(hjust = 0.5)
              # , panel.grid.major = element_blank()
              # , panel.grid.minor = element_blank()
              , axis.text.y = element_text(size = 10)
              , axis.text.x = element_text(size = 10)
              , axis.title = element_text(size = 12))
    
      # Define file names of the png and pdf versions of the volcano plot showing the associations
      # between a fit measure and a measure of model significance
      plot_name.png <- paste("volcano_plot"
                             , type_of_analysis
                             , sig_type_i
                             , fit_measure_j
                             , "mortality.png"
                             , sep = "_")
      plot_name.pdf <- paste("volcano_plot"
                             , type_of_analysis
                             , sig_type_i
                             , fit_measure_j
                             , "mortality.pdf"
                             , sep = "_")
      
      # Save the basic volcano plot as a png and pdf
      print(plot_name.png)
      ggsave(filename = plot_name.png
             , plot = volcano_plot
             , width = 14
             , height = 9)
      print(plot_name.pdf)
      ggsave(filename = plot_name.pdf
             , plot = volcano_plot
             , width = 14
             , height = 9)
      
      # Make a faceted plot by the different sensitivity analysis
      volcano_plot_by_sensitivity <- volcano_plot +
        facet_grid(rows = vars(sensitivity_range)) 
      
      # Define the file names of the png and pdf versions of the panel volcano plot faceted by the
      # sensitivity analysis
      plot_name.png <- paste("facet_volcano_plot"
                             , type_of_analysis
                             , sig_type_i
                             , fit_measure_j
                             , "mortality.png"
                             , sep = "_")
      plot_name.pdf <- paste("facet_volcano_plot"
                             , type_of_analysis
                             , sig_type_i
                             , fit_measure_j
                             , "mortality.pdf"
                             , sep = "_")
      
      # Save the panel volcano plot faceted by the sensitivity analysis as a png and pdf
      print(plot_name.png)
      ggsave(filename = plot_name.png
             , plot = volcano_plot_by_sensitivity
             , width = 14
             , height = 19)
      print(plot_name.pdf)
      ggsave(filename = plot_name.pdf
             , plot = volcano_plot_by_sensitivity
             , width = 14
             , height = 19)
      
      # Make a faceted plot by the different significance tests
      volcano_plot_by_tests <- volcano_plot +
        facet_grid(cols = vars(sig_stats)) 
      
      # Define the file names of the png and pdf versions of the panel volcano plot faceted by the
      # significance tests
      plot_name.png <- paste("facet_test_volcano_plot"
                             , type_of_analysis
                             , sig_type_i
                             , fit_measure_j
                             , "mortality.png"
                             , sep = "_")
      plot_name.pdf <- paste("facet_test_volcano_plot"
                             , type_of_analysis
                             , sig_type_i
                             , fit_measure_j
                             , "mortality.pdf"
                             , sep = "_")
      
      # Save the panel volcano plot faceted by the significance tests as a png and pdf
      print(plot_name.png)
      ggsave(filename = plot_name.png
             , plot = volcano_plot_by_tests
             , width = 14
             , height = 9)
      print(plot_name.pdf)
      ggsave(filename = plot_name.pdf
             , plot = volcano_plot_by_tests
             , width = 14
             , height = 9)
      
    }
  }
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
  # volcano_plot_by_tests
}