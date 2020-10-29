#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######## FUNCTION TO MAKE STAIRWAY PLOT OF MORTALITY RISK TO COMPARE TO RESULTS ADJUSTED FOR SMOKING ##########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of the stariway plot of mortality risk across the distribution of 
#          the physiological indicators to compare between the unadjusted results with those adjusted for 
#          smoking
#
# Inputs: list_regression_stats - a list of dataframe of the regression results. A dataframe is available each  
#                                 to contain the coefficients, prediction performance, and predicted risk
#         list_regression_stats_adjust - a list of dataframe of the regression results adjusted for smoking
#         dataset_thresholds - dataframe of the clinical thresholds
#         dataset_long - long-formatted dataset based on the physiological indictors
#         datasest_long_adjust - long-formatted dataset based on the physiological indictors with smoking as a 
#                                column
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         folder_name - string of the name of the new folder to hold the plots
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         df_body_systems - dataframe of the body system of the physiological indicators 
#
# Outputs: none - png and pdf versions of the a panel plot of the stariway plots
#

stairway_plots_with_without_adjustment <- function(list_regression_stats
                                                   , list_regression_stats_adjust
                                                   , dataset_thresholds
                                                   , dataset_long
                                                   , datasest_long_adjust
                                                   , type_of_analysis
                                                   , folder_name
                                                   , current_directory
                                                   , df_body_systems)
  
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Make a new folder if the folder doesn't exist
  name_folder <- paste("Stairway Plots"
                       , folder_name
                       , sep = " - ")
  
  # Define a string for the working directory for the new folder
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
  
  # Define string vector of sensitivity analyses of interest
  sensitivity_ranges <- c("00_100", "01_99")
  # Determine the number of sensitivity analyses
  num_sensitivity <- length(sensitivity_ranges)
  
  # Determine the codename of the physiological indicator
  codenames_pi <- list_regression_stats_adjust$fit %>%
    dplyr::select(pi) %>%
    unique(.) %>%
    unlist(., use.names = FALSE)
  
  # Determine the number of selected physiological indicators
  num_pi <- length(codenames_pi)
  
  # Merge the corresponding dataset from both lists together
  list_merged <- merge_lists_regression_stats(list_regression_stats
                                              , list_regression_stats_adjust)
  
  # Determine the adjustment types (i.e. "adjusted" or "unadjusted")
  adjustment_types <- unique(list_merged[["fit"]]$adjustment)
  # Determine the number of adjustment types
  num_adjustment_types <- length(adjustment_types)
  
  # Extract pertinent legend boxes to snitch them together with the panel plot of stairway plots
  legend_boxes <- make_legend_boxes(list_regression_stats
                                    , dataset_thresholds
                                    , size_legend_title = 12
                                    , size_legend_text = 10)
  
  for(i in seq(num_pi))
  {
    # Determine the codename of the selected physiological indicator
    pi_i <- codenames_pi[i]
    # print(pi_i)

    # Determine the name of the selected physiological indicator
    name_pi_i <- df_body_systems %>%
      filter(pi == pi_i) %>%
      dplyr::select(pi_names) %>%
      unlist(.
             , use.names = FALSE)
    print(name_pi_i)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain the Clinical Thresholds  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    # Extract the clinical thresholds pertaining to the given physiological indicator
    dataset_thresholds_i <- dataset_thresholds %>%
      filter(pi  == pi_i) %>%
      left_join(.
                , df_body_systems
                , by = "pi")
    # print(dataset_thresholds_i)

    # Determine the unique populations of the thresholds
    unique_populations <- dataset_thresholds_i$population %>%
      unique(.)

    # Determine the number of different population for the threshold
    num_unique_populations <- length(unique_populations)

    # If the thresholds are applicable for everyone, then the linetype will be solid
    if(num_unique_populations == 1 & anyNA(unique_populations) == FALSE)
    {
      linetypes_thresholds <- c("solid")

    # If the thresholds are sex-specific, then the linetype will be different by gender
    } else if(num_unique_populations == 2 & anyNA(unique_populations) == FALSE) {

      linetypes_thresholds <- c( "dashed"
                                 , "dotted")
    } else {

    }
    
    # For each adjustment type, determine the dataset of measurements of the physiological and determine
    # the range of each novemtile 
    for(a in seq(num_adjustment_types))
    {
      # Determine the adjustment type
      adjustment_a <- adjustment_types[a]
      
      # Extract the corresponding original dataset for a given adjustment type
      if(adjustment_a == "adjusted")
      {
        dataset_long_i <- datasest_long_adjust %>%
          filter(pi == pi_i)
        
      } else if(adjustment_a == "unadjusted") {
        
        dataset_long_i <- dataset_long %>%
          filter(pi == pi_i)
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~  Create Dataset to Display the Distribution as a Rug  ~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Define a dataset of the values of the physiological indicator for a given sensitivity analysis
      dataset_distributions_a <- list_merged[["predicted_risk"]] %>%
        filter(adjustment == adjustment_a) %>%
        filter(pi == pi_i) %>%
        filter(sensitivity_range %in% sensitivity_ranges) %>%
        group_by(sensitivity_range) %>%
        do(define_distribution(.
                               , dataset_long_i
                               , "pi")) %>%
        ungroup(.) %>%
        mutate(adjustment = rep(adjustment_a, nrow(.)))
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~  Form Dataset of Ranges and Hazard Ratios for each Novemtile  ~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Determine the dataset of coefficients for the selected physiological indicator and sensitivity analysis
      dataset_coeff_a <- list_merged[["coefficients"]] %>%
        filter(adjustment == adjustment_a) %>%
        filter(pi == pi_i) %>%
        filter(sensitivity_range %in% sensitivity_ranges)
      
      # Define the ranges of each novemtiles 
      dataset_novemtiles_a <- dataset_distributions_a %>%
        group_by(sensitivity_range) %>%
        do(define_range_of_novemtile(.
                                     , dataset_coeff_a
                                     , "pi"
                                     , type_of_analysis)) %>%
        ungroup(.) %>%
        mutate(adjustment = rep(adjustment_a, nrow(.)))
      
      # View(dataset_novemtiles_a)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~  Merge the Dataset of Distribution and Novemtiles Together  ~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # For the 1st iteration, store the current datasets
      if(a == 1)
      {
        dataset_distributions_i <- dataset_distributions_a
        dataset_novemtiles_i <- dataset_novemtiles_a
      
      # For the 2nd iteration, merged the current dataset with the previous
      } else {
        
        dataset_distributions_i <- dataset_distributions_i %>%
          # Merge with preivous dataset
          full_join(.
                    , dataset_distributions_a
                    , by = colnames(.)) %>%
          ungroup(.) %>%
          # Rename the categories of the sensitivity ranges for legibility
          mutate(sensitivity_range = case_when(sensitivity_range == "00_100" ~
                                                 "All Participants"
                                               , sensitivity_range == "01_99" ~ 
                                                 "Participants in 1st and 99th Percentiles")) %>%
          # Convert to a factor to ensure the correct order of the categories
          mutate(sensitivity_range = factor(sensitivity_range
                                            , levels = c("All Participants"
                                                         , "Participants in 1st and 99th Percentiles"))) %>%
          # Rename the categories of the adjustment types for legbility 
          mutate(adjustment = case_when(adjustment == "adjusted" ~ 
                                          "mortality ~ physiological indicator + age + sex + race + smoking"
                                        , adjustment == "unadjusted" ~ 
                                          "mortality ~ physiological indicator + age + sex + race")) %>%
          # Convert to a factor to ensure the correct order of the categories
          mutate(adjustment = factor(adjustment
                                     , levels = 
                                       c("mortality ~ physiological indicator + age + sex + race"
                                         , "mortality ~ physiological indicator + age + sex + race + smoking")))
        
        dataset_novemtiles_i <- dataset_novemtiles_i %>%
          # Merge with preivous dataset
          full_join(.
                    , dataset_novemtiles_a
                    , by = colnames(.)) %>%
          ungroup(.) %>%
          # Rename the categories of the sensitivity ranges for legibility
          mutate(sensitivity_range = case_when(sensitivity_range == "00_100" ~ 
                                                 "All Participants"
                                               , sensitivity_range == "01_99" ~ 
                                                 "Participants in 1st and 99th Percentiles")) %>%
          # Convert to a factor to ensure the correct order of the categories
          mutate(sensitivity_range = factor(sensitivity_range
                                            , levels = c("All Participants"
                                                         , "Participants in 1st and 99th Percentiles"))) %>%
          # Rename the categories of the adjustment types for legbility 
          mutate(adjustment = case_when(adjustment == "adjusted" ~ 
                                          "mortality ~ physiological indicator + age + sex + race + smoking"
                                        , adjustment == "unadjusted" ~
                                          "mortality ~ physiological indicator + age + sex + race")) %>%
          # Convert to a factor to ensure the correct order of the categories
          mutate(adjustment = factor(adjustment
                                     , levels = 
                                       c("mortality ~ physiological indicator + age + sex + race"
                                         , "mortality ~ physiological indicator + age + sex + race + smoking")))
      }
    }
    
    # Determine the range of the measurements of the physiological indicator
    range_distribution <- range(dataset_distributions_i$pi_value)
    
    # Determine the median of the measurements of the physiological indicator
    midpoint_pi_value <- dataset_distributions_i %>%
      group_by(sensitivity_range, adjustment) %>%
      summarise(median = quantile(pi_value, probs = 0.5)) %>%
      ungroup(.)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain Hazard Ratios for Linear and Spline Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Define the dataset of predicted risk from the linear and splines models
    dataset_splines_linear_i <- list_merged[["predicted_risk"]] %>%
      filter(pi == pi_i) %>%
      filter(sensitivity_range %in% sensitivity_ranges) %>%
      # Rename the categories of the sensitivity ranges for legibility
      mutate(sensitivity_range = case_when(sensitivity_range == "00_100" ~ 
                                             "All Participants"
                                           , sensitivity_range == "01_99" ~ 
                                             "Participants in 1st and 99th Percentiles")) %>%
      # Convert to a factor to ensure the correct order of the categories
      mutate(sensitivity_range = factor(sensitivity_range
                                        , levels = c("All Participants"
                                                     , "Participants in 1st and 99th Percentiles"))) %>%
      # Rename the categories of the adjustment types for legbility 
      mutate(adjustment = case_when(adjustment == "adjusted" ~ 
                                      "mortality ~ physiological indicator + age + sex + race + smoking"
                                    , adjustment == "unadjusted" ~ 
                                      "mortality ~ physiological indicator + age + sex + race")) %>%
      # Convert to a factor to ensure the correct order of the categories
      mutate(adjustment = factor(adjustment
                                 , levels = 
                                   c("mortality ~ physiological indicator + age + sex + race"
                                     , "mortality ~ physiological indicator + age + sex + race + smoking")))
    
    # Determine the range of the hazard ratios from the linear and splines model
    range_hr_splines_linear <- range(dataset_splines_linear_i$hazard_ratio)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~  Update Dataset of Thresholds to Draw Arrows in the Stairway Plots  ~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    if(anyNA(unique_populations) == FALSE)
    {
      # Determine the percentage of the range of the measurements to help determine the width of the arrows 
      # to show the unfavorable directions
      percent_of_distribution <- 0.03*abs(diff(range_distribution))
      
      # Determine the percentage of the range of the hazard ratios
      percent_of_hr_dist <- 0.5*abs(diff(range_hr_splines_linear))
      
      # Include x and y positions to help draw the arrows of unfavorable directions 
      dataset_thresholds_i <- dataset_thresholds_i %>%
        # Define the y-coordinate of where to draw the arrows of unfavorable direction of the thresholds
        mutate(y_position = 2 + percent_of_hr_dist) %>%
        # Define the x-coordinate of where to start the arrows of unfavorable direction of the thresholds 
        mutate(x_position = case_when(direction_final == "lower threshold" ~ min_threshold
                                      , direction_final == "upper threshold" ~ max_threshold)) %>%
        # Define the x coordinate of where to end the arrows of the unfavorable direction of the thresholds 
        mutate(xend_position = case_when(direction_final == "lower threshold" ~ 
                                           min_threshold - percent_of_distribution
                                         , direction_final == "upper threshold" ~ 
                                           max_threshold + percent_of_distribution)) 
    }
    
    # Define a subset of the thresholds to include those where the minium and maximum thresholds are different 
    # for the same type of bound.
    # We will draw boxes for these thresholds to show the ranges of the threshold.
    dataset_thresholds_rect <- dataset_thresholds_i %>%
      mutate(dif = abs(max_threshold - min_threshold)) %>%
      filter(dif != 0)
    
    # Determine the number of rows for the thresholds that have ranges.
    num_rows_thresholds_rect <- nrow(dataset_thresholds_rect)
    
    # Define a subset of the thresholds to include those where the minium and maximum thresholds are the same 
    # for the same type of bound.
    # We will draw lines for these thresholds.
    dataset_thresholds_line <- dataset_thresholds_i %>%
      mutate(dif = abs(max_threshold - min_threshold)) %>%
      filter(dif == 0)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create the Stairway Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # For thresholds with ranges, then draw boxes with the width showing the range of the clinical threshold
    if(num_rows_thresholds_rect == 0)
    {
      stairway_plot_i <- ggplot(data = dataset_novemtiles_i)
    } else {
      stairway_plot_i <- ggplot(data = dataset_novemtiles_i) +
        geom_rect(data = dataset_thresholds_rect
                  , aes(xmin = min_threshold
                        , xmax = max_threshold
                        , ymin = 0
                        , ymax = Inf
                        , linetype = factor(population))
                  , fill = "pink"
                  , inherit.aes = FALSE
                  , alpha = 0.4
        )
    }
    
    
    stairway_plot_i <- stairway_plot_i +
      # Facet the plot by sensitiivty range and by adjustment types
      facet_grid(rows = vars(sensitivity_range)
                 , cols = vars(adjustment)
                 , scales = "free_y") +
      # Draw boxes to represent the mortality risk from the novemtile models
      geom_rect(data = dataset_novemtiles_i
                , mapping = aes(xmin = lower_bounds
                                , xmax = upper_bounds
                                , ymin = exp(conf.low)
                                , ymax = exp(conf.high)
                                , color = treatment_on_pi
                                , fill = p.adjust(p.value))
                , alpha = 0.8) +
      # Draw numbers to represent the mean hazard ratio for each novemtile
      geom_text(data = dataset_novemtiles_i
                , mapping = aes(x = mean_within_ntile
                                , y = exp(estimate)
                                , label = novemtile_name)
                , color = "black"
                , size = 4) +
      # Draw lines to represent the mortality from the linear and spline models
      geom_line(data = dataset_splines_linear_i
                , aes(x = pi_value
                      , y = hazard_ratio
                      , group = treatment_on_pi
                      , color = treatment_on_pi
                )
                , inherit.aes = FALSE
                , linetype = "solid"
                , size = 1.0) +
      # Draw a purple dot to show the reference point
      geom_point(data = midpoint_pi_value
                 , mapping = aes(x = median
                                 , y = 1.0)
                 , color = "purple"
                 , size = 4) +
      # Draw black horizontal line to represent when the hazard ratio is 1
      geom_hline(yintercept = 1.0
                 , color = "black"
                 , linetype = "solid"
                 , size = 0.2) +
      scale_color_manual(name = "Types of Model"
                         , values = c("#CC0000", "#F0B707", "#000080")) +
      scale_fill_gradientn(colors = c("gray10", "gray50", "gray85", "gray95")
                           # , trans = "log10"
                           , limits = c(0,1)
                           , breaks = c(0.01, 0.05, 0.1,  1)
                           , values = c(0, 0.01
                                        , 0.011, 0.05
                                        , 0.051, 0.1
                                        , 0.101, 1)) +
      guides(color = guide_legend(order = 1)
             , linetype = guide_legend(order = 2)
             , fill = guide_colorbar(barwidth = 30
                                     , label.theme = element_text(size = 6)
                                     , nbin = 200)) +
      scale_x_log10(
        breaks = round(quantile(dataset_long_i$pi_value
                                , probs = c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.99,1)
                                , names = FALSE)
                       , digits = 1)) +
      scale_y_log10(
        # limits = c(0.05,100)
      ) +
      xlab(name_pi_i) +
      ylab("Hazard Ratios") +
      theme_bw() +
      theme(legend.position = "none"
            , legend.title = element_text(size = 12)
            , legend.text = element_text(size = 12)
            , strip.text = element_text(size = 14)
            , axis.title = element_text(size = 14)
            , axis.text.y = element_text(size = 12)
            , axis.text.x = element_text(size = 11)
            , plot.title = element_text(hjust = 0.5
                                        , size = 14))
    
    # If thresholds are available for the physiological indicator, then draw the arrows of unfavorable
    # directions and lines to represent the thresholds
    if(anyNA(unique_populations) == FALSE)
    {
      stairway_plot_i <- stairway_plot_i +
        # Draw pink arrows to show the unfavorable directions of the thresholds
        geom_segment(data = dataset_thresholds_i
                     , aes(x = x_position
                           , y = y_position
                           , xend = xend_position
                           , yend = y_position)
                     , size = 1.25
                     , arrow = arrow(length = unit(0.3
                                                   , "cm"))
                     , color = "pink") +
        # Draw vertical pink lines for the thresholds that are single valued
        geom_vline(data = dataset_thresholds_line
                   , aes(xintercept = min_threshold
                         , linetype = population)
                   , color = "pink"
                   , size = 1
                   , alpha = 0.75) +
        scale_linetype_manual(name = "Populations for Clinical Thresholds"
                              , values = linetypes_thresholds) +
        guides(color = guide_legend(order = 1),
               linetype = guide_legend(order = 2))
      
      # For sex-specific thresholds and those with ranges, the pink boxes may overlapped, 
      # so draw the dashed lines to help differentiate the thresholds by sex
      if(num_unique_populations == 2)
      {
        stairway_plot_i <- stairway_plot_i +
          # Draw vertical pink lines for the thresholds that are single valued
          geom_vline(data = dataset_thresholds_i
                     , aes(xintercept = min_threshold
                           , linetype = population)
                     , color = "black"
                     , size = 0.8
                     , alpha = 0.75) +
          # Draw vertical pink lines for the thresholds that are single valued
          geom_vline(data = dataset_thresholds_i
                     , aes(xintercept = max_threshold
                           , linetype = population)
                     , color = "black"
                     , size = 0.8
                     , alpha = 0.75)
      }
    }
    # Make a legend containing the legend boxes for the models, thresholds, and unfavorable directions
    legend_models_thresholds <- grid.arrange(legend_boxes[["models_thresholds"]]
                                             , legend_boxes[["arrows"]]
                                             , widths = c(1.5,0.5)
                                             , nrow = 1)
    
    # Make a legend containing the legend boxes for the p-values and the reference point
    legend_median_p_values <- grid.arrange(legend_boxes[["p_values"]]
                                           , legend_boxes[["ref_purple"]]
                                           , widths = c(1,0.5)
                                           , nrow = 1)
    
    # Make the panel plot of stairway plots
    stairway_plot_i <- grid.arrange(legend_models_thresholds
                                    , legend_median_p_values
                                    , stairway_plot_i
                                    , heights = c(0.6, 0.6, (14 - 1.2))
                                    , nrow = 3)
    
    # Define file names of the png and pdf versions of the panel of stairway plots
    plot_name.png <- paste("stairway_plot"
                           , "_"
                           , pi_i
                           , "_"
                           , type_of_analysis
                           , ".png"
                           , sep = "")
    plot_name.pdf <- paste("stairway_plot"
                           , "_"
                           , pi_i
                           , "_"
                           , type_of_analysis
                           , ".pdf"
                           , sep = "")
    
    # Save the panel of stairway plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = stairway_plot_i
           , width = 14
           , height = 9 )
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = stairway_plot_i
           , width = 14
           , height = 9)

  }


  # Set the working directory back to the main directory 
  setwd(current_directory)
  # stairway_plot_i
}