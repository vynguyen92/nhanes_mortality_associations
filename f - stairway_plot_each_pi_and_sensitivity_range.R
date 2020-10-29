#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############ FUNCTION TO MAKE STAIRWAY PLOT FOR PHYSIOLOGICAL INDICATOR AND SENSITIVITY ANALYSIS ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a stariway plot of mortality risk across the distribution of for each
#          combination of a physiological indicators and a sensitivity analysis.
#
# Inputs: sensitivity_analyses - string vector of labels for the sensitivity analyses
#         codenames_pi - string vector of the codenames of the physiological indictors
#         names_pi - string vector of the names of the physiological indictors
#         dataset_long - long-formatted dataset based on the physiological indictors
#         list_regression_stats - a list of dataframe of the regression results. A dataframe is available each  
#                                 to contain the coefficients, prediction performance, and predicted risk
#         dataset_thresholds - dataframe of the clinical thresholds
#         dataset_refs - default to NA. If specified, then it's a dataframe of reference groups
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         legend_boxes - list of attributes on the legends 
#         df_extra_measurements - dataframe of position of the measurements when the hazard ratio is at a 10% 
#                                 increase from the minimum mortality risk
#         type_plot - string to indicate an "individual" or "panel" stairway plot
#
# Outputs: none - png and pdf versions of the stairway plot
#

stairway_plot_each_pi_and_sensitivity_range <- function( sensitivity_analyses
                                                        , codenames_pi
                                                        , names_pi
                                                        , dataset_long
                                                        , list_regression_stats
                                                        , dataset_thresholds
                                                        , dataset_refs
                                                        , type_of_analysis
                                                        , legend_boxes
                                                        , df_extra_measurements
                                                        , type_plot)
{
  
  # Make a legend containing the legend boxes for the models, thresholds, and unfavorable directions
  legend_models_thresholds <- grid.arrange(legend_boxes[["models_thresholds"]]
                                           , legend_boxes[["arrows"]]
                                           , widths = c(1.5, 0.5)
                                           , nrow = 1)
  
  # Make a legend containing the legend boxes for the p-values, the hazard ratio at 1.1 times the minimum, the
  # median, and the reference point
  legend_median_hr_measurements <- grid.arrange(legend_boxes[["p_values"]]
                                                , legend_boxes[["hr_measurements"]]
                                                , legend_boxes[["median_black"]]
                                                , legend_boxes[["ref_purple"]]
                                                , widths = c(0.55, 0.35, 0.2, 0.2)
                                                , nrow = 1)
  
  # Define a string vectors of the letters
  LETTERS <- append(LETTERS, "AA")
  
  # Determine the number of senstivity analyses
  num_sensitivity_analyses <- length(sensitivity_analyses)

  # Determine the number of selected physiological indicators
  num_pi <- length(codenames_pi)
  
  # For each sensitivity analysis, make a panel of stairway plots
  for(j in seq(num_sensitivity_analyses))
  {
    # Intialize an empty list to store the stairway plot for the selected physiological indicators
    list_stairway_plots <- list()
    
    # Determine the sensitivity analysis
    sensitivity_j <- sensitivity_analyses[j]
    print(sensitivity_j)
    
    # Make the stairway plot for a selected physiological indicator
    for(i in seq(num_pi))
    {
      # Determine the codename of the selected physiological indicator
      pi_i <- codenames_pi[i]
      print(pi_i)
      
      # Determine the name of the selected physiological indicator
      name_pi_i <- names_pi[i]
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~  Create Dataset to Display the Distribution as a Rug  ~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Determine the minimum measurement of the selected physiological indicator
      min_measurement <- dataset_long %>%
        filter(pi == pi_i) %>%
        dplyr::select(pi_value) %>%
        min(.)
      
      # Determine the maximum measurement of the selected physiological indicator
      max_measurement <- dataset_long %>%
        filter(pi == pi_i) %>%
        dplyr::select(pi_value) %>%
        max(.)
      
      # Define a dataset of the values of the physiological indicator for a given sensitivity analysis
      dataset_distributions_by_pi <- list_regression_stats[["predicted_risk"]] %>%
        filter(pi == pi_i) %>%
        filter(sensitivity_range == sensitivity_j) %>%
        # Extract all values of the physiological indicator from the original dataset
        define_distribution(.
                            , dataset_long
                            , "pi") %>%
        ungroup(.)
      # print(dataset_distributions_by_pi)
      
      # Determine the median of the measurements of the selected physiological indicator
      midpoint_pi_value <- quantile(dataset_distributions_by_pi$pi_value, probs = 0.5) %>%
        unlist(., use.names = FALSE)
      
      # Determine the range of the measurements of the selected physiological indicator
      range_distribution <- range(dataset_distributions_by_pi$pi_value)
      
      # Extract the reference points for the linear and spline models
      if(anyNA(dataset_refs) == TRUE)
      {
        ref_pi_point <- midpoint_pi_value
        
      } else {
        
        ref_pi_point <- dataset_refs %>%
          filter(pi == pi_i) %>%
          filter(sensitivity_range == sensitivity_j) %>%
          filter(treatment_on_pi %in% c("linear", "splines")) %>%
          dplyr::select(ref) %>%
          unique(.) %>%
          unlist(., use.names = FALSE)
        
      }
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~  Form Dataset of Ranges and Hazard Ratios for each Novemtile  ~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Determine the dataset of coefficients for the selected physiological indicator and sensitivity analysis
      dataset_coeff_i <- list_regression_stats[["coefficients"]] %>%
        filter(pi == pi_i) %>%
        filter(sensitivity_range == sensitivity_j)
      
      # Define the ranges of each novemtiles 
      dataset_novemtiles <- dataset_distributions_by_pi %>%
        define_range_of_novemtile(.
                                  , dataset_coeff_i
                                  , "pi"
                                  , type_of_analysis
                                  , dataset_refs
                                  ) %>%
        ungroup(.)
      # print(dataset_novemtiles)
      # print(p.adjust(dataset_novemtiles$p.value))
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain Hazard Ratios for Linear and Spline Models  ~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Define the dataset of predicted risk from the linear and splines models
      dataset_splines_linear_i <- list_regression_stats[["predicted_risk"]] %>%
        filter(pi == pi_i) %>%
        filter(sensitivity_range == sensitivity_j)
      # print(dataset_splines_linear_i)
      
      # Determine the range of the hazard ratios from the linear and splines model
      range_hr_splines_linear <- range(dataset_splines_linear_i$hazard_ratio)
      # Determine the median of the hazard ratios from the linear and splines model
      median_hr_splines_linear <- quantile(dataset_splines_linear_i$hazard_ratio
                                           , probs = 0.5)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Find Hazard Ratio that is 1.1x the Minimum  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Define a dataset of predicted risk from the splines models
      dataset_slines_predicted_risk_i <- dataset_splines_linear_i %>%
        filter(treatment_on_pi == "splines")
      # print(dataset_slines_predicted_risk_i)
      
      # Provide info on the hazard ratio is 1.1 times the minimum hazard ratio, the measurements at 1.1x the 
      # minimum hazard ratio, and a boolean to indicate that the 1.1x the minimum hazard ratio intersects with 
      # the spline model
      list_of_features <- define_hr_and_measurements_at_increased_risk(dataset_slines_predicted_risk_i)
      # print(list_of_features)
      
      # Extract the hazard ratio that is 1.1 times the minimum hazard ratio
      hr_splines_1.1 <- list_of_features[["hr_splines_1.1"]]
      # print(hr_splines_1.1)
      
      # Extract the boolean to indicate whether to plot the blue diamonds to show the intersection between the 
      # splines model and when the hazard ratio is 1.1 times the minimum hazard ratio
      plot_intersects <- list_of_features[["to_plot_intersects"]]
      
      # Extract the dataset of measurements when the hazard ratio is 1.1 from the algorithm
      df_measurements_splines_1.1 <- list_of_features[["df_measurements"]]
      
      # Extract measurements when the hazard ratio is 1.1 if the algorithm cannot automatically detect the 
      # measurement
      df_extra_measurements_i_j <- df_extra_measurements %>%
        filter(pi == pi_i) %>%
        filter(sensitivity_range == sensitivity_j) %>%
        dplyr::select(x, y)
      
      # Merge the datasets of measurements when the hazard ratio is 1.1 from the algorithm and the excel 
      # sheet
      if(plot_intersects == TRUE)
      {
        df_measurements_splines_1.1 <-  df_measurements_splines_1.1 %>%
          full_join(.
                    , df_extra_measurements_i_j
                    , by = colnames(.))
      } else {

        df_measurements_splines_1.1 <- df_extra_measurements_i_j
      }
      # print(df_measurements_splines_1.1)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain the Clinical Thresholds  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Determine the percentage of the range of the measurements to help determine the width of the arrows to show
      # the unfavorable directions
      percent_of_distribution <- 0.03*abs(diff(range_distribution))
      # Determine the percentage of the range of the hazard ratios
      percent_of_hr_dist <- 0.6*abs(diff(range_hr_splines_linear))
      
      # Include x and y positions to help draw the arrows of unfavorable directions 
      dataset_thresholds_i <- dataset_thresholds %>%
        filter(pi  == pi_i) %>%
        left_join(.
                  , df_body_systems
                  , by = "pi") %>%
        # Define the y-coordinate of where to draw the arrows of unfavorable direction of the thresholds
        mutate(y_position = hr_splines_1.1 + percent_of_hr_dist) %>%
        # Define the x-coordinate of where to start the arrows of unfavorable direction of the thresholds 
        mutate(x_position = case_when(direction_final == "lower threshold" ~ min_threshold
                                      , direction_final == "upper threshold" ~ max_threshold)) %>%
        # Define the x coordinate of where to end the arrows of the unfavorable direction of the thresholds 
        mutate(xend_position = case_when(direction_final == "lower threshold" ~ 
                                           min_threshold - percent_of_distribution
                                         , direction_final == "upper threshold" ~ 
                                           max_threshold + percent_of_distribution)) %>%
        # Determine the difference between the max and minimum threshold for the same type of bound. 
        mutate(dif = abs(max_threshold - min_threshold))
      
      # Define a subset of the thresholds to include those where the minium and maximum thresholds are different 
      # for the same type of bound.
      # We will draw boxes for these thresholds to show the ranges of the threshold.
      dataset_thresholds_rect <- dataset_thresholds_i %>%
        filter(dif != 0)
      # print(dataset_thresholds_rect)
      
      # Determine the number of rows for the thresholds that have ranges.
      num_rows_thresholds_rect <- nrow(dataset_thresholds_rect)
      
      # Define a subset of the thresholds to include those where the minium and maximum thresholds are the same 
      # for the same type of bound.
      # We will draw lines for these thresholds.
      dataset_thresholds_line <- dataset_thresholds_i %>%
        filter(dif == 0)
      # print(dataset_thresholds_line)
      
      # Determine the number of different population for the threshold
      num_unique_populations <- dataset_thresholds_i$population %>%
        unique(.) %>%
        length(.)
      
      # If the thresholds are applicable for everyone, then the linetype will be solid
      if(num_unique_populations == 1)
      {
        linetypes_thresholds <- c("solid")
        
        # If the thresholds are sex-specific, then the linetype will be different by gender
      } else if(num_unique_populations == 2) {
        
        linetypes_thresholds <- c( "dashed"
                                   , "dotted")
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create the Stairway Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # For thresholds with ranges, then draw boxes with the width showing the range of the clinical threshold
      if(num_rows_thresholds_rect == 0)
      {
        stairway_plot_i <- ggplot(data = dataset_novemtiles)
      } else {
        stairway_plot_i <- ggplot(data = dataset_novemtiles) +
          geom_rect(data = dataset_thresholds_rect
                    , aes(xmin = min_threshold
                          , xmax = max_threshold
                          , ymin = 0
                          , ymax = Inf
                          , linetype = population)
                    , fill = "pink"
                    , inherit.aes = FALSE
                    , alpha = 0.75
                    # , size = 0.8
          )
      }
      
      # Make the stairway plot to show the mortality risk progression across physiological indicator for the linear,
      # novemtiles, and splines models
      stairway_plot_i <- stairway_plot_i +
        # Draw boxes to represent the mortality risk from the novemtile models
        geom_rect(data = dataset_novemtiles
                  , mapping = aes(xmin = lower_bounds
                                  , xmax = upper_bounds
                                  , ymin = exp(conf.low)
                                  , ymax = exp(conf.high)
                                  , color = treatment_on_pi
                                  , fill = p.adjust(p.value))
                  , alpha = 0.8) +
        # Draw numbers to represent the mean hazard ratio for each novemtile
        geom_text(data = dataset_novemtiles
                  , mapping = aes(x = mean_within_ntile
                                  , y = exp(estimate)
                                  , label = novemtile_name)
                  , color = "black"
                  , size = 5) +
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
        # Draw a rug or flatten histogram to show distribution of the physiological indicator
        geom_rug(data = dataset_distributions_by_pi
                 , aes(x = pi_value
                       , y = rep(1, length(pi_value)))
                 , size = 0.2
                 , sides = "b"
                 , alpha = 0.5) +
        # Draw black horizontal line to represent when the hazard ratio is 1
        geom_hline(yintercept = 1.0
                   , color = "black"
                   , linetype = "solid"
                   , size = 0.2) +
        # Draw a green dashed line to represent when the hazard ratio is 1.1 times the minimum hazard ratio
        geom_hline(yintercept = c(hr_splines_1.1)
                   , color = c("#4BB74C")
                   , linetype = c( "dotdash")
                   , size = 0.6) +
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
        # Draw a purple dot to show the reference point
        geom_point(aes(x = ref_pi_point
                       , y = 1.0)
                   , color = "purple"
                   , size = 4) +
        # Draw a black dot to show the median
        geom_point(aes(x = midpoint_pi_value
                       , y = 1.0)
                   , color = "black"
                   , size = 4) +
        geom_point(data = df_measurements_splines_1.1
                   , aes(x = x
                         , y = y)
                   , color = "#000080"
                   , shape = 18
                   , size = 6) +
        # Draw vertical pink lines for the thresholds that are single valued
        geom_vline(data = dataset_thresholds_line
                   , aes(xintercept = min_threshold
                         , linetype = population)
                   , color = "pink"
                   , size = 1
                   , alpha = 0.75) +
        # Color the boxes based on significance of the novemtiles
        scale_fill_gradientn(colors = c("gray10", "gray50", "gray85", "gray95")
                             # , trans = "log10"
                             , limits = c(0,1)
                             , breaks = c(0.01, 0.05, 0.1, 1)
                             , values = c(0, 0.01
                                          , 0.011, 0.05
                                          , 0.051, 0.1
                                          , 0.101, 1)) +
        scale_linetype_manual(name = "Populations for Clinical Thresholds"
                              , values = linetypes_thresholds) +
        guides(color = guide_legend(order = 1)
               , linetype = guide_legend(order = 2)
               , fill = guide_colorbar(barwidth = 30
                                       , label.theme = element_text(size = 6)
                                       , nbin = 200)) +
        scale_color_manual(name = "Types of Model"
                           , values = c("#CC0000"
                                        , "#F0B707"
                                        , "#000080"
                                        )) +
        # Label the tick marks that show the percentiles of the measurements
        scale_x_continuous(
          breaks = round(quantile(dataset_distributions_by_pi$pi_value
                                  , probs = c(0,0.1,0.25,0.5,0.75,0.9,1)
                                  , names = FALSE)
                         , digits = 1)) +
        xlab(name_pi_i) +
        theme_bw() +
        theme(legend.position = "none"
              , axis.title = element_text(size = 14)
              , axis.text.y = element_text(size = 12)
              , axis.text.x = element_text(size = 8.0)
              , axis.title.y = element_blank()
              , plot.title = element_text(hjust = 0.5
                                          , size = 14))
      
      # For sex-specific thresholds and those with ranges, the pink boxes may overlapped, 
      # so draw the dashed lines to help differentiate the thresholds by sex
      if(num_unique_populations == 2)
      {
        stairway_plot_i <- stairway_plot_i +
          # Draw vertical black lines for the thresholds that are single valued
          geom_vline(data = dataset_thresholds_rect
                     , aes(xintercept = min_threshold
                           , linetype = population)
                     , color = "black"
                     , size = 0.8
                     , alpha = 0.75) +
          # Draw vertical black lines for the thresholds that are single valued
          geom_vline(data = dataset_thresholds_rect
                     , aes(xintercept = max_threshold
                           , linetype = population)
                     , color = "black"
                     , size = 0.8
                     , alpha = 0.75)
      }
      
      # Create a stairway plot with the confidence intervals for linear and spline models
      stairway_plot_ci <- stairway_plot_i +
        geom_line(data = dataset_splines_linear_i
                  , aes(x = pi_value
                        , y = hazard_ratio_low_ci
                        , group = treatment_on_pi
                        , color = treatment_on_pi)
                  , linetype = 2
                  , size = 0.6) +
        geom_line(data = dataset_splines_linear_i
                  , aes(x = pi_value
                        , y = hazard_ratio_high_ci
                        , group = treatment_on_pi
                        , color = treatment_on_pi)
                  , linetype = 2
                  , size = 0.6)
      
      stairway_plot_log10 <- stairway_plot_i +
        scale_x_log10(breaks = round(quantile(dataset_distributions_by_pi$pi_value
                                              , probs = c(0,0.1,0.25,0.5,0.75,0.9,1)
                                              , names = FALSE)
                                     , digits = 1))
      
      stairway_plot_ci_log10 <- stairway_plot_ci +
        scale_x_log10(breaks = round(quantile(dataset_distributions_by_pi$pi_value
                                              , probs = c(0,0.1,0.25,0.5,0.75,0.9,1)
                                              , names = FALSE)
                                     , digits = 1))
      
      if(type_plot == "individual")
      {

        stairway_plot_types <- c("stairway_plot"
                                 , "stairway_ci_plot"
                                 , "stairway_log10_plot"
                                 , "stairway_ci_log10_plot"
                                 )

        # Output each stairway plot
        for(plot_type in stairway_plot_types)
        {
    
          if(plot_type == "stairway_plot")
          {
            plot_output <- stairway_plot_i

          } else if(plot_type == "stairway_ci_plot") {
            plot_output <- stairway_plot_ci
          } else if(plot_type == "stairway_log10_plot") {
            plot_output <- stairway_plot_log10
          } else if(plot_type == "stairway_ci_log10_plot") {
            plot_output <- stairway_plot_ci_log10
          }

          plot_output <- plot_output +
            ylab("Hazard Ratios") +
            theme(legend.position = "none"
                  , axis.title = element_text(size = 14)
                  , axis.text.y = element_text(size = 12)
                  , axis.text.x = element_text(size = 8.0)
                  , axis.title.y = element_text(size = 14
                                                , angle = 90)
                  , plot.title = element_text(hjust = 0.5
                                              , size = 14))

          # Attach the legends at the top of the figure
          plot_output <- grid.arrange(legend_models_thresholds
                                      , legend_median_hr_measurements
                                      , plot_output
                                      , heights = c(0.7, 0.7, 12.6)
                                      , nrow = 3)

          # Define the file name of the png of the stairway plot for a given physiological indicator
          plot_name.png <- paste(plot_type
                                  , "_"
                                  , pi_i
                                  , "_"
                                  , sensitivity_j
                                  , "_"
                                  , type_of_analysis
                                  , ".png"
                                  , sep = "")
          plot_name.pdf <- paste(plot_type
                                 , "_"
                                 , pi_i
                                 , "_"
                                 , sensitivity_j
                                 , "_"
                                 , type_of_analysis
                                 , ".pdf"
                                 , sep = "")

          # Save the png and pdf of the stairway plot for a given physiological indicator
          print(plot_name.png)
          ggsave(filename = plot_name.png
                 , plot = plot_output
                 , width = 16
                 , height = 9)

          print(plot_name.pdf)
          ggsave(filename = plot_name.pdf
                 , plot = plot_output
                 , width = 16
                 , height = 9)
        }




      }
     
      
      # Extract a letter to label the stairway plot
      letter_i <- LETTERS[i]

      # Define a title label of the letter and the position of that title for the physiological indicator
      title_i <- textGrob(label = letter_i
                          , x = unit(0.5, "lines")
                          , y = unit(0, "lines")
                          , hjust = 0
                          , vjust = 0
                          , gp = gpar(fontsize = 20
                                      , fontface = "bold"))

      # Store the plot of the stairway plot and the title label into a list
      list_stairway_plots[[i]] <- arrangeGrob(stairway_plot_i
                                              , top = title_i)
      
    }
    
    if(type_plot == "panel")
    {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create the Panel Stairway Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      
      # Make the panel plot of stairway plots
      panel_stairway <- do.call("grid.arrange"
                                , c(list_stairway_plots
                                    , ncol = 2))
      
      # Label the y-axis
      panel_stairway <- arrangeGrob(panel_stairway
                                    , left = textGrob("Hazard Ratios"
                                                      , gp = gpar(fontface = "bold"
                                                                  , cex = 1.5)
                                                      , rot = 90))
      
      # Make the panel plot of stairway plots with the new legends
      panel_stairway <- grid.arrange(legend_models_thresholds
                                     , legend_median_hr_measurements
                                     , panel_stairway
                                     , heights = c(0.5, 0.5, 15)
                                     , nrow = 3)
      
      # Define file names of the png and pdf versions of the panel of stairway plots
      plot_name.png <- paste("stairway_plot_selected"
                             , "_"
                             , sensitivity_j
                             , "_"
                             , type_of_analysis
                             , ".png"
                             , sep = "")
      plot_name.pdf <- paste("stairway_plot_selected"
                             , "_"
                             , sensitivity_j
                             , "_"
                             , type_of_analysis
                             , ".pdf"
                             , sep = "")
      
      # Save the panel of stairway plots as a png and pdf
      print(plot_name.png)
      ggsave(filename = plot_name.png
             , plot = panel_stairway
             , width = 20
             , height = 16)
      print(plot_name.pdf)
      ggsave(filename = plot_name.pdf
             , plot = panel_stairway
             , width = 20
             , height = 16)
    }
    

    
  }
}