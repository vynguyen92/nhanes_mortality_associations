#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###### FUNCTION TO MAKE STAIRWAY PLOT OF MORTALITY RISK ACROSS THE DISTRIBUTION OF REMAINING INDICATORS #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of the stariway plot of mortality risk across the distribution of 
#          the remaining physiological indicators to compare the results between the sensitivity analysis with
#          all participants to those with measurements within the 1st to 99th percentiles for each body system.
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
#         plot_type - string indicating whether to print the plot as a "png" or a "pdf"
#         dataset_refs - default to NA. If specified, then it's a dataframe of reference groups
#
# Outputs: none - a png or pdf version of the a panel plot of the stariway plots
#

stairway_plots_remaining <- function(list_regression_stats
                                     , dataset_thresholds
                                     , dataset_long
                                     , pi_selected
                                     , type_of_analysis
                                     , current_directory
                                     , name_of_folder
                                     , df_body_systems
                                     , plot_type
                                     , dataset_refs = NA)
{
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
  
  # Include the body systems for remaining physiological indicators
  df_body_systems_other <- df_body_systems %>%
    filter((pi %in% pi_selected) == FALSE)
  
  # Determine the different body systems
  body_systems <- unique(df_body_systems$body_system_categories)
  # Determine the number of body systems
  num_body_systems <- length(body_systems)

  # Define the length of plot for each body system
  length_plot_body_system <- c(22, 26, 5.5, 16, 14)
  
  # Determine the fraction of the plot to hold the stairway plots
  height_plot_system <- c(0.951, 0.96, 0.81, 0.935, 0.925)
  
  # Determine the y position of the box used to separate the analyses performed on all participants to 
  # those performed on the 1st to 99th percentiles
  y_plot_system <- c(0.477, 0.482, 0.41, 0.47, 0.465)
  
  # Define a vector of the sensitivity analyses of interest
  sensitivity_ranges <- c("00_100", "01_99")
  # Determine the number of senstivity analyses of interest
  num_sensitivity <- length(sensitivity_ranges)
  
  # Define a string vector of the letters
  LETTERS <- append(LETTERS, "AA")
  
  # Extract pertinent legend boxes to snitch them together with the panel plot of stairway plots
  legend_boxes <- make_legend_boxes(list_regression_stats
                                    , dataset_thresholds
                                    , size_legend_title = 15
                                    , size_legend_text = 13)
  
  
  # For each body system, make a panel of stairway plots
  for(i in seq(num_body_systems))
  {
    # Determine the body system
    body_system_i <- body_systems[i]
    print(body_system_i)

    # Determine the length of the plot for this body system
    length_plot_i <- length_plot_body_system[i]
    
    # Determine the fraction of the plot use for the panel for this body system
    height_plot_i <- height_plot_system[i]
    
    # Determine the position of the box for delineation for this body system
    y_plot_i <- y_plot_system[i]

    # Define a dataest that contain the physiological indicators belonging to this body system
    df_body_systems_other_i <- df_body_systems_other %>%
      filter(body_system_categories == body_system_i)

    # Determine the names of the physiological indicators in this body system
    names_pi <- df_body_systems_other_i %>%
      dplyr::select(pi_names) %>%
      unlist(.
             , use.names = FALSE)

    # Determine the codenames of the physiological indicators in this body system
    codenames_pi <- df_body_systems_other_i %>%
      dplyr::select(pi) %>%
      unlist(.
             , use.names = FALSE)
    
    # Determine the number of physiological indicators
    num_pi <- length(names_pi)

    # Initialize an empty list to contain the stairway plots for this body system
    list_body_system <- list()

    # Start the counter to help with assign a letter label to each stairway plot
    counter <- 1

    for(j in seq(num_pi))
    {
      # Determine a given physiological indicator within the body system
      pi_j <- codenames_pi[j]
      print(pi_j)

      # Determine the name of the physiological indicator
      names_pi_j <- names_pi[j]

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain the Clinical Thresholds  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

      # Extract the thresholds pertaining to this physiological indicator
      dataset_thresholds_i <- dataset_thresholds %>%
        filter(pi  == pi_j) %>%
        left_join(.
                  , df_body_systems
                  , by = "pi")
      # print(dataset_thresholds_i)
      
      # Determine the different population for the threshold
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

      for(k in seq(num_sensitivity))
      {
        # Determine the sensitivity analysis
        sensitivity_k <- sensitivity_ranges[k]
        print(sensitivity_k)

        # Define a letter to label this plot 
        letter_counter <- LETTERS[counter]

        # Define a title label of the letter and the position of that title for the physiological indicator
        title_counter <- textGrob(label = letter_counter
                                  , x = unit(0.5, "lines")
                                  , y = unit(0, "lines")
                                  , hjust = 0
                                  , vjust = 0
                                  , gp = gpar(fontsize = 20
                                              , fontface = "bold"))


        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~  Create Dataset to Display the Distribution as a Rug  ~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        # Extract the dataset with measurements pertaining to the physiological indicator
        dataset_long_j <- dataset_long %>%
          filter(pi == pi_j)

        # Determine the minimum measurement of the physiological indicator
        min_measurement <- dataset_long_j  %>%
          dplyr::select(pi_value) %>%
          min(.)

        # Determine the maximum measurement of the physiological indicator
        max_measurement <- dataset_long_j %>%
          dplyr::select(pi_value) %>%
          max(.)

        # Define a dataset of the values of the physiological indicator for a given sensitivity analysis
        dataset_distributions_by_pi <- list_regression_stats[["predicted_risk"]] %>%
          filter(pi == pi_j) %>%
          filter(sensitivity_range == sensitivity_k) %>%
          define_distribution(.
                              , dataset_long_j
                              , "pi") %>%
          ungroup(.)
        # View(dataset_distributions_by_pi)

        # Determine the range of the measurements of the selected physiological indicator
        range_distribution <- range(dataset_distributions_by_pi$pi_value)

        # Determine the median of the measurements of the selected physiological indicator
        midpoint_pi_value <- quantile(dataset_distributions_by_pi$pi_value, probs = 0.5) %>%
          unlist(., use.names = FALSE)
        
        # Extract the reference points for the linear and spline models
        if(anyNA(dataset_refs) == TRUE)
        {
          ref_pi_point <- midpoint_pi_value
          
        } else {
          
          ref_pi_point <- dataset_refs %>%
            filter(pi == pi_j) %>%
            filter(sensitivity_range == sensitivity_k) %>%
            filter(treatment_on_pi %in% c("linear", "splines")) %>%
            dplyr::select(ref) %>%
            unique(.) %>%
            unlist(., use.names = FALSE)
          
        }

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~  Form Dataset of Ranges and Hazard Ratios for each Novemtile  ~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        # Determine the dataset of coefficients for the selected physiological indicator and sensitivity analysis
        dataset_coeff <- list_regression_stats[["coefficients"]] %>%
          filter(pi == pi_j) %>%
          filter(sensitivity_range == sensitivity_k)

        # Define the ranges of each novemtiles 
        dataset_novemtiles <- dataset_distributions_by_pi %>%
          define_range_of_novemtile(.
                                    , dataset_coeff
                                    , "pi"
                                    , type_of_analysis
                                    , dataset_refs) %>%
          ungroup(.)

        # print(dataset_novemtiles)

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain Hazard Ratios for Linear and Spline Models  ~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        # Define the dataset of predicted risk from the linear and splines models
        dataset_splines_linear <- list_regression_stats[["predicted_risk"]] %>%
          filter(pi == pi_j) %>%
          filter(sensitivity_range == sensitivity_k)

        # Determine the range of the hazard ratios from the linear and splines model
        range_hr_splines_linear <- range(dataset_splines_linear$hazard_ratio)

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~  Find Hazard Ratios for Splines and Novemtiles that is 1.1x the Minimum  ~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        # Define a dataset of predicted risk from the splines models
        dataset_slines_predicted_risk_i <- dataset_splines_linear %>%
          filter(treatment_on_pi == "splines")

        # Provide info on the hazard ratio is 1.1 times the minimum hazard ratio, the measurements at 1.1x the 
        # minimum hazard ratio, and a boolean to indicate that the 1.1x the minimum hazard ratio intersects with 
        # the spline model
        list_of_features <- define_hr_and_measurements_at_increased_risk(dataset_slines_predicted_risk_i)
        # print(list_of_features)
        
        # Extract the hazard ratio that is 1.1 times the minimum hazard ratio
        hr_splines_1.1 <- list_of_features[["hr_splines_1.1"]]
        
        # Extract the boolean to indicate whether to plot the blue diamonds to show the intersection between 
        # the splines model and when the hazard ratio is 1.1 times the minimum hazard ratio
        plot_intersects <- list_of_features[["to_plot_intersects"]]
        
        # Extract the dataset of measurements when the hazard ratio is 1.1 from the algorithm
        df_measurements_splines_1.1 <- list_of_features[["df_measurements"]]
        
        # Extract measurements when the hazard ratio is 1.1 if the algorithm cannot automatically detect the 
        # measurement
        df_extra_measurements_i_j <- df_extra_measurements %>%
          filter(pi == pi_j) %>%
          filter(sensitivity_range == sensitivity_k) %>% 
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

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~  Update Dataset of Thresholds to Draw Arrows in the Stairway Plots  ~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        if(anyNA(unique_populations) == FALSE)
        {
          # Determine the percentage of the range of the measurements to help determine the width of the arrows 
          # to show the unfavorable directions
          percent_of_distribution <- 0.03*abs(diff(range_distribution))
          
          # Determine the percentage of the range of the hazard ratios
          percent_of_hr_dist <- 0.6*abs(diff(range_hr_splines_linear))

          # Include x and y positions to help draw the arrows of unfavorable directions 
          dataset_thresholds_i <- dataset_thresholds_i %>%
            # Define the y-coordinate of where to draw the arrows of unfavorable direction of the thresholds
            mutate(y_position = hr_splines_1.1 + percent_of_hr_dist) %>%
            # Define the x-coordinate of where to start the arrows of unfavorable direction of the thresholds 
            mutate(x_position = case_when(direction_final == "lower threshold" ~ min_threshold
                                          , direction_final == "upper threshold" ~ max_threshold)) %>%
            # Define the x coordinate of where to end the arrows of the unfavorable direction of the thresholds 
            mutate(xend_position = case_when(direction_final == "lower threshold" ~ 
                                               min_threshold - percent_of_distribution
                                             , direction_final == "upper threshold" ~ 
                                               max_threshold + percent_of_distribution)) 
        }

        # Define a subset of the thresholds to include those where the minium and maximum thresholds are
        # differentfor the same type of bound.
        # We will draw boxes for these thresholds to show the ranges of the thresold.
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

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create the Stairway Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

        # For thresholds with ranges, then draw boxes with the width showing the range of the clinical 
        # threshold
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
                            , linetype = factor(population))
                      , fill = "pink"
                      , inherit.aes = FALSE
                      , alpha = 0.4
                      # , size = 0.8
            )
        }

        # Make the stairway plot to show the mortality risk progression across physiological indicator for 
        # the linear, novemtiles, and splines models
        stairway_plot <- stairway_plot_i +
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
          geom_line(data = dataset_splines_linear
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
          # Draw a navy dashed line to represent when the hazard ratio is 1.1 times the minimum hazard ratio
          geom_hline(yintercept = c(hr_splines_1.1)
                     , color = c("#000080")
                     , linetype = c( "dotdash")
                     , size = 0.6)  +
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
          scale_color_manual(name = "Types of Model"
                             , values = c("#CC0000", "#F0B707", "#000080")) +
          scale_y_log10(
            # limits = c(0.05,100)
            ) +
          scale_x_continuous(
            breaks = round(quantile(dataset_distributions_by_pi$pi_value
                                    , probs = c(0,0.1,0.25,0.5,0.75,0.9,1)
                                    , names = FALSE)
                           , digits = 1)
            # , limits = c(min_measurement, max_measurement)
            ) +
          # Color the boxes based on significance of the novemtiles
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
          xlab(names_pi_j) +
          theme_bw() +
          theme(legend.position = "none"
                , axis.title = element_text(size = 14)
                , axis.text.y = element_text(size = 12)
                , axis.title.y = element_blank()
                , plot.title = element_text(hjust = 0.5
                                            , size = 14))

        
        # If thresholds are available for the physiological indicator, then draw the arrows of unfavorable
        # directions and lines to represent the thresholds
        if(anyNA(unique_populations) == FALSE)
        {
          stairway_plot <- stairway_plot +
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
            stairway_plot <- stairway_plot +
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
        }

        # If there are intersections between the splines model and when the hazard ratio is 1.1
        # times the minimum hazard ratio, then draw navy diamonds to represent these measurements
        # if(plot_intersects == TRUE)
        # {
          # df_measurements_splines_1.1 <- list_of_features[["df_measurements"]]

          stairway_plot <- stairway_plot +
            geom_point(data = df_measurements_splines_1.1
                     , aes(x = x
                           , y = y)
                     , color = "#000080"
                     , shape = 18
                     , size = 6)
        # }
        
        # Make the plot of the stairway plot and the title label
        stairway_plot <- arrangeGrob(stairway_plot
                                     , top = title_counter)

        # If the physiological is the first physiological both either sensitivity analysis, 
        # put a title label indicating the sensitivity analysis
        if(j == 1)
        {
          if(k == 1)
          {
            stairway_plot <- arrangeGrob(stairway_plot
                                         , top = textGrob("All Participants"
                                                          , gp = gpar(cex = 1.5
                                                                      , fontface = "bold"
                                                                      , col = "red2")))

          } else {
            stairway_plot <- arrangeGrob(stairway_plot
                                         , top = textGrob("Participants in 1st and 99th Percentiles"
                                                          , gp = gpar(cex = 1.5
                                                                      , fontface = "bold"
                                                                      , col = "red2")))
          }

          stairway_plot <- arrangeGrob(stairway_plot
                                       , top = textGrob(""
                                                        , gp = gpar(cex = 1.0)))
        }

        # Store the plot of the stairway plot and the title label(s) into a list
        list_body_system[[counter]] <- stairway_plot

        # Increment the counter 
        counter <- counter + 1
      }
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create the Panel Stairway Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
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
                                                  , widths = c(0.55, 0.4, 0.15, 0.15)
                                                  , nrow = 1)

    # Make the panel plot of stairway plots
    body_system_panel_stairway <- do.call("grid.arrange"
                                          , c(list_body_system
                                              , ncol = 2))

    # Label the y-axis
    body_system_panel_stairway <- arrangeGrob(body_system_panel_stairway
                                              , left = textGrob("Hazard Ratios"
                                                                , gp = gpar(fontface = "bold"
                                                                            , cex = 1.5)
                                                                , rot = 90))

    # Make the panel plot of stairway plots with the new legends
    body_system_panel_stairway <- grid.arrange(legend_models_thresholds
                                               , legend_median_hr_measurements
                                               , body_system_panel_stairway
                                               , heights = c(0.4, 0.4, (length_plot_i - 1))
                                               , nrow = 3)

    # Define the body system name to be conducive for a file name
    body_system_pattern <- tolower(body_system_i) %>%
      gsub(" ", "_", .)

    plot_name <- paste("stairway_plot_remaining"
                           , "_"
                           , body_system_pattern
                           , "_"
                           , type_of_analysis
                           , "."
                           , plot_type
                           , sep = "")
    
    print(plot_name)
    
    if(plot_type == "png")
    {
      
      png(plot_name, height = length_plot_i, width = 17, units = "in", res = 1000)
      body_system_panel_stairway <- grid.draw(arrangeGrob(body_system_panel_stairway))
      # Draw two red boxes to differentiate the sensitivity analysis
      grid.rect(x = c(0.265, 0.753)
                , y = rep(y_plot_i, 2)
                , width = c(0.49, 0.487)
                , height = rep(height_plot_i, 2)
                , gp = gpar(col = "red2", fill = NA))
      dev.off()
      
    } else if(plot_type == "pdf") {
      
      pdf(plot_name, height = length_plot_i, width = 17)
      body_system_panel_stairway <- grid.draw(arrangeGrob(body_system_panel_stairway))
      # Draw two red boxes to differentiate the sensitivity analysis
      grid.rect(x = c(0.265, 0.753)
                , y = rep(y_plot_i, 2)
                , width = c(0.49, 0.487)
                , height = rep(height_plot_i, 2)
                , gp = gpar(col = "red2", fill = NA))
      dev.off()
      
    }

  }
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
  # body_system_panel_stairway
}