#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###### FUNCTION TO MAKE SPLINES PLOT OF MORTALITY RISK BY GENDER FOR REMAINING PHYSIOLOGICAL INDICATORS #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of the splines plot by sex of mortality risk across the   
#          distributionof the remaining physiological indicators for each sensitivity analysis and for each 
#          body system.
#
# Inputs: list_regression_stats - a list of dataframe of the regression results. A dataframe is available each  
#                                 to contain the coefficients, prediction performance, and predicted risk
#         dataset_long - long-formatted dataset based on the physiological indictors
#         dataset_wide - wide-formatted dataset containing the demographics, physiological indicators,  
#                        mortality info for each participant
#         dataset_thresholds - dataframe of the clinical thresholds
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         name_of_folder - string of the name of the new folder to hold the plots
#         df_body_systems - dataframe of the body system of the physiological indicators 
#         list_dataset_refs - default to NA. If specified, then it's a list of dataframe of reference groups 
#                             with one for unweighted and the other for weighted results
#
# Outputs: none - png and pdf versions of the a panel plot of the splines plots
#

splines_plots_sex_remaining <- function(list_regression_stats
                                        , dataset_long
                                        , dataset_wide
                                        , dataset_thresholds
                                        , current_directory
                                        , name_of_folder
                                        , df_body_systems
                                        , list_dataset_refs = NA)
{
  df_extra_measurements <- read_excel("NHANES - Dataset of Physiological Measurements with Hazard Ratio at 1.1.xlsx"
                                      , sheet = "gender")
  
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
  
  # Determine the analysis types (i.e. "weighted" or "unweighted")
  analysis_types <- names(list_regression_stats)
  # Determine number of analysis types
  num_analysis_types <- length(analysis_types)
  
  # Determine the physiological indicators that don't have sex-specific thresholds
  pi_remaining <- dataset_thresholds %>%
    ungroup(.) %>%
    filter(grepl("^all", population) == TRUE) %>%
    dplyr::select(pi) %>%
    unique(.) %>%
    unlist(., use.names = FALSE)
  
  # Include the body systems for remaining physiological indicators
  df_body_systems_other <- df_body_systems %>%
    filter((pi %in% pi_remaining) == TRUE)
  
  # Determine the different body systems
  body_systems <- unique(df_body_systems$body_system_categories)
  # Determine the number of body systems
  num_body_systems <- length(body_systems)
  
  # Extract pertinent legend boxes to snitch them together with the panel plot of stairway plots
  legend_boxes <- make_legend_boxes(list_pi_unweighted_results
                                    , dataset_thresholds
                                    , size_legend_title = 15
                                    , size_legend_text = 13)
  
  # Define the length of plot for each body system
  length_plot_body_system <- c(5.5, 26, 11, 20, 14)
  
  # For each analysis type make a series of panel plot of splines plot by gender for each sensitivity analysis
  for(k in seq(num_analysis_types))
  {
    # Determine the analysis type
    analysis_type_k <- analysis_types[k]
    print(analysis_type_k)
    
    # Extract the dataset of reference points by gender for the analysis type
    if(anyNA(list_dataset_refs) == TRUE)
    {
      df_refs <- NA
      
    } else {
      
      df_refs <- list_dataset_refs[[analysis_type_k]]
      
    }
    
    # Extract the dataset of predicted risk for the selected physiological indicators and 
    # change the gender to have categories that matches the gender names in the thresholds dataset
    df_predicted_risk <- list_regression_stats[[k]] %>%
      filter(pi %in% pi_remaining) %>%
      mutate(gender = case_when(gender == "_female" ~ "adult females"
                                , gender == "_male" ~ "adult males")) 
    # View(df_predicted_risk)

    # Determine the sensitivity analyses
    sensitivity_analyses <- df_predicted_risk %>%
      ungroup(.) %>%
      dplyr::select(sensitivity_range) %>%
      unique(.) %>%
      unlist(.
             , use.names = FALSE)
    # Determine the number of sensitivity analyses
    num_sensitivity_analyses <- length(sensitivity_analyses)
    
    # Make a panel of splines plot for a given sensitivity analysis
    for(j in seq(num_sensitivity_analyses))
    {
      # Determine the sensitivity analysis
      sensitivity_j <- sensitivity_analyses[j]
      print(sensitivity_j)
      
      # Use the information of the sensitivity analysis to determine the minimum and maximum percentiles
      range_percentiles <- strsplit(sensitivity_j, "_") %>%
        unlist(.) %>%
        as.numeric(.)
      
      # Determine the minimum percentile for the sensitivity analyses
      min_perc <- range_percentiles[1]
      
      # Determine the maximum percentile for the sensitivity analysis
      max_perc <- range_percentiles[2]
      
      
      # For each body system, make a panel of stairway plots
      for(i in seq(num_body_systems))
      {
        # Determine the body system
        body_system_i <- body_systems[i]
        print(body_system_i)

        # Initialize an empty list to hold the spline plot for physiological indicator and gender
        list_splines_plots <- list()
        
        # Determine the length of the plot for this body system
        length_plot_i <- length_plot_body_system[i]

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

        # Start the counter to help with assign a letter label to each splines plot
        counter <- 1
        
        # Draw a spline plot for both gender for each physiological indicator
        for(p in seq(num_pi))
        {
          # Determine the codename of the selected physiological indicator
          pi_p <- codenames_pi[p]
          print(pi_p)
          
          # Determine the name of the selected physiological indicator
          pi_name_i <- attr(dataset_wide[,pi_p],"label")
          
          # Extract the dataset of original measurements for the selected physiological indicator and 
          # change the gender to have categories that matches the gender names in the thresholds dataset
          dataset_long_i <- dataset_long %>%
            filter(pi == pi_p) %>%
            mutate(gender = case_when(gender == "_female" ~ "adult females"
                                      , gender == "_male" ~ "adult males"))
          
          # Determine the physiological measurement corresponding to the minimum percentile for the sensitivity 
          # analysis
          min_pi_perc_k <- quantile(dataset_long_i$pi_value
                                    , probs = min_perc/100
                                    , names = FALSE)
          
          # Determine the physiological measurement corresponding to the maximum percentile for the sensitivity
          # analysis
          max_pi_perc_k <- quantile(dataset_long_i$pi_value
                                    , probs = max_perc/100
                                    , names = FALSE)
          
          # Determine the dataset to include participants who are with these percentiles for analysis
          dataset_long_i <- dataset_long_i %>%
            filter(pi_value >= min_pi_perc_k &
                     pi_value <= max_pi_perc_k)
          
          # Determine the genders
          gender <- unique(dataset_long_i$gender)
          # Determine the number of genders
          num_gender <- length(gender)
          
          # Determine the median of the physiological measurements
          midpoint_pi_value <- quantile(dataset_long_i$pi_value, probs = 0.5) %>%
            unlist(., use.names = FALSE)
          
          # Determine the range of the physiological measurements
          range_distribution <- range(dataset_long_i$pi_value)
          
          
          for(g in seq(num_gender))
          {
            # Determine the gender 
            gender_g <- gender[g]
            print(gender_g)
            
            # Assign the linetype and color based on the gender 
            if(gender_g == "adult males")
            {
              linetypes_thresholds <- "dotted"
              color_sex <- "purple"
              gender_original <- "_male"
              
            } else if(gender_g == "adult females") {
              
              linetypes_thresholds <- "dashed"
              color_sex <- "#FF8C00"
              gender_original <- "_female"
            }
            
            # Determine the median measurement of the physiological indicator for a given gender
            midpoint_pi_value <- dataset_long_i %>%
              filter(gender == gender_g) %>%
              dplyr::select(pi_value) %>%
              unlist(., use.names = FALSE) %>%
              quantile(., probs = 0.5, names = FALSE) 
            
            # Extract the reference points for a given physiological indicator, sensitivity analysis, and 
            # gender
            if(anyNA(df_refs) == TRUE)
            {
              ref_pi_point <- midpoint_pi_value
              
            } else {
              
              ref_pi_point <- df_refs %>%
                filter(pi == pi_p) %>%
                filter(sensitivity_range == sensitivity_j) %>%
                filter(gender == gender_original) %>%
                dplyr::select(ref) %>%
                unique(.) %>%
                unlist(., use.names = FALSE)
              
            }
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain Hazard Ratios for Spline Models  ~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            
            # Define the dataset of predicted risk from the splines model for a given physiological indicator, 
            # sensitivity analysis, and gender
            df_predicted_risk_g <- df_predicted_risk %>%
              filter(pi == pi_p) %>%
              filter(sensitivity_range == sensitivity_j) %>%
              filter(gender == gender_g)
            
            # Determine the range of the hazard ratios from the splines model
            range_hr_splines_linear <- range(df_predicted_risk_g$hazard_ratio)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~  Find Hazard Ratio that is 1.1x the Minimum  ~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            
            # Provide info on the hazard ratio is 1.1 times the minimum hazard ratio, the measurements at 1.1x  
            # the minimum hazard ratio, and a boolean to indicate that the 1.1x the minimum hazard ratio  
            # intersects with the spline model
            list_of_features <- define_hr_and_measurements_at_increased_risk(df_predicted_risk_g)
            
            # Extract the hazard ratio that is 1.1 times the minimum hazard ratio
            hr_splines_1.1 <- list_of_features[["hr_splines_1.1"]]
            # print(hr_splines_1.1)
            
            # Extract the boolean to indicate whether to plot the blue diamonds to show the intersection between 
            # the splines model and when the hazard ratio is 1.1 times the minimum hazard ratio
            plot_intersects <- list_of_features[["to_plot_intersects"]]
            
            # Extract measurements when the hazard ratio is 1.1 if the algorithm cannot automatically detect the 
            # measurement
            df_extra_measurements_i_j <- df_extra_measurements %>%
              filter(pi == pi_p) %>%
              filter(sensitivity_range == sensitivity_j) %>%
              filter(gender == gender_g) %>%
              filter(analysis_type == analysis_type_k) %>%
              dplyr::select(x, y)
            
            # Extract the dataset of measurements when the hazard ratio is 1.1 from the algorithm
            df_measurements_splines_1.1 <- list_of_features[["df_measurements"]]
            
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
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain the Clinical Thresholds  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            
            # Determine the percentage of the range of the measurements to help determine the width of the  
            # arrows to show the unfavorable directions
            percent_of_distribution <- 0.02*abs(diff(range_distribution))
            
            # Determine the percentage of the range of the hazard ratios
            percent_of_hr_dist <- 0.25*abs(diff(range_hr_splines_linear))
            
            # Include x and y positions to help draw the arrows of unfavorable directions 
            dataset_thresholds_g <- dataset_thresholds %>%
              filter(pi == pi_p) %>%
              mutate(gender = population) %>%
              # Define the y-coordinate of where to draw the arrows of unfavorable direction of the thresholds
              mutate(y_position = hr_splines_1.1 + percent_of_hr_dist) %>%
              # Define the x-coordinate of where to start the arrows of unfavorable direction of the thresholds 
              mutate(x_position = case_when(direction_final == "lower threshold" ~ min_threshold
                                            , direction_final == "upper threshold" ~ max_threshold)) %>%
              # Define the x coordinate of where to end the arrows of the unfavorable direction of the 
              # thresholds
              mutate(xend_position = case_when(direction_final == "lower threshold" ~ 
                                                 min_threshold - percent_of_distribution
                                               , direction_final == "upper threshold" ~ 
                                                 max_threshold + percent_of_distribution)) %>%
              # Determine the difference between the max and minimum threshold for the same type of bound. 
              mutate(dif = abs(max_threshold - min_threshold))
            
            # Define a subset of the thresholds to include those where the minium and maximum thresholds are 
            # different for the same type of bound.
            # We will draw boxes for these thresholds to show the ranges of the threshold.
            dataset_thresholds_rect <- dataset_thresholds_g %>%
              filter(dif != 0)
            # print(dataset_thresholds_rect)
            
            # Determine the number of rows for the thresholds that have ranges.
            num_rows_thresholds_rect <- nrow(dataset_thresholds_rect)
            
            # Define a subset of the thresholds to include those where the minium and maximum thresholds are 
            # the same for the same type of bound.
            # We will draw lines for these thresholds.
            dataset_thresholds_line <- dataset_thresholds_g %>%
              filter(dif == 0)
            # print(dataset_thresholds_line)
            
            # Determine the genders
            unique_populations <- dataset_thresholds_g$population %>%
              unique(.)
            
            # Determine the number of unique gender
            num_unique_populations <- unique_populations %>%
              length(.)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~  Create Dataset to Display the Distribution as a Rug  ~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            
            # Define a dataset of the values of the physiological indicator for a given sensitivity analysis and 
            # gender
            dataset_distributions_by_sensitivity_g <- df_predicted_risk_g %>%
              filter(pi %in% pi_p) %>%
              filter(sensitivity_range == sensitivity_j) %>%
              filter(gender == gender_g) %>%
              define_distribution(.
                                  , dataset_long_i
                                  , "gender") %>%
              ungroup(.)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create the Splines Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            
            # For thresholds with ranges, then draw boxes with the width showing the range of the clinical 
            # threshold
            if(num_rows_thresholds_rect == 0)
            {
              splines_plot <- ggplot(data = df_predicted_risk_g)
            } else {
              splines_plot <- ggplot(data = df_predicted_risk_g) +
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
            
            # Make the splines plot to show the mortality risk progression across physiological indicator the 
            # splines model for a given gender
            splines_plot <- splines_plot +
              # Draw a line to show the mortality risk across the physiological indicator for a given gender
              geom_line(aes(x = pi_value
                            , y = hazard_ratio
                            , group = gender)
                        , color = color_sex
                        , inherit.aes = FALSE
                        , linetype = "solid"
                        , size = 1.0) +
              # Draw a rug or flatten histogram to show distribution of the physiological indicator
              geom_rug(data = dataset_distributions_by_sensitivity_g
                       , aes(x = pi_value
                             , y = rep(1, length(pi_value)))
                       # , color = "#000080"
                       , size = 0.2
                       , sides = "b"
                       , alpha = 0.5) +
              # Draw pink arrows to show the unfavorable directions of the thresholds
              geom_segment(data = dataset_thresholds_g
                           , aes(x = x_position
                                 , y = y_position
                                 , xend = xend_position
                                 , yend = y_position)
                           , size = 1.25
                           , arrow = arrow(length = unit(0.3
                                                         , "cm"))
                           , color = "pink") +
              # Draw a navy dashed line to represent when the hazard ratio is 1.1 times the minimum hazard 
              # ratio
              geom_hline(yintercept = c(hr_splines_1.1)
                         , color = c("#000080")
                         , linetype = c( "dotdash")
                         , size = 0.6) +
              # Draw black horizontal line to represent when the hazard ratio is 1
              geom_hline(yintercept = 1.0
                         , color = "black"
                         , linetype = "solid"
                         , size = 0.2) +
              # Draw a brown dot to show the reference point
              geom_point(aes(x = ref_pi_point
                             , y = 1.0)
                         , color = "brown"
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
              xlab(pi_name_i) +
              ylab("Hazard Ratios") +
              scale_y_log10() +
              scale_x_continuous(
                breaks = round(quantile(dataset_long_i$pi_value
                                        , probs = c(0,0.1,0.25,0.5,0.75,0.9,1)
                                        , names = FALSE)
                               , digits = 1)
                # , limits = c(min(dataset_long_i$pi_value)
                #              , max(dataset_long_i$pi_value))
              ) +
              theme_bw() +
              theme(legend.position = "none"
                    , axis.title = element_text(size = 14)
                    , axis.text.y = element_text(size = 12)
                    , axis.text.x = element_text(size = 8.0)
                    , axis.title.y = element_blank()
                    , plot.title = element_text(hjust = 0.5
                                                , size = 14))
            
            # If thresholds are available for the physiological indicator, then draw the arrows of unfavorable
            # directions and lines to represent the thresholds
            if(anyNA(unique_populations) == FALSE)
            {
              splines_plot <- splines_plot +
                # Draw pink arrows to show the unfavorable directions of the thresholds
                geom_segment(data = dataset_thresholds_g
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
                           , aes(xintercept = min_threshold)
                           , linetype = "solid"
                           , color = "pink"
                           , size = 1
                           , alpha = 0.75) +
                scale_linetype_manual(name = "Populations for Clinical Thresholds"
                                      , values = linetypes_thresholds) +
                guides(color = guide_legend(order = 1),
                       linetype = guide_legend(order = 2))
              
            }
            
            
            # Define a letter to label this plot
            letter_counter <- LETTERS[counter]

            # Define a title label of the letter and the position of that title for the physiological 
            # indicator
            title_counter <- textGrob(label = letter_counter
                                      , x = unit(0.5, "lines")
                                      , y = unit(0, "lines")
                                      , hjust = 0
                                      , vjust = 0
                                      , gp = gpar(fontsize = 20
                                                  , fontface = "bold"))

            # Make the plot of the splines plot and the title label
            splines_plot <- arrangeGrob(splines_plot
                                        , top = title_counter)

            # Store the plot of the splines plot and the title label(s) into a list
            list_splines_plots[[counter]] <- splines_plot

            # Increment the counter
            counter <- counter + 1
          }
          
        }
        
        # Make a legend containing the legend boxes for the gender, median, reference points, and the
        # hazard ratio at 1.1 times the minimum
        legend_gender_median <- grid.arrange(legend_boxes$gender
                                             , legend_boxes$median_black
                                             , legend_boxes$ref_brown
                                             , legend_boxes$hr_measurements
                                             , widths = c(1.0, 0.3, 0.3, 1.0)
                                             , nrow = 1)

        # Make a legend containing the thresholds and unfavorable directions
        legend_thresholds <- grid.arrange(legend_boxes$thresholds
                                          , legend_boxes$arrows
                                          , widths = c(1.0, 1.0)
                                          , nrow = 1)

        # Make the panel plot of splines plots
        panel_splines <- do.call("grid.arrange"
                                 , c(list_splines_plots
                                     , ncol = 2))

        # Label the y-axis
        panel_splines <- arrangeGrob(panel_splines
                                     , left = textGrob("Hazard Ratios"
                                                       , gp = gpar(fontface = "bold"
                                                                   , cex = 1.5)
                                                       , rot = 90))

        # Make the panel plot of splines plots with the new legends
        panel_splines <- grid.arrange(legend_gender_median
                                      , legend_thresholds
                                      , panel_splines
                                      , heights = c(0.4, 0.4, (length_plot_i - 0.8))
                                      , nrow = 3)

        # Define file names of the png and pdf versions of the panel of splines plots
        plot_name.png <- paste("splines_plot_remaining"
                               , "_"
                               , sensitivity_j
                               , "_"
                               , body_system_i
                               , "_"
                               , analysis_type_k
                               , ".png"
                               , sep = "")
        plot_name.pdf <- paste("splines_plot_remaining"
                               , "_"
                               , sensitivity_j
                               , "_"
                               , body_system_i
                               , "_"
                               , analysis_type_k
                               , ".pdf"
                               , sep = "")

        # Save the panel of stairway plots as a png and pdf
        print(plot_name.png)
        ggsave(filename = plot_name.png
               , plot = panel_splines
               , width = 20
               , height = length_plot_i)
        print(plot_name.pdf)
        ggsave(filename = plot_name.pdf
               , plot = panel_splines
               , width = 20
               , height = length_plot_i)
        
      }
      
    }

  }
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
  # splines_plot

}