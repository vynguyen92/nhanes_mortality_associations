#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############################  FUNCTION TO MAKE LEGEND BOXES FROM DUMMY PLOTS  ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a series of dummy plots to extract the legends for the different statistics 
#
# Inputs: list_stats - a list of dataframe of the regression results. A dataframe is available each to 
#                         contain the coefficients, prediction performance, and predicted risk
#         thresholds_dataset - dataframe of the clinical thresholds
#         size_legend_title - size of the legend title. Defaults as 17.
#         size_legend_text - size of the legend text. Defaults as 15.
#
# Outputs: list_legends - a list containing the information to make the legend boxes
#

make_legend_boxes <- function(list_stats
                              , thresholds_dataset
                              , size_legend_title = 17
                              , size_legend_text = 15)
{
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~  Extract the Legend Box for the Colors of the Models and Linetype of Thresholds  ~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Define a dummy dataset of the coefficents of the novemtiles for a physiological indicator
  dataset_novemtiles <- list_stats[["coefficients"]] %>%
    filter(grepl("^novemtile", term) == TRUE) %>%
    filter(pi == "BMXBMI") %>%
    filter(sensitivity_range == "00_100")
    
  # Define a dummy dataset of the predicted risk of the linear and splines model for a physiological indicator
  dataset_splines_linear <- list_stats[["predicted_risk"]] %>%
    filter(pi == "BMXBMI") %>%
    filter(sensitivity_range == "00_100")
  
  # Define a dummy dataset of the thresholds to contain the different types of population
  thresholds_dataset <- thresholds_dataset %>%
    ungroup(.) %>%
    filter(pi %in% c("BMXBMI", "BMXWAIST")) %>%
    mutate(population = factor(population
                               , levels = c("all"
                                            , "adult females"
                                            , "adult males"))) %>%
    # Define a dummy column to draw the thresholds
    mutate(x_position = 1
           , y_position = 1
           , xend_position = 2)
  
  # Make a dummy plot of the stairway plot with the pink vertical lines for the thresholds
  plot_models_thresholds <- ggplot() +
    # Draw boxes to represent the mortality risk from the novemtile models 
    geom_rect(data = dataset_novemtiles
                  , mapping = aes(xmin =  exp(conf.low)
                                  , xmax = exp(conf.high)
                                  , ymin = exp(conf.low)
                                  , ymax = exp(conf.high)
                                  , color = treatment_on_pi) 
                , alpha = 0.8) +
    # Draw lines to represent the mortality from the linear and spline models
    geom_line(data = dataset_splines_linear
              , aes(x = pi_value
                    , y = hazard_ratio
                    , group = treatment_on_pi
                    , color = treatment_on_pi)
              , inherit.aes = FALSE
              , linetype = "solid"
              , size = 1.0) +
    # Draw vertical lines to represent the thresholds for different populations
    geom_vline(data = thresholds_dataset
               , aes(xintercept = min_threshold
                     , linetype = population)
               , color = "pink"
               , size = 0.8) +
    guides(color = guide_legend(order = 1)
           , linetype = guide_legend(order = 2)) +
    scale_color_manual(name = "Types of Model"
                       , values = c("#CC0000", "#F0B707", "#000080")) +
    scale_linetype_manual(name = "Populations for Clinical Thresholds"
                          , values = c("solid", "dashed", "dotted")) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))

  # Extract the legend box of colors and linetype
  legend_models_thresholds <- gtable_filter(ggplotGrob(plot_models_thresholds)
                                            , "guide-box")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~  Extract the Legend Box for the Thresholds for the Different Population  ~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Make a dummy plot of vertical lines for the threshold
  plot_thresholds <- ggplot() +
    # Draw vertical lines to represent the thresholds for different populations
    geom_vline(data = thresholds_dataset
               , aes(xintercept = min_threshold
                     , linetype = population)
               , color = "pink"
               , size = 0.8) +
    guides( linetype = guide_legend(order = 2)) +
    scale_linetype_manual(name = "Populations for Clinical Thresholds"
                          , values = c("solid", "dashed", "dotted")) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))
  
  # Extract the legend box of linetype
  legend_thresholds <- gtable_filter(ggplotGrob(plot_thresholds)
                                            , "guide-box")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~  Extract the Legend Box for the Arrows Showing the Unfavorable Directions  ~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Make a dummy plot of arrows showing the unfavorable directions of the thresholds
  plot_arrow <- ggplot() +
    # Draw arrows of the unfavorable direction
    geom_segment(data = thresholds_dataset
               , aes(x = x_position
                     , y = y_position
                     , xend = xend_position
                     , yend = y_position
                     , linetype = "")
               , size = 1.25
               , arrow = arrow(length = unit(0.3
                                             , "cm"))
               , color = "pink") +
    scale_linetype_manual(name = "Unfavorable Direction"
                          , values = c("solid")) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))
  
  # Extract the legend box of arrows
  legend_arrow <- gtable_filter(ggplotGrob(plot_arrow)
                                            , "guide-box")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~  Extract the Legend Box for the Color Bar of Significance  ~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Make a dummy plot of the stairway plot to show the significance of each novemtile
  plot_p_values <- ggplot() +
    # Draw boxes to represent the mortality risk from the novemtile models 
    geom_rect(data = dataset_novemtiles
              , mapping = aes(xmin =  exp(conf.low)
                              , xmax = exp(conf.high)
                              , ymin = exp(conf.low)
                              , ymax = exp(conf.high)
                              # , color = treatment_on_pi
                              , fill = p.value) 
              , alpha = 0.8) +
    guides(fill = guide_colorbar(barwidth = 30
                                 , label.theme = element_text(size = 8)
                                 , nbin = 200)) +
    # Color the boxes based on significance of the novemtiles
    scale_fill_gradientn(name = "p-values"
                         , colors = c("gray10","gray50", "gray85", "gray95")
                         , limits = c(0,1)
                         , breaks = c(0.01, 0.05, 0.1, 1)
                         , values = c(0, 0.01
                                      , 0.011, 0.05
                                      , 0.051, 0.1
                                      , 0.101, 1)) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))
  
  # Extract the legend box of color bar
  legend_p_values <- gtable_filter(ggplotGrob(plot_p_values)
                                            , "guide-box")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~  Extract the Legend Box for Dashed Navy Line and Diamonds for When the Hazard Ratio is 1  ~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Define a dummy dataset of when the hazard ratio is 1.1
  df_hr_1.1 <- data.frame(analysis_type = c( "")
                          , hr_1.1 = c(1))
  
  # Make a dummy plot to draw the horizontal line and diamonds when the hazard ratio is 1.1
  plot_measurement_at_risk <- ggplot() +
    # Draw the horizontal navy line when the mortality risk is 1.1
    geom_line(data = df_hr_1.1
               , aes(y = hr_1.1
                     , x = c( 1)
                     , linetype = analysis_type)
              , color = "#4BB74C"
               , size = 0.8) +
    # Draw the diamonds to represent the intersection between the spline model and when the hazard ratio is 1.1
    geom_point(data = df_hr_1.1
               , aes(x = 1
                     , y = hr_1.1
                     , color = analysis_type)
               , size = 5
               , shape = 18) +
    scale_color_manual(name = "1.1 Times Minimum Hazard Ratio"
                       , values = c("#000080")) +
    scale_linetype_manual(name = "1.1 Times Minimum Hazard Ratio"
                           , values = "dotdash") +
    guides(color = guide_legend(order = 2
                                , title = element_blank())
           , linetype = guide_legend(order = 1)) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))

  # Extract the legend box for the navy line and diamonds
  legend_measurement_at_risk <- gtable_filter(ggplotGrob(plot_measurement_at_risk)
                                              , "guide-box")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~  Extract the Legend Box for the Purple Dot to Represent the Reference Point  ~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Define a dummy dataset for the Reference Point
  df_ref <- data.frame(x = c(1)
                          , y = c(1)
                          , stats = c("Reference Point")
                          , stringsAsFactors = FALSE)
  
  # Make a dummy plot of the reference point
  plot_ref_purple <- ggplot() +
    # Draw a purple dot to represent the reference point
    geom_point(data = df_ref
               , aes(x = x
                     , y = y
                     , color = stats
                     )
               , size = 4)  +
    scale_color_manual(name = ""
                       , values = c("purple")) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))
  
  # Extract the legend box for the purple dot
  legend_ref_purple <- gtable_filter(ggplotGrob(plot_ref_purple)
                                     , "guide-box")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~  Extract the Legend Box for the Brown Dot to Represent the Reference Point  ~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Make a dummy plot of the reference point
  plot_ref_brown <- ggplot() +
    # Draw the brown point to represent the reference point
    geom_point(data = df_ref
               , aes(x = x
                     , y = y
                     , color = stats)
               , size = 4)  +
    scale_color_manual(name = ""
                       , values = c("brown")) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))
  
  # Extract the legend box for the brown dot
  legend_ref_brown <- gtable_filter(ggplotGrob(plot_ref_brown)
                                     , "guide-box")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~  Extract the Legend Box for the Black Dot to Represent the Median  ~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Make a dummy dataset of the median
  df_median <- data.frame(x = c(1)
                          , y = c(1)
                          , stats = c("Median")
                          , stringsAsFactors = FALSE)
  
  # Make a dummy plot of the median
  plot_median_black <- ggplot() +
    # Draw the black point for the median
    geom_point(data = df_median
               , aes(x = x
                     , y = y
                     , color = stats
               )
               , size = 4)  +
    scale_color_manual(name = "Central Statistic"
                       , values = c("black")) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))
  
  # Extract the legend box for the black dot
  legend_median_black <- gtable_filter(ggplotGrob(plot_median_black)
                                 , "guide-box")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~  Extract the Legend Box for the Color to Represent Gender  ~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Make a dummy dataset for gender
  dataset_gender <- data.frame(x = c(1,1)
                               , y = c(2,2)
                               , gender = c("Males", "Females")) %>%
    mutate(gender = factor(gender
                           , levels = c("Males", "Females")))
  
  # Make a dummy plot to show the lines for each gender
  plot_gender <- ggplot() +
    # Draw the lines
    geom_line(data = dataset_gender
               , aes(x = x
                     , y = y
                     , color = gender)
              , linetype = "solid"
              , size = 1.5)  +
    scale_color_manual(name = "Gender"
                       , values = c("purple", "#FF8C00")) +
    theme_bw() +
    theme(legend.position = "top"
          , legend.title = element_text(size = size_legend_title)
          , legend.text = element_text(size = size_legend_text))
  
  # Extract the legend box for the color
  legend_gender <- gtable_filter(ggplotGrob(plot_gender)
                                       , "guide-box")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Define a List to Contain all the Legend Boxes  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  list_legends <- list("models_thresholds" = legend_models_thresholds
                       , "arrows" = legend_arrow
                       , "p_values" = legend_p_values
                       , "hr_measurements" = legend_measurement_at_risk
                       , "ref_purple" = legend_ref_purple
                       , "ref_brown" = legend_ref_brown
                       , "median_black" = legend_median_black
                       , "thresholds" = legend_thresholds
                       , "gender" = legend_gender)

  return(list_legends)
  # plot_ref
  
}