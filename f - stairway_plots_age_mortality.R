#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################## FUNCTION TO MAKE STAIRWAY PLOT OF MORTALITY RISK ACROSS AGE ##########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of the stariway plot of mortality risk across age and the  
#          alphabet soup plots of prediction performance across the fit measures (Nagelkerke R2, AIC, and 
#          Concordance Index)
#
# Inputs: list_stats_pi - a list of dataframe of the regression results. A dataframe is available each to 
#                         contain the coefficients, prediction performance, and predicted risk
#         type_of_analysis - string indicating whether the survey weights were used or not, i.e. "weighted" or 
#                         "unweighted"
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#
# Outputs: none - png and pdf versions of the a panel plot of the stariway plot and the alphabet soup plot
#                 of prediction performance across the fit measures
#

stairway_plots_age_mortality <- function(list_regression_stats
                                         , dataset_merged
                                         , current_directory)
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the stairway plots
  name_folder <- "Stairway Plots - Age and Mortality"
  
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
  
  # Determine analysis types, i.e. "weighted" or "unweighted"
  analysis_types <- names(list_regression_stats)
  # Determine the number of analysis types
  num_analysis_types <- length(analysis_types)
  
  # Define a dataset containing participants who have complete data for mortality, age, sex, and race
  dataset_merged <- dataset_merged %>%
    # Define new columns with more legible codenames
    mutate(mortality_status = MORTSTAT
           , time_to_death = PERMTH_INT
           , weights = WTINT2YR
           , cluster = paste(dataset_merged$SDMVPSU, dataset_merged$SDMVSTRA)
           , cycles = SDDSRVYR
           , age = RIDAGEYR
           , gender = relevel(factor(if_else(RIAGENDR == 1, "_male", "_female"))
                              , ref = "_male")
           , race = relevel(factor(case_when(RIDRETH1 == 1 ~ "_mexican_american"
                                             , RIDRETH1 == 2 ~ "_other_hispanic"
                                             , RIDRETH1 == 3 ~ "_whites"
                                             , RIDRETH1 == 4 ~ "_blacks"
                                             , RIDRETH1 == 5 ~ "_other" ))
                            , ref = "_whites")) %>%
    select("SEQN"
           , "mortality_status"
           , "time_to_death"
           , "weights"
           , "cluster"
           , "cycles"
           , "age"
           , "gender"
           , "race") %>%
    na.omit(.) %>%
    filter(time_to_death != 0) %>%
    mutate(adjusted_weights = (1/length(unique(cycles)))*weights)
  
  # Determine the median age as this is the reference point for the linear and splines model
  midpoint_pi_value <- quantile(dataset_merged$age, probs = 0.5)
  
  # Make a panel plot that includes a stairway plot show the mortality risk progress across age and 3 
  # alphabet soup plots showing the prediction performance for 3 fit measures
  for(j in seq(num_analysis_types))
  {
    # Determine the analysis type
    analysis_type_j <- analysis_types[j]
    print(analysis_type_j)
    
    # Extract the list of regression results pertaining to the given analysis type
    list_regression_stats_j <- list_regression_stats[[j]]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create Alphabet Soup Plot of Fit Measures  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Form a long-formated dataset containing a column of prediction performance 
    df_fit <- list_regression_stats_j[["fit"]] %>%
      filter(population_type == "all") %>%
      gather(., fit_stats, value, rsq_mean:concordance_std_error) %>%
      separate(fit_stats, c("fit", "stats_1", "stats_2"), sep = "_") %>%
      unite(., "stats", stats_1:stats_2) %>%
      mutate(stats = gsub("_NA$", "", stats)) %>%
      spread(., stats, value) %>%
      dplyr::select("treatment_on_demo"
                    , "sensitivity_range"
                    , "fit"
                    , "ci_high"
                    , "ci_low"
                    , "mean"
                    , "std_error") %>%
      # Define a more descriptive name for each fit measure
      mutate(fit = case_when(fit == "rsq" ~  "Nagelkerke R2"
                             , fit == "AIC" ~ "Akaike Information Criterion"
                             , fit == "concordance" ~ "Concordance Index")) 
        
    # Determine the row indices pertaining to the AIC
    index_aic <- which(df_fit$fit == "Akaike Information Criterion")
    # Determine the minimum AIC
    min_aic <- min(df_fit$ci_low[index_aic])
    
    # Determine the row indices pertaining to the Concordance Index
    index_concordance<- which(df_fit$fit == "Concordance Index")
    # Determine the minimum concordance index
    min_concordance <- min(df_fit$ci_low[index_concordance]) 
    
    # Include a column of the minimum value of each fit measure
    df_fit <- df_fit %>%
      mutate(min = case_when(fit == "Nagelkerke R2" ~ 0
                             , fit == "Akaike Information Criterion" ~ min_aic
                             , fit == "Concordance Index" ~ min_concordance ))
    # View(df_fit)
    
    # # Make an alphabet soup plot of the prediction performance of the models faceted by the fit measure
    alphabet_soup_plot_j <- ggplot(data = df_fit
                                 , mapping = aes(x = mean
                                                 , y = treatment_on_demo
                                                 , color = treatment_on_demo
                                                 , shape = treatment_on_demo)) +
      geom_point(size = 5) +
      geom_point(aes(x = min
                     , y = treatment_on_demo)
                 , shape = 32) +
      geom_errorbar(aes(xmax = ci_high
                        , xmin = ci_low)) +
      facet_wrap(vars(fit)
                 , nrow = 3
                 , scales = "free") +
      scale_color_manual(name = "Types of Model"
                         , values = c("#CC0000", "#F0B707", "#000080")) +
      scale_shape_manual(name = "Types of Model"
                         , values = c(76, 78, 83)) +
      xlab("Values of Fit Measure") +
      theme_bw() +
      theme(legend.position = "none"
            , strip.text = element_text(size = 12)
            , axis.title.y = element_blank()
            , axis.text.y = element_text(size = 12)
            , axis.text.x = element_text(size = 12)
            , axis.title.x = element_text(size = 12))
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~  Form Dataset of Ranges and Hazard Ratios for each Novemtile  ~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Extract the dataset of regression coefficients
    df_coefficients <- list_regression_stats_j[["coefficients"]] %>%
      filter(population_type == "all")
    # print(df_coefficients)

    # Define the ranges of age of each novemtile
    dataset_novemtiles <-  define_range_of_novemtile(dataset_merged
                                                     , df_coefficients
                                                     , "demo"
                                                     , analysis_type_j) %>%
      mutate(novemtile_name = gsub("^novemtiles_age_"
                                   ,  ""
                                   , novemtile_name))
    # print(dataset_novemtiles)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Obtain Hazard Ratios for Linear and Spline Models  ~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    # Extract the predicted risk from the splines and linear models
    df_predicted_risk <- list_regression_stats_j[["predicted_risk"]]%>%
      filter(population_type == "all")
    # print(df_predicted_risk)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create the Stairway Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    # Make the stairway plot to show the mortality risk progression across age for the linear, novemtiles, and
    # splines models
    stairway_plot_j <- ggplot(data = dataset_novemtiles) +
      # Draw boxes to represent the mortality risk from the novemtile models
      geom_rect(data = dataset_novemtiles
                , mapping = aes(xmin = lower_bounds
                                , xmax = upper_bounds
                                , ymin = exp(conf.low)
                                , ymax = exp(conf.high)
                                , color = treatment_on_demo
                                , fill = p.adjust(p.value))
                , alpha = 0.8) +
      # Draw lines to represent the mortality from the linear and spline models
      geom_line(data = df_predicted_risk
                , aes(x = demo_value
                      , y = hazard_ratio
                      , group = treatment_on_demo
                      , color = treatment_on_demo
                )
                , inherit.aes = FALSE
                , linetype = "solid"
                , size = 1.0) +
      # Draw numbers to represent the mean hazard ratio for each novemtile
      geom_text(data = dataset_novemtiles
                , mapping = aes(x = mean_within_ntile
                                , y = exp(estimate)
                                , label = novemtile_name)
                , color = "black"
                , size = 5) +
      # Draw black horizontal line to represent when the hazard ratio is 1
      geom_hline(yintercept = 1.0
                 , color = "black"
                 , linetype = "solid"
                 , size = 0.2) +
      # Draw a purple dot to show the median as the reference point
      geom_point(aes(x = midpoint_pi_value
                     , y = 1.0)
                 , color = "purple"
                 , size = 4) +
      scale_color_manual(name = "Types of Model"
                         , values = c("#CC0000", "#F0B707", "#000080")) +
      # Color the boxes based on significance of the novemtiles
      scale_fill_gradientn(name = "p-values"
                           , colors = c("gray10", "gray50", "gray85", "gray95")
                           # , trans = "log10"
                           , limits = c(0,1)
                           , breaks = c(0.01, 0.05, 0.1,  1)
                           , values = c(0, 0.01
                                        , 0.011, 0.05
                                        , 0.051, 0.1
                                        , 0.101, 1)) +
      guides(color = guide_legend(order = 1)
             , fill = guide_colorbar(barwidth = 30
                                     , label.theme = element_text(size = 6)
                                     , nbin = 200
                                     , order = 2)) +
      scale_y_log10() +
      xlab("Age (Years)") +
      ylab("Hazard Ratios") +
      theme_bw() +
      theme(legend.position = "top"
            , legend.box = "vertical"
            , axis.title = element_text(size = 14)
            , axis.text = element_text(size = 12)
            , plot.title = element_text(hjust = 0.5
                                        , size = 14))

    # Define file names of the png and pdf versions of the panel plots
    plot_name.png <- paste("stairway_plot_age"
                           , "_"
                           , analysis_type_j
                           , ".png"
                           , sep = "")
    plot_name.pdf <- paste("stairway_plot_age"
                           , "_"
                           , analysis_type_j
                           , ".pdf"
                           , sep = "")

    # Make the panel plot of the stairway plot and the alphabet soup plots
    panel_stairway_alphabet <- grid.arrange(stairway_plot_j
                                            , alphabet_soup_plot_j
                                            , widths = c(1.5,1)
                                            , nrow = 1)

    # Save the alphabet soup plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = panel_stairway_alphabet
           , width = 14
           , height = 9)

    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = panel_stairway_alphabet
           , width = 14
           , height = 9)
    
  }
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
  # panel_stairway_alphabet
  alphabet_soup_plot_j
}