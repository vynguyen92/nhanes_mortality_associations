#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############  FUNCTION TO CREATE CORRELATION PLOTS OF DIFFERENT READINGS OF BLOOD PRESSURE  ################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of pairwaise correlation between the different blood pressure 
#          readings. 
#
# Inputs: unclean_response_dataset - the dataset that contains excess variables. This should be the result 
#                                    of the compile function that extracted the individual response datasets
#                                    and merged all individual response datasets across the cycles
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#
# Outputs: none - png and pdf versions of the correlation plots are printed to a folder

create_bp_correlation_plots <- function(unclean_dataset
                                        , working_directory)
{
  # Determine the column indices pertaining to systolic or diastolic blood pressure
  index_bp_systolic <- grep("^BPXSY"
                            , colnames(unclean_dataset))
  index_bp_diastolic <- grep("^BPXDI"
                             , colnames(unclean_dataset))
  
  # Determine the number of readings. There are 4 available
  num_of_measurements_bps <- length(index_bp_systolic)
  num_of_measurements_bpd <- length(index_bp_diastolic)
  
  # Define a string vector with the names of the blood pressure
  bp_types_name <- c("diastolic"
                     ,"systolic")
  # Determine the number of types of blood pressure
  num_types_of_bp <- length(bp_types_name)
  
  # Define a dataframe with the column indices pertaining to each type of blood pressure
  df_bp_indices <- data.frame(index_bp_diastolic
                              , index_bp_systolic)
  
  # Determine the maximum measurements for each type of blood pressure
  max_dbp <- max(unclean_dataset[,index_bp_diastolic], na.rm = TRUE)
  max_sbp <- max(unclean_dataset[,index_bp_systolic], na.rm = TRUE)
  
  # Store the maximum measurements into a vector
  max_bp <- c(max_dbp,max_sbp)
  
  # This numeric vector will help to position the label of statistics on the correlation plot
  step_size <- c(7, 15)
  
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the correlation plots
  name_folder <- "Correlation Plots - Blood Pressure Measurements"
  
  # Make a new folder if the folder doesn't exist
  if(name_folder %in% all_files_in_current_directory)
  {
    
  } else {
    dir.create(name_folder)
  }
  
  # Define a string for the working directory for the new folder
  new_working_directory <- paste(working_directory
                                 , name_folder
                                 , sep = "/")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  # Make a panel of correlation plots of all pair-wise combination of the different reading for each type of
  # blood pressure
  for(k in seq(num_types_of_bp))
  {
    # Determine type of blood pressure
    bp_type_name_k <- bp_types_name[k]
    print(bp_type_name_k)
    
    # Extract the column indices pertaining to the given type of blood pressure
    indices_bp_type_k <- df_bp_indices[,k]
    
    # Initialize an empty list
    bp_panel_plot <- list()

    # Start a counter to help with storing the individual correlation plots into a list 
    counter <- 1

    # To obtain all pairwise combination of the readings, i will iterate from 1 to 3
    for(i in seq(num_of_measurements_bpd - 1))
    {
      # Determine the column index for a given reading
      index_bp_i <- indices_bp_type_k[i]
      
      # Determine the codename of that reading
      codename_bp_i <- colnames(unclean_dataset)[index_bp_i]
      
      # Determine the measurements of that reading
      measurements_bp_i <- as.numeric(unclean_dataset[,codename_bp_i])
      
       # Then j will iterate from 2 to 4
      for(j in (i+1):4)
      {

        # Determine the column index for a reading that isn't i
        index_bp_j <- indices_bp_type_k[j]

        # Determine the codename of that reading that isn't i
        codename_bp_j <- colnames(unclean_dataset)[index_bp_j]

        # Determine the measurements of that reading that isn't i
        measurements_bp_j <- as.numeric(unclean_dataset[,codename_bp_j])

        # Determine the Pearson correlation between the ith and jth reading 
        corr_bp_coeffi <- cor(measurements_bp_i
                              , measurements_bp_j
                              , method = "pearson"
                              , use = "complete.obs")

        # Round the correlation coefficient to have two decimal points
        corr_bp_coeffi_round <- round(corr_bp_coeffi
                                      , digits = 2)

        # Make the label for the R2
        corr_label <- bquote(R^2 == .(corr_bp_coeffi_round))

        # Run a paired t-test to assess if the pairing was effective, meaning that the two readings are 
        # significantly correlated.
        t_test_bp <- t.test(measurements_bp_i
                            , measurements_bp_j
                            , paired = TRUE
                            , na.action = na.omit)

        # Reformat the p-value in scientific notation
        p_value <- formatC(t_test_bp$p.value
                           , format = "e"
                           , digits = 2)

        # Make a string label for the p-value of the paired t-test
        p_value_label <- paste("p-value = "
                               , p_value
                               , sep = "")

        # Determine and round the mean of differences between the two readings
        estimate_bp <- round(t_test_bp$estimate
                             , digits = 2)

        # Make a string label for the mean of differences
        estimate_bp_label <- paste("mean of difference = "
                                   , estimate_bp
                                   , sep = "")

        # Make the correlation plot between the ith and jth reading with labels for the R2, the
        # significance of the correlation, and the mean difference between the readings
        correlation_plot <- ggplot(unclean_dataset
                                   , aes(x = measurements_bp_i
                                         , y = measurements_bp_j)) +
          geom_point(na.rm = TRUE
                     , color = "darkred") +
          xlab(paste("Reading", i)) +
          ylab(paste("Reading", j)) +
          geom_abline(intercept = 0
                      , slope = 1) +
          xlim(0, max_bp[k]) +
          ylim(0, max_bp[k]) +
          annotate("text"
                   , x = 40
                   , y = (max_bp[k] - step_size[k])
                   , label = deparse(corr_label)
                   , size = 4
                   , parse = TRUE) +
          annotate("text"
                   , x = 40
                   , y = (max_bp[k] - 2*step_size[k])
                   , label = p_value_label
                   , size = 4) +
          annotate("text"
                   , x = 40
                   , y = (max_bp[k] - 3*step_size[k])
                   , label = estimate_bp_label
                   , size = 4)

        # Store this individual plot into a list
        bp_panel_plot[[counter]] <- correlation_plot

        # Increment the counter
        counter <- counter + 1

        # Print the ith and jth reading 
        print(paste(i, j))

      }
    }

    # Define a title label with the name of the type of blood pressure
    title_label <- paste(capitalize(bp_type_name_k)
                         , "Blood Pressure")

    # Place all the pair-wise correlation plots together into one plot
    panel_plot <- do.call("grid.arrange"
                          , c(bp_panel_plot[as.vector(gdata::interleave(1:3, 4:6))]
                              , list(ncol = 2)
                              , list(top = title_label)
                          ))

    # Define file names of the png and pdf versions of the correlation plots
    panel_figure_name.png <- paste("correlation_plot"
                                   , bp_type_name_k
                                   , "bp.png"
                                   , sep = "_")
    panel_figure_name.pdf <- paste("correlation_plot"
                                   , bp_type_name_k
                                   , "bp.pdf"
                                   , sep = "_")

    # Save the correlation plots as a png and pdf
    ggsave(filename = panel_figure_name.png
           , plot = panel_plot
           , width = 11
           , height = 15)
    ggsave(filename = panel_figure_name.pdf
           , plot = panel_plot
           , width = 11
           , height = 15)

  }
  # Set the working directory to the main directory 
  setwd(working_directory)
}
