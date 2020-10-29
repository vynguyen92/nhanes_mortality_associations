#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############  FUNCTION TO CREATE A PANEL PLOT OF HISTOGRAMS OF PARTICIPANTS BY MEASUREMENTS  ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function creates a panel plot of histograms to show the number of participants across the 
#          distribution of the physiological indicators. 
#
# Inputs: dataset_long - long-formatted dataset based on the physiological indictors
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#         pi_names_body_syst_cats - dataframe of the body system of the physiological indicators 
#
# Outputs: none - png and pdf versions of the panel plot of histograms are printed to a folder


histogram_pi_distribution <- function(dataset_long
                                      , current_directory
                                      , pi_names_body_syst_cats)
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the histograms
  name_folder <- "Histograms - Distribution of Physiological Indicators"
  
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
  
  # Make a dummy plot of the body systems to extract the legend box of the colors for the body systems
  dummy_plot <- ggplot() +
    geom_bar(data = pi_names_body_syst_cats
             , aes(x = factor(pi)
                   , fill = body_system_categories)) +
    guides(fill = guide_legend(title = "Body Systems")) +
    theme(legend.position = "top"
          , legend.title = element_text(size = 22
                                        , face = "bold")
          , legend.text = element_text(size = 16
          ))
  
  # Extract the legend box of the colors for the body systems
  legend_body_system <- gtable_filter(ggplotGrob(dummy_plot)
                                      , "guide-box")
  
  # Merge the body systems into the long-formatted dataset with the original measurements
  dataset_long <- dataset_long %>%
    left_join(.
              , pi_names_body_syst_cats
              , by = "pi") %>%
    # Rearrange the data in alphabetical order of the body systems
    arrange(body_system_categories)
  
  # Initialize an empty list to store the histogram for all the physiological indicators
  list_histograms <- list()
  
  # Define a string vector of alphabetical letters to label each histogram
  LETTERS <- append(LETTERS, "AA")
  
  # Determine the codenames for all physiological indicators
  unique_pi <- unique(dataset_long$pi)
  # Determine the number of physiological indicators 
  num_pi <- length(unique(dataset_long$pi))
  
  # Define a function to output a vector of hexcodes for the default ggplot color scheme for a given number
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  # Determine the names of all body systems
  unique_body_systems <- unique(dataset_long$body_system_categories)
  # Determine the number of body systems 
  num_body_systems <- length(unique_body_systems)
  
  # Define a dataframe containing the corresponding color for each body system 
  df_colors_body_system <- data.frame(body_system_categories = unique_body_systems
                                      , colors = gg_color_hue(num_body_systems)
                                      , stringsAsFactors = FALSE)
  
  # Define a vector of codename of physiological indicators indicating that the x-axis needs to be 
  # log10-transformed
  pis_x_log10 <- c("BMXBMI"
                   , "BMXWT"
                   , "LBXSTR"
                   , "VNLDHDLRATIO"
                   , "VNTOTHDRATIO"
                   , "LBXCRP"
                   , "LBXWBCSI"
                   , "LBXGH"
                   , "LBXGLU"
                   , "LBXSAPSI"
                   , "VNHOMAIR"
                   , "VNINGLURATIO"
                   , "LBXSCR")
  
  # Define an index for given physiological indicators. This aspect helps with debugging
  index_one <- which(unique_pi == "VNINGLURATIO")
  
  # Make a histogram for each physiological and store it in a list
  for(i in seq(num_pi))
  {
    # Determine a given physiological indicator
    pi_i <- unique_pi[i]
    
    # Define a dataset of the measurements for a given physiological indicator
    subset_i <- dataset_long %>%
      filter(pi == pi_i)
    
    # Determine the codename of physiological indicator
    pi_name_i <- unique(subset_i$pi_names)
    print(pi_i)
    
    # Determine the number of unique measurements
    num_unique_pi_values <- length(unique(subset_i$pi_value))
    
    # Determine the body system of this physiological indicator
    body_system_i <- unique(subset_i$body_system_categories)
    
    # Define a datamframe corresponding to the body system color for this physiological indicator
    df_color_i <- df_colors_body_system %>%
      filter(body_system_categories == body_system_i) 
    
    # Determine the body system color for this physiological indicator
    color_i <- df_color_i[,"colors"] %>%
      unlist(.
             , use.names = FALSE)
    
    # Make the histogram of the number of participants by each unique measurement of the physiological indicator
    histogram_i <- ggplot() +
      geom_histogram(data = subset(dataset_long
                                   , pi == pi_i)
                     , aes(x = pi_value)
                     , fill = color_i
                     , bins = num_unique_pi_values)  +
      xlab(pi_name_i) +
      theme_bw() +
      theme(axis.title.y = element_blank())
    
    # Define the tick marks of the x-axis to indicate the 0th, 10th, 50th, 90th, and 100th percentiles
    # Transform the x-axis to log10 scale for some physiological indicators
    if(pi_i %in% pis_x_log10)
    {
      histogram_i <- histogram_i +
        scale_x_log10(breaks = round(quantile(subset_i$pi_value
                                                   , probs = c(0,0.1,0.5,0.9,1)
                                                   , names = FALSE)
                                     , digits = 3
                                  )
                      , limits = c(min(subset_i$pi_value), max(subset_i$pi_value)))
      
    } else {
      
      histogram_i <- histogram_i +
        scale_x_continuous(breaks = round(quantile(subset_i$pi_value
                                                   , probs = c(0,0.1,0.5,0.9,1)
                                                   , names = FALSE)
                                          , digits = 2)
                           , limits = c(min(subset_i$pi_value), max(subset_i$pi_value)))
    }
    
    # Define a letter label for this given physiological indicator
    letter_i <- LETTERS[i]
    
    # Define a title label of the letter and the position of that title for the physiological indicator
    title_i <- textGrob(label = letter_i
                        , x = unit(0.5, "lines")
                        , y = unit(0, "lines")
                        , hjust = 0
                        , vjust = 0
                        , gp = gpar(fontsize = 20
                                    , fontface = "bold"))
    
    # Store the plot of the histogram and the title label into a list
    list_histograms[[i]] <-  arrangeGrob(histogram_i
                                         , top = title_i)
  }

  # Make a panel plot of histograms of all physiological indicators with 3 columns and 9 rows
  panel_histogram <- do.call("grid.arrange"
                             , c(list_histograms
                                 , ncol = 3))

  # Add a legend of body system colors at the top of the panel of histograms
  # Add an y-axis label 
  panel_histogram <- grid.arrange(legend_body_system
                                  , arrangeGrob(panel_histogram
                                                , left = textGrob("Number of Participants"
                                                                  , gp = gpar(fontface = "bold"
                                                                              , cex = 1.5)
                                                                  , rot = 90))
                                  , heights = c(0.6, 21.4)
                                  , nrow = 2)


  # Define file names of the png and pdf versions of the panel of histograms 
  plot_name.png <- paste("histogram_panel"
                         , "0_100"
                         , "pi_mortality.png"
                         , sep = "_")
  plot_name.pdf <- paste("histogram_panel"
                         , "0_100"
                         , "pi_mortality.pdf"
                         , sep = "_")


  # Save the panel of histograms as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = panel_histogram
         , width = 15
         , height = 22)


  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = panel_histogram
         , width = 15
         , height = 22)
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
  # histogram_i
}