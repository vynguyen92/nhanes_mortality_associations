#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################  FUNCTION TO COMPILE THE NHANES DEMOGRAPHICS DATASET #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extracts the demographics dataset for each cycle and merged all cycle-specific   
#          demographics datasets into one merged demographic dataset
#
# Inputs: main_directory - the working directory of the folder that contains the files of cycle-specific
#                          demographics dataset
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project is housed.
#
# Outputs: merged_nhanes_dataset - dataframe with the merged demographic dataset 

compile_demographics_dataset <- function(main_directory
                                         , current_directory)
{
  # Establish the working directory to be the folder that contains the files of the cycle-specific demographics
  # datasets
  setwd(main_directory)
  
  # Determine the names of the files within this working directory
  nhanes_dataset_by_cycle <- list.files()
  
  # Determine the number of files in this working directory
  # The number of files should be the number of cycles in the NHANES continuous database
  num_cycles <- length(nhanes_dataset_by_cycle)
  
  # Determine the name of the first file in this working directory
  first_file_name.xpt <- nhanes_dataset_by_cycle[1]
  # Replace the ".XPT" in the file name with ""
  first_file_name <- gsub(".XPT"
                          , ""
                          , first_file_name.xpt)
  # Use the updated file name to extract the appropriate dataset from the nhanesA package
  # Store the demographics dataset for the first cycle into merged_nhanes_dataset, so that subsequent
  # demographics datasets can be merged with the first one
  merged_nhanes_dataset <- nhanes(first_file_name)
  
  # For the subsequent demographics datasets, the file name will be used to extract the ith cycle demographics
  # dataset and merge them with the demographics datasets from the previous cycles to form a merged demographics
  # dataset
  for(i in 2:num_cycles)
  {
    # Extract the file name for the demographics dataest in the ith cycle
    file_i_name.xpt <- nhanes_dataset_by_cycle[i]
    # Replace the ".XPT" in the file name with "" 
    file_i_name <- gsub(".XPT"
                        , ""
                        , file_i_name.xpt)
    
    # Extract the demographics dataset for the ith cycle
    cycle_i_dataset <- nhanes(file_i_name)
    
    # Merge the demographics dataset for the ith cycle with the previous demographic dataset
    merged_nhanes_dataset <- full_join(merged_nhanes_dataset
                                       , cycle_i_dataset
                                       , by = NULL)
    
    # Message to help know which cycle has been merged in
    print(paste("Merge in Cycle ", i, sep = ""))
    
  }
  
  setwd(current_directory)
  
  # Return the merged demographics dataset
  return(merged_nhanes_dataset)
  
}