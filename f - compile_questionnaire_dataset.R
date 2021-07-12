#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########################  FUNCTION TO COMPILE THE NHANES QUESTIONANIRE DATASET #############################
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

compile_questionnaire_dataset <- function(main_directory
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
  
  # Initialize a list to store a dataframe of biomarker measurements for each cycle
  all_nhanes_datasets_by_cycle <- list()
  
  for(i in seq(num_cycles))
  {
    # Define the directory of the ith cycle-specific folder
    cycle_specific_nhanes_dataset_directory <- paste(main_directory
                                                     , nhanes_dataset_by_cycle[i]
                                                     , sep = "/")
    print(cycle_specific_nhanes_dataset_directory)
    
    # Establish the working directory for the ith cycle-specific folder
    setwd(cycle_specific_nhanes_dataset_directory)
    
    # Determine a vector of file names in the ith cycle-specific folder
    files_names.xpt <- list.files()
    # print(files_names.xpt)
    
    # Determine the number of files in the ith cycle-specific folder
    num_files_in_cycle_specific_folder <- length(files_names.xpt)
    
    # Determine the name of the first file in the folder
    first_file <- files_names.xpt[1]
    # Replace the ".XPT" in the file name with ""
    first_file <- gsub(".XPT"
                       , ""
                       , first_file)
    
    # Use the updated file name to extract the appropriate dataset from the nhanesA package
    # Store the first chemical dataset for the ith cycle into cycle_specific_datasest, so that subsequent
    # chemical datasets can be merged with the first one
    cycle_specific_dataset <- nhanes(first_file) 
    
    # For the 2nd file and beyond (jth) in the cycle-specific folder, the corresponding chemical dataset will
    # be merged with the first chemical dataset by SEQN to form the cycle-specific dataset of chemical
    # measurements
    for(j in 2:num_files_in_cycle_specific_folder)
    {
      # Determine the name of the jth file in the folder and replace the ".XPT" in the file name with ""
      file_name_j <- gsub(".XPT"
                          , ""
                          , files_names.xpt[j])
      # Message to know which chemical dataset is being extracted
      # print(file_name_j)
      
      # Store jth chemical dataset into temp_file
      temp_file <- nhanes(file_name_j) #%>%
        # unlabel(.)
      
      # Merge the jth chemical dataset with the previous chemicals datset by SEQN
      cycle_specific_dataset <- merge(cycle_specific_dataset
                                      , temp_file
                                      , all = TRUE
                                      , by = "SEQN") 
    }
    
    # Store the merged chemical datset for the ith cycle into a list
    all_nhanes_datasets_by_cycle[[i]] <- data.frame(cycle_specific_dataset) %>%
      mutate(study_year = i) %>%
      unlabel(.)
  }
  
  merged_nhanes_datasets <- all_nhanes_datasets_by_cycle[[1]]
  
  # Determine the number of cycles or technically, determine the number of dataframes stored in this list
  num_elements_in_list <- length(lengths(all_nhanes_datasets_by_cycle))
  
  # For subsequent cycles, the kth cycle chemical dataset will be merged with the previous merged chemical
  # dataset
  for(k in 2:num_elements_in_list)
  {
    # Perform the merge by the codenames
    merged_nhanes_datasets <- full_join(merged_nhanes_datasets
                                        , all_nhanes_datasets_by_cycle[[k]]
                                        , by = NULL)
    # Message to relay which cycle has been merged in
    print(paste("Merge in Cycle ", k, sep = ""))
  }
  
  
  setwd(current_directory)
  
  # Return the merged demographics dataset
  return(merged_nhanes_datasets)
  
}