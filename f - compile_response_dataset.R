#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############################  FUNCTION TO COMPILE THE NHANES RESPONSE DATASET  #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extracts the individual response dataset for each cycle and merge into one combined  
#          response dataset
#
# Inputs: main_directory - the working directory of the folder that contains the folders for each cycle. Each 
#                          cycle-specific folder contains the file names for each the response dataset
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#
# Outputs: merged_nhanes_datasets - dataframe with the merged response dataset 

compile_response_dataset <- function(main_directory
                                     , current_directory)
{
  # Establish the working directory to be the folder that contains the folders of the cycle-specific response
  # datasets
  setwd(main_directory)
  
  # Obtain a vector of folder names, one for each cycle
  nhanes_dataset_by_cycle <- list.files()
  
  # Determine the number of cycles 
  num_cycles <- length(nhanes_dataset_by_cycle)
  
  # Initialize a list to store a dataframe of biomarker measurements for each cycle
  all_nhanes_datasets_by_cycle <- list()
  
  # For each cycle in NHANES, go into a cycle-specific folder and extract the corresponding files to form a 
  # merged chemicals datasest for an ith cycle
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
      temp_file <- nhanes(file_name_j)
      
      # Merge the jth chemical dataset with the previous chemicals datset by SEQN
      cycle_specific_dataset <- merge(cycle_specific_dataset
                                      , temp_file
                                      , all = TRUE
                                      , by = "SEQN")
    }
    
    # Determine the number of participants in the ith cycle
    num_participants_cycle_i <- dim(cycle_specific_dataset)[1]
    # Define the study year that each participant belongs to
    study_year <- rep(i, num_participants_cycle_i)
    
    # Store the merged chemical datset for the ith cycle into a list
    all_nhanes_datasets_by_cycle[[i]] <- data.frame(cycle_specific_dataset)
    
    # Determine the indices of duplicates for the ith cycle
    index_dupl_codenames <- grep(".(x|y)", colnames(all_nhanes_datasets_by_cycle[[i]]))
    # Determine the codename of the duplicates for the ith cycle
    dupl_codenames <- colnames(all_nhanes_datasets_by_cycle[[i]])[index_dupl_codenames]
    # Determine a vector of unique codenames that were duplicated
    problematic_codenames <- unique(gsub(".(x|y)", "", dupl_codenames))
    
    # For each biomarker that were duplicated, all measurements will be stored into the first duplicate
    # to retain the maximum number of measurements pertaining to this biomarker
    for(p in seq(length(problematic_codenames)))
    {
      # Start the pth duplicated biomarker
      probl_codename <- problematic_codenames[p]
      
      # Establish a pattern to find all duplicates pertaining to this pth biomarker
      probl_code_pattern <- paste("\\b"
                                  , probl_codename
                                  , "\\b"
                                  , sep = "")
      # use the pattern to determine the indices of duplicate pertaining to this pth biomarker
      index_probl_code_dupl <- grep(probl_code_pattern
                                    , colnames(all_nhanes_datasets_by_cycle[[i]]))
      
      # Determine the number of duplicates for this pth biomarker
      num_probl_dupl <- length(index_probl_code_dupl)
      
      # Determine the first index of all the duplicates for this pth biomarker
      index_first_probl_code_dupl <- index_probl_code_dupl[1]
      
      # For any subsequent duplicate (kth), extract all measurements and store it to the corresponding 
      # participant in the first duplicated vector 
      for(k in 2:num_probl_dupl)
      {
        # Determine the index pertaining the kth duplicated column vector for the pth biomarker
        dupl_index_k <- index_probl_code_dupl[k]
        
        # Store the column vector for this kth duplicate into a more readable variable name
        measurements_dupl_k <- all_nhanes_datasets_by_cycle[[i]][,dupl_index_k]
        
        # Determine the indices of participants who have measurements for this pth biomarker in the 
        # kth duplicated column vector
        index_measurements_for_dupl_k <- which(!is.na(measurements_dupl_k))
        
        # Extract the measurements and store it into the column vector pertaining to the first duplicate
        all_nhanes_datasets_by_cycle[[i]][index_measurements_for_dupl_k,index_first_probl_code_dupl] <- 
          measurements_dupl_k[index_measurements_for_dupl_k]
        
      }
      
      # change the codename of the column vector that now contains all measurements for the pth biomarker
      # to be without any extraneous symbol (i.e. ".x" or ".y")
      colnames(all_nhanes_datasets_by_cycle[[i]])[index_first_probl_code_dupl] <- probl_codename
      
      # The indices pertaining to the 2nd duplicates and beyond for the pth biomarker needs to be 
      # removed from the chemical dataset for the ith cycle
      index_remove_dupl <- index_probl_code_dupl[2:num_probl_dupl]
      # Remove the column vectors pertaining to the duplicates for the pth biomarker from the 
      # ith cycle chemical dataset
      all_nhanes_datasets_by_cycle[[i]] <- all_nhanes_datasets_by_cycle[[i]][,-index_remove_dupl]
      
      
    }
    
    # Append the study_year for this ith cycle with the ith cycle chemical dataset
    all_nhanes_datasets_by_cycle[[i]] <- data.frame(all_nhanes_datasets_by_cycle[[i]]
                                                    , study_year)
    
  }
  
  
  # Rename the list containing all the cycle-specific chemicals datasets
  list_merged_nhanes_datasets <- all_nhanes_datasets_by_cycle
  # return(list_merged_nhanes_datasets)
  
  # # Determine the number of cycles or technically, determine the number of dataframes stored in this list
  num_elements_in_list <- length(lengths(list_merged_nhanes_datasets))
  
  # Ensure that these variables are numeric to prevent merging issues
  # These variables are available in all cycles
  for(m in 2:num_elements_in_list)
  {
    list_merged_nhanes_datasets[[m]][,"LBXSNASI"] <- as.numeric(list_merged_nhanes_datasets[[m]][,"LBXSNASI"])
    list_merged_nhanes_datasets[[m]][,"LBXSCLSI"] <- as.numeric(list_merged_nhanes_datasets[[m]][,"LBXSCLSI"])
    list_merged_nhanes_datasets[[m]][,"LBXGLU"] <- as.numeric(list_merged_nhanes_datasets[[m]][,"LBXGLU"])
    list_merged_nhanes_datasets[[m-1]][,"LBDSTPSI"] <- as.numeric(list_merged_nhanes_datasets[[m-1]][,"LBDSTPSI"])
  }
  # Ensure that these variables are numeric to prevent merging issues
  # These variables are only available in a few cycles
  list_merged_nhanes_datasets[[1]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[1]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[4]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[4]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[5]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[5]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[6]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[6]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[7]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[7]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[8]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[8]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[1]][,"CVDR3TIM"] <- as.numeric(list_merged_nhanes_datasets[[1]][,"CVDR3TIM"])
  list_merged_nhanes_datasets[[2]][,"CVDR3TIM"] <- as.numeric(list_merged_nhanes_datasets[[2]][,"CVDR3TIM"])
  list_merged_nhanes_datasets[[3]][,"CVDR3TIM"] <- as.numeric(list_merged_nhanes_datasets[[3]][,"CVDR3TIM"])
  list_merged_nhanes_datasets[[3]][,"LBXPT21"] <- as.numeric(list_merged_nhanes_datasets[[3]][,"LBXPT21"])
  list_merged_nhanes_datasets[[4]][,"LBXPT21"] <- as.numeric(list_merged_nhanes_datasets[[4]][,"LBXPT21"])
  
  
  merged_nhanes_datasets <- list_merged_nhanes_datasets[[1]]
  
  # For subsequent cycles, the kth cycle chemical dataset will be merged with the previous merged chemical
  # dataset
  for(k in 2:num_elements_in_list)
  {
    # Perform the merge by the codenames
    merged_nhanes_datasets <- full_join(merged_nhanes_datasets
                                        , list_merged_nhanes_datasets[[k]]
                                        , by = NULL)
    # Message to relay which cycle has been merged in
    print(paste("Merge in Cycle ", k, sep = ""))
  }
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  
  # Return the merged chemical biomarker dataset
  return(merged_nhanes_datasets)
  
}
