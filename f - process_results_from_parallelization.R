#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##################### FUNCTION TO PROCESS THE REGRESSION RESULTS INTO A LIST OF DATASETS ######################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function performs k-fold cross-validation on the associations between transformed physiological 
#          indicator and mortality for the entire NHANES and restricted populations
#          
# Inputs: list_regressions - a list of lists of datasets ("pi") or a list of dataset ("demo") containing the 
#                            regression results
#         name_motif - string indicating whether the regression results are for the demographics ("demo") or 
#                      for the physiological indicators ("pi")
#
# Outputs: list_final - a list containing a dataset each for the prediction performance, coefficients, and
#                       predicted risk

process_results_from_parallelization <- function(list_regressions
                                                 , name_motif = "pi")
{
  # Convert from a list of lists of datasets to a list of datasets
  list_updated <- unlist(list_regressions, recursive = FALSE)
  
  # Peform extra processing on the list of regression results to convert to a list of datasets
  if(name_motif == "pi")
  {
    list_updated <- list_updated %>%
      unlist(., recursive = FALSE)
  }
  
  # Determine the names of all the datasets
  names_all_datasets <- names(list_updated)
  
  # Define the type of information
  types_dataset <- c("fit", "coefficients", "predicted_risk")
  num_dataset_types <- length(types_dataset)
  
  # Define a function to merge the datasets by column names
  joining_by_colnames <- function(x, y) full_join(x, y, by = colnames(x))
  
  # Initialize an empty list to store the merged regression results
  list_final <- list()
  
  # list_finalized <- parLapply(cl, 1:num_dataset_types, function(d)
  # For a type of information, extract all datasets containing this type of information and merge them together
  for(d in 1:num_dataset_types)
  {
    # Determine the given type of information
    type_dataset_d <- types_dataset[d]
    
    # Define a pattern to extract all datasets containing the given type of information
    pattern_type_dataset_d <- paste(type_dataset_d
                                    , "$"
                                    , sep = "")
    
    # Obtain the indices for these datasets
    index_type_d <- which(grepl(pattern_type_dataset_d, names_all_datasets))
    # print(names_with_type_d)
    
    # Store the datasets into a list
    list_subset <- list_updated[index_type_d]
    
    # print(names(list_subset))
    
    # Merge all the datasets together 
    dataset_merged <- list_subset %>%
      reduce(joining_by_colnames)
    
    # Assign the merged dataset of a given information type into a list
    list_final[[type_dataset_d]] <- dataset_merged
    
    
  }
  
  return(list_final)
}