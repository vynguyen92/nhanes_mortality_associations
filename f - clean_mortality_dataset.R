#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################  FUNCTION TO CLEAN THE NHANES MORTALITY DATASET ##############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function cleans the mortality dataset to have non-NA columns
#
# Inputs: dataset_unclean - the working directory of the folder that contains the folders for each cycle. Each 
#                           cycle-specific folder contains the file names for each the chemical dataset
#
# Outputs: dataset_unclean_updated - dataframe of the cleaned mortality dataset

clean_mortality_dataset <- function(dataset_unclean)
{
  # Determine the number of columns in the dataset
  num_cols <- ncol(dataset_unclean)
  
  # Initiate an empty vector to hold the indices to remove empty columns
  indices_to_remove <- c()
  
  # Perform for each variable
  for(i in seq(num_cols))
  {
    # Determine whether the column is empty
    is_empty <- all(is.na(dataset_unclean[,i]))
    
    # If the column is empty, then indicate that this variable will be removed
    if(is_empty == TRUE)
    {
      indices_to_remove <- append(indices_to_remove
                                  , i)
    }
    
    # Ensure the class of this variable 
    class_i <- class(dataset_unclean[,i])
    class(dataset_unclean[,i]) <- c("labelled", class_i)
    print(class(dataset_unclean[,i]))
    
  }
  
  # Define a dataframe with the empty columns removed 
  dataset_unclean_updated <- dataset_unclean[,-indices_to_remove]
  
  return(dataset_unclean_updated)
}
