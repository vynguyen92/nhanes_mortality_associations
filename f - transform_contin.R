#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############  FUNCTION TO TRANFORM THE CONTIUOUS VARIABLE INTO LINEAR, NOVEMTILES, OR SPLINES  ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function transform a specified continous variable into a linear or non-linear variable
#          
# Inputs: values_of_contin - numeric vector of the continous variable 
#         dataset - dataframe containing the pertinent variables for running the regression model
#         treatment_of_contin - string indicating treatment on the continous variable, i.e. "linear", 
#                               "novemtiles", or "splines"
#         name_motif - string on the name of the variable 
#         df_refs - default to NA. If specified, then it's a dataframe of reference groups
#
# Outputs: list_return - a list containing the updated dataset [["dataset"]] with the newly added column  
#                        vectors for the transformed continuous variable and a string [["treatment_pattern"]]  
#                        indicating the pattern used to extract the transformed variables

transform_contin <- function(values_of_contin
                             , dataset 
                             , treatment_of_contin
                             , name_motif
                             , df_refs = NA)
{
  # Define a string of the treatment on the continuous variable and name of the variable
  treatment_pattern <- paste(treatment_of_contin
                             , name_motif
                             , sep = "_")
  
  # For a given treatment, define a column vector of the transformed variable with the column 
  # name as "[insert treatment]_[insert name of variable]"
  if(treatment_of_contin == "linear")
  {
    # No transformation on the continuous variable as the transformation is linear
    linear_contin <- values_of_contin
    
    # Include the column vector of the linear-transformed variable 
    # Assign the column name of the vector
    dataset[,treatment_pattern] <- linear_contin
    
  } else if(treatment_of_contin == "novemtiles") {
    
    # If the reference group is not specified, then set the 5th novemtile as the reference group
    if(anyNA(df_refs) == TRUE)
    {
      ref_group <- "_5"
      
    # If the reference is specified, then set that novemtile as the reference group
    } else {
      
      ref_group <- df_refs %>%
        # Include only the treatment for novemtiles 
        filter(treatment_on_pi == "novemtiles") %>%
        # Select the column vector pertaining to novemtile serving as the reference group
        select(ref) %>%
        unlist(., use.names = FALSE) %>%
        # Put an underscore before the novemtile number
        paste("_"
              , .
              , sep = "")
      
    }
    
    # Define the number of quantiles, here it's 9
    step_size <- 1/9
    # Define labels for the novemtiles as "_[insert novemtile number]"
    ntile_labels <- paste("_"
                          , seq(9)
                          , sep = "")
    
    # Define the values to serve as boundaries of the quantiles
    ntiles_contin <- quantile(values_of_contin
                              , na.rm = TRUE
                              , probs = seq(0, 1, by = step_size)
                              , names = FALSE)
    
    # Ensure that the start and end values of each quantile are unique
    ntiles_contin <- unique(ntiles_contin)
    
    # Since the smallest value will be exclude from the 1st quantile, the lower bound of this quantile
    # must be a value that is lower than the minimum value 
    ntiles_contin[1] <- min(ntiles_contin, na.rm = TRUE) - 1
    
    # Determine the number of boundaries of the quantiles
    # If the number of boundaries for the novemtiles is equal to the 10 (9 + 1), then the vector
    # of the labels will remain unchanged
    if(length(ntiles_contin) == (1/step_size + 1))
    {
      ntile_labels
    
    # If the number of number of boundaries for the novemtiles is less than 10, then this means that 
    # a novemtile was excluded and the vector of labels must change accordingly 
    } else {
      ntile_labels <- ntile_labels[1:(length(ntiles_contin)-1)]
    }
    
    # Descritize the continuous variable into quantiles 
    novemtile_contin <- cut(values_of_contin
                            , breaks = ntiles_contin
                            , labels = ntile_labels)
    
    # print(unique(novemtile_contin))
    
    # Include the column vector of the novemtiles-transformed variable 
    # Assign the column name of the vector
    dataset[,treatment_pattern] <- relevel(factor(novemtile_contin)
                                           , ref = ref_group)
    
  
  } else if(treatment_of_contin == "splines") {
    
    # Define for the number of cut-off points 
    step_size <- 1/4

    # Cut the continous variables into quartiles to determine the cut-off points or knots
    ntiles_contin <- quantile(values_of_contin
                          , na.rm = TRUE
                          , probs = seq(0, 1, by = step_size)
                          , names = FALSE)

    # Define a vector to contain the cut-off points, which is all boundaries except the minimum and 
    # maximum ones
    ntiles_contin <- ntiles_contin[2:((1/step_size))]

    # print(ntiles_contin)

    # Define a basis matrix of cubic polynomials based on quartiles with no intercept 
    splines_contin <- bs(x = values_of_contin
                         , knots = ntiles_contin
                         , degree = 3
                         , intercept = FALSE)
    
    # splines_contin <- pspline(values_of_contin)
    # print(dim(splines_contin))
    # splines_contin <- matrix(as.vector(splines_contin)
    #                          , nrow = dim(splines_contin)[1]
    #                          , ncol =  dim(splines_contin)[2]) %>%
    #   as.data.frame(.)
    
    # Define the column name of each spline to be "_spline_[insert number]"
    colnames(splines_contin) <- paste(treatment_pattern
                                  , colnames(splines_contin)
                                  , sep = "_")
    
    # Put the basis matrix as part of the dataset  
    dataset <- dataset %>%
      cbind(.
            , splines_contin)
    
    
  } else {
    
    print("Error: transformation was not considered.")
    
  }
  
  # Initialize an empty list
  list_return <- list()
  # Put the updated dataset with the newly added columns pertaining to a given transformation into the dataset
  list_return[["dataset"]] <- dataset
  # Put the transformation pattern into the list. This transformation pattern will help with sequential 
  # extraction of the transformed variable 
  list_return[["treatment_pattern"]] <- treatment_pattern
  
  return(list_return)
}