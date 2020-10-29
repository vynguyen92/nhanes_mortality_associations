#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~  FUNCTION TO PERFORM A SPLINE MODEL OF MORTALITY AND TRANSFORMED PHYSIOLOGICAL INDICATOR BY SEX  ~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function runs a spline model on the outcome as mortality and the transformed physiological  
#          indicator with adjustment for age and race for a given gender
#          
# Inputs: x - dataframe the pertinent variables for running the regression model 
#         analysis_type - string indicating whether the survey weights should be used or not, i.e. "weighted"  
#                         or "unweighted"
#         sensitivity_range - string indicating the percentiles of the transformed variable on which 
#                             participants to include in the regression models 
#         dataset_references - default to NA. If specified, then it's a dataframe of reference groups
#
# Outputs: df_predicted_risk - dataframe of predicted risk for all physiological measurements from a spline 
#                              model for a given gender

treat_pi_and_splines <- function(x
                                 , analysis_type
                                 , sensitivity_range
                                 , pi_values_all
                                 , dataset_refs = NA)
{
  # Refine with a better name
  complete_dataset <- x
  
  # Determine the gender
  gender_i <- unique(complete_dataset$gender) %>%
    as.character(.)
  # print(gender_i)
  
  # Determine the codename of the physiological indicator
  unique_codename <- unlist(unique(complete_dataset[,"pi"])
                            , use.names = FALSE)
  
  # Determine the measurements for the given gender
  pi_values <- unlist(complete_dataset[,"pi_value"]
                      , use.names = FALSE)
  
  # Apply the linear and non-linear transformations on the physiological indicator
  list_updated_dataset_and_transformed_columns <- transform_contin(pi_values
                                                                   , complete_dataset
                                                                   , "splines"
                                                                   , "pi"
                                                                   , dataset_refs)
  
  # Extract the updated dataset
  updated_dataset <- list_updated_dataset_and_transformed_columns[["dataset"]]
  
  # Extract the treatment pattern to be used for extract the column vectors pertaining to the transformed
  # variable 
  pattern_treated_pi <- list_updated_dataset_and_transformed_columns[["treatment_pattern"]]
  
  # Determine the column names of the transformed variable to be used for extracting these columns 
  colnames_pi <- grepl(pattern_treated_pi
                       , colnames(updated_dataset)) %>%
    colnames(updated_dataset)[.]
  
  # Define the pertinent variables to include for the spline models
  if(analysis_type == "weighted")
  {
    colnames_include <- c("age"
                          , "race"
                          , "cluster"
                          , colnames_pi)
    
  } else if(analysis_type == "unweighted") {
  
    colnames_include <- c("age"
                          , "race"
                          , colnames_pi)
    
  }
  
  # Extract the dataset of reference points for a given gender
  if(anyNA(dataset_refs) == TRUE)
  {
    dataset_ref_temp <- dataset_refs
    
    
  } else {
    dataset_ref_temp <- dataset_refs %>%
      filter(gender == gender_i)
    
  }

  # Calculate the predicted risk for splines model for a given gender
  df_predicted_risk <- determine_predicted_risk(updated_dataset
                                                , pi_values
                                                , colnames_include
                                                , analysis_type
                                                , unique_codename
                                                , "splines"
                                                , pattern_treated_pi
                                                , sensitivity_range
                                                , "pi"
                                                , dataset_ref_temp)
  
  return(df_predicted_risk)
}