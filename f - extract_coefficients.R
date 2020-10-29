#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############################  FUNCTION TO EXTRACT THE REGRESSION STATISTICS  ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function runs a cox regression model to extract the regression statistics for each explanatory
#          predictors
#          
# Inputs: dataset_updated - dataframe the pertinent variables for running the regression model 
#         included_colnames - string vector of the pertinent column names
#         analysis_type - string indicating whether the survey weights should be used or not, i.e. "weighted"  
#                         or "unweighted"
#         codename - string indicating the name of the population type ("all" or "[insert code name of  
#                         physiological indicator]") or the codename of the physiological indicator
#         treatment_on_contin - string indictating the treatment on the continous variable, i.e. "linear", 
#                               "novemtiles", or "splines"
#         sensitivity_range - string indicating the percentiles of the transformed variable on which participants
#                             to include in the regression models 
#         model_types - string indicating whether the transformation was applied on age or the physiological 
#                       indicator
#
# Outputs: coeff_dataframe - a dataframe of the regression statistics for a given transformed variable


extract_coefficients <- function(dataset_updated
                                 , included_colnames
                                 , type_analysis
                                 , codename
                                 , treatment_on_contin
                                 , sensitivity_range
                                 , model_types)
{
  # Define a vector of column names for the explanatory and outcome variables
  included_columns_updated <- c(included_colnames
                                #, "cluster"
                                , "time_to_death"
                                , "mortality_status")
  
  # Define the subset to have the pertinent variables
  subset_updated <- dataset_updated[,included_columns_updated]
  
  # Run a regression model to obtain the regression coefficients
  if(type_analysis == "weighted")
  {
    # Extract the survey weights
    survey_weights <- dataset_updated$adjusted_weights
    
    # Extract the sampling clusters
    survey_clusters <- dataset_updated$cluster
    
    # If "cluster" is in the vector of pertinent variables, remove it because we need to apply
    # a function on it to correctly account for NHANES sampling design
    if("cluster" %in% included_columns_updated)
    {
      # Determine the index pertaining to "cluster"
      index_cluster <- which(included_columns_updated == "cluster")
      
      # Remove it from the vector of pertinent variables
      included_columns_updated <- included_columns_updated[-index_cluster]
      
    }
    
    # Run the cox regression model accounting for NHANES sampling design and the survey weights
    coxph_model <- coxph(Surv(time = time_to_death
                              , event = mortality_status) ~ . +
                           cluster(survey_clusters)
                         , weights = survey_weights
                         , data = subset_updated)
    # print(coxph_model)

    
  } else if(type_analysis == "unweighted") {
    
    # Run the cox regression model without accounting for NHANES sampling design and the survey weights
    coxph_model <- coxph(Surv(time = time_to_death
                              , event = mortality_status) ~ .
                         , data = subset_updated)
    
  } else {
    
    print("Error: type of analysis is not valid.")
    
  }
  
  # Form the regression coefficients into a tidy dataframe
  coeff <- tidy(coxph_model)
  # print(coeff)
  
  # Form the dataframe of regression coefficients
  if(model_types == "pi")
  {
    # Define a dataframe to containing the codename of the physiological indicator, the treatment on
    # the indicator, the string indicator for the sensitivity analysis, and the regression coefficients
    coeff_dataframe <- data.frame(pi = codename
                                  , treatment_on_contin
                                  , sensitivity_range
                                  , coeff
                                  , stringsAsFactors = FALSE) %>%
      # Exclude regression coefficients pertaining the survey clusters. 
      filter(grepl("cluster", term) == FALSE)
    
  } else if(model_types == "demo") {
    
    # Define a dataframe to containing the codename of the population type (i.e. "all" or "[insert codename of
    # physiological indicator]"), the treatment on age, the string indicator for the sensitivity analysis, and 
    # the regression coefficients
    coeff_dataframe <- data.frame(population_type = codename
                                  , treatment_on_demo = treatment_on_contin
                                  , sensitivity_range
                                  , coeff
                                  , stringsAsFactors = FALSE) %>%
      # Exclude regression coefficients pertaining the survey clusters. 
      filter(grepl("cluster", term) == FALSE)
  }
  
  return(coeff_dataframe)
  
}