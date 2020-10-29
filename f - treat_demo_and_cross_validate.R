#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############  FUNCTION TO PERFORM A K-FOLD CROSS-VALIDATED MODEL OF MORTALITY AND TRANSFORMED AGE  ############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function runs a k-fold cross-validated model on the outcome as mortality and the transformed 
#          age variable with adjustment for sex and race. 
#          
# Inputs: x - dataframe the pertinent variables for running the regression model 
#         demo_treatment - string indictating the treatment on the continous variable, i.e. "linear", 
#                          "novemtiles", or "splines"
#         analysis_type - string indicating whether the survey weights should be used or not, i.e. "weighted"  
#                         or "unweighted"
#         number_of_folds - numeric indicating the number of folds for k-fold cross validation
#         number_of_replicates - numeric indicating the number of bootstrap replicates
#         sensitivity_range - string indicating the percentiles of the transformed variable on which 
#                             participants to include in the regression models 
#         subpopulation - string indicating the type of popluation, i.e. "all" or "[insert code name of 
#                         physiological indicator]"
#
# Outputs: list_regression_results - a list 3 dataframes with each for the prediction performance, 
#                                    coefficients, and predicted risk

treat_demo_and_cross_validate <- function(x
                                          , demo_treatment
                                          , analysis_type
                                          , number_of_folds
                                          , number_of_replicates
                                          , sensitivity_range
                                          , subpopulation)
{
  # Refine with a better name
  complete_dataset <- x
  
  # Define a vector for the continuous variable to conduct the linear and non-linear transformations
  contin_variable <- unlist(complete_dataset[,"age"]
                            , use.names = FALSE)
  
  # Apply the linear and non-linear transformations on age
  list_updated_dataset_and_transformed_columns <- transform_contin(contin_variable
                                                                   , complete_dataset
                                                                   , demo_treatment
                                                                   , "age")
  
  # Extract the updated dataset
  updated_dataset <- list_updated_dataset_and_transformed_columns[["dataset"]]
  # View(updated_dataset)
  
  # Extract the treatment pattern to be used for extract the column vectors pertaining to the transformed
  # variable 
  pattern_treated <- list_updated_dataset_and_transformed_columns[["treatment_pattern"]]
  # print(pattern_treated)
  
  print(unique(updated_dataset$time_to_death))
  
  # Define the outcome variable as a survival object
  y <- Surv(time = updated_dataset$time_to_death
            , event = updated_dataset$mortality_status)
  
  # Determine the column names of the transformed variable 
  colnames_contin <- grepl(pattern_treated
                           , colnames(updated_dataset)) %>%
    colnames(updated_dataset)[.]
  
  # Run the 10-fold cross-validated models
  if(analysis_type == "weighted")
  {
    # Extract the survey weights 
    survey_weights <- updated_dataset$adjusted_weights
    
    # Define a vector of column names pertaining to the explanatory variables
    colnames_include <- c( "gender"
                           , "race"
                           , "cluster"
                           , colnames_contin)
    
    # Define the predictors dataframe by extracting the explanatory variables
    x_dataframe <- updated_dataset[,colnames_include] %>%
      na.omit(.)
    
    # Convert the predictors dataframe into a matrix
    x <- model.matrix(~.
                      , data = x_dataframe)
    
    # Set the random number generator to ensure reproducible results 
    set.seed(1)
    
    # Run 10-fold cross-validated cox model with survey weights
    cv_models <- cv.glmnet(x = x
                           , y = y
                           , family = "cox"
                           , alpha = 0
                           , lambda = c(0,0.1,0.01)
                           , weights = survey_weights
                           , nfolds = number_of_folds
                           , parallel = TRUE)
    
  } else if(analysis_type == "unweighted") {
    
    # Define a vector of column names pertaining to the explanatory variables
    colnames_include <- c("gender"
                          , "race"
                          , colnames_contin)
    
    # Define the predictors dataframe by extracting the explanatory variables
    x_dataframe <- updated_dataset[,colnames_include] %>%
      na.omit(.)
    
    # Convert the predictors dataframe into a matrix
    x <- model.matrix(~.
                      , data = x_dataframe)
    
    # Set the random number generator to ensure reproducible results 
    set.seed(1)
    
    # Run 10-fold cross-validated cox model without survey weights
    cv_models <- cv.glmnet(x = x
                           , y = y
                           , family = "cox"
                           , alpha = 0
                           , lambda = c(0,0.1,0.01)
                           , nfolds = number_of_folds
                           , parallel = TRUE)
  }
  
  
  # Determine the dimension of the matrix of predictors
  dimension_matrix <- dim(x)
  
  # Initialize an empty list
  list_regression_results <- list()
  
  # Determine the statistics for each predictor from the regression model
  list_regression_results[["coefficients"]] <- extract_coefficients(updated_dataset
                                                                    , colnames_include
                                                                    , analysis_type
                                                                    , subpopulation
                                                                    , demo_treatment
                                                                    , sensitivity_range
                                                                    , "demo")
  
  # Calculate the predicted risk for the linear and splines models
  if(demo_treatment %in% c("linear", "splines"))
  {
    list_regression_results[["predicted_risk"]] <- determine_predicted_risk(updated_dataset
                                                                            , contin_variable
                                                                            , colnames_include
                                                                            , analysis_type
                                                                            , subpopulation
                                                                            , demo_treatment
                                                                            , pattern_treated
                                                                            , sensitivity_range
                                                                            , "demo")
    # View(list_regression_results[["predicted_risk"]] )
    
  }
  
  # Define a vector of the names of the pertinent fit measures
  fit_measures <- c("rsq"
                    , "AIC"
                    , "concordance")

  # Determine the confidence intervals on the fit measures with bootstrapping
  list_regression_results[["fit"]] <- determine_range_on_fit_measures(x
                                                                      , y
                                                                      , cv_models
                                                                      , fit_measures
                                                                      , subpopulation
                                                                      , demo_treatment
                                                                      , number_of_replicates
                                                                      , sensitivity_range
                                                                      , "demo"
                                                                      , dimension_matrix)


  return(list_regression_results)
}