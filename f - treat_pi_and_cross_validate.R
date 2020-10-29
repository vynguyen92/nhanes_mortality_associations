#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##  FUNCTION TO PERFORM A K-FOLD CROSS-VALIDATED MODEL OF MORTALITY AND TRANSFORMED PHYSIOLOGICAL INDICATOR  ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function runs a k-fold cross-validated model on the outcome as mortality and the transformed 
#          physiological indicator with adjustment for age, sex, and race. 
#          
# Inputs: x - dataframe the pertinent variables for running the regression model 
#         pi_treatment - string indictating the treatment on the physiological indicator, i.e. "linear", 
#                        "novemtiles", or "splines"
#         analysis_type - string indicating whether the survey weights should be used or not, i.e. "weighted"  
#                         or "unweighted"
#         number_of_folds - numeric indicating the number of folds for k-fold cross validation
#         number_of_replicates - numeric indicating the number of bootstrap replicates
#         sensitivity_range - string indicating the percentiles of the transformed variable on which 
#                             participantsto include in the regression models 
#         dataset_references - default to NA. If specified, then it's a dataframe of reference groups
#         demo_variables - string vector of the names of the covariates. Defaults as c("age", "gender", "race")  
#                          due to coupling with run_cross_validation_pi_mortality().
#
# Outputs: list_regression_results - a list 3 dataframes with each for the prediction performance, coefficients,
#                                    and predicted risk

treat_pi_and_cross_validate <- function(x
                                        , pi_treatment
                                        , analysis_type
                                        , number_of_folds
                                        , number_of_replicates
                                        , sensitivity_range
                                        , dataset_references = NA
                                        , demo_variables)
{
  # Refine with a better name
  complete_dataset <- x
  
  # Determine the codename of the physiological indicaotr 
  unique_codename <- unlist(unique(complete_dataset[,"pi"])
                            , use.names = FALSE)
  
  # Determine the vector for the physiological indicator to conduct the linear and non-linear transformations
  pi_values <- unlist(complete_dataset[,"pi_value"]
                      , use.names = FALSE)
  
  # Apply the linear and non-linear transformations on the physiological indicator
  list_updated_dataset_and_transformed_columns <- transform_contin(pi_values
                                                                   , complete_dataset
                                                                   , pi_treatment
                                                                   , "pi"
                                                                   , dataset_references)
  
  # Extract the updated dataset
  updated_dataset <- list_updated_dataset_and_transformed_columns[["dataset"]]
  
  # Extract the treatment pattern to be used for extract the column vectors pertaining to the transformed
  # variable 
  pattern_treated_pi <- list_updated_dataset_and_transformed_columns[["treatment_pattern"]]
  
  # print(unique(updated_dataset$time_to_death))
  
  # Define the outcome variable as a survival object
  y <- Surv(time = updated_dataset$time_to_death
            , event = updated_dataset$mortality_status)
  
  # Determine the column names of the transformed variable to be used for extracting these columns 
  colnames_pi <- grepl(pattern_treated_pi
                       , colnames(updated_dataset)) %>%
    colnames(updated_dataset)[.]
  
  # contin_variable <- unlist(updated_dataset[,"age"]
  #                           , use.names = FALSE)
  # 
  # list_updated_dataset_and_transformed_age <- transform_contin(contin_variable
  #                                                              , updated_dataset
  #                                                              , "splines"
  #                                                              , "age")
  # 
  # updated_dataset <- list_updated_dataset_and_transformed_age[["dataset"]]
  # 
  # pattern_treated_age <- list_updated_dataset_and_transformed_age[["treatment_pattern"]]
  # 
  # colnames_age <- grepl(pattern_treated_age
  #                      , colnames(updated_dataset)) %>%
  #   colnames(updated_dataset)[.]
  
  # Run the 10-fold cross-validated models
  if(analysis_type == "weighted")
  {
    # Extract the survey weights 
    survey_weights <- updated_dataset$adjusted_weights

    # Define a vector of column names pertaining to the explanatory variables
    colnames_include <- c(demo_variables
                          , "cluster"
                          , colnames_pi)

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
                           , lambda = seq(0, 1, 0.1)
                           , weights = survey_weights
                           , nfolds = number_of_folds
                           )

  } else if(analysis_type == "unweighted") {

    # Define a vector of column names pertaining to the explanatory variables
    colnames_include <- c(demo_variables
                          , colnames_pi)

    # Define the predictors dataframe by extracting the explanatory variables
    x_dataframe <- updated_dataset[,colnames_include] %>%
      na.omit(.)
    print(colnames(x_dataframe))

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
                           , lambda = c(seq(0, 1, 0.1))
                           , nfolds = number_of_folds
                           )
    # print(cv_models)

  }
  
  # Determine the dimension of the matrix of predictors
  dimension_matrix <- dim(x)

  # Initialize an empty list
  list_regression_results <- list()

  # Determine the statistics for each predictor from the regression model
  list_regression_results[["coefficients"]] <- extract_coefficients(updated_dataset
                                                                    , colnames_include
                                                                    , analysis_type
                                                                    , unique_codename
                                                                    , pi_treatment
                                                                    , sensitivity_range
                                                                    , "pi")
  # View(list_regression_results[["coefficients"]])
  
  
  # Calculate the predicted risk for the linear and splines models
  if(pi_treatment %in% c("linear", "splines"))
  {
    list_regression_results[["predicted_risk"]] <- determine_predicted_risk(updated_dataset
                                                                            , pi_values
                                                                            # , pi_values
                                                                            , colnames_include
                                                                            , analysis_type
                                                                            , unique_codename
                                                                            , pi_treatment
                                                                            , pattern_treated_pi
                                                                            , sensitivity_range
                                                                            , "pi"
                                                                            , dataset_refs = dataset_references)

    # View(list_regression_results[["predicted_risk"]])

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
                                                                      , unique_codename
                                                                      , pi_treatment
                                                                      , number_of_replicates
                                                                      , sensitivity_range
                                                                      , "pi"
                                                                      , dimension_matrix)
  # View(list_regression_results[["fit"]])

  return(list_regression_results)
}