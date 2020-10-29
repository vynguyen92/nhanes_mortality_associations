#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### FUNCTION TO PERFORM CROSS VALIDATION ON THE ASSOCIATIONS BETWEEN PHYSIOLOGICAL INDICATOR AND MORTALITY ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function performs k-fold cross-validation on the associations between transformed physiological 
#          indicator and mortality for the entire NHANES and restricted populations
#          
# Inputs: dataset_long - long-formatted dataset based on the physiological indictors
#         type_analysis - string indicating whether the survey weights should be used or not, i.e. "weighted"  
#                         or "unweighted"
#         num_folds - number of folds for k-fold cross validation
#         num_bootstrap_replicates - number of bootstrap replicates 
#         ref_dataset - default to NA. If specified, then it's a dataframe of reference groups
#         demo_covariates - string vector of the names of the covariates. Defaults as c("age", "gender", 
#                           "race").
#
# Outputs: list_results - a list containing a dataset each for the prediction performance, coefficients, and
#                         predicted risk

run_cross_validation_pi_mortality <- function(dataset_long
                                              , type_analysis
                                              , num_folds
                                              , num_bootstrap_replicates
                                              , ref_dataset 
                                              , demo_covariates = c("age", "gender", "race"))
{
  # Determine the codename for all physiologica indicators 
  pi_include <- unique(dataset_long$pi)
  
  # Define an index for given physiological indicators. This aspect helps with debugging
  index_pi <- which(pi_include %in% c("BMXBMI"
                                      #,  "LBXCRP"
                                       ))
  # Include all physiological indicators or a subset for debugging
  pi_include <- pi_include#[index_pi]
  # Determine the number of physiological indicators
  num_pi <- length(pi_include)
  
  # Define all transformation on the physiological indicators 
  treatments_on_pi <- c("linear"
                        , "novemtiles"
                        , "splines"
                        )
  # Determine the number of transformations
  num_treatments <- length(treatments_on_pi)
  
  # Specify the restrictions on the physiological measurements
  types_sensitivity_analyses <- c("00_100"
                                  , "01_99"
                                  , "05_95"
                                  , "10_90"
                                  )
  # Determine the number of sensitivity analyses
  num_sensitivity_analyses <- length(types_sensitivity_analyses)
  
  # Define all combinations of the sensitivity analyses and the physiological indicators 
  combos <- paste(sort(rep(types_sensitivity_analyses, num_pi))
                  , rep(pi_include, num_sensitivity_analyses)
                  , sep = " - ") %>%
    sort(.)
  # Determine the number of combinations between the sensitivity analyses and the physiological indicators 
  num_combos <- length(combos)
  
  # Initialize the list to hold the regression results
  regression_results <- list()
  

  
  # Run a series of regression models for each type of transformation.
  for(k in seq(num_treatments))
  {
    # Determine a transformation
    treatment_k <- treatments_on_pi[k]
    print(treatment_k)
    
    start_time <- Sys.time()

  # Define the number of cores for parallelizing
  numCores <- num_combos #detectCores(logical = TRUE)
  # Create a set of copies of R for running in parallel
  cl <-  makeCluster(numCores)
  clusterSetRNGStream(cl)
  # Provide the necessary libraries to each cluster
  clusterEvalQ(cl, {
     library("tidyverse")
     library("broom")
     library("survival")
     library("sjlabelled")
     library("labelled")
     library("survey")
     library("glmnet")
     library("boot")
     library("bootstrap")
     library("splines")
                  })
  # Provide the necessary custom functions to each cluster
   clusterExport(cl
                 , c("treat_pi_and_cross_validate"
                     , "transform_contin"
                     , "extract_coefficients"
                     , "determine_predicted_risk"
                     , "determine_range_on_fit_measures"
                     , "bootstrap_on_fit_measures"
                     , "dataset_ref_smoking"
                     , "dataset_ref_unweighted"
                     , "dataset_ref_weighted"
                     ))

    # Run the parallelizing over the combinations
    # regression_results[[treatment_k]] <- foreach(i = 1:num_combos) %do%
    regression_results[[treatment_k]] <- parLapply(cl, 1:num_combos, function(i)
    {
      # Determine the combination
      combo_i <- combos[i]

      # Print out the combination. This won't be printed to the console during the parallelization.
      # But this helps with debugging.
      print(paste(type_analysis
                  , treatment_k
                  , combo_i
                  , sep = " - "))
      
      # Paste the treatment with the sensitivity analysis and the physiological indicator 
      treatment_sensitivity_pi <- paste(treatment_k
                                        , combo_i
                                        , sep = " - ")
      
      # Split the combination based on a delimiter to extract information
      sensitivity_pi <- strsplit(combo_i, " - ") %>%
        unlist(.)
        
      # Extract information on the sensitivity analysis
      sensitivity <- sensitivity_pi[1]

      # Extract the information on the physiological indicator
      pi_i <- sensitivity_pi[2]

      # Use the information of the sensitivity analysis to determine the minimum and maximum percentiles
      range_percentiles <- strsplit(sensitivity, "_") %>%
        unlist(.) %>%
        as.numeric(.)

      # Determine the minimum percentile for the sensitivity analyses
      min_perc <- range_percentiles[1]

      # Determine the maximum percentile for the sensitivity analysis
      max_perc <- range_percentiles[2]

      # Extract the dataset corresponding to participants with measurements for this physiological indicator
      dataset_pi <- dataset_long %>%
        filter(pi == pi_i) %>%
        na.omit(.)

      # Determine the physiological measurement corresponding to the minimum percentile for the sensitivity 
      # analysis
      min_pi_perc_k <- quantile(dataset_pi$pi_value
                                , probs = min_perc/100
                                , names = FALSE)

      # Determine the physiological measurement corresponding to the maximum percentile for the sensitivity 
      # analysis
      max_pi_perc_k <- quantile(dataset_pi$pi_value
                                , probs = max_perc/100
                                , names = FALSE)

      # Determine the dataset to include participants who are with these percentiles for analysis
      dataset_pi <- dataset_pi %>%
        filter(pi_value >= min_pi_perc_k &
                 pi_value <= max_pi_perc_k)

      # Extract the reference points and novemtiles for a given physiological indicator and sensitivity 
      # analysis
      if(anyNA(ref_dataset) == TRUE)
      {
        ref_dataset_temp <- NA
      } else {
        
        ref_dataset_temp <- ref_dataset %>%
          filter(pi == pi_i) %>%
          filter(sensitivity_range == sensitivity)
        
      }
      # print(ref_dataset_temp)
      
      # Perform the transformation on the physiological indicator and run the cross-validated regression models 
      # on the specified population
      regression_results_temp <- dataset_pi %>%
        treat_pi_and_cross_validate(.
                                    , pi_treatment = treatment_k
                                    , analysis_type = type_analysis
                                    , number_of_folds = num_folds
                                    , number_of_replicates = num_bootstrap_replicates
                                    , sensitivity_range = sensitivity
                                    , dataset_references = ref_dataset_temp
                                    , demo_variables = demo_covariates)

      # Return a list of a dataset each for the coefficients, prediction performance, and predicted risk for 
      # a given combination and store it in a list
      return(regression_results_temp)

    })
    # Eliminate all copies of R
    stopCluster(cl)

    end_time <- Sys.time()
    time_treatment <- end_time - start_time
    print(time_treatment)

  }
 
  
  # Process the regression results so that there is a list with a dataset each for the prediction performance, 
  # the coefficients, and the predicted risk
  list_results <- process_results_from_parallelization(regression_results)

  return(list_results)
}