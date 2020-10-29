#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############  FUNCTION TO PERFORM CROSS VALIDATION ON THE ASSOCIATIONS BETWEEN AGE AND MORTALITY  ############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function performs k-fold cross-validation on the associations between transformed age and 
#          mortality for the entire NHANES population and the subpopulations for a given physiological indicator
#          
# Inputs: dataset_merged - dataset of demographics, mortality, and physiological indicators
#         dataset_long - long-formatted dataset based on the physiological indictors
#         num_folds - number of folds for k-fold cross validation
#         num_bootstrap_replicates - number of bootstrap replicates 
#
# Outputs: list_results - a list of two lists (one for the unweighted and the other for the weighted analysis) 
#                         with each list containing a dataset each for the prediction performance, coefficients,
#                         and predicted risk

run_cross_validation_demo <- function(dataset_merged
                                      , dataset_long
                                      , num_folds
                                      , num_bootstrap_replicates
                                      , df_refs)
{
  # Define a dataset containing participants who have complete data for mortality, age, sex, and race
  dataset_merged <- dataset_merged %>%
    # Define new columns with more legible codenames
    mutate(mortality_status = MORTSTAT
           , time_to_death = PERMTH_INT
           , weights = WTINT2YR
           , cluster = paste(dataset_merged$SDMVPSU, dataset_merged$SDMVSTRA)
           , cycles = SDDSRVYR
           , age = RIDAGEYR
           , gender = relevel(factor(if_else(RIAGENDR == 1, "_male", "_female"))
                              , ref = "_male")
           , race = relevel(factor(case_when(RIDRETH1 == 1 ~ "_mexican_american"
                                             , RIDRETH1 == 2 ~ "_other_hispanic"
                                             , RIDRETH1 == 3 ~ "_whites"
                                             , RIDRETH1 == 4 ~ "_blacks"
                                             , RIDRETH1 == 5 ~ "_other" ))
                            , ref = "_whites")) %>%
    # Select the pertinent variables
    dplyr::select("SEQN"
                  , "mortality_status"
                  , "time_to_death"
                  , "weights"
                  , "cluster"
                  , "cycles"
                  , "age"
                  , "gender"
                  , "race") %>%
    # Exclude participants who have missing data
    na.omit(.) %>%
    # Exclude participants with no follow-up data
    filter(time_to_death != 0) %>%
    # Define a column for cycle-adjusted weights
    mutate(adjusted_weights = (1/length(unique(cycles)))*weights)
  
  # Specify the different transformations on age
  treatments_on_age <- c("linear"
                         , "novemtiles"
                         , "splines"
                         )

  # Determine the number of transformations on age
  num_treatments <- length(treatments_on_age)

  # Specify the different types of analyses: adjusting and NOT adjusting for the survey weights
  analysis_type <- c("unweighted"
                     , "weighted"
                     )
  # Determine the number of analysis types
  num_analysis_types <-  length(analysis_type)

  # Specify the restrictions on age
  types_sensitivity_analyses <- c("00_100"
                                  , "01_99"
                                  , "05_95"
                                  , "10_90"
                                  )
  # Determine the number of sensitivity analyses
  num_sensitivity_analyses <- length(types_sensitivity_analyses)

  # Determine a vector of the codename for the physiological indicators
  pi_include <- unique(dataset_long$pi)
  # Define an index for given physiological indicators. This aspect helps with debugging
  index_pi <- which(pi_include %in% c("BMXBMI"
                                      , "LBXCRP"
                                      ))
  # Include all physiological indicators or a subset for debugging
  pi_include <- pi_include#[index_pi]
  # Determine the number of physiological indicators
  num_pi <- length(pi_include)

  # Determine all combinations of sensitivity analyses and transformation on age for all NHANES participants
  combos_treatment_on_age <- paste("00_100"
                                   , treatments_on_age
                                   , "all"
                                   , sep = " - ")

  # Determine all combinations of sensitivity analyses and linear transformation on age for each 
  # subpopulation with data available for a given physiological indicator
  combos_pi <- paste(rep(types_sensitivity_analyses, num_pi)
                     , "linear"
                     , sort(rep(pi_include, num_sensitivity_analyses))
                     , sep = " - ")

  # Define a vector of strings with all the combinations
  combos_all <- c(combos_treatment_on_age
                  , combos_pi
                  )
  # Determine the number of combinations
  num_combos_all <- length(combos_all)

  # Initialize an empty list to fill it with the regression results
  regression_results <- list()

  
  # Run a series of regression models for each type of analysis.
  for(k in seq(num_analysis_types))
  {
    # Determine the type of analysis
    analysis_type_k <- analysis_type[k]
    # Print out the analysis type to provide a marker of where the code is in the run-process
    print(analysis_type_k)

  # Define the number of cores for parallelizing
  numCores <- num_combos_all # detectCores(logical = TRUE)
  # Create a set of copies of R for running in parallel
  cl <-  makeCluster(numCores)
  clusterSetRNGStream(cl)
  # Provide the necessary libraries to each cluster
  clusterEvalQ(cl, {
    library("tidyverse")
    library("broom")
    library("survival")
    library("sjlabelled")
    library(time"labelled")
    library("survey")
    library("glmnet")
    library("boot")
    library("bootstrap")
    library("splines")
  })
  # Provide the necessary custom functions to each cluster
  clusterExport(cl
                , c("treat_demo_and_cross_validate"
                    , "transform_contin"
                    , "extract_coefficients"
                    , "determine_predicted_risk"
                    , "determine_range_on_fit_measures"
                    , "bootstrap_on_fit_measures"
                ))

    # Run the parallelizing over the combinations
    # regression_results[[analysis_type_k]] <- for(i in 1:num_combos_all)
    regression_results[[analysis_type_k]] <- parLapply(cl, 1:num_combos_all, function(i)
    {
      # Determine the combination
      combo_i <- combos_all[i]
      # Print out the combination. This won't be printed to the console during the parallelization.
      # But this helps with debugging.
      print(combo_i)

      # Split the combination based on a delimiter to extract information
      sensitivity_treatment_population <- strsplit(combo_i, " - ") %>%
        unlist(.)

      # Extract information on the sensitivity analysis
      sensitivity_i <- sensitivity_treatment_population[1]

      # Extract information on the transformation
      treatments_on_age_i <- sensitivity_treatment_population[2]

      # Extract information on the population
      subpopulation_i <- sensitivity_treatment_population[3]

      # Use the information of the sensitivity analysis to determine the minimum and maximum percentiles
      range_percentiles <- strsplit(sensitivity_i, "_") %>%
        unlist(.) %>%
        as.numeric(.)

      # Use the cleaned merged dataset for any analysis performed on the entire NHANES population
      if(subpopulation_i == "all")
      {
        dataset_main <- dataset_merged

      # Otherwise, extract the cleaned dataset for any analysis to be performed on a subpopulation
      } else {

        # Extract the dataset for a given physiological indicator
        dataset_pi <- dataset_long %>%
          filter(pi == subpopulation_i) %>%
          na.omit(.)

        # Determine the minimum percentile for the sensitivity analyses
        min_perc <- range_percentiles[1]

        # Determine the maximum percentile for the sensitivity analysis
        max_perc <- range_percentiles[2]

        # Determine the physiological measurement corresponding to the minimum percentile for the 
        # sensitivity analysis
        min_pi_perc_k <- quantile(dataset_pi$pi_value
                                  , probs = min_perc/100
                                  , names = FALSE)

        # Determine the physiological measurement corresponding to the maximum percentile for the 
        # sensitivity analysis
        max_pi_perc_k <- quantile(dataset_pi$pi_value
                                  , probs = max_perc/100
                                  , names = FALSE)

        # Determine the dataset to include participants who are with these percentiles for analysis
        dataset_main <- dataset_pi %>%
          filter(pi_value >= min_pi_perc_k &
                   pi_value <= max_pi_perc_k)

      }

      # Perform the transformation on age and run the cross-validated regression models on the specified 
      # population
      regression_results_temp <- dataset_main %>%
        treat_demo_and_cross_validate(.
                                      , demo_treatment = treatments_on_age_i
                                      , analysis_type = analysis_type_k
                                      , number_of_folds = num_folds
                                      , number_of_replicates = num_bootstrap_replicates
                                      , sensitivity_range = sensitivity_i
                                      , subpopulation = subpopulation_i)

      # Return a list of a dataset each for the coefficients, prediction performance, and predicted risk 
      # for a given combination and store it in a list
      return(regression_results_temp)
    })

  # Eliminate all copies of R
  stopCluster(cl)
  }



  # Initialize an empty list to store the reorganized regression results
  list_results <- list()

  # Process the regression results for each analysis type
  for(k in seq(num_analysis_types))
  {
    # Determine the analysis type
    analysis_type_k <- analysis_type[k]
    # print(analysis_type_k)

    # Process the regression results so that there are a dataset for the prediction performance, the
    # coefficients, and the predicted risk
    list_results[[analysis_type_k]] <- process_results_from_parallelization(regression_results[[analysis_type_k]]
                                                                            , "demo")
  }

  return(list_results)
}
