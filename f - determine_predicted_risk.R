#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############  FUNCTION TO DETERMINE THE HAZARD RATIO FOR EACH VALUE OF THE CONTINOUS VARIABLE  ##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function runs a cox regression model to calculate the hazard ratio of each value of the 
#          transformed variable
#          
# Inputs: dataset_updated - dataframe the pertinent variables for running the regression model 
#         contin_values - numeric vector of the continous variable 
#         included_colnames - string vector of the pertinent column names
#         analysis_type - string indicating whether the survey weights should be used or not, i.e. "weighted" 
#                         or "unweighted"
#         codename - string indicating the name of the population type ("all" or "[insert code name of  
#                    physiological indicator]") or the codename of the physiological indicator
#         treatment_on_contin - string indictating the treatment on the continous variable, i.e. "linear", 
#                               "novemtiles", or "splines"
#         colname_treated - string of the column name pertaining to the transformed variable 
#         sensitivity_range - string indicating the percentiles of the transformed variable on which 
#                             participants to include in the regression models 
#         model_types - string indicating whether the transformation was applied on age or the physiological 
#                       indicator
#         dataset_refs - default to NA. If specified, then it's a dataframe of reference groups
#
# Outputs: df_contin_value - a dataframe of the hazard ratios of each value of the transformed variable

determine_predicted_risk <- function(dataset_updated
                                     , contin_values
                                     , included_colnames
                                     , type_analysis
                                     , codename
                                     , treatment_on_contin
                                     , colname_treated
                                     , sensitivity_range
                                     , model_types
                                     , dataset_refs = NA)
{
  # Define a vector of column names for the explanatory and outcome variables
  included_columns_updated <- c(included_colnames
                                , "time_to_death"
                                , "mortality_status")

  # print(included_columns_updated)
  
  # Define the subset to have the pertinent variables
  subset_updated <- dataset_updated[,included_columns_updated]
  
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
  
  # Define the subset to have the pertinent variables
  subset_updated <- dataset_updated[,included_columns_updated]
  # print(colnames(subset_updated))
  
  # Run a regression model to calculate the predicted risk
  if(treatment_on_contin %in% c("linear", "novemtiles"))
  {
    
    if(type_analysis == "weighted")
    {
      # Run the cox regression model accounting for NHANES sampling design and the survey weights
      coxph_model <- coxph(Surv(time = time_to_death
                                , event = mortality_status) ~ . +
                             cluster(survey_clusters)
                           , weights = survey_weights
                           , data = subset_updated)
      
     
    } else if(type_analysis == "unweighted") {
      
      # Run the cox regression model without accounting for NHANES sampling design and the survey weights
      coxph_model <- coxph(Surv(time = time_to_death
                                , event = mortality_status) ~ .
                           , data = subset_updated)
      
    }
    
  } else if(treatment_on_contin == "splines") {
    
    # print(colnames(subset_updated))
    index_splines <- which(grepl("^splines", colnames(subset_updated)))
    
    columns_with_out_splines <- colnames(subset_updated)[-index_splines]
    # print(columns_with_out_splines)
    
    subset_updated <- subset_updated[,columns_with_out_splines]
    
    # Define for the number of cut-off points 
    step_size <- 1/4
    
    # Cut the continous variables into quartiles to determine the cut-off points or knots
    ntiles_pi <- quantile(contin_values
                          , na.rm = TRUE
                          , probs = seq(0, 1, by = step_size)
                          , names = FALSE)

    # Define a vector to contain the cut-off points, which is all boundaries except the minimum and maximum 
    # ones
    ntiles_pi <- ntiles_pi[2:((1/step_size))]
    
    if(type_analysis == "weighted") 
    {
      # Run the cox regression splines model accounting for NHANES sampling design and the survey weights
      coxph_model <- coxph(Surv(time = time_to_death
                                , event = mortality_status) ~
                             bs(contin_values, knots = ntiles_pi) +
                             cluster(survey_clusters) +
                             .
                           , weights = survey_weights
                           , data = subset_updated)

      
    } else if(type_analysis == "unweighted") {
      
      # Run the cox regression splines model without accounting for NHANES sampling design and the survey 
      # weights
      coxph_model <- coxph(Surv(time = time_to_death
                                , event = mortality_status) ~ 
                             bs(contin_values, knots = ntiles_pi) + .
                           , data = subset_updated)
     
      
    }
    # print(summary(coxph_model))
    
    # Define a string so that it can be used to extract the correct dataframe containing the predicted risk 
    # for the spline model
    colname_treated <- "contin_values"
    
  } else {
    print("Error: Transformation on Physiological Indicator was not considered.")
  }
  
  # print((coxph_model))
  
  # print(colname_treated)
  
  # Determine the predicted risk for each explanatory variable
  predicted_data_points <- termplot(coxph_model
                                    , se = TRUE
                                    , plot = FALSE)
  # print(predicted_data_points)
  
  # Extract the predicted risk for the transformed variable 
  # This dataframe has a predicted risk for each unique value of the transformed variable
  df_contin_value <- predicted_data_points[[colname_treated]]
  # print(df_contin_value)
  
  # If the reference group is not specified, then set the median as the reference group
  if(anyNA(dataset_refs) == TRUE)
  {
    reference_point <- quantile(contin_values
                                , probs = 0.5
                                , names = FALSE)
    
  # If the reference is specified, then set that value as the reference group
  } else {
    reference_point <- dataset_refs %>%
      # Include a given treatment 
      filter(treatment_on_pi == treatment_on_contin) %>%
      # Select the column vector pertaining to value serving as the reference group
      select(ref) %>%
      unlist(., use.names = FALSE) %>%
      # Ensure that the value is numeric
      as.numeric(.)
  }
  
  # print(reference_point)
  
  # If the reference point is a value of the continuous variable, then the reference point remains unchanged 
  if(reference_point %in% df_contin_value$x)
  {
    reference_point <- reference_point
    
  # If the reference point is not, then determine the value that is closest to the reference point
  } else {
    
    # Find the difference between the reference point and each value of the continous variable
    diffs <- abs(contin_values - reference_point)
    
    # Determine the minimum difference
    min_diff <- min(diffs)
    
    # Determine the index pertaining to the minimum difference
    index_min <- which(diffs == min_diff)
    
    # Find the value that is close to the reference point
    value_close_to_reference_point <- contin_values[index_min] %>%
      unique()
    
    # If there are several values close to the reference point, then close one of them
    reference_point <- value_close_to_reference_point[1] %>%
      unname()
  }
  # print(reference_point)
  
  # Define a dataframe of the predicted risk 
  if(model_types == "pi")
  {
    # Rename the column names for legibility 
    colnames(df_contin_value) <- c("pi_value", "predicted_risk", "se")
    
    # Find the predicted risk corresponds to the reference point
    center <- df_contin_value$predicted_risk[df_contin_value$pi_value == reference_point]
    # print(center)
    
    # Define the dataframe of predicted risk for a physiological indicator
    df_contin_value <- df_contin_value %>%
      # Define a column vector of the codename of the physiological indicator
      mutate(pi = rep(codename, nrow(.))) %>%
      # Define a column vector of the treatment on the physiological indicator
      mutate(treatment_on_pi = rep(treatment_on_contin, nrow(.))) %>%
      # Define a column vector of sensitivity analysis on the physiological indicator
      mutate(sensitivity_range = rep(sensitivity_range, nrow(.))) %>%
      # Define the mean hazard ratio for each value of the physiological indicator
      mutate(hazard_ratio = exp(predicted_risk - center)) %>%
      # Define the lower bound of the 95% confidence interval on hazard ratio for each value of the 
      # physiological indicator
      mutate(hazard_ratio_low_ci = exp(predicted_risk - se*1.96 - center)) %>%
      # Define the upper bound of the 95% confidence interval on hazard ratio for each value of the 
      # physiological indicator
      mutate(hazard_ratio_high_ci = exp(predicted_risk + se*1.96 - center))
    
    # Define a vector of the column names rearranged in a reasonable order
    colnames_arranged <- c("pi"
                           , "treatment_on_pi"
                           , "sensitivity_range"
                           , "pi_value"
                           , "predicted_risk"
                           , "se"
                           , "hazard_ratio"
                           , "hazard_ratio_low_ci"
                           , "hazard_ratio_high_ci")
    
  } else if(model_types == "demo") {
    
    # Rename the column names for legibility 
    colnames(df_contin_value) <- c("demo_value", "predicted_risk", "se")
    
    # Find the predicted risk corresponds to the reference point
    center <- df_contin_value$predicted_risk[df_contin_value$demo_value == reference_point]
    
    # Define the dataframe of predicted risk for the age variable
    df_contin_value <- df_contin_value %>%
      # Define a column vector of the population type ("all" or "[insert code name of physiological indicator]")
      mutate(population_type = rep(codename, nrow(.))) %>%
      # Define a column vector of the treatment on the age variable
      mutate(treatment_on_demo = rep(treatment_on_contin, nrow(.))) %>%
      # Define a column vector of sensitivity analysis on the age variable
      mutate(sensitivity_range = rep(sensitivity_range, nrow(.))) %>%
      # Define the mean hazard ratio for each value of the age variable
      mutate(hazard_ratio = exp(predicted_risk - center)) %>%
      # Define the lower bound of the 95% confidence interval on hazard ratio for each value of the age variable
      mutate(hazard_ratio_low_ci = exp(predicted_risk - se*1.96 - center)) %>%
      # Define the uper bound of the 95% confidence interval on hazard ratio for each value of the age variable
      mutate(hazard_ratio_high_ci = exp(predicted_risk + se*1.96 - center))
    
    # Define a vector of the column names rearranged in a reasonable order
    colnames_arranged <- c("population_type"
                           , "treatment_on_demo"
                           , "sensitivity_range"
                           , "demo_value"
                           , "predicted_risk"
                           , "se"
                           , "hazard_ratio"
                           , "hazard_ratio_low_ci"
                           , "hazard_ratio_high_ci")
    
  }
  
  # Arrange the column into the specified order
  df_contin_value <- df_contin_value[,colnames_arranged]
  # View(df_contin_value)

  return(df_contin_value)
}
