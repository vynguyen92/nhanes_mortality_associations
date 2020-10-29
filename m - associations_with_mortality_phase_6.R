#############################################################################################################
################  MAIN SCRIPT - ASSOCIATIONS BETWEEN PHYSIOLOGICAL INDICATORS AND MORTALITY  ################
#############################################################################################################

working_directory <- "/Volumes/Data/Mortality Project/ASSOCIATIONS/Phase 6"

setwd(working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Install and Upload Any Necessary Packages  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

install_packages_upload_libraries()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Compile and Clean the NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Set the working directory that contains the individual chemical datasets
chemical_datasets_directory <- paste(working_directory
                                     , "Chemical Datasets"
                                     , sep = "/")

# Extract the individual chemical datasets and compile them into the unclean chemicals dataset
chemicals_unclean <- compile_chemicals_dataset(chemical_datasets_directory
                                               , working_directory)

# Using the unclean chemicals dataset, this function will clean the chemical dataset while forming the
# comments dataset (a dataset of indicators to show whether a participant's measurement were above or below 
# the LOD) and the unfinished weights dataset (a dataset containing the survey weights)
# All of these datasets are stored in a list
nhanes_clean_list <- clean_chemicals_dataset(chemicals_unclean)

# Extract the clean chemical dataset from the list 
chemicals_clean <- nhanes_clean_list$chemicals_dataset

# Extract the unfinished dataset from the list
# This dataset is unfinished, since it does not containing the weights pertaining to the interview or MEC
# examinations. These weights are in the demographics dataset
weights_unfinished <- nhanes_clean_list$weights_dataset

# Set the working directory that contains the individual demographic datasets
demographics_dataset_directory <- paste(working_directory
                                        , "Demographics Datasets"
                                        , sep = "/")

# Extract the individual demographics datasets and compile them into the unclean demographics dataset
demographics_unclean <- compile_demographics_dataset(demographics_dataset_directory)

# Using the unclean demographics datasets, this function will clean the demographics dataset while forming
# the merged chemicals and demographics dataset and the finished weight dataset
nhanes_new_list <- 
  clean_demographics_dataset_and_merge_with_chemicals_and_weights_datasets(demographics_unclean_dataset = 
                                                                             demographics_unclean
                                                                           , chemicals_clean_dataset = 
                                                                             chemicals_clean
                                                                           , weights_unfinished_dataset = 
                                                                             weights_unfinished)

directory_for_mortality <- paste(working_directory
                                 , "Mortality Datasets/Follow-up to 2015"
                                 , sep = "/") 

mortality_unclean <- compile_mortality_dataset(directory_for_mortality
                                               , working_directory)

# Set the working directory that contains the individual response datasets
response_datasets_directory <- paste(working_directory
                                     , "Response Datasets"
                                     , sep = "/")

responses_unclean <- compile_response_dataset(response_datasets_directory
                                              , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Extract the Clean Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Extract the comments dataset, which contains indicators to show whether a participant measurements are 
# below or above LOD
comments_clean <- nhanes_clean_list$comments_dataset

# Extract the clean chemical dataset, which contains biomarkers that are indicative of chemical exposure 
# along with albumin and creatinine
chemicals_clean <- nhanes_clean_list$chemicals_dataset

# Extract the clean demographics dataset
demographics_clean <- nhanes_new_list$demographics_dataset  

# Extract the merged chemical and demographic datasets
# Note that participants who have chemical measurements are in this dataset
chem_demo <- nhanes_new_list$chemicals_demographics_dataset

# Extract the finished weights dataset, which contains the weights for the subsample and those for the interview
# and MEC examinations 
weights_finished <- nhanes_new_list$weights_finished_dataset

# Clean the mortality dataset
mortality_clean <- clean_mortality_dataset(mortality_unclean)

# Clean the dataset of physiological indicators 
responses_clean <- clean_response_dataset(responses_unclean
                                          , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Merge the Clean Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Form a merged dataset
nhanes_merged_dataset <- merge_datasets_together(demographics_dataset = demographics_clean
                                                 , mortality_dataset = mortality_clean
                                                 , response_dataset = responses_clean
                                                 , chemicals_dataset = chemicals_clean
                                                 , weights_dataset = weights_finished)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~  Exclude Physiological Indicators by Sample Size and Overlap with Mortality Data  ~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Determine the population statistics for each physiological indicator
stats_pi <- determine_population_stats_by_pi(nhanes_merged_dataset
                                             , responses_clean)

# Reformat the wide dataset into a long format for ease to analyze
nhanes_long_dataset <- clean_and_reformat_dataset(nhanes_merged_dataset
                                                  , stats_pi)

# Reformat the dataset for analysis involving adjusting for smoking
nhanes_long_dataset_smoking <- clean_and_reformat_dataset(nhanes_merged_dataset
                                                          , stats_pi
                                                          , "smoking")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~  Upload Dataset of the Body System of the Physiological Indicators  ~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

body_systems_df <- read_excel("NHANES - Statistics and Thresholds on Physiological Indicators 1b.xlsx"
                              , sheet = "Body System Categories")

# Change the column name of the 1st column from "codename" to "pi" for ease of merging 
colnames(body_systems_df)[1] <- "pi"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Determine Population Statistics  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Determine the population and distribution statistics for Table 1
determine_statistics_for_table_1(nhanes_merged_dataset
                                 , stats_pi
                                 , body_systems_df)

# Determine the population and distribution statistics for each subpopulation specific to a physiological indicator
determine_statistics_for_dif_subpopulations(nhanes_merged_dataset
                                            , nhanes_long_dataset
                                            , body_systems_df)

# Upload the dataset of clinical thresholds
pi_thresholds_df <- read_excel("NHANES - Statistics and Thresholds on Physiological Indicators 1b.xlsx"
                               , sheet = "Thresholds")

# Determine the ranges of the clinical thresholds 
final_pi_thresholds_df <- determine_final_thresholds(pi_thresholds_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Run Cross-Validated Regression Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Run the regression models for mortality as the outcome variable and the main predictor as transformed age
# adjusted for sex and race
start_time <- Sys.time()
list_demo_results <- run_cross_validation_demo(nhanes_merged_dataset
                                               , nhanes_long_dataset
                                               , num_folds = 10
                                               , num_bootstrap_replicates = 1000)
end_time <- Sys.time()
time_demo <- end_time - start_time 

save.image("C:\\Users\\T7920\\Desktop\\Vy\\Mortality Association Project\\w - 5-25 main_associations_with_mortality_phase_6.RData")

dataset_ref_unweighted <- NA
dataset_ref_weighted <- NA


# Run the regression models for mortality as the outcome variable and the main predictor as transformed physiological 
# indicator adjusted for age, sex, and race
start_time <- Sys.time()
list_pi_unweighted_results <- run_cross_validation_pi_mortality(dataset_long = nhanes_long_dataset
                                                                , type_analysis = "unweighted"
                                                                , num_folds = 10
                                                                , num_bootstrap_replicates = 10
                                                                , ref_dataset = dataset_ref_unweighted
                                                                , demo_covariates = c("age", "gender", "race"))
end_time <- Sys.time()
time_unweighted <- end_time - start_time 
save.image("C:\\Users\\T7920\\Desktop\\Vy\\Mortality Association Project\\w - 5-25 main_associations_with_mortality_phase_6.RData")



# Run the regression models for mortality as the outcome variable and the main predictor as transformed physiological 
# indicator adjusted for age, sex, and race and accounting for NHANES sampling design
start_time <- Sys.time()
list_pi_weighted_results <- run_cross_validation_pi_mortality(dataset_long = nhanes_long_dataset
                                                              , type_analysis = "weighted"
                                                              , num_folds = 10
                                                              , num_bootstrap_replicates = 10
                                                              , ref_dataset = dataset_ref_weighted
                                                              , demo_covariates = c("age", "gender", "race"))
end_time <- Sys.time()
time_weighted <- end_time - start_time
save.image("C:\\Users\\T7920\\Desktop\\Vy\\Mortality Association Project\\w - 5-25 main_associations_with_mortality_phase_6.RData")



# Run the regression models for mortality as the outcome variable and the main predictor as transformed 
# physiological indicator adjusted for age, sex, race, and smoking

dataset_ref_smoking <- NA

list_pi_weighted_results_smoking <- run_cross_validation_pi_mortality(dataset_long = nhanes_long_dataset_smoking
                                                                      , type_analysis = "weighted"
                                                                      , num_folds = 10
                                                                      , num_bootstrap_replicates = 1000
                                                                      , ref_dataset = dataset_ref_smoking
                                                                      , demo_covariates = c("age", "gender", "race", "smoking"))

# Define a dataset of reference points and novemtile based on the regression results on the physiological indicators
# These new reference points are based on measurements with the minimum hazard ratio
# Perform for the unweighted analyses
dataset_ref_unweighted <- determine_minimum_hr_pi_values(list_pi_unweighted_results
                                                         , nhanes_long_dataset)

# Perform for the weighted analyses
dataset_ref_weighted <- determine_minimum_hr_pi_values(list_pi_weighted_results
                                                       , nhanes_long_dataset)

# Rerun the cross-validated models on the physiological indicators with the new reference points and novemtiles
start_time <- Sys.time()
list_norm_pi_unweighted_results <- run_cross_validation_pi_mortality(dataset_long = nhanes_long_dataset
                                                                     , type_analysis = "unweighted"
                                                                     , num_folds = 10
                                                                     , num_bootstrap_replicates = 1000
                                                                     , ref_dataset = dataset_ref_unweighted)
end_time <- Sys.time()
time_unweighted_norm <- end_time - start_time
save.image("C:\\Users\\T7920\\Desktop\\Vy\\Mortality Association Project\\w - 5-25 main_associations_with_mortality_phase_6.RData")



# Rerun the cross-validated models on the physiological indicators with the new reference points and novemtiles with
# the survey weights and accounting for NHANES sampling design
start_time <- Sys.time()
list_norm_pi_weighted_results <- run_cross_validation_pi_mortality(dataset_long = nhanes_long_dataset
                                                                   , type_analysis = "weighted"
                                                                   , num_folds = 10
                                                                   , num_bootstrap_replicates = 1000
                                                                   , ref_dataset = dataset_ref_weighted)
end_time <- Sys.time()
time_weighted_norm <- end_time - start_time
save.image("C:\\Users\\T7920\\Desktop\\Vy\\Mortality Association Project\\w - 5-25 main_associations_with_mortality_phase_6.RData")


# dumb_age_linear <- run_cross_validation_pi_mortality(dataset_long = nhanes_long_dataset
#                                                    , type_analysis = "unweighted"
#                                                    , num_folds = 10
#                                                    , num_bootstrap_replicates = 5)
# 
# dumb_age_spline <- run_cross_validation_pi_mortality(dataset_long = nhanes_long_dataset
#                                                      , type_analysis = "unweighted"
#                                                      , num_folds = 10
#                                                      , num_bootstrap_replicates = 5)
# 
# dumb_age_linear_fit <- dumb_age_linear$fit %>% 
#   mutate(age_transformation = rep("linear age", nrow(.))) 
# 
# dumb_age_splines_fit <- dumb_age_spline$fit %>% 
#   mutate(age_transformation = rep("splines age", nrow(.)))
# 
# dumb_dumb <- dumb_age_linear_fit %>%
#   full_join(.
#             , dumb_age_splines_fit
#             , by = colnames(.)) %>%
#   select("pi","treatment_on_pi","sensitivity_range","age_transformation", everything())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Run Sex-Stratified Splines Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Run sex-stratified spline models with the median measurement as the reference point
list_sex_stratified_splines_results <- run_sex_splines_models_pi_mortality(nhanes_long_dataset)

# Determine the measurements with the lowest hazard ratio to serve as new reference points
list_sex_dataset_ref <- determine_minimum_hr_pi_values_by_sex(list_sex_stratified_splines_results
                                                              , nhanes_long_dataset)

# Rerun the sex-stratified spline models with the new reference points
list_norm_sex_stratified_splines_results <- run_sex_splines_models_pi_mortality(nhanes_long_dataset
                                                                                , list_sex_dataset_ref)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~  Table to Compare Clinical Thresholds with Association-Based Thresholds  ~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

df_compare_thresholds <- compare_thresholds_table_2(final_pi_thresholds_df
                                                    , list_norm_pi_weighted_results
                                                    , dataset_ref_weighted
                                                    , list_norm_sex_stratified_splines_results
                                                    , list_sex_dataset_ref
                                                    , body_systems_df
                                                    , "weighted")

df_compare_thresholds_weighted <- compare_thresholds(final_pi_thresholds_df
                                                     , list_norm_pi_weighted_results
                                                     , dataset_ref_weighted
                                                     , list_norm_sex_stratified_splines_results
                                                     , list_sex_dataset_ref
                                                     , body_systems_df
                                                     , "weighted")

df_compare_thresholds_unweighted <- compare_thresholds(final_pi_thresholds_df
                                                       , list_norm_pi_unweighted_results
                                                       , dataset_ref_unweighted
                                                       , list_norm_sex_stratified_splines_results
                                                       , list_sex_dataset_ref
                                                       , body_systems_df
                                                       , "unweighted")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Histograms  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Make a panel plot of histograms for all physiological indicators 
histogram_pi_distribution(nhanes_long_dataset
                          , working_directory
                          , body_systems_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~  Scatterplots of Sample Size and Fit Measures  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Make scatterplots of the predicton performance and the sample size of the physiological indicators 
scatterplot_facet_sample_size_fit(list_norm_pi_unweighted_results
                                  , "unweighted"
                                  , working_directory
                                  , body_systems_df)

scatterplot_facet_sample_size_fit(list_norm_pi_weighted_results
                                  , "weighted"
                                  , working_directory
                                  , body_systems_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Alphabet Soup Plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Make alphabet soup plots of the prediction performance of each fit measure
alphabet_soup_plot_fit_pi(list_norm_pi_unweighted_results
                          , "unweighted"
                          , working_directory
                          , body_systems_df)

alphabet_soup_plot_fit_pi(list_norm_pi_weighted_results
                          , "weighted"
                          , working_directory
                          , body_systems_df)

# Make faceted alphabet soup plot of the prediction performance straitifed by the sensitivity analyses for each
# fit measure
alphabet_soup_plot_panel_sensitivity_fit_pi(list_norm_pi_unweighted_results
                                            , "unweighted"
                                            , working_directory
                                            , body_systems_df)

alphabet_soup_plot_panel_sensitivity_fit_pi(list_norm_pi_weighted_results
                                            , "weighted"
                                            , working_directory
                                            , body_systems_df)

# Make faceted alphabet soup plot of the prediction performance for the Concordance Index and Nagelkerke R2
alphabet_soup_plot_facet_cindex_rsq(list_norm_pi_unweighted_results
                                    , "unweighted"
                                    , working_directory
                                    , body_systems_df)

alphabet_soup_plot_facet_cindex_rsq(list_norm_pi_weighted_results
                                    , "weighted"
                                    , working_directory
                                    , body_systems_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Volcano Plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Make volcano plots of the prediction performance and the model significance 
volcano_plot_fit_significance(list_norm_pi_unweighted_results
                              , "unweighted"
                              , working_directory)

volcano_plot_fit_significance(list_norm_pi_weighted_results
                              , "weighted"
                              , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Stairway Plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Make stairway plots showing the mortality risk progression across age
stairway_plots_age_mortality(list_demo_results
                             , nhanes_merged_dataset
                             , working_directory)

# Make a stairway plot for each combination of a physiological indicator and a sensitivity analysis
stairway_plots_individual(list_norm_pi_unweighted_results
                          , final_pi_thresholds_df
                          , nhanes_long_dataset
                          , selected_pi
                          , "unweighted"
                          , working_directory
                          , "Stairway Plots Normalized - Individual Physiological Indicators"
                          , body_systems_df
                          , dataset_ref_unweighted)

setwd("/Volumes/Data/Mortality Project/ASSOCIATIONS/Phase 6")
stairway_plots_individual(list_norm_pi_weighted_results
                          , final_pi_thresholds_df
                          , nhanes_long_dataset
                          , selected_pi
                          , "weighted"
                          , working_directory
                          , "Stairway Plots Normalized - Individual Physiological Indicators"
                          , body_systems_df
                          , dataset_ref_weighted)

# Define a vector of codenames of physiological indicators to include in the main stairway plot
selected_pi <- c("BMXBMI", "VNTOTHDRATIO", "VNAVEBPXSY", "LBXCRP", "VNHOMAIR", "VNEGFR")

# Make panel of stairway plots for selected physiological indicators 
stairway_plots_selected(list_norm_pi_unweighted_results
                        , final_pi_thresholds_df
                        , nhanes_long_dataset
                        , selected_pi
                        , "unweighted"
                        , working_directory
                        , "Stairway Plots Normalized - Selected Physiological Indicators"
                        , body_systems_df
                        , dataset_ref_unweighted)

stairway_plots_selected(list_norm_pi_weighted_results
                        , final_pi_thresholds_df
                        , nhanes_long_dataset
                        , selected_pi
                        , "weighted"
                        , working_directory
                        , "Stairway Plots Normalized - Selected Physiological Indicators"
                        , body_systems_df
                        , dataset_ref_weighted)

# Define a vector of strings indicatoring the types of plots 
figures_types <- c("png", "pdf")

# Define a for loop to make a png and pdf version of the panel of stairway plot for each body system
# for the remaining physiological indicators. stairway_plots_remaining uses pdf() and png() to draw boxes
# to differentiate the sensitivity analysis and needed to be separately to produce both types of plot. 
for(f in seq(length(figures_types)))
{
  figure_type_f <- figures_types[f]
  
  stairway_plots_remaining(list_norm_pi_unweighted_results
                           , final_pi_thresholds_df
                           , nhanes_long_dataset
                           , selected_pi
                           , "unweighted"
                           , working_directory
                           , "Stairway Plots Normalized - Remaining Physiological Indicators"
                           , body_systems_df
                           , figure_type_f
                           , dataset_ref_unweighted)
  
  stairway_plots_remaining(list_norm_pi_weighted_results
                           , final_pi_thresholds_df
                           , nhanes_long_dataset
                           , selected_pi
                           , "weighted"
                           , working_directory
                           , "Stairway Plots Normalized - Remaining Physiological Indicators"
                           , body_systems_df
                           , figure_type_f
                           , dataset_ref_weighted)
}

stairway_plots_with_without_adjustment(list_pi_weighted_results
                                       , list_pi_weighted_results_smoking
                                       , final_pi_thresholds_df
                                       , nhanes_long_dataset
                                       , nhanes_long_dataset_smoking
                                       , "weighted"
                                       , "BMI & Mortality Adjusted for Smoking"
                                       , working_directory
                                       , body_systems_df)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Splines Plots by Sex  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("/Volumes/Data/Mortality Project/ASSOCIATIONS/Phase 6")
splines_plots_sex_individual(list_norm_sex_stratified_splines_results
                             , nhanes_long_dataset
                             , nhanes_merged_dataset
                             , final_pi_thresholds_df
                             , working_directory
                             , "Spline Plots Normalized - Individual Physiological Indicators"
                             , list_sex_dataset_ref)

splines_plots_sex_selected(list_norm_sex_stratified_splines_results
                           , nhanes_long_dataset
                           , nhanes_merged_dataset
                           , final_pi_thresholds_df
                           , working_directory
                           , "Spline Plots Normalized - Selected Physiological Indicators with Sex-Specific Thresholds"
                           , list_sex_dataset_ref)

splines_plots_sex_remaining(list_norm_sex_stratified_splines_results
                            , nhanes_long_dataset
                            , nhanes_merged_dataset
                            , final_pi_thresholds_df
                            , working_directory
                            , "Spline Plots Normalized - Remaining Physiological Indicators"
                            , body_systems_df
                            , list_sex_dataset_ref)
