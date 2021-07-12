#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############################  FUNCTION TO CLEAN THE NHANES QUESTIONNAIRE DATASET ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function cleans the questionnaire dataset to contain data on comorbidity and number of comorbidities
#          for each participant
#
# Inputs: dataset_unclean - dataframe of unclean questionnaire data 
#
# Outputs: dataset_clean - dataframe of the cleaned questionnaire dataset containing only the selected comorbidities

clean_questionnaire_dataset <- function(dataset_unclean)
{
  # Select the columns pertaining to the pertinent conditions
  dataset_clean <- dataset_unclean %>%
    select(SEQN
           , DIQ010
           , MCQ010
           , MCQ160A
           , MCQ160B
           , MCQ160E
           , MCQ160F
           , MCQ220
           , MCQ160L
           , MCQ160G
           , MCQ160K
           , study_year) 
  
  # 3 indicates that the condition is present in the participant
  dataset_clean[dataset_clean == 3] <- 1
  
  # 2 indicates that the condition is not present in the participant
  dataset_clean[dataset_clean == 2] <- 0
  
  # 7 and 9 indicates that the participant refused to disclose the presence or absence of the condition and 
  # do not of having such a condition, respectively
  dataset_clean[dataset_clean == 7] <- NA
  dataset_clean[dataset_clean == 9] <- NA
  
  # Calculate the number of conditions present for each participant
  dataset_clean <- dataset_clean %>%
    mutate(VNMORBIDITY = select(.
                                , DIQ010
                                , MCQ010
                                , MCQ160A
                                , MCQ160B
                                , MCQ160E
                                , MCQ160F
                                , MCQ220
                                , MCQ160L
                                , MCQ160G
                                , MCQ160K) %>%
             rowSums(na.rm = TRUE))
  
  
  return(dataset_clean)
}