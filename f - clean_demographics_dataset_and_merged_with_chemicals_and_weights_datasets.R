#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#########  FUNCTION TO CLEAN MERGED DEMOGRAPHICS DATASET & MERGE WITH CHEMICALS AND WEIGHTS DATASETS ##########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function cleans the merged demographic dataset to eliminate excessive variables and harmonize  
#          variables that refer to the same demographic factor but have different NHANES codenames. Then extract 
#          the weights variables pertaining to the Interview and MEC examination to merge with the weights   
#          dataset. And finally, form the merged chemical and demographics dataset. 
#
# Inputs: demographics_unclean_dataset - merged demograhpics dataset
#         chemicals_clean_dataset - clean chemical dataset
#         weights_unfinished_dataset - unfinished weights dataset that requires the weights from the Interview 
#                                      and MEC examination. 
#
# Outputs: new_datasets_list - list containing the clean demographics dataset, the merged chemicals and 
#          demographics dataset, and the finished weights dataset

clean_demographics_dataset_and_merge_with_chemicals_and_weights_datasets <- function(demographics_unclean_dataset
                                                                                     , chemicals_clean_dataset
                                                                                     , weights_unfinished_dataset)
{
  # Determine the codenames of the demographic factors in the unclean demographic dataset
  demo_codenames <- colnames(demographics_unclean_dataset)
  # Determine the indices pertaining to the MEC Exam Weight Jack Knife Replicate and Interview Weight Jack 
  # Knife Replicate 
  jack_knife_index <- c(grep("^WTMREP", demo_codenames)
                        , grep("^WTIREP", demo_codenames))
  
  # Determine the indices pertaining to the study year for 1, 2, 3, and 4 to use for harmonization
  study_year_1 <- which(demographics_unclean_dataset$SDDSRVYR == 1) 
  study_year_2 <- which(demographics_unclean_dataset$SDDSRVYR == 2)
  study_year_3 <- which(demographics_unclean_dataset$SDDSRVYR == 3)
  study_year_4 <- which(demographics_unclean_dataset$SDDSRVYR == 4)
  
  # INDHHIN2 and INDHHINC represent the annual household income, so harmonize 
  demographics_unclean_dataset[study_year_1, "INDHHIN2"] <- demographics_unclean_dataset[study_year_1
                                                                                         , "INDHHINC"]
  demographics_unclean_dataset[study_year_2, "INDHHIN2"] <- demographics_unclean_dataset[study_year_2
                                                                                         , "INDHHINC"]
  demographics_unclean_dataset[study_year_3, "INDHHIN2"] <- demographics_unclean_dataset[study_year_3
                                                                                         , "INDHHINC"]
  demographics_unclean_dataset[study_year_4, "INDHHIN2"] <- demographics_unclean_dataset[study_year_4
                                                                                         , "INDHHINC"]
  
  # INDFMIN2 and INDFMINC represent the annual family income, so harmonize
  demographics_unclean_dataset[study_year_1, "INDFMIN2"] <- demographics_unclean_dataset[study_year_1
                                                                                         , "INDFMINC"]
  demographics_unclean_dataset[study_year_2, "INDFMIN2"] <- demographics_unclean_dataset[study_year_2
                                                                                         , "INDFMINC"]
  demographics_unclean_dataset[study_year_3, "INDFMIN2"] <- demographics_unclean_dataset[study_year_3
                                                                                         , "INDFMINC"]
  demographics_unclean_dataset[study_year_4, "INDFMIN2"] <- demographics_unclean_dataset[study_year_4
                                                                                         , "INDFMINC"]
  
  # Determine the indicies pertaining to the weights for the Interview and the MEC Examination
  weights_index <- grep("^WT(INT|MEC)", demo_codenames)
  # Form a separate dataframe containing the SEQN and the weights for the Interview and the MEC Examination
  demo_weights_dataset <- demographics_unclean_dataset[,c(1,weights_index)]
  
  # Determine the indices of excessive variables so that they can be eliminated
  demo_excess <- c(jack_knife_index
                   , weights_index
                   , which(demo_codenames == "INDHHINC")
                   , which(demo_codenames == "INDFMINC"))
  
  # Remove the excessive variables to form the clean demographics dataset
  demographics_clean_dataset <- demographics_unclean_dataset[,-demo_excess]
  
  # Merge the weights for the Interview and the MEC Examination with the weights for the subsamples
  weights_finished_dataset <- merge(weights_unfinished_dataset
                                    , demo_weights_dataset
                                    , by = "SEQN"
                                    , all.x = TRUE)
  
  # Merged the chemicals and demographics datasets together
  chem_demo_dataset <- left_join(chemicals_clean_dataset
                                 , demographics_clean_dataset
                                 , by = "SEQN")
  
  # Initialize the list for hold the different datasets
  new_datasets_list <- list()
  
  # Store the finished weights dataset into the list
  new_datasets_list[["weights_finished_dataset"]] <- weights_finished_dataset
  # Store the clean demographics dataset into the list
  new_datasets_list[["demographics_dataset"]] <- demographics_clean_dataset
  # Store the merged chemicals and demographics dataset into the list
  new_datasets_list[["chemicals_demographics_dataset"]] <- chem_demo_dataset
  
  # Return the list that contains the finished weights dataset, the clean demographics dataset, and the 
  # merged chemicals and demographics dataset
  return(new_datasets_list)
}