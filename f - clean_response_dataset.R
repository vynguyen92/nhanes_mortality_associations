#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################################  FUNCTION TO CLEAN THE RESPONSE DATASET  ####################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function cleans the response dataset by extracting the physiological indicators 
#
# Inputs: unclean_response_dataset - the dataset that contains excess variables. This should be the result 
#                                    of the compile function that extracted the individual response datasets
#                                    and merged all individual response datasets across the cycles
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project are housed.
#
# Outputs: cleaned_response_dataset - dataframe with the physiological indicators. 

clean_response_dataset <- function(unclean_response_dataset
                                   , current_directory)
{
  # Define a vector of indices indicating the cycle
  index_cycle_1 <- which(unclean_response_dataset$study_year == 1)
  index_cycle_2 <- which(unclean_response_dataset$study_year == 2)
  index_cycle_3 <- which(unclean_response_dataset$study_year == 3)
  index_cycle_4 <- which(unclean_response_dataset$study_year == 4)
  index_cycle_5 <- which(unclean_response_dataset$study_year == 5)
  index_cycle_6 <- which(unclean_response_dataset$study_year == 6)
  index_cycle_7 <- which(unclean_response_dataset$study_year == 7)
  index_cycle_8 <- which(unclean_response_dataset$study_year == 8)
  
  # Harmonize variables with multiple codenames
  unclean_response_dataset[index_cycle_1, "LBDHDD"] <- unclean_response_dataset[index_cycle_1, "LBDHDL"]
  unclean_response_dataset[index_cycle_2, "LBDHDD"] <- unclean_response_dataset[index_cycle_2, "LBDHDL"]
  unclean_response_dataset[index_cycle_3, "LBDHDD"] <- unclean_response_dataset[index_cycle_3, "LBXHDD"]
  unclean_response_dataset[index_cycle_2, "LBXHCY"] <- unclean_response_dataset[index_cycle_2, "LBDHCY"]
  
  # Create a new variable name for hematocrit because it has the same codename (LBXHCT) as Hydroxycotinine, 
  # Serum 
  unclean_response_dataset <- unclean_response_dataset %>%
    mutate(VNHEMACR = LBXHCT)
  
  # Based on the results of the blood pressure correlation plots and the high correlations
  # between each measurements of BP, the average of the BP measurements will be taken. 
  # create_bp_correlation_plots(unclean_response_dataset
  #                             , current_directory)
  
  # Determine the index of Systolic and Diastolic BP
  index_bp_systolic <- grep("^BPXSY"
                            , colnames(unclean_response_dataset))
  index_bp_diastolic <- grep("^BPXDI"
                             , colnames(unclean_response_dataset))
  
  # Define a dataframe with the indices of the Systolic and Diastolic BP
  df_bp_indices <- data.frame(index_bp_diastolic
                              , index_bp_systolic)
  
  # Define a vector with the prefix of the codenames for Systolic and Diastolic BP
  bp_types_name <- c("BPXDI"
                     , "BPXSY")
  
  # Define a vector with the names of Systolic and Diastolic BP
  bp_type_attr_labels <- c("Diastolic"
                           , "Systolic")
  
  # Determine the number of BP types
  num_types_of_bp <- length(bp_types_name)
  
  # Determine the number of participants
  num_of_participants <- nrow(unclean_response_dataset)
  
  # Perform for each BP type
  for(k in seq(num_types_of_bp))
  {
    # Determine column indices for a BP type. Notes that there can be up to 4 measurements of BP for a given 
    # participant
    indices_bp_type_k <- df_bp_indices[,k]
    
    # Extract the column vectors of BP measurements
    dataset_bp_k <- unclean_response_dataset[,indices_bp_type_k]
    
    # Measurements labeled as 0 as redefined as NA
    dataset_bp_k[dataset_bp_k == 0] <- NA
    # View(dataset_bp_k)
    
    # Determine the averge of BP for each participant
    average_bp_k <- apply(dataset_bp_k
                          , 1
                          , mean
                          , na.rm = TRUE)
    
    # Define a new codename with my initials and indicate that it's an average
    codename_bp_k <- paste("VNAVE"
                           , bp_types_name[k]
                           , sep = "")
    
    # Include this column of the average of BP measurements into the dataset of physiological indicators
    unclean_response_dataset <- data.frame(unclean_response_dataset
                                           , average_bp_k)
    
    # Name this column vector with the new codename
    index_average_bp_k <- which(colnames(unclean_response_dataset) == "average_bp_k")
    colnames(unclean_response_dataset)[index_average_bp_k] <- codename_bp_k
    
    # Redefine the attribute name of the variable
    bp_type_attr_labels_k <- bp_type_attr_labels[k]
    attr(unclean_response_dataset[,index_average_bp_k], "label") <- paste(bp_type_attr_labels_k
                                                                          , ": "
                                                                          , "Average blood pressure (mm Hg)"
                                                                          , sep = "")
    class(unclean_response_dataset[,index_average_bp_k]) <- c("labelled", "numeric")
  }
  
  # Define the variables that are ratios of two physiological indicators
  unclean_response_dataset[,"VNINGLURATIO"] <- unclean_response_dataset$LBXIN/
    unclean_response_dataset$LBXGLU
  unclean_response_dataset[,"VNTOTHDRATIO"] <- unclean_response_dataset$LBXTC/
    (unclean_response_dataset$LBDHDD)
  unclean_response_dataset[,"VNLDHDLRATIO"] <- unclean_response_dataset$LBDLDL/
    (unclean_response_dataset$LBDHDD)
  
  # Define indices of males and females to define the Relative Fat Mass Index
  index_males <- which(unclean_response_dataset$RIAGENDR == 1)
  index_females <- which(unclean_response_dataset$RIAGENDR == 2)
  
  # Define the Relative Fat Mass Index for each participant
  unclean_response_dataset[index_males,"VNRFPI"] <- 64 - (20*unclean_response_dataset[index_males,"BMXHT"]/
                                                            unclean_response_dataset[index_males,"BMXWAIST"])
  unclean_response_dataset[index_females,"VNRFPI"] <- 76 - (20*unclean_response_dataset[index_females,"BMXHT"]/
                                                              unclean_response_dataset[index_females,"BMXWAIST"])
  
  
  # Formula for HOMA-IR is from https://www.jcdr.net/article_fulltext.asp?id=6742
  mg_dl_to_mmol_L <- 0.05551 # https://www.labcorp.com/resource/si-unit-conversion-table
  constant_homa_ir <- 22.5
  converted_insulin <- unclean_response_dataset$LBXIN
  fasting_glucose <- unclean_response_dataset$LBXGLU*mg_dl_to_mmol_L
  unclean_response_dataset[,"VNHOMAIR"] <- converted_insulin*fasting_glucose/constant_homa_ir
  # View(unique(unclean_response_dataset[,c("VNHOMAIR", "LBXIN", "LBXGLU")]))
  
  # Define a variable for Glomerular Filtration Rate
  unclean_response_dataset <- calculate_estimated_gfr(unclean_response_dataset)
  
  # Ensure that the attribute name of the following variables
  attr(unclean_response_dataset$LBXCRP, "label") <- "C-reactive protein (mg/dL)"
  attr(unclean_response_dataset$BPXPLS, "label") <- "60 sec. pulse (30 sec. pulse * 2)"
  attr(unclean_response_dataset$CVDR3TIM, "label") <- "Amount of time spent in Recovery 3 (minutes)"
  attr(unclean_response_dataset$LBXGH, "label") <- "Glycohemoglobin (%)"
  attr(unclean_response_dataset$LBDSTPSI, "label") <- "Total protein (g/L)"
  attr(unclean_response_dataset$LBXPLTSI, "label") <- "Platelet count SI (1000 cells/uL)"
  attr(unclean_response_dataset$LBXPT21, "label") <- "Parathyroid Hormone Elecys method (pg/mL)"
  attr(unclean_response_dataset$SPXBQFV1, "label") <- "2nd Test FEV1 Quality Attribute"
  attr(unclean_response_dataset$SPXBQEFF, "label") <- "2nd Test Effort Quality Attribute"
  attr(unclean_response_dataset$LBXSAPSI, "label") <- "Alkaline phosphatase (U/L)" 
  attr(unclean_response_dataset$LBXWBCSI, "label") <- "White blood cell count (1000 cells/uL)" 
  attr(unclean_response_dataset$VNINGLURATIO, "label") <- "Ratio of Insulin to Glucose (uU*dL)/(mg*mL)" 
  attr(unclean_response_dataset$VNTOTHDRATIO, "label") <- "Ratio of Total to HDL Cholesterol (-)"
  attr(unclean_response_dataset$VNLDHDLRATIO, "label") <- "Ratio of LDL to HDL Cholesterol (-)"
  attr(unclean_response_dataset$VNRFPI, "label") <- "Relative Fat Mass Index (-)"
  attr(unclean_response_dataset$VNHOMAIR, "label") <- "Homeostatic Model Assessment of Insulin Resistance (-)"
  attr(unclean_response_dataset$VNEGFR, "label") <- "Estimated Glomerular Filtration Rate (mL/min/1.73 m2)"
  
  # Ensure that the dat type for these variables are numeric
  class(unclean_response_dataset$VNRFPI) <- c("labelled", "numeric")
  class(unclean_response_dataset$VNEGFR) <- c("labelled", "numeric")
  class(unclean_response_dataset$LBXPT21) <- c("labelled", "numeric")
  class(unclean_response_dataset$VNRFPI) <- c("labelled", "numeric")
  
  # Define a vector of codenames to keep for further analyses
  codenames_keep <- c("SEQN"
                      , "BMXBMI"
                      , "BMXWAIST"
                      , "BMXWT"
                      , "BMXHT"
                      , "VNRFPI"
                      , "BMXSUB"
                      , "BMXTRI"
                      , "BIDPFAT"
                      , "CVDESVO2"
                      , "BPXPLS"
                      , "VNAVEBPXSY"
                      , "VNAVEBPXDI"
                      , "LBXCRP"
                      , "LBXTC"
                      # , "LBDHDL"
                      , "LBDHDD"
                      , "LBDLDL"
                      , "VNTOTHDRATIO"
                      , "VNLDHDLRATIO"
                      # , "LBXTR"
                      , "LBXSAL"
                      , "LBXSTR"
                      , "LBXGH"
                      , "LBXSCR"
                      , "VNEGFR"
                      , "LBXHCY"
                      , "LBXGLU"
                      , "LBXGLT"
                      , "VNHOMAIR"
                      , "VNINGLURATIO"
                      , "LBXSAPSI"
                      , "LBXFB"
                      , "LBXSBU"
                      , "LBXHE1"
                      , "LBXHE2"
                      , "LBXWBCSI"
                      , "LBXAPB"
                      , "LBXPT21"
                      , "SSEBV"
                      , "SSEBVIND"
                      , "LBXCD4"
                      , "LBXCD8"
                      , "SSCMVOD"
                      , "SSCMV"
                      , "SSCMIGM"
                      , "SSCMIGGA"
                      , "SPXBF257"
                      , "SPXNF257"
                      , "SPXBFET"
                      , "SPXNFET"
                      , "SPXBFEV1"
                      , "SPXNFEV1"
                      , "SPXBFEV3"
                      , "SPXNFEV3"
                      , "SPXBFEV5"
                      , "SPXNFEV5"
                      , "SPXBFEV6"
                      , "SPXNFEV6"
                      , "SPXBFEV7"
                      , "SPXNFEV7"
                      , "SPXBPEF"
                      , "SPXNPEF"
                      , "LBXRBCSI"
                      , "LBXLYPCT" 
                      , "LBXMOPCT"
                      , "LBXNEPCT"
                      , "LBXEOPCT"
                      , "LBXBAPCT"
                      , "LBDLYMNO" 
                      , "LBDMONO" 
                      , "LBDNENO" 
                      , "LBDEONO" 
                      , "LBDBANO" 
                      , "LBXHGB" 
                      , "VNHEMACR"
                      , "LBXMCVSI" 
                      , "LBXMCHSI" 
                      , "LBXMC" 
                      , "LBXRDW" 
                      , "LBXPLTSI" 
                      , "LBXMPSI"
                      # , "study_year"
  )
  
  # Define the cleaned dataset of physiological indicators 
  cleaned_response_dataset <- unclean_response_dataset[,codenames_keep]
  
  return(cleaned_response_dataset)
}