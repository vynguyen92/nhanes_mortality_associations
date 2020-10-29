#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################  FUNCTION TO MERGE NHANES DATASET TOGETHER  ##################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function merge all NHANES dataset together and include only variables that are need
#          
# Inputs: demographics_dataset - data frame containing the demographic features for each participant 
#         mortality_dataset <- data frame containing information on deceased status and time until death
#         response_dataset <- data frame containing measurements for 10 allostatic load components
#         chemicals_dataset <- data frame containing measurements for >400 chemical biomarkers
#         weights_dataset <- data frame containing the values on survey weights
#
# Outputs: dataset_merged_updated - clean merged dataset of demographics, mortality, response, and weights 
#                                   datasets

merge_datasets_together <- function(demographics_dataset
                                    , mortality_dataset
                                    , response_dataset
                                    , chemicals_dataset
                                    , weights_dataset)
{
  # Determine the number of arguments
  num_of_arguments <- nargs()
  
  # Assemble a list containing the datasets
  list_of_datasets <- list(response_dataset = response_dataset
                           , mortality_dataset = mortality_dataset
                           , demographics_dataset = demographics_dataset
                           , chemicals_dataset = chemicals_dataset
                           , weights_dataset = weights_dataset)
  
  # Define a function to join the dataset by the participant identifiers
  joining_by_seqn <- function(x, y) full_join(x, y, by = "SEQN")
  
  # Merge all the datasets together 
  dataset_merged <- list_of_datasets %>%
    reduce(joining_by_seqn)
  
  # Copy the attributes and labels into the merged dataset
  for(i in seq(num_of_arguments))
  {
    dataset_merged <- copy_labels(from = list_of_datasets[[i]], to =  dataset_merged)
  }
  
  # Define a vector of the variable codename to include
  colnames_include <- c("SEQN"
                        , "MORTSTAT"
                        , "PERMTH_INT"
                        , "PERMTH_EXM"
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
                        , "LBDHDD"
                        , "LBDLDL"
                        , "VNTOTHDRATIO"
                        , "VNLDHDLRATIO"
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
                        # , "RIAGENDR"
                        # , "RIDAGEYR"
                        # , "RIDRETH1"
                        # , "INDFMPIR"
                        # , "SDDSRVYR"
                        , colnames(demographics_dataset)[-1]
                        , "WTMEC2YR"
                        , "WTINT2YR"
                        # , "SDMVPSU"
                        # , "SDMVSTRA"
                        , "LBXCOT")

  # Define a vector of codenames that are not duplicated
  colnames_include_updated <- colnames_include[grepl("\\.1$", colnames_include) == FALSE]

  # Define a dataframe of pertinent variables for further analyses
  dataset_merged_updated <- dataset_merged[,colnames_include_updated]

  return(dataset_merged_updated)
}