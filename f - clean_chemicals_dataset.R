#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###  FUNCTION TO CLEAN THE CHEMICAL DATASET AND FORMING THE CLEAN CHEMICALS, WEIGHTS, AND COMMENTS DATASETS ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function cleans the chemicals dataset by eliminating biomarkers that are not indicative 
#          of chemical exposures. It will also form the weights and comments dataset
#
# Inputs: unclean_chemical_dataset - dataframe that contains excess variables. This should be the result 
#                                    of the compile function that extracted the individual chemical datasets
#                                    and merged all individual chemical datasets across the cycles
#
# Outputs: nhanes_list - list with dataframe each for the chemicals, weights, and comments dataset

clean_chemicals_dataset <- function(unclean_chemical_dataset)
{
  # Initialize this list so that we can store the clean chemical dataset, the weights dataset, and the 
  # comments dataset 
  nhanes_list <- list()
  
  chem_codenames <- colnames(unclean_chemical_dataset)
  
  # Determine the index of participants with urine cadmium levels recorded as 0 and who are below the lower 
  # level of detection (LOD)
  index_cd_0_below_LOD <- which(unclean_chemical_dataset$URDUCD == 0 
                                & unclean_chemical_dataset$URDUCDLC == 1)
  # Replace the participants with urine cadmium levels recorded as 0 and who are below the LOD to the 
  # LOD/sqrt(2). 0.06 was determined to be the LOD for Cycle 1 and Cycle 2 for Urinary Cadmium 
  # according to the NHANES 4th Report
  lod_cadimum_cycle1_cycle2 <- 0.06
  unclean_chemical_dataset[index_cd_0_below_LOD, "URDUCD"] <- lod_cadimum_cycle1_cycle2/sqrt(2)
  
  # Determine the indices of variables starting with the pattern WTSPO and ends with 2 digits
  dioxin_jack_knife_index <- grep("^WTSPO\\d{2}$"
                                  , chem_codenames)
  
  # Determine the indices of variables starting with the pattern WTSPP and ends with 2 digits
  pesticides_jack_knife_index <- grep("^WTSPP\\d{2}$"
                                      , chem_codenames)
  
  # Determine the indices of variables starting with the pattern WTSPH and ends with 2 digits
  phthalate_jack_knife_index <- grep("^WTSPH\\d{2}$"
                                     , chem_codenames)
  # Determine the indices of variables starting with the pattern WTSHM and ends with 2 digits
  heavy_metals_jack_knife_index <- grep("^WTSHM\\d{2}$"
                                        , chem_codenames)
  
  # Determine the indices of variables that pertains to lead dust
  dust_lead <- grep("^DC(D|Q)"
                    , chem_codenames)
  
  # Determine the index that pertains to methymalonic 
  methylmalonic_index <- which(chem_codenames == "LBXMMA")
  
  # Determine the indices of variables that pertains to voc questionaires
  voc_questionaires_index <- grep("^VTQ"
                                  , chem_codenames)
  
  # Determine the indices of variables that pertains to VOCS measured in water, since we are 
  # interested in VOCS that are measured in blood or urine
  voc_water_index <- grep("^LBXW"
                          , chem_codenames)
  
  # Determine the indices of variables that have SI units, since these same biomarkers have
  # non-SI units 
  units_SI_index <- grep("SI$"
                         , chem_codenames)
  
  # Determine the index for Protoporphyrin, since this is a health biomarker that is used to 
  # diagnose blood problems caused by lead 
  prot_index <- which(chem_codenames == "LBXEPP")
  
  # Determine the indices of variables pertains to transferrin, which is not indicative of 
  # chemical exposure 
  transferrin_index <- grep("PCT"
                            , chem_codenames)
  
  # Determine the indices of variables pertains to Ferritin, which is not indicative of 
  # chemical exposure
  ferri_index <- grep("FER$"
                      , chem_codenames)
  
  # Determine the indices of variables pertains to urinary Stronium, Tin, and Manganese
  str_tin_index <- c(which(chem_codenames == "URXUSN")
                     , which(chem_codenames == "URXUSR")
                     , which(chem_codenames == "URXUMN"))
  
  # Determine the indices of variables pertains to Vitamin B12, which is not indicative of 
  # chemical exposure
  vb12_index <- which(chem_codenames == "LBXB12")
  
  # Determine the indices of variables pertains to Homocysteine, which is an endogenous compound
  homocy_index <- grep("HCY$"
                       , chem_codenames)
  
  # Determine the indices of variables pertains to folate, which is indicative of dietary 
  # supplements and thus are outside of the scope of identifying chemical disparities and patterns
  folate_index <- c(which(chem_codenames == "LBXRBF")
                    , which(chem_codenames == "LBXFOL")
  )
  
  # Determine the indices of variables pertains to iron, which can be indicative of dietary 
  # supplements and thus are outside of the scope of identifying chemical disparities and patterns
  iron_index <- c(which(chem_codenames == "LBXIRN"))
  
  # Determine the indices of variables pertains to Vitamin A and E, which can be indicative of 
  # dietary supplements and thus are outside of the scope of identifying chemical 
  # disparities and patterns
  vitamin_index <- c(which(chem_codenames == "LBXVIA")
                     , which(chem_codenames == "LBXVIE")
  )
  
  # Determine the indices of variables pertains to selenium, which can be indicative of dietary 
  # supplements and thus are outside of the scope of identifying chemical disparities and patterns
  sele_index <- c(which(chem_codenames == "LBXSEL")
                  , which(chem_codenames == "LBXBSE")
                  , which(chem_codenames == "LBXSSE")
  )
  
  # Determine the indices of variables pertains to gamma tocopherol, which can be indicative of dietary 
  # supplements and thus are outside of the scope of identifying chemical disparities and patterns
  toco_index <- which(chem_codenames == "LBXGTC")
  
  # Determine the indices of variables pertains to vitamin A, which can be indicative of dietary 
  # supplements and thus are outside of the scope of identifying chemical disparities and patterns
  retinpa_index <- which(chem_codenames=="LBXRPL")
  retinst_index <- which(chem_codenames=="LBXRST") 
  
  # Determine the indices of variables pertains to latex, which can be indicative of dietary 
  # supplements and thus are outside of the scope of identifying chemical disparities and patterns
  latex_index <- grep("^LBXLA"
                      , chem_codenames) 
  
  # Determine the indicies of variables pertaining to excess albumin and creatinine measurements
  alcr_2_index <- c(which(chem_codenames=="URDACT")
                    , which(chem_codenames=="URXUMA2")
                    , which(chem_codenames=="URDUMA2S")
                    , which(chem_codenames=="URXUCR2")
                    , which(chem_codenames=="URDUCR2S")
                    , which(chem_codenames=="URDACT2")
  )
  
  # Determine the indices of variables pertains to total iron binding capacity, which is not indicative 
  # of chemical exposure
  tibc_index <- which(chem_codenames=="LBXTIB")
  
  # Determine the indices of variables pertaining to flooring and windows
  floor_windows_index <- c(which(chem_codenames=="LBXDFS")
                           , which(chem_codenames=="LBXDFSF")
                           , which(chem_codenames=="LBDDWS"))
  
  # Determine the index of variables pertaining toe extra creatinine and albumin measurements, since 
  # these measurements differ by a unit conversion
  extra_creatinine <- which(chem_codenames == "URXUMS")
  extra_albumin <- which(chem_codenames == "URXCRS")
  
  
  # These PFCs were measured in children in the 8th cycle but have different chemical codenames to the
  # PFCs measured in adults. 
  pfsa_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFSA"]))
  mpah_children_index <- which(!is.na(unclean_chemical_dataset[,"SSMPAH"]))
  epah_children_index <- which(!is.na(unclean_chemical_dataset[,"SSEPAH"]))
  pfde_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFDE"]))
  pfbs_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFBS"]))
  pfhp_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFHP"]))
  pfna_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFNA"]))
  pfua_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFUA"]))
  pfdo_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFDO"]))
  pfhs_children_index <- which(!is.na(unclean_chemical_dataset[,"SSPFHS"]))
  
  # Harmonize the PFC codenames measured in children with the PFC codenames that were used for adults
  unclean_chemical_dataset[pfsa_children_index,"LBXPFSA"] <- unclean_chemical_dataset[pfsa_children_index,"SSPFSA"]
  unclean_chemical_dataset[mpah_children_index,"LBXMPAH"] <- unclean_chemical_dataset[mpah_children_index,"SSMPAH"]
  unclean_chemical_dataset[epah_children_index,"LBXEPAH"] <- unclean_chemical_dataset[epah_children_index,"SSEPAH"]
  unclean_chemical_dataset[pfde_children_index,"LBXPFDE"] <- unclean_chemical_dataset[pfde_children_index,"SSPFDE"]
  unclean_chemical_dataset[pfbs_children_index,"LBXPFBS"] <- unclean_chemical_dataset[pfbs_children_index,"SSPFBS"]
  unclean_chemical_dataset[pfhp_children_index,"LBXPFHP"] <- unclean_chemical_dataset[pfhp_children_index,"SSPFHP"]
  unclean_chemical_dataset[pfna_children_index,"LBXPFNA"] <- unclean_chemical_dataset[pfna_children_index,"SSPFNA"]
  unclean_chemical_dataset[pfua_children_index,"LBXPFUA"] <- unclean_chemical_dataset[pfua_children_index,"SSPFUA"]
  unclean_chemical_dataset[pfdo_children_index,"LBXPFDO"] <- unclean_chemical_dataset[pfdo_children_index,"SSPFDO"]
  unclean_chemical_dataset[pfhs_children_index,"LBXPFHS"] <- unclean_chemical_dataset[pfhs_children_index,"SSPFHS"]
  
  # Determine the indices of PFCs that were measured in children 
  pfcs_extra_index <- c(which(chem_codenames == "SSPFSA")
                        , which(chem_codenames == "SSMPAH")
                        , which(chem_codenames == "SSEPAH")
                        , which(chem_codenames == "SSPFDE")
                        , which(chem_codenames == "SSPFBS")
                        , which(chem_codenames == "SSPFHP")
                        , which(chem_codenames == "SSPFNA")
                        , which(chem_codenames == "SSPFUA")
                        , which(chem_codenames == "SSPFDO")
                        , which(chem_codenames == "SSPFHS")
  )
  
  # Determine indices for participants in cycle 1, 2, and 3
  index_cycle_1 <- which(unclean_chemical_dataset$study_year == 1)
  index_cycle_2 <- which(unclean_chemical_dataset$study_year == 2)
  index_cycle_3 <- which(unclean_chemical_dataset$study_year == 3)
  index_cycle_8 <- which(unclean_chemical_dataset$study_year == 8)
  
  
  # Certain chemical biomarkers have different units by cycles and needs to be harmonized to have 
  # the same units across the cycles 
  # Refer to NHANES - Determine Changes in Units Over the Cycles 1a.xlsx for documentation
  
  ng_to_pg <- 1000
  
  unclean_chemical_dataset[index_cycle_8, "LBXVBF"] <- ng_to_pg*unclean_chemical_dataset[index_cycle_8, "LBXVBF"]
  unclean_chemical_dataset[index_cycle_8, "LBXVBM"] <- ng_to_pg*unclean_chemical_dataset[index_cycle_8, "LBXVBM"]
  unclean_chemical_dataset[index_cycle_8, "LBXVCF"] <- ng_to_pg*unclean_chemical_dataset[index_cycle_8, "LBXVCF"]
  unclean_chemical_dataset[index_cycle_8, "LBXVCM"] <- ng_to_pg*unclean_chemical_dataset[index_cycle_8, "LBXVCM"]
  unclean_chemical_dataset[index_cycle_8, "LBXVME"] <- ng_to_pg*unclean_chemical_dataset[index_cycle_8, "LBXVME"]
  
  # Certain chemical biomarkers have different codenames but are actually the same chemical, so these
  # biomarkers were harmonized 
  unclean_chemical_dataset[index_cycle_1, "LBXBHC"] <- unclean_chemical_dataset[index_cycle_1, "LBDBHC"]
  unclean_chemical_dataset[index_cycle_1, "LBXBHCLA"] <- unclean_chemical_dataset[index_cycle_1, "LBDBHCLA"]
  unclean_chemical_dataset[index_cycle_1, "LBXEPAH"] <- unclean_chemical_dataset[index_cycle_1, "SEPAH"]
  unclean_chemical_dataset[index_cycle_1, "LBXMPAH"] <- unclean_chemical_dataset[index_cycle_1, "SMPAH"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFDE"] <- unclean_chemical_dataset[index_cycle_1, "SPFDE"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFDO"] <- unclean_chemical_dataset[index_cycle_1, "SPFDO"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFHP"] <- unclean_chemical_dataset[index_cycle_1, "SPFHP"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFHS"] <- unclean_chemical_dataset[index_cycle_1, "SPFHS"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFNA"] <- unclean_chemical_dataset[index_cycle_1, "SPFNA"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFOA"] <- unclean_chemical_dataset[index_cycle_1, "SPFOA"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFOS"] <- unclean_chemical_dataset[index_cycle_1, "SPFOS"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFSA"] <- unclean_chemical_dataset[index_cycle_1, "SPFSA"]
  unclean_chemical_dataset[index_cycle_1, "LBXPFUA"] <- unclean_chemical_dataset[index_cycle_1, "SPFUA"]
  unclean_chemical_dataset[index_cycle_1, "URXUCD"] <- unclean_chemical_dataset[index_cycle_1, "URDUCD"]
  unclean_chemical_dataset[index_cycle_1, "URXOXY"] <- unclean_chemical_dataset[index_cycle_1, "URXDIZ"]
  unclean_chemical_dataset[index_cycle_2, "URXOXY"] <- unclean_chemical_dataset[index_cycle_2, "URXDIZ"]
  unclean_chemical_dataset[index_cycle_2, "URXUCD"] <- unclean_chemical_dataset[index_cycle_2, "URDUCD"]
  unclean_chemical_dataset[index_cycle_2, "LBD199LA"] <- unclean_chemical_dataset[index_cycle_2, "LBX199LA"]
  unclean_chemical_dataset[index_cycle_2, "URXSCN"] <- unclean_chemical_dataset[index_cycle_2, "SSXSCN"]
  unclean_chemical_dataset[index_cycle_2, "URXUP8"] <- unclean_chemical_dataset[index_cycle_2, "SSXUP8"]
  unclean_chemical_dataset[index_cycle_2, "URXNO3"] <- unclean_chemical_dataset[index_cycle_2, "SSXNO3"]
  unclean_chemical_dataset[index_cycle_3, "URXTRS"] <- unclean_chemical_dataset[index_cycle_3, "URDTRS"]
  unclean_chemical_dataset[index_cycle_3, "LBXV4E"] <- unclean_chemical_dataset[index_cycle_3, "LBXV4T"] # Blood 1,1,2,2-Tetrachloroethane
  unclean_chemical_dataset[index_cycle_1, "LBXVTE"] <- unclean_chemical_dataset[index_cycle_1, "LBXV3A"] # 1,1,1-Trichloroethane and 1,1,1-Trichloroethene are the same chemical but with different spellings
  
  # Certain chemical comments have different codenames but are actually referring to the same chemical,
  # so these comments were harmonized 
  unclean_chemical_dataset[index_cycle_1, "LBDEPAHL"] <- unclean_chemical_dataset[index_cycle_1, "SEPAHLC"]
  unclean_chemical_dataset[index_cycle_1, "LBDMPAHL"] <- unclean_chemical_dataset[index_cycle_1, "SMPAHLC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFDEL"] <- unclean_chemical_dataset[index_cycle_1, "SPFDELC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFDOL"] <- unclean_chemical_dataset[index_cycle_1, "SPFDOLC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFHPL"] <- unclean_chemical_dataset[index_cycle_1, "SPFHPLC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFHSL"] <- unclean_chemical_dataset[index_cycle_1, "SPFHSLC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFNAL"] <- unclean_chemical_dataset[index_cycle_1, "SPFNALC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFOAL"] <- unclean_chemical_dataset[index_cycle_1, "SPFOALC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFOSL"] <- unclean_chemical_dataset[index_cycle_1, "SPFOSLC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFSAL"] <- unclean_chemical_dataset[index_cycle_1, "SPFSALC"]
  unclean_chemical_dataset[index_cycle_1, "LBDPFUAL"] <- unclean_chemical_dataset[index_cycle_1, "SPFUALC"]
  unclean_chemical_dataset[index_cycle_1, "URDOXYLC"] <- unclean_chemical_dataset[index_cycle_1, "URDDIZLC"]
  unclean_chemical_dataset[index_cycle_2, "URDOXYLC"] <- unclean_chemical_dataset[index_cycle_2, "URDDIZLC"]
  unclean_chemical_dataset[index_cycle_3, "LBDV4ELC"] <- unclean_chemical_dataset[index_cycle_3, "LBDV4TLC"]
  
  # These PFCs were measured in children in the 8th cycle but have different chemical codenames to the
  # PFCs measured in adults. 
  pfsa_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFSAL"]))
  mpah_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSMPAHL"]))
  epah_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSEPAHL"]))
  pfde_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFDEL"]))
  pfbs_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFBSL"]))
  pfhp_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFHPL"]))
  pfna_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFNAL"]))
  pfua_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFUAL"]))
  pfdo_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFDOL"]))
  pfhs_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSPFHSL"]))
  
  # Harmonize the PFC codenames measured in children with the PFC codenames that were used for adults
  unclean_chemical_dataset[pfsa_children_comments_index,"LBDPFSAL"] <- unclean_chemical_dataset[pfsa_children_comments_index,"SSPFSAL"]
  unclean_chemical_dataset[mpah_children_comments_index,"LBDMPAHL"] <- unclean_chemical_dataset[mpah_children_comments_index,"SSMPAHL"]
  unclean_chemical_dataset[epah_children_comments_index,"LBDEPAHL"] <- unclean_chemical_dataset[epah_children_comments_index,"SSEPAHL"]
  unclean_chemical_dataset[pfde_children_comments_index,"LBDPFDEL"] <- unclean_chemical_dataset[pfde_children_comments_index,"SSPFDEL"]
  unclean_chemical_dataset[pfbs_children_comments_index,"LBDPFBSL"] <- unclean_chemical_dataset[pfbs_children_comments_index,"SSPFBSL"]
  unclean_chemical_dataset[pfhp_children_comments_index,"LBDPFHPL"] <- unclean_chemical_dataset[pfhp_children_comments_index,"SSPFHPL"]
  unclean_chemical_dataset[pfna_children_comments_index,"LBDPFNAL"] <- unclean_chemical_dataset[pfna_children_comments_index,"SSPFNAL"]
  unclean_chemical_dataset[pfua_children_comments_index,"LBDPFUAL"] <- unclean_chemical_dataset[pfua_children_comments_index,"SSPFUAL"]
  unclean_chemical_dataset[pfdo_children_comments_index,"LBDPFDOL"] <- unclean_chemical_dataset[pfdo_children_comments_index,"SSPFDOL"]
  unclean_chemical_dataset[pfhs_children_comments_index,"LBDPFHSL"] <- unclean_chemical_dataset[pfhs_children_comments_index,"SSPFHSL"]
  
  # Sum linear and branched isomers of PFOS and PFOA to the total levels reported in previous cycles of NHANES
  unclean_chemical_dataset[index_cycle_8, "LBXPFOS"] <- unclean_chemical_dataset[index_cycle_8, "SSNPFOS"] +
    unclean_chemical_dataset[index_cycle_8, "SSMPFOS"] 
  unclean_chemical_dataset[index_cycle_8, "LBXPFOA"] <- unclean_chemical_dataset[index_cycle_8, "SSNPFOA"] +
    unclean_chemical_dataset[index_cycle_8, "SSBPFOA"]  
  
  # Determine indices of participants below and above the LOD
  index_pfos_above_lod <- which(unclean_chemical_dataset[, "LBXPFOS"] >= 0.1)
  index_pfos_below_lod <- which(unclean_chemical_dataset[, "LBXPFOS"] < 0.1 )
  index_pfoa_above_lod <- which(unclean_chemical_dataset[, "LBXPFOA"] >= 0.1)
  index_pfoa_below_lod <- which(unclean_chemical_dataset[, "LBXPFOA"] < 0.1 )
  
  # Assign these participants with the correct corresponding indicator for being below or above the LOD
  unclean_chemical_dataset[index_pfos_above_lod, "LBDPFOSL"] <- 0
  unclean_chemical_dataset[index_pfos_below_lod, "LBDPFOSL"] <- 1
  unclean_chemical_dataset[index_pfoa_above_lod, "LBDPFOAL"] <- 0
  unclean_chemical_dataset[index_pfoa_below_lod, "LBDPFOAL"] <- 1
  
  # Determine the indices for participants with measurements of PFOS and PFOA isomers
  lin_pfos_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSNPFOSL"]))
  branch_pfos_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSMPFOSL"]))
  lin_pfoa_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSNPFOAL"]))
  branch_pfoa_children_comments_index <- which(!is.na(unclean_chemical_dataset[,"SSBPFOAL"]))
  
  # 
  # View(unclean_chemical_dataset[lin_pfos_children_comments_index,c("SEQN","SDNPFOSL")])
  unclean_chemical_dataset[lin_pfos_children_comments_index,"SDNPFOSL"] <- unclean_chemical_dataset[lin_pfos_children_comments_index,"SSNPFOSL"]
  unclean_chemical_dataset[branch_pfos_children_comments_index,"SDMPFOSL"] <- unclean_chemical_dataset[branch_pfos_children_comments_index,"SSMPFOSL"]
  unclean_chemical_dataset[lin_pfoa_children_comments_index,"SDNPFOAL"] <- unclean_chemical_dataset[lin_pfoa_children_comments_index,"SSNPFOAL"]
  unclean_chemical_dataset[branch_pfoa_children_comments_index,"SDBPFOAL"] <- unclean_chemical_dataset[branch_pfoa_children_comments_index,"SSBPFOAL"] 
  # View(unclean_chemical_dataset[lin_pfos_children_comments_index,c("SEQN","SDNPFOSL")])
  
  
  # Determine the indices of variables pertaining to comments, which are codes that dictate whether the
  # participant was below or above the LOD
  chem_comments_index <- c(grep("[A-z0-9]{5,6}LC(|\\d)$"
                                , chem_codenames)
                           , which(chem_codenames == "URDUMMAL")
                           , which(chem_codenames == "LBDBR66C")
                           , which(chem_codenames == "LBDPFOAL")
                           , which(chem_codenames == "LBDPFOSL")
                           , which(chem_codenames == "LBDPFHSL")
                           , which(chem_codenames == "LBDEPAHL")
                           , which(chem_codenames == "LBDMPAHL")
                           , which(chem_codenames == "LBDPFDEL")
                           , which(chem_codenames == "LBDPFBSL")
                           , which(chem_codenames == "LBDPFHPL")
                           , which(chem_codenames == "LBDPFNAL")
                           , which(chem_codenames == "LBDPFSAL")
                           , which(chem_codenames == "LBDPFUAL")
                           , which(chem_codenames == "LBDPFDOL")
                           , grep("^(SS|SD)[A-z0-9]{4,5}L$"
                                  , chem_codenames)
  )
  
  # Determine indices of variables that are excess due to the harmonization 
  harmonize_excess_index <- c(which(chem_codenames == "LBDBHC")
                              , which(chem_codenames == "LBDBHCLA")
                              , which(chem_codenames == "SEPAH")
                              , which(chem_codenames == "SMPAH")
                              , which(chem_codenames == "SPFDE")
                              , which(chem_codenames == "SPFDO")
                              , which(chem_codenames == "SPFHP")
                              , which(chem_codenames == "SPFHS")
                              , which(chem_codenames == "SPFNA")
                              , which(chem_codenames == "SPFOA")
                              , which(chem_codenames == "SPFOS")
                              , which(chem_codenames == "SPFSA")
                              , which(chem_codenames == "SPFUA")
                              , which(chem_codenames == "URDUCD")
                              , which(chem_codenames == "URXDIZ")
                              , which(chem_codenames == "LBX199LA")
                              , which(chem_codenames == "SSXSCN")
                              , which(chem_codenames == "SSXUP8")
                              , which(chem_codenames == "SSXNO3")
                              , which(chem_codenames == "URDTRS")
                              , which(chem_codenames == "LBXV4T")
                              , which(chem_codenames == "LBXV3A")
  )
  
  # Store all the indices pertaining to any excess variable into a vector called excess
  excess <- c(dioxin_jack_knife_index
              , pesticides_jack_knife_index
              , phthalate_jack_knife_index
              , heavy_metals_jack_knife_index
              , dust_lead
              , methylmalonic_index
              , voc_questionaires_index
              , voc_water_index
              , units_SI_index
              , prot_index
              , transferrin_index
              , ferri_index
              , vb12_index
              , str_tin_index
              , homocy_index
              , folate_index
              , iron_index
              , vitamin_index
              , sele_index
              , toco_index
              , retinpa_index
              , retinst_index
              , latex_index
              , alcr_2_index
              , tibc_index
              , floor_windows_index
              , extra_albumin
              , extra_creatinine
              , chem_comments_index
              , pfcs_extra_index
              , harmonize_excess_index
  )
  
  # Fixed the chemical name based on wrong spelling 
  attr(unclean_chemical_dataset[,"LBXF04"], "label") <- "1,2,3,4,7,8-hxcdf (fg/g)"
  attr(unclean_chemical_dataset[,"LBXF08LA"], "label") <- "1,2,3,4,6,7,8-hpcdf Lipid Adj (pg/g)"
  attr(unclean_chemical_dataset[,"LBXPCB"], "label") <- "3,3',4,4',5-pcnb (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBHCLA"], "label") <- "Beta-hexachlorocyclohexane Lipid Adj (ng/g)"
  attr(unclean_chemical_dataset[,"URXMIB"], "label") <- "Mono-isobutyl phthalate (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMHNC"], "label") <- "MHNCH (ng/mL)"
  attr(unclean_chemical_dataset[,"URXUIO"], "label") <- "Iodine, urine (ng/mL)"
  attr(unclean_chemical_dataset[,"SSURHIBP"], "label") <- "Mono-2-hydroxy-iso-butyl phthalate (ng/mL)"
  
  # Fixed the chemical name based having excess words such as "result" 
  attr(unclean_chemical_dataset[,"URXMET"], "label") <- "Metolachlor mercapturate (ug/L)"
  attr(unclean_chemical_dataset[,"URXACE"], "label") <- "Acetochlor mercapturate (ug/L)"
  attr(unclean_chemical_dataset[,"URX14D"], "label") <- "2,5-dichlorophenol (ug/L)"
  attr(unclean_chemical_dataset[,"URXOPP"], "label") <- "O-Phenyl phenol (ug/L)"
  attr(unclean_chemical_dataset[,"URXDCB"], "label") <- "2,4-dichlorophenol (ug/L)"
  attr(unclean_chemical_dataset[,"URX1TB"], "label") <- "2,4,5-trichlorophenol (ug/L)"
  attr(unclean_chemical_dataset[,"URX3TB"], "label") <- "2,4,6-trichlorophenol (ug/L)"
  attr(unclean_chemical_dataset[,"URXAAZ"], "label") <- "Atrazine (ug/L)"
  attr(unclean_chemical_dataset[,"URXDCZ"], "label") <- "Diaminochloroatrazine (ug/L)"
  attr(unclean_chemical_dataset[,"URXDTZ"], "label") <- "Desethyl atrazine (ug/L)"
  attr(unclean_chemical_dataset[,"URXSIS"], "label") <- "Desisopropyl atrazine (ug/L)"
  attr(unclean_chemical_dataset[,"URXOXY"], "label") <- "Oxypyrimidine (ug/L)"
  
  # Fixed the chemical name based on having a lack of units or no space between the chemical name and 
  # the units 
  attr(unclean_chemical_dataset[,"URXMNM"], "label") <- "Mono-n-methyl phthalate (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMC1"], "label") <- "Mono-(3-carboxypropyl) phthalate (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMHH"], "label") <- "Mono-(2-ethyl-5-hydroxyhexyl) phthalate (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMOH"], "label") <- "Mono-(2-ethyl-5-oxohexyl) phthalate (ng/mL)"
  attr(unclean_chemical_dataset[,"URXP01"], "label") <- "1-napthol (ng/L)"
  attr(unclean_chemical_dataset[,"URXP02"], "label") <- "2-napthol (ng/L)"
  attr(unclean_chemical_dataset[,"LBXBB1"], "label") <- "2,2',4,4',5,5'-hexabromobiphenyl PBB-153 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBB1LA"], "label") <- "2,2',4,4',5,5'-hexabromobiphenyl lipid adj PBB-153 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR1"], "label") <- "2,2',4-tribromodiphenyl ether PBDE-17 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR1LA"], "label") <- "2,2',4-tribromodiphenyl ether lipid adj PBDE-17 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR2"], "label") <- "2,4,4'-tribromodiphenyl ether PBDE-28 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR2LA"], "label") <- "2,4,4'-tribromodiphenyl ether lipid adj PBDE-28 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR3"], "label") <- "2,2',4,4'-tetrabromodiphenyl ether PBDE-47 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR3LA"], "label") <- "2,2',4,4'-tetrabromodiphenyl ether lipid adj PBDE-47 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR4"], "label") <- "2,2',3,4,4'-pentabromodiphenyl ether PBDE-85 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR4LA"], "label") <- "2,2',3,4,4'-pentabromodiphenyl ether lipid adj PBDE-85 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR5"], "label") <- "2,2',4,4',5-pentabromodiphenyl ether PBDE-99 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR5LA"], "label") <- "2,2',4,4',5-pentabromodiphenyl ether lipid adj PBDE-99 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR6"], "label") <- "2,2',4,4',6-pentabromodiphenyl ether PBDE-100 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR6LA"], "label") <- "2,2',4,4',6-pentabromodiphenyl ether lipid adj PBDE-100 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR7"], "label") <- "2,2',4,4',5,5'-hexabromodiphenyl ether PBDE-153 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR7LA"], "label") <- "2,2',4,4',5,5'-hexabromodiphenyl ether lipid adj PBDE-153 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR8"], "label") <- "2,2',4,4',5,6'-hexabromodiphenyl ether PBDE-154 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR8LA"], "label") <- "2,2',4,4',5,6'-hexabromodiphenyl ether lipid adj PBDE-154 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR9"], "label") <- "2,2',3,4,4',5',6-heptabromodiphenyl ether PBDE-183 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR9LA"], "label") <- "2,2',3,4,4',5',6-heptabromodiphenyl ether lipid adj PBDE-183 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR66"], "label") <- "2,3',4,4'-tetrabromodiphenyl ether PBDE-66 (pg/g)"
  attr(unclean_chemical_dataset[,"LBXBR66L"], "label") <- "2,3',4,4'-tetrabromodiphenyl ether lipid adj PBDE-66 (pg/g)"
  attr(unclean_chemical_dataset[,"SSMEL"], "label") <- "Melamine (ng/mL)"
  attr(unclean_chemical_dataset[,"SSCYA"], "label") <- "Cyanuric acid (ng/mL)"
  attr(unclean_chemical_dataset[,"URXECP"], "label") <- "Mono-2-ethyl-5-carboxypentyl phthalate (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFOA"], "label") <- "Perfluorooctanoic acid PFOA (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFOS"], "label") <- "Perfluorooctane sulfonic acid PFOS (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFHS"], "label") <- "Perfluorohexane sulfonic acid PFHxS (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXEPAH"], "label") <- "2-(N-ethyl-PFOSA) acetate (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXMPAH"], "label") <- "2-(N-methyl-PFOSA) acetate (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFDE"], "label") <- "Perfluorodecanoic acid PFDA (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFBS"], "label") <- "Perfluorobutane sulfonic acid PFBS (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFHP"], "label") <- "Perfluoroheptanoic acid PFHxA (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFNA"], "label") <- "Perfluorononanoic acid PFNA (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFSA"], "label") <- "Perfluorooctane sulfonamide PFOSA (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFUA"], "label") <- "Perfluoroundecanoic acid PFUnDA (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXPFDO"], "label") <- "Perfluorododecanoic acid PFDoA (ng/mL)"
  attr(unclean_chemical_dataset[,"LBXV2P"], "label") <- "Blood 1,2-Dibromo-3-chloropropane (ng/mL)"
  attr(unclean_chemical_dataset[,"URXSISM"], "label") <- "Desisopropyl atrazine mercapturate (ug/L)"
  attr(unclean_chemical_dataset[,"URX1DC"], "label") <- "N-Acel-S-(1,2-dichlorovinl)-L-cys (ng/mL)"
  attr(unclean_chemical_dataset[,"URX2DC"], "label") <- "N-Acel-S-(2,2-Dichlorvinyl)-L-cys (ng/mL)"
  attr(unclean_chemical_dataset[,"URX34M"], "label") <- "3-methylhippuric acid & 4-methylhippuric acid (ng/mL)"
  attr(unclean_chemical_dataset[,"URXAAM"], "label") <- "N-Ace-S-(2-carbamoylethyl)-L-cys (ng/mL)"
  attr(unclean_chemical_dataset[,"URXAMC"], "label") <- "N-Ace-S-(N-methlcarbamoyl)-L-cys (ng/mL)"
  attr(unclean_chemical_dataset[,"URXATC"], "label") <- "2-Aminothiazoline-4-carboxylic acid (ng/mL)"
  attr(unclean_chemical_dataset[,"URXBMA"], "label") <- "N-Acetyl-S-(benzyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXBPM"], "label") <- "N-Acetyl-S-(n-propyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXCEM"], "label") <- "N-Acetyl-S-(2-Carbxyethyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXCYM"], "label") <- "N-Acetyl-S-(2-cyanoethyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXDHB"], "label") <- "N-Ace-S-(3,4-Dihidxybutl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXDPM"], "label") <- "N-Ace-S-(dimethylphenyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXGAM"], "label") <- "N-Ace-S-(2-carbmo-2-hydxel)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXHEM"], "label") <- "N-Ace-S-(2-Hydroxyethyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXHP2"], "label") <- "N-Ace-S-(2-hydroxypropyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXHPM"], "label") <- "N-Ace-S-(3-Hydroxypropyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXPMM"], "label") <- "N-A-S-(3-hydrxprpl-1-metl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMAD"], "label") <- "Mandelic acid (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMB3"], "label") <- "N-A-S-(4-hydrxy-2butn-l-yl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMB2"], "label") <- "N-Ac-S-(2-Hydrxy-3-butnyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXMB1"], "label") <- "N-A-S-(1-HydrxMet)-2-Prpn)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXPHE"], "label") <- "N-Ace-S-(phenl-2-hydxyetl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXPHG"], "label") <- "Phenylglyoxylic acid (ng/mL)"
  attr(unclean_chemical_dataset[,"URXPMA"], "label") <- "N-Acetyl-S-(phenyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXTCV"], "label") <- "N-Acetyl-S-(trichlorovinyl)-L-Cysteine (ng/mL)"
  attr(unclean_chemical_dataset[,"URXTTC"], "label") <- "2-thoxothazlidne-4-carbxylic acid (ng/mL)"
  attr(unclean_chemical_dataset[,"URXCB3"], "label") <- "dibromovinyl-dimeth prop carboacid (ug/L)"
  attr(unclean_chemical_dataset[,"URXTCC"], "label") <- "trans dichlorovnl-dimeth carboacid (ug/L)"
  attr(unclean_chemical_dataset[,"URXCOP"], "label") <- "Mono(carboxyoctyl) phthalate (ng/mL)"
  attr(unclean_chemical_dataset[,"URXEPB"], "label") <- "Ethyl paraben (ng/ml)"
  attr(unclean_chemical_dataset[,"SSBDCPP"], "label") <- "Bis(1,3-dichloro-2-propyl) phosphate (ug/L)"
  
  # Determine the index of the SEQN, so that it can be extract and merged with the chemical comment
  # codes to form the comments_dataset
  seqn_index_0 <- which(chem_codenames == "SEQN")
  # Form the comments dataset
  nhanes_list[["comments_dataset"]] <- unclean_chemical_dataset[,c(seqn_index_0
                                                                   , chem_comments_index)]
  
  # Determine indices of comment variables that are excess after the harmonization
  excess_comments <- c(grep("^SS[A-z0-9]{4,5}L$"
                            , colnames(nhanes_list[["comments_dataset"]])) 
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SEPAHLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SMPAHLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFDELC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFDOLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFHPLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFHSLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFNALC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFOALC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFOSLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFSALC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "SPFUALC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "URDDIZLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDV4TLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDWBFLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDWCFLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDWBMLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDWCMLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDWMELC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDDFSLC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDD3LC")
                       , which(colnames(nhanes_list[["comments_dataset"]]) == "LBDDWSLC")
  )
  
  # Remove the excessive comment variables from the comments dataset
  nhanes_list[["comments_dataset"]] <- nhanes_list[["comments_dataset"]][,-excess_comments]
  
  # Remove the excessive variable from the chemical dataset to form the first iteration of the clean 
  # chemical dataset
  clean_chemical_dataset_1 <- unclean_chemical_dataset[,-excess]
  
  # Determine the indices of variables that pertains to the survey weight codenames to form the weight dataset
  weights_index <- grep("^WT"
                        , colnames(clean_chemical_dataset_1))
  
  study_year_index <- which(colnames(clean_chemical_dataset_1) == "study_year")
  # Determine the index pertaining to the SEQN
  seqn_index_1 <- which(colnames(clean_chemical_dataset_1) == "SEQN")
  # Form the weights dataset to have SEQN and the corresponding weights associated with each participant
  nhanes_list[["weights_dataset"]] <- clean_chemical_dataset_1[,c(seqn_index_1
                                                                  , weights_index)]
  
  # Form the final, clean chemicals dataset by excluding the weight variables 
  nhanes_list[["chemicals_dataset"]] <- clean_chemical_dataset_1[,-c(weights_index
                                                                     , study_year_index)]
  
  # Return the list containing the comments, chemicals, and weights datasets
  return(nhanes_list)
}