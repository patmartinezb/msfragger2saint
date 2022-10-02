
# Bait file ---------------------------------------------------------------

bait_file <- function(df){
  
  if (inherits(df, "list")){
    
    validate("There are 2 or more files uploaded - make sure to merge them before creating SAINT files")
    
  } else {
  
  sampleNames <- colnames(df)[-which(colnames(df) %in% c("PROTID", "GENEID", "PROTLEN"))]
  
  df <- data.frame(sampleNames = sampleNames) %>% # Create dataframe where first var is the sample names
    mutate(baitNames = str_extract(sampleNames, "[^_]+"), # Bait names are extracted from the characters right before the first underscore
           ct_vector = case_when(str_detect(sampleNames, "GFP|FLAG|NLS") ~ "C",
                                 TRUE ~ "T")) # Classify as control -C- or treated -T-
  
  return(df)
  
  }
}
