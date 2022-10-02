
# Interaction file --------------------------------------------------------


interaction_file <- function(df){
  
  if (inherits(df, "list")){
    
    validate("There are 2 or more files uploaded - make sure to merge them before creating SAINT files")
    
  } else {
    
    df <- df %>% 
      slice(-1) %>% # Remove first row - contains metadata
      select(- GENEID,
             - PROTLEN) %>% # Remove unnecessary vars
      type_convert() %>% # Corrects type of data (i.e. numeric var saved as character var, makes it type dbl)
      pivot_longer(!PROTID, names_to = "IP", values_to = "value") %>% # Pivot data so that sample names have their own var, and also their values
      mutate(Bait = str_extract(IP, "[^_]+")) %>% # Bait names are extracted from the characters right before the first underscore
      rename(Accession = PROTID) %>%
      select(IP,
             Bait,
             Accession,
             value) %>% 
      na_if(0) %>% # Transform 0 into NAs
      na.omit() # Remove every observation that has NA
    
    return(df)
    
  }
  
}