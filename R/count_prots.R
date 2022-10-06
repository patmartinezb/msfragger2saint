
# Count number of non-0 proteins per sample -------------------------------

count.prot <- function(df){
  
  # Creates df with the number of non-0 proteins per sample
  
  if (inherits(df, "list")){
    
    validate("There are 2 or more files uploaded - make sure to merge them before plotting")
    
  } else {
    
    df <- df %>% 
      slice(-1) %>% # Remove first row - contains metadata
      select(- GENEID,
             - PROTLEN) %>% # Remove unnecessary vars
      type_convert() %>% # Corrects type of data (i.e. numeric var saved as character var, makes it type dbl)
      na_if(0) %>% # Transform 0 into NAs
      pivot_longer(!PROTID, names_to = "Samples", values_to = "vals") %>% # Pivot data so that sample names have their own var, and also their values
      na.omit() %>% # Remove every observation that has NA
      mutate(Samples = str_remove(Samples, "_SPC")) %>%  # Remove "_SPC" from the samples names so that it looks cleaner
      group_by(Samples) %>% 
      count() # Count the number of proteins per sample
    
    return(df)
    
  }
}
