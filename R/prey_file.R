
# Prey file ---------------------------------------------------------------

prey_file <- function(df){
  
  if (inherits(df, "list")){
    
    validate("There are 2 or more files uploaded - make sure to merge them before creating SAINT files")
    
  } else {
    
    df <- df %>% 
      select(PROTID,
             PROTLEN) %>% # Select vars of interest
      slice(-1) %>% # Remove first row - contains metadata
      rename(Accession = PROTID,
             Length = PROTLEN) %>% 
      mutate(Accession.1 = Accession) %>% # Duplicate Accession var
      distinct(Accession, .keep_all = TRUE) %>% # Remove possible duplicates
      arrange(Accession) # In alphabetical order
    
    return(df)
    
  }
  
}
