

# Merge function ----------------------------------------------------------


merge.files <- function(dfs, by = "PROTID"){
  
  if (all(unlist(lapply(dfs, function(x){ str_detect(colnames(x[,1:3]), c("PROTID|GENEID|PROTLEN")) })))){
    
    ## Check that vars PROTID, GENEID, PROTLEN are present in every file
    
    merged <- reduce(dfs, full_join, by = by) %>% # Unlist and merge dfs based on by argument
      mutate(prot.len = coalesce(!!! select(., matches("PROTLEN"))),
             gene.id = coalesce(!!! select(., matches("GENEID")))) %>% # Merge PROTLEN and GENEID vars from different dfs so that no information is lost
      select(-starts_with("PROTLEN"),
             -starts_with("GENEID")) %>% # Remove duplicate vars from different dfs
      rename(PROTLEN = prot.len,
             GENEID = gene.id) %>% # Rename the new merged vars as to keep the original name
      select(PROTID,
             GENEID,
             PROTLEN,
             everything()) %>% # Order vars
      mutate(across(!c(PROTID, GENEID, PROTLEN), ~ replace_na(as.character(.x), "0"))) # Replace NAs with 0 in all vars except PROTID, GENEID and PROTLEN
    
    return(merged)
    
  } else {
    
    validate("WARNING: At least one of the uploaded files do not have the PROTID, GENEID and/or PROTLEN variables.")
    
  }
  
}
