
# Correlation plot --------------------------------------------------------


corrplot_prot <- function(df){
  
  # Creates correlation plot
  
  if (inherits(df, "list")){
    
    validate("There are 2 or more files uploaded - make sure to merge them before plotting")
    
  } else {
  
  colnames(df) <- str_remove(colnames(df), "_SPC|_SPC`") # Remove "_SPC" from the samples names so that it looks cleaner
  
  df.cor <- df %>% 
    slice(-1) %>% # Remove first row - contains metadata
    type_convert() %>% # Corrects type of data (i.e. numeric var saved as character var, makes it type dbl)
    select(!GENEID:PROTLEN) %>% # Remove unnecessary vars
    na_if(0) %>% # Transform 0 into NAs
    select_if(is.numeric) %>%
    cor(., use = "pairwise.complete.obs", method = "pearson") # Calculates correlation matrix
  
    # Plot correlation matrix
    corrplot(df.cor, 
             method="color", 
             na.label = "NA",
             tl.col="black", 
             tl.cex=0.5, 
             tl.srt=70, 
             cl.lim=c(0,1))
  }
}
