library(RODBC)
library(tidyverse)

spls <- file.path("spl",list.files("spl"))
waters_extract_spl <- function(file){
  require(RODBC)
  con <- odbcConnectAccess2007(file)
  if(con==-1) return(data_frame())
  out <- sqlFetch(con, "ANALYSIS", stringsAsFactors = FALSE) %>%
    mutate_at(vars(one_of('SAMPLE_LOCATION', 'FILE_NAME')), funs(as.character)) %>%
    mutate_at(vars(contains('CONC_')), funs(as.character))
  close(con)
  return(out)}

n <- 1

sample_names <- 
  sapply(n,
       function(n)
         {waters_extract_spl(spls[n]) %>% if(rownames()=="FILE_TEXT") %>% pull(FILE_TEXT) %>% unique()})
