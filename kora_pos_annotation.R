library(readr)

kora_pos <- read_csv("I:/SCIENCE-NEXS-NyMetabolomics/Projects/DINAMIC/KORA/Positive/190624_KORA_positive_blanks_XCMS_Camera_v2_for_annotation.csv")

mz <- kora_pos$X3[10:nrow(kora_pos)]
rt <- kora_pos$X6[10:nrow(kora_pos)]


annotate_kudb <- function(mz_window=...,
                          rt_window=...,
                          polarity=...,
                          mz=...,
                          rt=...){
  require(dplyr)
  require(readr)
  require(purrr)
  require(stringr)
  
  db_ku_1 <- read_csv(file.path("I:","SCIENCE-NEXS-NyMetabolomics","db","db_ku.csv")) %>% filter(is.na(exact_mass)==FALSE)
  db_ku_sodium <- db_ku_1 %>% mutate(name=paste0(name,"+Na"), exact_mass=exact_mass+22.99)
  db_ku_dimer <- db_ku_1 %>% mutate(name=paste0("2[",name,"]"), exact_mass=exact_mass*2)
  db_ku_potassium <- db_ku_1 %>% mutate(name=paste0(name,"+K"), exact_mass=exact_mass+38.964)
  db_ku_h2olose <- db_ku_1 %>% mutate(name=paste0(name,"-H2O"), exact_mass=exact_mass-18.011)
  db_ku <- bind_rows(db_ku_1,db_ku_sodium,db_ku_dimer,db_ku_potassium,db_ku_h2olose)
  
  
  mzrt <- tibble(mz,rt)
  n <- 1:nrow(mzrt)
  polarity_adjust <- ifelse(polarity=="pos",1.007,-1.007)
  
  #Predret annotation
  result <- sapply(n,function(n){
    mz_1 <- mzrt[n,1] %>% as.numeric() 
    rt_1 <- mzrt[n,2] %>% as.numeric() 
    db_ku %>% 
      filter(source=="predret") %>%
      filter(exact_mass+polarity_adjust>mz_1-mz_window) %>%
      filter(exact_mass+polarity_adjust<mz_1+mz_window) %>%
      filter(predicted_rt*60>rt_1-rt_window) %>%
      filter(predicted_rt*60<rt_1+rt_window)} %>% pull(name))
  result_1 <- mzrt %>% mutate(id_predret=map(result,function(x)paste(x,collapse = ",")) %>% map_chr(.,1)) 
  
  result_2 <- sapply(n,function(n){
    mz_1 <- mzrt[n,1] %>% as.numeric() 
    rt_1 <- mzrt[n,2] %>% as.numeric() 
    db_ku %>% 
      filter(source=="kudb") %>%
      filter(exact_mass+polarity_adjust>mz_1-mz_window) %>%
      filter(exact_mass+polarity_adjust<mz_1+mz_window) %>%
      filter(recorded_rt*60>rt_1-rt_window) %>%
      filter(recorded_rt*60<rt_1+rt_window)} %>% pull(name))
  result_3 <- result_1 %>% mutate(id_kudb=map(result_2,function(x)paste(x,collapse = ",")) %>% map_chr(.,1)) 
  return(result_3)}

kora_pos_annotation <- annotate_kudb(mz_window=0.015,rt_window=10,polarity="pos",mz,rt)
write_csv(kora_pos_annotation, path = "I:/SCIENCE-NEXS-NyMetabolomics/Projects/DINAMIC/KORA/postman/kora_pos_kudbannotation.csv")
