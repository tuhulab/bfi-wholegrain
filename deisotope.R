##manual deisotope
deisotope <- function(data=...){
  require(stringr)
  str_which(data$isotopes, pattern = "\\+\\d")
  data_1 <- data[-str_which(data$isotopes, pattern = "\\+\\d|\\d\\+"),]
return(data_1)}

  
   