m2r <- function(path=...,polarity=...) {
  require(R.matlab)
  
  data.frame(protein_source=matdata[[1]]$label[9],)
  
   matdata <- readMat(path)
   matdata1 <- matdata[[1]] #[1] and [[1]] are different
   
  #subject info
  # ##extract filename
   filename1 <- matdata1$label[[1]]
   filename <- filename1[[1]]
   ##extract samplename
   samplename1 <- matdata1$label[[5]]
   samplename <- samplename1[[1]]
   ##extract subjectnumber
   subject1 <- matdata1$label[[13]]
   subject <- subject1[[1]]
   ##extract timepoint
   timepoint1 <- matdata1$label[[17]]
   timepoint <- timepoint1[[1]]
   ##extract intervention
   intervention1 <- matdata1$label[[21]]
   intervention <- intervention1[[1]]
   ##extract rt
   rt1 <- matdata1$label[[2]]
   rt <- rt1[[1]]
   ##extract mz
   mz1 <- matdata1$label[[6]]
   mz <- mz1[[1]]
   ##extract groupinfo
   group1 <- matdata1$label[[10]]
   group <- group1[[1]]
   ##extract intensities
   int <- matdata1$data
   
   ##shape new data
   
   data <- data.frame(filename,samplename,subject,timepoint,intervention)
   pools <- data %>% filter(samplename =='pool  ')
   nonpools <- data %>% filter(samplename != 'pool  ')
   pools$subject <- 'pool'
   pools$timepoint <- NA
   pools$intervention <- 'pool'
   data <- bind_rows(nonpools,pools)
   
   intdata <- data.frame(int)
   datan <- bind_cols(data,intdata)
   datan1 <- datan %>% gather(feature,intensity,6:ncol(datan))
   
   ##write rt and mz
   rtdata <- t(rt) %>% data.frame() %>% gather(feature, rt, 1: nrow(rt))
   mzdata <- t(mz) %>% data.frame() %>% gather(feature, mz, 1: nrow(mz))
   
   datan2 <- left_join(datan1,rtdata,by='feature') %>% left_join(mzdata,by='feature') %>% mutate(polarity)
   return(datan2)
}
