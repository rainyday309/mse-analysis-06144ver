#process_aa.r
#compare asymmetry of spectral data

aa_compare <- function(input){
  aa_left_channels <- c('FP1','F7','F3','T3','C3','T5','P3','O1')
  aa_right_channels <- c('FP2','F8','F4','T4','C4','T6','P4','O2')
  aa_pre_post <- c('pre','post')
  aa_bands <- items
  
  left <- outer(outer(aa_pre_post,aa_left_channels,paste,sep='_'),aa_bands,paste,sep='_')
  right <- outer(outer(aa_pre_post,aa_right_channels,paste,sep='_'),aa_bands,paste,sep='_')
  
  results <- data.frame(difference=numeric(nrow(input)),stringsAsFactors=FALSE)
  
  
  
  for (i in 1:length(left)){
    diff <- ((input[left[i]] - input[right[i]]) / (input[left[i]] + input[right[i]]))
    results <- cbind(results,diff)
  }
  results<-results[,-1]
  results
}

