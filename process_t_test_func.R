#process_t_test_func.r
#dependency: process.r

library('dplyr')

process_t <- function(input)
{

  # perform paired t-test on pre-post values
  variables <- as.vector(outer(channels, items, paste, sep='_'))
  t_test_results <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  
  for (stuff in variables) {
    name1 <- paste('post',stuff,sep='_')
    name2 <- paste('pre',stuff,sep='_')
    
    res<-t.test(input[,name1],input[,name2],paired=TRUE)
    r <- cor(input[,name1],input[,name2])
    d <- res$statistic * ((2*(1-r)/length(input[,name1]))^0.5)
    
    this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = stuff
    this_one[1,2] = res$p.value
    this_one[1,3] = res$estimate
    this_one[1,4] = res$conf.int[1]
    this_one[1,5] = res$conf.int[2]
    this_one[1,6] = d
    t_test_results <- rbind(t_test_results, this_one)}
  t_test_results
}

process_wilcox <- function(input)
{
  
  # perform wilcoxon signed rank sum test
  variables <- as.vector(outer(channels, items, paste, sep='_'))
  final_test_results <- data.frame(name=character(),p_value=numeric(),stringsAsFactors=FALSE)
  
  for (stuff in variables) {
    name1 <- paste('post',stuff,sep='_')
    name2 <- paste('pre',stuff,sep='_')
    
    res<-wilcox.test(input[,name1],input[,name2],paired=TRUE)
    
    this_one <- data.frame(name=character(),p_value=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = stuff
    this_one[1,2] = res$p.value
    final_test_results <- rbind(final_test_results, this_one)}
  final_test_results
}


my_t <- function(input,variables)
{
  
  # perform paired t-test on pre-post values
  t_test_results <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  
  for (stuff in variables) {
    name1 <- paste('post',stuff,sep='_')
    name2 <- paste('pre',stuff,sep='_')
    
    res<-t.test(input[,name1],input[,name2],paired=TRUE)
    r <- cor(input[,name1],input[,name2])
    d <- res$statistic * ((2*(1-r)/length(input[,name1]))^0.5)
    
    this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = stuff
    this_one[1,2] = res$p.value
    this_one[1,3] = res$estimate
    this_one[1,4] = res$conf.int[1]
    this_one[1,5] = res$conf.int[2]
    this_one[1,6] = d
    t_test_results <- rbind(t_test_results, this_one)}
  t_test_results
}

t_vs_0 <- function(input)
{
  
  # perform paired t-test on pre-post values
  t_test_results <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),stringsAsFactors=FALSE)
  variables <- names(input)
  
  for (stuff in variables) {
    
    res<-t.test(input[,stuff])
    
    this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = stuff
    this_one[1,2] = res$p.value
    this_one[1,3] = res$estimate
    this_one[1,4] = res$conf.int[1]
    this_one[1,5] = res$conf.int[2]
    t_test_results <- rbind(t_test_results, this_one)}
  t_test_results
}
