#process_t_test.r
#dependency: process.r

library('dplyr')

# perform paired t-test on pre-post valus
variables <- as.vector(outer(channels, items, paste, sep='_'))
t_test_results <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  name1 <- paste('post',i,sep='_')
  name2 <- paste('pre',i,sep='_')
  
  res<-t.test(completer[,name1],completer[,name2],paired=TRUE)
  r <- cor(completer[,name1],completer[,name2])
  d <- res$statistic * ((2*(1-r)/length(completer[,name1]))^0.5)
  
  this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = res$p.value
  this_one[1,3] = res$estimate
  this_one[1,4] = res$conf.int[1]
  this_one[1,5] = res$conf.int[2]
  this_one[1,6] = d
  t_test_results <- rbind(t_test_results, this_one)
  
}

# clean up messes

rm(name1)
rm(name2)
rm(res)

rm(i)
rm(this_one)
rm(d)
rm(r)
t_test_positive <- filter(t_test_results, p_value<0.05)