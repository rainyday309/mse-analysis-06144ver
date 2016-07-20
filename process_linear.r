#process_linear.r
#dependency: process.r
library('stringr')


variables <- as.vector(outer(channels, items, paste, sep='_'))

# model 1: 
# pre_mse ~ age+ sex + HAMD0

lm_model1 <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_HAMD0=numeric(),p_value_HAMD0=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  name <- paste('pre',i,sep='_')
  equation <- paste(name, ' ~ age + sex + HAMD0', sep='')
  
  res<-summary(lm(data = completer, formula = equation))
  
  # get p-value for f statistic examining model fit
  f <- res$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  
  this_one <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_HAMD0=numeric(),p_value_HAMD0=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = p
  this_one[1,3] = res$adj.r.squared
  this_one[1,4] = res$coefficients[4,1]
  this_one[1,5] = res$coefficients[4,4]
  lm_model1 <- rbind(lm_model1, this_one)
  
}

lm1_positive <- filter(lm_model1, overall_p_value_for_fit < 0.05)
lm1_positive <- filter(lm1_positive, p_value_HAMD0 < 0.05)

# clean up
rm(equation)
rm(f)
rm(i)
rm(name)
rm(p)
rm(res)


# model 2:
# (post-mse - pre-mse)/pre-mse ~ age + sex + (HAMD6-HAMD0)/HAMD0

lm_model2 <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_response_rate=numeric(),p_value_response_rate=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  name1 <- paste('pre',i,sep='_')
  name2 <- paste('post',i,sep='_')
  equation <- paste('(',name2,' - ',name1,')/',name1, ' ~ age + sex + response_rate', sep='')
  
  res<-summary(lm(data = completer, formula = equation))
  
  # get p-value for f statistic examining model fit
  f <- res$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  
  this_one <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_response_rate=numeric(),p_value_response_rate=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = p
  this_one[1,3] = res$adj.r.squared
  this_one[1,4] = res$coefficients[4,1]
  this_one[1,5] = res$coefficients[4,4]
  lm_model2 <- rbind(lm_model2, this_one)
  
}

lm2_positive <- filter(lm_model2, overall_p_value_for_fit < 0.05)
lm2_positive <- filter(lm2_positive, p_value_response_rate < 0.05)
