#process_linear3.r
#dependency: process.r
library('stringr')


variables <- names(output3_5)

# model 6: 
# HAMD0 ~ age + sex + mean_mse over scale 3-5

lm_model6 <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  equation <- paste('completer$HAMD0 ~ completer$age + completer$sex + ','output3_5$',i, sep='')
  
  res<-summary(lm(data = completer, formula = equation))
  
  # get p-value for f statistic examining model fit
  f <- res$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  
  this_one <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = p
  this_one[1,3] = res$adj.r.squared
  this_one[1,4] = res$coefficients[4,1]
  this_one[1,5] = res$coefficients[4,4]
  lm_model6 <- rbind(lm_model6, this_one)
  
}

lm6_positive <- filter(lm_model6, overall_p_value_for_fit < 0.05)
lm6_positive <- filter(lm6_positive, p_value_mse < 0.05)

# clean up
rm(equation)
rm(f)
rm(i)
rm(p)
rm(res)


# model 7: 
# HAMD6 ~ age + sex + mean_mse over scale 3-5

lm_model7 <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  equation <- paste('completer$HAMD6 ~ completer$age + completer$sex + ','output3_5$',i, sep='')
  
  res<-summary(lm(data = completer, formula = equation))
  
  # get p-value for f statistic examining model fit
  f <- res$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  
  this_one <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = p
  this_one[1,3] = res$adj.r.squared
  this_one[1,4] = res$coefficients[4,1]
  this_one[1,5] = res$coefficients[4,4]
  lm_model7 <- rbind(lm_model7, this_one)
  
}

lm7_positive <- filter(lm_model7, overall_p_value_for_fit < 0.05)
lm7_positive <- filter(lm7_positive, p_value_mse < 0.05)

# clean up
rm(equation)
rm(f)
rm(i)
rm(p)
rm(res)
