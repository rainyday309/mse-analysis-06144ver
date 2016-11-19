#process_linear2.r
#dependency: process.r
library('stringr')


variables <- names(output3_5)

# model 3: 
# response_rate ~ age + sex + HAMD0 + mean_mse over scale 3-5

lm_model3 <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  equation <- paste('completer$response_rate ~ completer$age + completer$sex + completer$HAMD0 + ','output3_5$',i, sep='')
  
  res<-summary(lm(data = completer, formula = equation))
  
  # get p-value for f statistic examining model fit
  f <- res$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  
  this_one <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = p
  this_one[1,3] = res$adj.r.squared
  this_one[1,4] = res$coefficients[5,1]
  this_one[1,5] = res$coefficients[5,4]
  lm_model3 <- rbind(lm_model3, this_one)
  
}

lm3_positive <- filter(lm_model3, overall_p_value_for_fit < 0.05)
lm3_positive <- filter(lm3_positive, p_value_mse < 0.05)

# clean up
rm(equation)
rm(f)
rm(i)
rm(p)
rm(res)


# model4
variables <- names(output1_2)
lm_model4 <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  equation <- paste('completer$response_rate ~ completer$age + completer$sex + completer$HAMD0 + ','output1_2$',i, sep='')
  
  res<-summary(lm(data = completer, formula = equation))
  
  # get p-value for f statistic examining model fit
  f <- res$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  
  this_one <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = p
  this_one[1,3] = res$adj.r.squared
  this_one[1,4] = res$coefficients[5,1]
  this_one[1,5] = res$coefficients[5,4]
  lm_model4 <- rbind(lm_model4, this_one)
  
}

lm4_positive <- filter(lm_model4, overall_p_value_for_fit < 0.05)
lm4_positive <- filter(lm4_positive, p_value_mse < 0.05)

# clean up
rm(equation)
rm(f)
rm(i)
rm(p)
rm(res)

# model5
variables <- names(output6_10)
lm_model5 <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  equation <- paste('completer$response_rate ~ completer$age + completer$sex + completer$HAMD0 + ','output6_10$',i, sep='')
  
  res<-summary(lm(data = completer, formula = equation))
  
  # get p-value for f statistic examining model fit
  f <- res$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  
  this_one <- data.frame(name=character(),overall_p_value_for_fit=numeric(), adjusted_R_square=numeric(), coefficient_mse=numeric(),p_value_mse=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = p
  this_one[1,3] = res$adj.r.squared
  this_one[1,4] = res$coefficients[5,1]
  this_one[1,5] = res$coefficients[5,4]
  lm_model5 <- rbind(lm_model5, this_one)
  
}

lm5_positive <- filter(lm_model5, overall_p_value_for_fit < 0.05)
lm5_positive <- filter(lm5_positive, p_value_mse < 0.05)

# clean up
rm(equation)
rm(f)
rm(i)
rm(p)
rm(res)



