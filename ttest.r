# dependency: process.r
# t-test trial
t_results <- vector()
t_mean_change <- vector()


# paired t-test for pre-trial average mse v post-trial average mse
# save into dataframe: pre_post_t
# run after process.r
for (i in 23:242) {
  temp <- t.test(fulltable[,i+220],fulltable[,i],paired = TRUE)
  t_results <- c(t_results, temp$p.value)
  t_mean_change <- c(t_mean_change, temp$estimate)
  
}

pre_post_t <- data.frame(name=names<-sub(pattern = "pre_",replacement ="" ,x = colnames(fulltable[23:242])), 
                         pValue=t_results, mean_change=t_mean_change)

pre_post_t_sig <- filter(pre_post_t, pValue < 0.05)


##############################################################################
# compare responder vs non-responder
# run after process.r

response_t_results <- vector()
responder <- filter(fulltable, response==1)
nonresponder <- filter(fulltable, response==0)

for (i in colnames(completer)[23:462]) {
  temp <- t.test(responder[i],nonresponder[i],paired = FALSE)$p.value
  response_t_results <- c(response_t_results, temp)
}

response_t_results <- data.frame(name=colnames(completer)[23:462], pValue=response_t_results)
response_t_results_sig <- filter(response_t_results, pValue < 0.05)