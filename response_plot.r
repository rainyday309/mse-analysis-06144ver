#response_plot.r
library('ggplot2')
library('grid')
library('gridExtra')

# plots_pre: plot of responder vs nonresponder in pre-test eeg
plots_pre = list()

# plots_pre: plot of responder vs nonresponder in post-test eeg
plots_post = list()

# plots_responder: plot of pretest vs posttest eeg in responder
plots_responder = list()

total_mean_pre <- filter(total_mean, group=='pre')
total_mean_post <- filter(total_mean, group=='post')
total_se_pre <- filter(total_se, group=='pre')
total_se_post <- filter(total_se, group=='post')

total_mean_responder <- filter(total_mean, response=='responder')
total_se_responder <- filter(total_se, response=='responder')

# plots_pre
for (i in 1:20){  
  df <- data.frame(x=c(1:14),
                   y=total_mean_pre[paste('V',toString(i),sep='')][[1]], 
                   ymin=total_mean_pre[paste('V',toString(i),sep='')][[1]]-total_se_pre[paste('V',toString(i),sep='')][[1]],
                   ymax=total_mean_pre[paste('V',toString(i),sep='')][[1]]+total_se_pre[paste('V',toString(i),sep='')][[1]], 
                   group=total_mean_pre['response'][[1]])
  pd <- position_dodge(0.1)
  plots_pre[[i]] <- ggplot(df,aes(x=x,y=y,colour=group)) + geom_point() + geom_errorbar(aes(ymin=ymin, ymax=ymax))+ ggtitle(paste('channel_',toString(i),sep=''))
}

# plots_post
for (i in 1:20){  
  df <- data.frame(x=c(1:14),
                   y=total_mean_post[paste('V',toString(i),sep='')][[1]], 
                   ymin=total_mean_post[paste('V',toString(i),sep='')][[1]]-total_se_post[paste('V',toString(i),sep='')][[1]],
                   ymax=total_mean_post[paste('V',toString(i),sep='')][[1]]+total_se_post[paste('V',toString(i),sep='')][[1]], 
                   group=total_mean_post['response'][[1]])
  pd <- position_dodge(0.1)
  plots_post[[i]] <- ggplot(df,aes(x=x,y=y,colour=group)) + geom_point() + geom_errorbar(aes(ymin=ymin, ymax=ymax))+ ggtitle(paste('channel_',toString(i),sep=''))
}

# plots_responder
for (i in 1:20){  
  df <- data.frame(x=c(1:14),
                   y=total_mean_responder[paste('V',toString(i),sep='')][[1]], 
                   ymin=total_mean_responder[paste('V',toString(i),sep='')][[1]]-total_se_responder[paste('V',toString(i),sep='')][[1]],
                   ymax=total_mean_responder[paste('V',toString(i),sep='')][[1]]+total_se_responder[paste('V',toString(i),sep='')][[1]], 
                   group=total_mean['group'][[1]])
  pd <- position_dodge(0.1)
  plots_responder[[i]] <- ggplot(df,aes(x=x,y=y,colour=group)) + geom_point() + geom_errorbar(aes(ymin=ymin, ymax=ymax)) + ggtitle(paste('channel_',toString(i),sep=''))
}

# arrange multiple plots in place
page_pre_responder <- marrangeGrob(plots_pre, ncol = 5, nrow=4, top = 'pre-trial, responder vs nonresponder')
page_post_responder <- marrangeGrob(plots_post, ncol = 5, nrow=4, top = 'post-trial, responder vs nonresponder')
page_responder <- marrangeGrob(plots_responder, ncol = 5, nrow=4, top = 'responder, pre-trial vs post-trial')

# save plots
ggsave("fig2_pre_responder_nonresponder.png", page_pre_responder, width=24, height = 16, dpi = 300)
ggsave("fig3_post_responder_nonresponder.png", page_post_responder, width=24, height = 16, dpi = 300)
ggsave("fig4_responder_pre_post.png", page_responder, width=24, height = 16, dpi = 300)