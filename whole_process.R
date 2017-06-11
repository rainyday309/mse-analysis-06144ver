# whole_process.R

# process.r -> process1.R : summarize data
# required output: for table summarizing 
# table 1:用z_completer_desc和z_non_completer_desc整理

# process.r -> process2.r : 
# get subitem of HAMD and cgi into data.frame

# process.r -> process_segment_mse.R -> process_linear0.R,process_linear1.r,process_linear2.R
# get linear model results

# process.r -> process_t_test_func.R -> process_asymmetry.R
# show asymmetry results

# 考慮把remission和non-remission分開，看各自mse前後的特徵
# process.r -> process_t_test_func.R 
# process_t(remission)
# process_t(non_remission)
# 各自畫圖出來
# 把effect size change畫出來

# process.r -> process_segment_mse.R -> process1.R : summarize data
# 把分段之後的mse平均接回去，然後再比較 
# table 1:用z_completer_desc和z_non_completer_desc整理

# process.r -> process_segment_mse_func.R
remission_segmented <- segment_topo(remission)
non_remission_segmented <- segment_topo(non_remission)
responder_segmented <- segment_topo(responder)
non_responder_segmented <- segment_topo(non_responder)