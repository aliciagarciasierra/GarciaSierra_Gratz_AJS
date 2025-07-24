
###################################################################
############### TABLE 2: WEALTH ####################################
###################################################################

###############################################################
############### First model: only genetic factors ############### 
###############################################################

m7 <- feols(wealthlog~ pgeducation*treatment + 
           pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
           timetrend +
           timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
           timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
           timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
           timetrend*pgeducation +
           timetrend*treatment,
           data=data)

etable(m7, cluster="timetrend", digits="r3", signif.code = signif_codes)

############################################################################
############### Second model: only ascribed characteristics ############### 
############################################################################

m8<- feols(wealthlog~ treatment*ses+
          timetrend*ses + 
          timetrend*treatment + 
          male, data=data)

etable(m8, cluster="timetrend", digits="r3", signif.code = signif_codes)

#######################################################################################
############### Third model: genetic factors and ascribed characteristics ############### 
#######################################################################################

m9<- feols(wealthlog~ treatment*pgeducation*ses +
          pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
          timetrend +
          timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
          timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
          timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
          timetrend*pgeducation +
          timetrend*treatment +
          timetrend*ses +
          male
        , data=data)

etable(m9, cluster="timetrend", digits="r3", signif.code = signif_codes)


