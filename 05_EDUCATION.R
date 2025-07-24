
###################################################################
############### TABLE 2: YEARS OF EDUCATION #########################
###################################################################

###############################################################
############### First model: only genetic factors ############### 
###############################################################


m1 <- feols(yoe~ pgeducation*treatment + 
           pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
           timetrend +
           timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
           timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
           timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
           timetrend*pgeducation +
           timetrend*treatment,
           data=data)

etable(m1, cluster="timetrend", digits = "r3", signif.code = signif_codes)


############################################################################
############### Second model: only ascribed characteristics ############### 
############################################################################

m2<- feols(yoe~ treatment*ses+
          timetrend*ses + 
          timetrend*treatment + 
          male, data=data)

etable(m2, cluster="timetrend", digits = "r3", signif.code = signif_codes)


#######################################################################################
############### Third model: genetic factors and ascribed characteristics ############### 
#######################################################################################


m3<- feols(yoe~ treatment*pgeducation*ses +
          pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
          timetrend +
          timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
          timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
          timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
          timetrend*pgeducation +
          timetrend*treatment +
          timetrend*ses +
          male,
          data=data)

etable(m3, cluster="timetrend", digits = "r3", signif.code = signif_codes)


