
########################################################################
######## ROBUSTNESS TESTS ############################################## 
########################################################################


######################################################################
######## TABLE A2. MAIN MODELS WITHOUT INTERACTIONS ############################### 
######################################################################

######################## EDUCATION ############################## 
m1<- feols(yoe~ treatment + pgeducation + ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             male,
           data=data)
etable(m1, cluster="timetrend", digits = "r3",signif.code = signif_codes) 


######################## INCOME ############################## 
m2<- feols(incomelog~ treatment + pgeducation + ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             male,
           data=data)
etable(m2, cluster="timetrend", digits = "r3",signif.code = signif_codes) 

######################## WEALTH ############################## 
m3<- feols(wealthlog~ treatment + pgeducation + ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             male,
           data=data)
etable(m3, cluster="timetrend", digits = "r3",signif.code = signif_codes) 


#####################################################################
######## TABLE A3. OPTIMAL BANDWIDTH  ###############################
#####################################################################

########################## EDUCATION ################################

# First, we select the optimal bandwidth using the function "rdbwselect" 

data$bandwidth<-NULL
bwsyoe <- rdbwselect(data$yoe,data$rabyear, p=1, c = 1933,
                  all=TRUE)
bwsyoe$bws
optimalbw<-bwsyoe$bws[1, 3] # we select the the mserd version bias corrected
print(optimalbw)

# Second, we filter the sample to account for those years within the optimal bandwidth
# (i.e. the cutoff point (1933) +,- the optimal bw)

optimal <- data %>%
  filter(rabyear >= 1927 & rabyear <= 1938)

# Third, we rerun the models with this new sample

m1<- feols(yoe~ treatment*pgeducation*ses +
          pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
          timetrend +
          male
        , data=optimal)
etable(m1, cluster="timetrend", digits = "r3", signif.code = signif_codes)


########################## INCOME ################################

# First, we select the optimal bandwidth using the function "rdbwselect" 

data$bandwidth<-NULL
bwsincome <- rdbwselect(data$incomelog,data$rabyear, p=1, c = 1933,
                     all=TRUE)
bwsincome$bws
optimalbw<-bwsincome$bws[1, 3] # we select the the mserd version bias corrected
print(optimalbw)

# Second, we filter the sample to account for those years within the optimal bandwidth
# (i.e. the cutoff point (1933) +,- the optimal bw)

optimal <- data %>%
  filter(rabyear >= 1927 & rabyear <= 1938)

# Third, we rerun the models with this new sample

m2<- feols(incomelog~ treatment*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              male
            , data=optimal)
etable(m2, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


########################## WEALTH ################################

# First, we select the optimal bandwidth using the function "rdbwselect" 

data$bandwidth<-NULL
bwswealth <- rdbwselect(data$wealthlog,data$rabyear, p=1, c = 1933,
                        all=TRUE)
bwswealth$bws
optimalbw<-bwswealth$bws[1, 3] # we select the the mserd version bias corrected
print(optimalbw)

# Second, we filter the sample to account for those years within the optimal bandwidth
# (i.e. the cutoff point (1933) +,- the optimal bw)

optimal <- data %>%
  filter(rabyear >= 1927 & rabyear <= 1938)

# Third, we rerun the models with this new sample

m3<- feols(wealthlog~ treatment*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              male
            , data=optimal)
etable(m3, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


#####################################################################
######## TABLE A4. PLACEBO TESTS CHANGING THE CUTOFF ################
#####################################################################

############################ CUTOFF 1928 ##########################

# Change treatment to 1928 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1928]<-1
data$treatmentplacebo[data$rabyear<1928]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)  #check it's a factor

# Model education
m1 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m1, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m2 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m2, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m3 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m3, cluster="timetrend", digits = "r3",  signif.code = signif_codes)



############################ CUTOFF 1930 ##########################

# Change treatment to 1930 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1930]<-1
data$treatmentplacebo[data$rabyear<1930]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)

# Model education
m4 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m4, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m5 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m5, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m6 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m6, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

############################ CUTOFF 1936 ##########################

# Change treatment to 1936 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1936]<-1
data$treatmentplacebo[data$rabyear<1936]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)

# Model education
m7 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m7, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m8 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m8, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m9 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m9, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


############################ CUTOFF 1938 ##########################

# Change treatment to 1938 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1938]<-1
data$treatmentplacebo[data$rabyear<1938]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)

# Model education
m10 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m10, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m11 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m11, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m12 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m12, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

######################################################################
######## TABLE A5. GENDER DIFFERENCES  ############################### 
######################################################################

#------------------ PANEL A -------------------------#

######################## EDUCATION ############################## 

# Filter dataset to separeate both genders
data_female <- data %>% filter(male == 0)
data_male <- data %>% filter(male == 1)

# Fit the models
m1_female <- feols(yoe ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_female)


m1_male <- feols(yoe ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_male)

# Display the results
etable(m1_female, cluster = "timetrend", digits = "r3",signif.code = signif_codes)
etable(m1_male, cluster = "timetrend", digits = "r3", signif.code = signif_codes)

######################## INCOME ############################## 


# Fit the models
m2_female <- feols(incomelog ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_female)


m2_male <- feols(incomelog ~ treatment * pgeducation * ses +
                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                   timetrend +
                   timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                   timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                   timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                   timetrend * pgeducation +
                   timetrend * treatment +
                   timetrend * ses ,
                 data = data_male)

# Display the results
etable(m2_female, cluster = "timetrend", digits = "r3",signif.code = signif_codes)
etable(m2_male, cluster = "timetrend", digits = "r3",signif.code = signif_codes) 

############################## WEALTH ####################################

# Fit the models
m3_female <- feols(wealthlog ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_female)


m3_male <- feols(wealthlog ~ treatment * pgeducation * ses +
                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                   timetrend +
                   timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                   timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                   timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                   timetrend * pgeducation +
                   timetrend * treatment +
                   timetrend * ses ,
                 data = data_male)

# Display the results
etable(m3_female, cluster = "timetrend", digits = "r3",signif.code = signif_codes)
etable(m3_male, cluster = "timetrend", digits = "r3",signif.code = signif_codes) 

#------------------ PANEL B -------------------------#

m1_interaction <- feols(yoe ~ treatment  * male *   pgeducation +ses + 
                          pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +  
                          timetrend +  
                          timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +  
                          timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +  
                          timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +  
                          timetrend * pgeducation +  
                          timetrend * treatment +  
                          timetrend * ses,  
                        data = data)
etable(m1_interaction, cluster = "timetrend", digits = "r3", signif.code = signif_codes)

m2_interaction <- feols(incomelog~ treatment  * male *   pgeducation +ses + 
                          pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +  
                          timetrend +  
                          timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +  
                          timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +  
                          timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +  
                          timetrend * pgeducation +  
                          timetrend * treatment +  
                          timetrend * ses,  
                        data = data)
etable(m2_interaction, cluster = "timetrend", digits = "r3", signif.code = signif_codes)



m3_interaction <- feols(wealthlog~ treatment  * male *   pgeducation +ses + 
                          pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +  
                          timetrend +  
                          timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +  
                          timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +  
                          timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +  
                          timetrend * pgeducation +  
                          timetrend * treatment +  
                          timetrend * ses,  
                        data = data)
etable(m3_interaction, cluster = "timetrend", digits = "r3", signif.code = signif_codes)

#####################################################################
######## FIGURE A4: THREE WAY INTERACTION FOR MALES ##################
#####################################################################

a<-plot_model(m3_male, type = "pred", terms = c("pgeducation[-3:3]","ses[-1.1426, -0.2473, 0.7228]", "treatment[0,1]"))

a<- a+ theme_pilot(axis_title_size = 11,
                   axis_text_size = 11,
                   legend_text_size = 12,
                   legend_title_size = 12,
                   legend_position= "right") +
  labs(x= "PGI",
       y = "Predicted income") +
  ggtitle(" ") +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  scale_color_manual(name = " ", values = line_colors, labels = c("Low SES", "Medium SES", "High SES")) +
  scale_fill_manual(name = " ", values = ribbon_colors, labels = c("Low SES", "Medium SES", "High SES")) 

a$data$facet<- ifelse(a$data$facet== 0, "Not exposed to the reform",
                      ifelse(a$data$facet== 1, "Exposed to the reform",NA))

# Relevel the facet variable to ensure correct order and labeling
a$data$facet <- factor(a$data$facet, levels = c("Not exposed to the reform", "Exposed to the reform"))

# Adjust size facet titles
a <- a +
  theme(
    strip.text = element_text(size = 14) # Adjust the size as needed
  )

a


################################################################################
############## TABLE A7: BINARY EDUCATION LEVEL  ##########################
################################################################################

# Select variables including tertiary binary variable 

myvars<-c("idauniq", # id
          "rabyear", # year of birth
          "timetrend", #time trend
          "treatment", #treatment
          "yoe",  "incomelog", "wealthlog",  #outcomes 
          "tertiary",
          "pgeducation", "pgeducationgroups", # PGI
          "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10", # principal components
          "male", "ses", # ascribed charactersitics
          "raedyrs_e") #raw years of education for RDD graphs


# Extract variables of interest
data_tert<-data_complete[myvars]

# Filter to have complete observations
data_tert <- data_tert[complete.cases(data_tert),] # final N should be 1922

# Standardize selected variables
vars_to_standardize <- c("pgeducation", "ses", "yoe", "incomelog", "wealthlog")

# Apply scaling to the selected variables
data_tert[vars_to_standardize] <- scale(data_tert[vars_to_standardize])

# Model

m1<- glm(tertiary~ treatment*pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
             timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
             timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
             timetrend*pgeducation +
             timetrend*treatment +
             timetrend*ses +
             male,
         family = binomial(link = "logit")
           , data=data_tert)

summary(m1, cluster="timetrend", digits = "r3",signif.code = signif_codes) 


# Compute Average Marginal Effects
ames_inter<- slopes(m1, variables="pgeducation", by="treatment")
ames_inter

######################################################################
######## TABLE A12. SCARR-ROWE TEST ############################### 
######################################################################

######################## EDUCATION ############################## 
m1<- feols(yoe~ treatment + pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             male,
           data=data)
etable(m1, cluster="timetrend", digits = "r3",signif.code = signif_codes) 

####################x#### INCOME ############################## 
m2<- feols(incomelog~ treatment + pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             male,
           data=data)
etable(m2, cluster="timetrend", digits = "r3",signif.code = signif_codes) 

######################## WEALTH ############################## 
m3<- feols(wealthlog~ treatment + pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             male,
           data=data)
etable(m3, cluster="timetrend", digits = "r3",signif.code = signif_codes) 

#########################################################################
######## FIGURE A5: AVERAGE PGI BY COHORT, SELECTION INTO DYING  ########
#########################################################################

# Calculate mean and 95% confidence intervals by cohort
cohort_data <- data %>%
  group_by(rabyear) %>%
  summarise(mean_pgeducation = mean(pgeducation, na.rm = TRUE),
            ci_lower = tidy(t.test(pgeducation, na.action = na.omit))$conf.low,
            ci_upper = tidy(t.test(pgeducation, na.action = na.omit))$conf.high)

# View the resulting cohort_data data frame
cohort_data

# Plot
ggplot(data = cohort_data, aes(x = rabyear, y = mean_pgeducation)) +
  geom_line(color = "royalblue1") +  # Line plot
  geom_point(color = "royalblue1", size = 3) +  # Points for each cohort
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, color = "royalblue1", alpha=0.5) +  
  labs(title = " ",
       x = "Year of birth",
       y = "Average PGI") +
  scale_x_continuous(breaks = seq(min(data$rabyear), max(data$rabyear), by = 2)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "bottom") # Colors for the three groups

#########################################################################
######## TABLE A6. Results accounting for mortality selection IPW  ########
#########################################################################

# Prepare the variables for the weights
data_complete <- data_complete %>%
  rename(
    selfhealth = r1shlt,   # self-rated health
    limitwork = r2hlthlm,  # limits to do work
    dailylimit = r1walkra, # functional limits in daily activities
    mobprob = r1mobilb,    # difficulties in mobility
    depressed = r1depres,  # mental health problems
    diagnoses = r1hibpe,   # Doctor Diagnosed Health Problems: Ever Have Condition
    alzheimer = r1alzhe,   # Doctor Diagnosed Health Problems: Memory-Related Disease
    smoking = r1smokev,    # Health Behaviors: Smoking (Cigarettes)
    diabetes = r2diabs     # Reported diabetes
  )

# Select variables of interest #

myvars<-c("idauniq", "radyear", "ses", "selfhealth", "limitwork", "dailylimit", "mobprob", "depressed", "diagnoses", "alzheimer", "smoking", "diabetes")
ipw<-data_complete[myvars]

# Create treatment variable for those who die before 2013 (when PGIs are obtained)
# We assume that if it hasn't been reported by 2012 the year of death is that they were still alive (even if in the data it figures as NA because the question is year of death)
ipw<- ipw %>% 
  mutate(dummy = if_else(!is.na(radyear), 1, 0, missing = 0))

# Merge with existent database #

data_ipw<-merge(data, ipw, by=c("idauniq"),all.x=TRUE)
data_ipw <- data_ipw %>% select(-radyear)

# Get the weights using the confounders

data_ipw <- data_ipw[complete.cases(data_ipw), ]

weights_weightit <- weightit(dummy ~ ses.x + selfhealth + limitwork +
                               dailylimit + mobprob + diagnoses +
                               alzheimer + smoking + diabetes,
                             data = data_ipw, method = "ps")

summary(weights_weightit$weights)

# Merge the weights to the existent data #
data_weights<- data_ipw %>% 
  mutate(ipw = weights_weightit$weights)

# Models #
m1<- lm(yoe~ pgeducation*treatment + 
          pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
          timetrend +male,
            weights=ipw,
           data=data_weights)

summary(m1, cluster="timetrend", digits = "r3")

m2<- lm(incomelog~ pgeducation*treatment + 
          pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
          timetrend + male,
        weights=ipw,
        data=data_weights)

summary(m2, cluster="timetrend", digits = "r3")


m3<- lm(wealthlog~ pgeducation*treatment + 
          pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
          timetrend + male,
        weights=ipw,
        data=data_weights)

summary(m3, cluster="timetrend", digits = "r3")
 
###############################################################################
######## TABLE A9. Non-linear effects for income and wealth ############################### 
###############################################################################

# GLM  non-linear modeling for income
m1 <- glm(
  incomelog ~ pgeducation * treatment + I(pgeducation^2)*treatment +
    pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
    timetrend +
    timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
    timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
    timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
    timetrend * pgeducation +
    timetrend * treatment,
  family = gaussian(link = "identity"),
  data = data
)
summary(m1, cluster="timetrend", digits = "r3")

# GLM  non-linear modeling for wealth
m2 <- glm(
  wealthlog ~ pgeducation * treatment + I(pgeducation^2)*treatment +
    pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
    timetrend +
    timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
    timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
    timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
    timetrend * pgeducation +
    timetrend * treatment,
  family = gaussian(link = "identity"),
  data = data
)
summary(m2)


###############################################################################
######## Table A10: Results with non linear time trends ############################### 
###############################################################################


m1 <- feols(yoe ~ treatment*pgeducation*ses +
              pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
              timetrend + I(timetrend^2) + I(timetrend^3) +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatment +
              timetrend*ses +
              male,
            data = data)
etable(m1, cluster="timetrend", digits = "r3",signif.code = signif_codes )


m2 <- feols(incomelog ~ treatment*pgeducation*ses +
              pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
              timetrend + I(timetrend^2) + I(timetrend^3) +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatment +
              timetrend*ses +
              male,
            data = data)
etable(m2, cluster="timetrend", digits = "r3",signif.code = signif_codes)



m3<- feols(wealthlog ~ treatment*pgeducation*ses +
              pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
              timetrend + I(timetrend^2) + I(timetrend^3) +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatment +
              timetrend*ses +
              male,
            data = data)
etable(m3, cluster="timetrend", digits = "r3",signif.code = signif_codes)

###############################################################################
######## Table A11: Including 1933 ############################### 
###############################################################################

# Filter for alternative treatments

# Select variables
myvars<-c("idauniq", # id
          "rabyear", # year of birth
          "timetrend", #time trend
          "alt_treatment", #treatment alt
          "yoe",  "incomelog", "wealthlog",  #outcomes 
          "pgeducation", "pgeducationgroups", # PGI
          "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10", # principal components
          "male", "ses", # ascribed charactersitics
          "raedyrs_e") #raw years of education for RDD graphs


# Extract variables of interest
data_1933<-data_complete[myvars]

# Filter to have complete observations
data_1933<- data_1933[complete.cases(data_1933),] 

# Standardize selected variables
vars_to_standardize <- c("pgeducation", "ses", "yoe", "incomelog", "wealthlog")

# Apply scaling to the selected variables
data_1933[vars_to_standardize] <- scale(data_1933[vars_to_standardize])


m1<- feols(yoe~ alt_treatment*pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
             timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
             timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
             timetrend*pgeducation +
             timetrend*alt_treatment +
             timetrend*ses +
             male,
           data=data_1933)

etable(m1, cluster="timetrend", digits = "r3",signif.code = signif_codes)


m2<- feols(incomelog~ alt_treatment*pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
             timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
             timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
             timetrend*pgeducation +
             timetrend*alt_treatment +
             timetrend*ses +
             male,
           data=data_1933)

etable(m2, cluster="timetrend", digits = "r3",signif.code = signif_codes)

m3<- feols(wealthlog~ alt_treatment*pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
             timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
             timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
             timetrend*pgeducation +
             timetrend*alt_treatment +
             timetrend*ses +
             male,
           data=data_1933)

etable(m3, cluster="timetrend", digits = "r3",signif.code = signif_codes)

