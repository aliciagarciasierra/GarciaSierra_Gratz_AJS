
#################################################
######## OPEN DATA ##############################
#################################################

######### MAIN DATASET #################

# harmonized dataset (provided by ELSA)
elsa <- read_dta("h_elsa_g3.dta") 

#######  PGI data ##################

# polygenic index data (provided by ELSA)
pgi <- read_dta("list_pgs_scores_elsa_2022.dta")
 
# Merge

merged<-merge(elsa, pgi, by="idauniq")

###### PRINCIPAL COMOPONENTS data ##################

# principal components data associated to PGIs (provided by ELSA)
pcs <- read_dta("principal_components_elsa_2022.dta")

# Merge

merged<-merge(merged, pcs, by="idauniq")

####### CHILDHOOD RECALL from third wave ##################

# wave 3 information on life histories (provided by ELSA)
childhood <- read_dta("wave_3_life_history_data.dta")

# Merge

merged<-merge(merged, childhood, by="idauniq", all.x=TRUE)

# Rename

data<-merged


#################################################################
######## CREATE TREATMENT and RUNNING VARIABLE ##################
#################################################################

# Year of birth

table(data$rabyear)

# Running variable (time trend) centered around its mean
data$timetrend <- data$rabyear - mean(data$rabyear, na.rm = TRUE)

# Treated cohorts: those who are born after 1934 (included)

data$treatment<-NA
data$treatment[data$rabyear>=1934 & data$rabyear<=1944]<-1
data$treatment[data$rabyear<=1932 & data$rabyear>=1922]<-0
table(data$treatment)
data$treatment<-as.factor(data$treatment)

# Alternative Treatment 1 for robustness (including 1933 in treated)

data$alt_treatment<-NA
data$alt_treatment[data$rabyear>=1933 & data$rabyear<=1944]<-1 #treated
data$alt_treatment[data$rabyear<=1932 & data$rabyear>=1922]<-0
table(data$alt_treatment)
data$alt_treatment<-as.factor(data$alt_treatment)


#########################################################
######## CLEAN OUTCOME VARIABLES ########################
#########################################################

######## EDUCATION #######

# Years of education

table(data$raedyrs_e) # original variable years of education

# Information on the process of assignation of years of education
# can be found in Table S1 of the Supplementary Materials

data <- data %>%
  mutate(yoe = case_when(
    raedyrs_e == 0 ~ NA_real_,  # those who reported none qualification, cannot be assessed (14 individuals)
    raedyrs_e == 1 ~ 9,
    raedyrs_e == 2 ~ 10,
    raedyrs_e == 3 ~ 11,
    raedyrs_e == 4 ~ 12,
    raedyrs_e == 5 ~ 13,
    raedyrs_e == 6 ~ 14,
    TRUE ~ raedyrs_e  
  )) 

# Tertiary education (binary outcome)

table(data$raeduc_e) # original variable 

data$tertiary<-data$raeduc_e
data$tertiary[data$tertiary==1 | data$tertiary==3]<-0 # non-tertiary educated
data$tertiary[data$tertiary==4 | data$tertiary==5]<-1 #tertiary ecucated
table(data$tertiary)


######## INCOME #######

# We average the total income across the available seven waves

data$income<-rowMeans(cbind(data$h1itot, data$h2itot, data$h3itot, data$h4itot, data$h5itot, data$h6itot, data$h7itot), na.rm=TRUE)
summary(data$income)

# Logtransformation to account for the skewed distribution

data$incomelog<-log(data$income+1, base=exp(1))

# Exploration of the variable
summary(data$incomelog)

######## WEALTH #######

# Average of the seven waves with available information

data$wealth<-rowMeans(cbind(data$h1atotb, data$h2atotb, data$h3atotb, data$h4atotb, data$h5atotb, data$h6atotb, data$h7atotb), na.rm=TRUE)

# Explore variable

summary(data$wealth)
hist(data$wealth)

#Log transformation for skewed distribution 

# Note that we have to remove negative values to implement the log
# i.e., we shift the whole distribution to have a minimum value of 0.

# Check the minimum value in the distribution
min_value <- min(data$wealth, na.rm=TRUE)

# Shift the data 
if(min_value <= 0) {
  data$wealth <- data$wealth - min_value + 1
}

# Apply log transformation
data$wealthlog<-log(data$wealth+1, base=exp(1))

# Explore new distribution
hist(data$wealthlog)
summary(data$wealthlog)


##################################################
######## PGI  ########################
##################################################

# PGI Educational Attainment

# Explore the variable
summary(data$EA_3)

# Rename the variable
data$pgeducation<-data$EA_3
summary(data$pgeducation) # check

# Standardize PGI for an easier interpretation
data$mean_pgeducation <- mean(data$pgeducation)
data$pgeducation  <- (data$pgeducation  - data$mean_pgeducation) / sd(data$pgeducation, na.rm=TRUE)
summary(data$pgeducation)
sd(data$pgeducation)

# Create categorical variable of groups of ability 
# This variable will only be used for plotting purposes
data <- data %>%
  mutate(pgeducationgroups = as.factor(ntile(pgeducation, 3)))
table(data$pgeducationgroups)


##################################################
######## ASCRIBED CHARACTERISTICS ########################
##################################################

########## GENDER: male dummy #########

# Explore 
table(data$ragender)

# Rename
data$male <- data$ragender

# Recode to have women as the reference category (0)
data$male[data$rage==2]<-0

# Check
table(data$male)

########## MIGRATION / ETHNICITY (note that only 4 respondents are non-white) ######### 

table(data$raracem) # coded by ELSA as 1 white, 4 non-white

########## SOCIOECONOMIC BACKGROUND #########

# We use Principal Components Analysis to create an index 
# that summarizes respondent's SES during childhood 
# (info from Wave 3 life history surveys):

# Variables of interest:

# 1. Own a house (family during childhood)
table(data$raown)
data$owners<-data$raown
data$owners[data$raown<0]<-NA #non-available responses
data$owners[data$owners!=1]<-0 #arrangement different from owning
table(data$owners) # 1 owned the house

#2. Number of rooms in the childhood house
table(data$raroo)
data$rooms<-data$raroo
data$rooms[data$rooms<0]<-NA
table(data$rooms) # N of rooms

#3. Facilities in the childhood house
table(data$rafac1)
data$rafac1[data$rafac1<=-1]<-NA
table(data$rafac1) # 1 if available facility

table(data$rafac2)
data$rafac2[data$rafac2<=-1]<-NA
table(data$rafac2)

table(data$rafac3)
data$rafac3[data$rafac3<=-1]<-NA
table(data$rafac3)

table(data$rafac4)
data$rafac4[data$rafac4<=-1]<-NA
table(data$rafac4)

table(data$rafac5)
data$rafac5[data$rafac5<=-1]<-NA
table(data$rafac5)

table(data$rafac6)
data$rafac6[data$rafac6<=-1]<-NA
table(data$rafac6)

# Combine how many of the former facilities are available by summing them
data$facilities<-NA
data$facilities<-rowSums(cbind(data$rafac1, data$rafac2, data$rafac3, data$rafac4, data$rafac5, data$rafac6), na.rm=TRUE)
table(data$facilities) # N of facilities

#4. Unemployed parents before she turned 16:
table(data$rsunemp)
data$unemp<-data$rsunemp
data$unemp[data$unemp<0]<-NA
data$unemp[data$unemp==2]<-0
table(data$unemp) # 1 if unemployed

#5. Number of books (family during childhood)

table(data$rabks)
data$books<-data$rabks
data$books[data$books<0]<-NA
table(data$books) # N of books

####  Construct Principal Component ###

#Select variables
myvars<-c("idauniq", "books", "unemp", "facilities", "rooms", "owners")

#Extract the variables
pcdata<-data[myvars]

#Transform to numeric and remove NAs
pcdata <- mutate_all(pcdata, as.numeric)
pcdata<-na.omit(pcdata)

# Perform PCA
pca_result <- prcomp(cbind(pcdata$books, pcdata$unemp, pcdata$facilities, pcdata$rooms, pcdata$owners), scale. = TRUE)
pca_result$sdev^2/sum(pca_result$sdev^2)

# Extract principal component scores
pc_scores <- pca_result$x

# Add principal components to the data
data_with_pcs <- cbind(pcdata, pc_scores)

# Extract first component
myvars<-c("idauniq", "PC1")
pcs<-data_with_pcs[myvars]

#Rename first component to SES
pcs<-pcs %>% 
  rename(ses = PC1)

# Center SES around 0
pcs$ses<- scale(pcs$ses)
summary(pcs$ses)

# Merge with data

merged <- merge(data, pcs, by="idauniq", all.x=TRUE)
data_complete<-merged

###############################################################
############### SELECT FINAL SAMPLE  ##########################
###############################################################

# Select variables
myvars<-c("idauniq", # id
          "rabyear", # year of birth
          "timetrend", #time trend
          "treatment", #treatment
          "yoe",  "incomelog", "wealthlog",  #outcomes 
          "pgeducation", "pgeducationgroups", # PGI
          "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10", # principal components
          "male", "ses", # ascribed charactersitics
          "raedyrs_e") #raw years of education for RDD graphs


# Extract variables of interest
data<-data_complete[myvars]

# Filter to have complete observations
data <- data[complete.cases(data),] # final N should be 2132

#############################################################################
############### Standardize variables of interest ##########################
#############################################################################

# Standardize selected variables
vars_to_standardize <- c("pgeducation", "ses", "yoe", "incomelog", "wealthlog")

# Apply scaling to the selected variables
data[vars_to_standardize] <- scale(data[vars_to_standardize])

# Check the result
summary(data[vars_to_standardize])

# Check the variables independently
summary(data$pgeducation)
