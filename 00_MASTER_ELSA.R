

##############################################################
### Does an Educational Reform Moderate the Relationship #####
########## between Genes and Life Chances? ###################
######### Evidence from the United Kingdom ###################
##############################################################

# Authors: Alicia García-Sierra & Michael Grätz

# Data sets needed
   # a) ELSA harmonized data set
   # b) Wave 3 ELSA life history
   # c) Polygenic Scores data from ELSA (2022)
   # d) Principal components data from ELSA (2022)
# All available to download from the UK Data Service 

# Date of preparation of this script: Nov 2024

#######################################################
#########   PREPARE THE ENVIRONMENT ################
######################################################

# CLEAN

rm(list=ls()) 

# LOAD PACKAGES

library(tidyverse)
library(haven)
library(stringr)
library(tidyr)
library(labelled)
library(readxl)
library(lme4)
library(fixest)
library(margins)
library(ggeffects)
library(rdrobust)
library(sandwich)
library(lmtest)
library(gridExtra)
library(cowplot)
library(sjPlot)
library(sjmisc)
library(broom)
library(marginaleffects)
library(scales)
library(WeightIt)
library(patchwork)
library(ragg)
library(cowplot)
library(effects)


# SET WD 

setwd("~/Documents/EDUCATIONAL REFORMS ELSA/DATA")

# SET SIGNIFICANCE LEVELS MARKS

signif_codes <- c( "***" = 0.01, "**" = 0.05, "*" = 0.1)

