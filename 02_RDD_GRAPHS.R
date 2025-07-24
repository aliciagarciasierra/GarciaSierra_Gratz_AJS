
###############################################
######## RDD GRAPHS   #####################
###############################################

# a. Create a dummy for those staying in school until age 15 at least
# in the original "raedyrs_e" 1 stands for having left education at age 14 or under,
# 2 age 15, 3 age 16, 4 age 17, 5 age 18, 6 age 19 or over

table(data$raedyrs_e)
data$age15<-data$raedyrs_e
data$age15[data$age15<=1]<-0 # age 14
data$age15[data$age15>=2]<-1 # age 15 at least
table(data$age15)

# b. Compute the fraction staying in school until age 15 
fraction_by_cohort <- data%>%
  group_by(rabyear) %>%
  summarise(fraction_age15 = mean(age15, na.rm=T))


# c. Plot the graph (first polynomial): FIGURE 1
plotedu<-rdplot(fraction_by_cohort$fraction_age15, fraction_by_cohort$rabyear, c = 1933,p=1 ,
                y.label="Fraction staying in school until age 15", 
                x.label="Year of birth",
                binselect = "qsmvpr",
                y.lim = c(0, 1),
                col.lines = "red",
                 title = "")

