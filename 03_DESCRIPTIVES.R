
######################################################################################################################
##########  Table 1. Association between the outcomes of interest and the educational reform.  #################
######################################################################################################################

m1 <- feols(yoe~treatment + i(timetrend), data=data)
etable(m1, digits = "r3")
m2 <- feols(incomelog~treatment + i(timetrend), data=data)
etable(m2, digits = "r3")
m3 <- feols(wealthlog~treatment + i(timetrend), data=data)
etable(m3, digits = "r3")

#######################################################################
##########  CORRELATION MATRIX FOR TABLE A1 ##########################
######################################################################

# Select the variables of interest
vars_of_interest <- data[, c("yoe", "incomelog", "wealthlog", "ses")]

# Calculate the correlation matrix and round to two decimals
cor_matrix <- round(cor(vars_of_interest, use = "complete.obs"), 2)

# Print the correlation matrix
print(cor_matrix)

###################################################################################################
################# FIGURE PGI coefficients on education pre and post trend ##################################
###################################################################################################


# Select variables
myvars<-c("idauniq", # id
          "rabyear", # year of birth
          "pgeducation", #PGI
          "yoe",  "incomelog", "wealthlog",
          "ses") #raw years of education for RDD graphs

# Extract variables of interest
data_figures<-data_complete[myvars]

# Filter to have complete observations
data_figures <- data_figures[complete.cases(data_figures),] 

# Filter for the years of interest
data_filtered <- data_figures %>% filter(rabyear >= 1921 & rabyear <= 1944)

# Establish reference category
data_filtered$rabyear <- relevel(factor(data_filtered$rabyear), ref = "1921")

# Standardize selected variables
vars_to_standardize <- c("pgeducation", "ses", "yoe", "incomelog", "wealthlog")

# Apply scaling to the selected variables
data_filtered[vars_to_standardize] <- scale(data_filtered[vars_to_standardize])

# Run your model
m1 <- feols(yoe ~ pgeducation * factor(rabyear) + 0, data = data_filtered)


# Extract the coefficients and their standard errors
coefs <- broom::tidy(m1)

# Filter for the interaction terms of interest (pgeducation:rabyyear)
coefs <- coefs[grep("pgeducation:factor\\(rabyear\\)", coefs$term), ]
coefs$rabyear <- as.numeric(gsub("pgeducation:factor\\(rabyear\\)", "", coefs$term))

# Calculate 95% confidence intervals
coefs$ci_lower <- coefs$estimate - 1.96 * coefs$std.error
coefs$ci_upper <- coefs$estimate + 1.96 * coefs$std.error

# Add a new column indicating whether the cohort is before or after 1933
coefs$period <- ifelse(coefs$rabyear <= 1932, "Not exposed to the reform", "Exposed to the reform")

# Ensure period is a factor and set the order explicitly
coefs$period <- factor(coefs$period, levels = c("Not exposed to the reform", "Exposed to the reform"))

# Plotting
education <- ggplot(coefs, aes(x = rabyear, y = estimate, color = period, shape = period)) +
  geom_point(alpha = 0.8, size = 3) +  # Larger, more opaque points with distinct shapes
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2) +  # Thicker dashed trend lines
  scale_color_manual(values = c("coral2", "skyblue2"),  # Keep color mapping but reordered
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_shape_manual(values = c(16, 17),  # Keep shape mapping but reordered
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_x_continuous(breaks = seq(min(coefs$rabyear), max(coefs$rabyear), by = 1)) +  # Yearly x-axis ticks
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25)) +  # Cleaner y-axis ticks
  labs(x = "Year of birth", y = "PGI coefficient", 
       title = "Education",
       color = " ", shape = " ") +  # Set one legend title
  guides(color = guide_legend(order = 1),  # Ensures color & shape legend are ordered together
         shape = guide_legend(order = 1)) +  
  theme_pilot(axis_title_size = 10,
              axis_text_size = 10,
              legend_text_size = 10,
              legend_title_size = 12,
              legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # Reduce space between x-axis title and x-axis text
        axis.title.x = element_text(margin = margin(t = 1, unit = "pt")),
        # Reduce space between y-axis title and y-axis text
        axis.title.y = element_text(margin = margin(r = 1, unit = "pt")))

###################################################################################################
################# FIGURE PGI coefficients on income with pre and post trend ##################################
###################################################################################################

# Filter for the years of interest
data_filtered <- data_figures %>% filter(rabyear >= 1922 & rabyear <= 1944)

# Establish reference category
data_filtered$rabyear <- relevel(factor(data_filtered$rabyear), ref = "1944")

# Standardize selected variables
vars_to_standardize <- c("pgeducation", "ses", "yoe", "incomelog", "wealthlog")

# Apply scaling to the selected variables
data_filtered[vars_to_standardize] <- scale(data_filtered[vars_to_standardize])

# Run your model
m1 <- feols(incomelog ~ pgeducation * factor(rabyear) + 0, data = data_filtered)

# Extract the coefficients and their standard errors
coefs <- broom::tidy(m1)

# Filter for the interaction terms of interest (pgeducation:rabyyear)
coefs <- coefs[grep("pgeducation:factor\\(rabyear\\)", coefs$term), ]
coefs$rabyear <- as.numeric(gsub("pgeducation:factor\\(rabyear\\)", "", coefs$term))

# Add a new column indicating whether the cohort is before or after 1933
coefs$period <- ifelse(coefs$rabyear <= 1932, "Not exposed to the reform", "Exposed to the reform")

# Ensure period is a factor and set the order explicitly
coefs$period <- factor(coefs$period, levels = c("Not exposed to the reform", "Exposed to the reform"))

# Plotting
income <- ggplot(coefs, aes(x = rabyear, y = estimate, color = period, shape = period)) +
  geom_point(alpha = 0.8, size = 3) +  # Larger, more opaque points with distinct shapes
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2) +  # Thicker dashed trend lines
  scale_color_manual(values = c("coral2", "skyblue2"),  # Keep color mapping but reordered
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_shape_manual(values = c(16, 17),  # Keep shape mapping but reordered
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_x_continuous(breaks = seq(min(coefs$rabyear), max(coefs$rabyear), by = 1)) +  # Yearly x-axis ticks
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25)) +  # Cleaner y-axis ticks
  labs(x = "Year of birth", y = "PGI coefficient", 
       title = "Income",
       color = " ", shape = " ") +  # Set one legend title
  guides(color = guide_legend(order = 1),  # Ensures color & shape legend are ordered together
         shape = guide_legend(order = 1)) +  
  theme_pilot(axis_title_size = 10,
              axis_text_size = 10,
              legend_text_size = 10,
              legend_title_size = 12,
              legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # Reduce space between x-axis title and x-axis text
        axis.title.x = element_text(margin = margin(t = 1, unit = "pt")),
        # Reduce space between y-axis title and y-axis text
        axis.title.y = element_text(margin = margin(r = 1, unit = "pt")))



###################################################################################################
################# FIGURE PGI coefficients on wealth with pre and post trend ##################################
###################################################################################################

# Filter for the years of interest
data_filtered <- data_figures %>% filter(rabyear >= 1922 & rabyear <= 1944)

# Establish reference category
data_filtered$rabyear <- relevel(factor(data_filtered$rabyear), ref = "1944")

# Standardize selected variables
vars_to_standardize <- c("pgeducation", "ses", "yoe", "incomelog", "wealthlog")

# Apply scaling to the selected variables
data_filtered[vars_to_standardize] <- scale(data_filtered[vars_to_standardize])

# Run your model
m1 <- feols(wealthlog ~ pgeducation * factor(rabyear) + 0, data = data_filtered)

# Extract the coefficients and their standard errors
coefs <- broom::tidy(m1)

# Filter for the interaction terms of interest (pgeducation:rabyyear)
coefs <- coefs[grep("pgeducation:factor\\(rabyear\\)", coefs$term), ]
coefs$rabyear <- as.numeric(gsub("pgeducation:factor\\(rabyear\\)", "", coefs$term))

# Add a new column indicating whether the cohort is before or after 1933
coefs$period <- ifelse(coefs$rabyear <= 1932, "Not exposed to the reform", "Exposed to the reform")
# Ensure period is a factor and set the order explicitly
coefs$period <- factor(coefs$period, levels = c("Not exposed to the reform", "Exposed to the reform"))

wealth <- ggplot(coefs, aes(x = rabyear, y = estimate, color = period, shape = period)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2) +
  scale_color_manual(values = c("coral2", "skyblue2"),
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_shape_manual(values = c(16, 17),
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_x_continuous(breaks = seq(min(coefs$rabyear), max(coefs$rabyear), by = 1)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25)) +
  labs(x = "Year of birth", y = "PGI coefficient", 
       title = "Wealth",
       color = " ", shape = " ") +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 1)) +  
  theme_pilot(axis_title_size = 10,
              axis_text_size = 10,
              legend_text_size = 10,
              legend_title_size = 12,
              legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # Reduce space between x-axis title and x-axis text
        axis.title.x = element_text(margin = margin(t = 1, unit = "pt")),
        # Reduce space between y-axis title and y-axis text
        axis.title.y = element_text(margin = margin(r = 1, unit = "pt")))

#-------- combine the three figures  --------------------#

# Function to extract the legend 
extract_legend <- function(my_ggplot) {
  # Get the ggplot grob (graphical object)
  tmp <- ggplot_gtable(ggplot_build(my_ggplot))
  
  # Find the legend in the grob
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  # Return the legend if found
  if(length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
    return(legend)
  } else {
    return(NULL)
  }
}

# Extract the legend from the wealth plot
legend <- extract_legend(wealth)

# Combined theme adjustments for each plot
plot_education <- education + 
  theme(
    legend.position = "none",
    plot.margin = margin(b = 4, t = 1.5, l = 2, r = 0, unit = "pt"),
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
  )

plot_income <- income + 
  theme(
    legend.position = "none",
    plot.margin = margin(b = 4, t = 1.5, l = 2, r = 0, unit = "pt"),
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
  )


plot_wealth <- wealth + 
  theme(
    legend.position = "none",
    plot.margin = margin(b = 4, t = 1.5, l = 2, r = 0, unit = "pt"),
    plot.title = element_text(hjust = 0.53, size=14, face="bold"),
  )

# Now combine with zero padding
combined_plot_tight <- gridExtra::grid.arrange(
  plot_education,
  plot_income,
  plot_wealth,
  legend,
  ncol = 1,
  heights = c(1, 1, 1, 0.3),
  padding = 0
)

###################################################################################################
################# FIGURE PGI coefficients on education by SES with pre and post trend ##################################
###################################################################################################

# Filter for the years of interest
data_filtered <- data_figures %>% filter(rabyear >= 1921 & rabyear <= 1944)
# Establish reference category
data_filtered$rabyear <- relevel(factor(data_filtered$rabyear), ref = "1921")

# Create SES groups below and over the mean
mean_ses <- mean(data_filtered$ses, na.rm = TRUE)

data_filtered$ses_group <- cut(data_filtered$ses, 
                               breaks = c(-Inf, mean_ses, Inf),  # Set the breaks as below and above the mean
                               labels = c("Low SES", "High SES"),  # Labels for the groups
                               right = FALSE)  # Including values equal to the mean in the 'Above Mean' group

# Run the model for each SES group and tidy the results
models <- data_filtered %>%
  group_by(ses_group) %>%
  do({
    model <- feols(yoe ~ pgeducation * factor(rabyear) + 0, data = .)
    tidy(model)
  })

# Filter for the interaction terms of interest (pgeducation:rabyyear)
coefs <- models %>%
  filter(grepl("pgeducation:factor\\(rabyear\\)", term))

# Remove rows with NA values in the ses column
coefs <- coefs %>%
  filter(!is.na(ses_group))

# Extract the year from the term name and add a new column indicating the period
coefs$rabyear <- as.numeric(gsub("pgeducation:factor\\(rabyear\\)", "", coefs$term))
coefs$period <- ifelse(coefs$rabyear <= 1932, "Not exposed to the reform", "Exposed to the reform")

# Ensure period is a factor and set the order explicitly
coefs$period <- factor(coefs$period, levels = c("Not exposed to the reform", "Exposed to the reform"))

# Plotting
ggplot(coefs, aes(x = rabyear, y = estimate, color = period, shape = period)) +
  geom_point(alpha = 0.8, size = 3) +  # Larger, more opaque points with distinct shapes
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2) +  # Thicker dashed trend lines
  scale_color_manual(values = c("coral2", "skyblue2"),  # Keep color mapping but reordered
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_shape_manual(values = c(16, 17),  # Keep shape mapping but reordered
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +  
  scale_x_continuous(breaks = seq(min(coefs$rabyear), max(coefs$rabyear), by = 1)) +  # Yearly x-axis ticks
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25)) +  # Cleaner y-axis ticks
  labs(x = "Year of birth", y = "PGI coefficient", 
       title = "",
       color = " ", shape = " ") +  # Set one legend title
  guides(color = guide_legend(order = 1),  # Ensures color & shape legend are ordered together
         shape = guide_legend(order = 1)) +  
  theme_pilot(axis_title_size = 12,
              axis_text_size = 10.5,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Facet by SES group (Low SES and High SES)
  facet_wrap(~ ses_group, scales = "free_y", ncol = 1) +  # Separate graphs for each SES group
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.box = "vertical",  # Legends arranged vertically
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 14)) 
        



###################################################################################################
################# FIGURE outcomes across birthyears ##################################
###################################################################################################


data_long <- data %>%
  gather(key = "Outcome", value = "Value", yoe, wealthlog, incomelog)

ggplot(data_long, aes(x = rabyear, y = Value, color = Outcome, linetype = Outcome)) +
  geom_smooth(method = "loess", se = FALSE) +  # Smoothed lines with different line types
  labs(title = " ", 
       x = "Birth Year", 
       y = "Value") +
  scale_x_continuous(breaks = seq(min(data$rabyear), max(data$rabyear), by = 1)) +  # X-axis with yearly ticks
  scale_color_manual(values = c("lightblue2", "olivedrab3", "coral"),
                     labels = c("Years of Education", "Wealth (log)", "Income (log)")) +  # Custom colors for outcomes
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +  # Custom line types
  guides(
    color = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dotted"))),  # Combine color and linetype in one legend
    linetype = "none"  # Remove linetype legend
  ) +
  theme_minimal() +
  theme_pilot(axis_title_size = 12,
              axis_text_size = 10.5,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################################################################################################
################# VARIANCE OF THE OUTCOME ##################################
###################################################################################################

# Split data into treated and non-treated groups
treated <- data[data$treatment == 1, ]
non_treated <- data[data$treatment == 0, ]

# Calculate variances for each outcome variable
vars_treated <- sapply(treated[, c("yoe", "incomelog", "wealthlog")], var, na.rm = TRUE)
vars_non_treated <- sapply(non_treated[, c("yoe", "incomelog", "wealthlog")], var, na.rm = TRUE)

# Calculate net difference
net_diff <- vars_treated - vars_non_treated

# Combine into a summary table
variance_comparison <- data.frame(
  Variable = names(vars_treated),
  Variance_Treated = vars_treated,
  Variance_NonTreated = vars_non_treated,
  Net_Difference = net_diff
)
print(variance_comparison)

# Perform F-tests to compare variances between treated and non-treated groups
f_tests <- lapply(c("yoe", "incomelog", "wealthlog"), function(var) {
  var.test(
    formula = as.formula(paste(var, "~ treatment")),
    data = data
  )
})

# Print F-test results for each variable
names(f_tests) <- c("yoe", "incomelog", "wealthlog")
f_tests


