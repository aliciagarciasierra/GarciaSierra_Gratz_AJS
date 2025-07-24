
###############################################################
######## FIGURE 4: INTERACTION GRAPHS #########################
###############################################################

#################### MODEL EDUCATION ##########################

# Create the effects object for the interaction term
interaction_effects <- effect("pgeducation*treatment", m1)

# Convert the effects object to a data frame
interaction_df <- as.data.frame(interaction_effects)

# Define custom colors (only need to do it once for the three graphs)
line_colors <- c("coral3", "lightskyblue3")
ribbon_colors <- c("coral", "lightskyblue")

# Plot 
education <-
  ggplot(interaction_df, aes(x = pgeducation, y = fit, color = treatment)) +
  geom_line(aes(group = treatment), size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = treatment), alpha = 0.2) +
  scale_color_manual(name = "Treatment", values = line_colors, labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_fill_manual(name = "Treatment", values = ribbon_colors, labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "right") +
  labs(x= "PGI Education",
       y = "Predicted years of education") +
  ggtitle("Years of education") +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

education



#################### MODEL INCOME ##########################

# Create the effects object for the interaction term
interaction_effects <- effect("pgeducation*treatment", m4)

# Convert the effects object to a data frame
interaction_df <- as.data.frame(interaction_effects)

# Plot with ggplot2 including custom colors
income <-
  ggplot(interaction_df, aes(x = pgeducation, y = fit, color = treatment)) +
  geom_line(aes(group = treatment), size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = treatment), alpha = 0.2) +
  scale_color_manual(name = "Treatment", values = line_colors, labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_fill_manual(name = "Treatment", values = ribbon_colors, labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "right") +
  labs(x= "PGI Education",
       y = "Predicted income") +
  ggtitle("Income") +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

income

#################### MODEL WEALTH ##########################

# Create the effects object for the interaction term
interaction_effects <- effect("pgeducation*treatment", m7)

# Convert the effects object to a data frame
interaction_df <- as.data.frame(interaction_effects)

# Plot with ggplot2 including custom colors
wealth <-
  ggplot(interaction_df, aes(x = pgeducation, y = fit, color = treatment)) +
  geom_line(aes(group = treatment), size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = treatment), alpha = 0.2) +
  scale_color_manual(name = "Treatment", values = line_colors, labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_fill_manual(name = "Treatment", values = ribbon_colors, labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "right") +
  labs(x= "PGI Education",
       y = "Predicted wealth") +
  ggtitle("Wealth") +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

wealth


########################## COMBINED THREE GRAPHS ##########################

ggarrange(education,income,wealth, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")



##############################################################
######## FIGURE 5: THREE WAY INTERACTION  ##################
##############################################################

# Set colors for the graph
line_colors <- c("coral3", "lightskyblue3", "olivedrab4")
ribbon_colors <- c("coral", "lightskyblue", "olivedrab3")

# Obtain the predicted values (corresponding model "m3" run in the "05_EDUCATION" file)
a <-plot_model(m3, type = "pred", terms = c("pgeducation[-3:3]","ses[-1.1426, -0.2473, 0.7228]", "treatment[0,1]"))

# Plot 
a <- a+ theme_pilot(axis_title_size = 11,
                    axis_text_size = 11,
                    legend_text_size = 12,
                    legend_title_size = 12,
                    legend_position= "right",
                    title_size = 13) +
  labs(x= "PGI",
       y = "Predicted years of education") +
  ggtitle(" ") +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  scale_color_manual(name = " ", values = line_colors, labels = c("Low SES", "Medium SES", "High SES")) +
  scale_fill_manual(name = " ", values = ribbon_colors, labels = c("Low SES", "Medium SES", "High SES")) 

# Rename the facts
a$data$facet<- ifelse(a$data$facet== 0, "Not exposed to the reform",
                      ifelse(a$data$facet== 1, "Exposed to the reform",NA))

# Relevel the facet variable to ensure correct order and labeling
a$data$facet <- factor(a$data$facet, levels = c("Not exposed to the reform", "Exposed to the reform"))

# Adjust size facet titles
a <- a +
  theme(
    strip.text = element_text(size = 14) # Adjust the size as needed
  )

# Final plot
a



