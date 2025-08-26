

##############################################################
######## FIGURE 2: INTERACTIONS  ##################
##############################################################

# ---- Common additions: boxed panels + Arial font ----
boxed_arial <- theme(
  text = element_text(family = "Arial"),
  panel.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black")
)

# Legend + title overrides (Arial + black legend background)
legend_arial_white <- theme(
  plot.title   = element_text(family = "Arial"),
  legend.title = element_text(family = "Arial"),
  legend.text  = element_text(family = "Arial", colour = "black"),
  legend.background     = element_rect(fill = "white", colour = "white"),
  legend.key            = element_rect(fill = "white", colour = "white"),
  legend.box.background = element_rect(fill = "white", colour = "white")
)

# Colors
line_colors   <- c("coral3", "lightskyblue3")
ribbon_colors <- c("coral",  "lightskyblue")

#################### MODEL EDUCATION ##########################
interaction_effects <- effect("pgeducation*treatment", m1)
interaction_df <- as.data.frame(interaction_effects)

education <-
  ggplot(interaction_df, aes(x = pgeducation, y = fit, color = treatment)) +
  geom_line(aes(group = treatment), size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = treatment), alpha = 0.2) +
  scale_color_manual(name = "Treatment", values = line_colors,
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_fill_manual(name = "Treatment", values = ribbon_colors,
                    labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 12,
              legend_text_size = 12,
              legend_title_size = 14,
              legend_position= "right") +
  labs(x= "PGI Education", y = "Predicted years of education") +
  ggtitle("Years of education") +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  boxed_arial +
  legend_arial_white   
education

#################### MODEL INCOME ##########################
interaction_effects <- effect("pgeducation*treatment", m4)
interaction_df <- as.data.frame(interaction_effects)

income <-
  ggplot(interaction_df, aes(x = pgeducation, y = fit, color = treatment)) +
  geom_line(aes(group = treatment), size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = treatment), alpha = 0.2) +
  scale_color_manual(name = "Treatment", values = line_colors,
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_fill_manual(name = "Treatment", values = ribbon_colors,
                    labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 12,
              legend_text_size = 12,
              legend_title_size = 14,
              legend_position= "right") +
  labs(x= "PGI Education", y = "Predicted income") +
  ggtitle("Income") +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  boxed_arial
income

#################### MODEL WEALTH ##########################
interaction_effects <- effect("pgeducation*treatment", m7)
interaction_df <- as.data.frame(interaction_effects)

wealth <-
  ggplot(interaction_df, aes(x = pgeducation, y = fit, color = treatment)) +
  geom_line(aes(group = treatment), size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = treatment), alpha = 0.2) +
  scale_color_manual(name = "Treatment", values = line_colors,
                     labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_fill_manual(name = "Treatment", values = ribbon_colors,
                    labels = c("Not exposed to the reform", "Exposed to the reform")) +
  scale_y_continuous(limits = c(-2, 2)) +
  theme_pilot(axis_title_size = 14,
              axis_text_size = 12,
              legend_text_size = 12,
              legend_title_size = 14,
              legend_position= "right") +
  labs(x= "PGI Education", y = "Predicted wealth") +
  ggtitle("Wealth") +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  boxed_arial
wealth

########################## COMBINED THREE GRAPHS ##########################
title_style <- theme(plot.title = element_text(family = "Arial", face = "bold", size= 17))

education <- education + title_style
income    <- income    + title_style
wealth    <- wealth    + title_style

combined <- ggarrange(
  education, income, wealth,
  ncol = 3, nrow = 1,
  common.legend = TRUE, legend = "bottom"
)

combined <- combined + theme(
  legend.background     = element_rect(fill = "white", colour = NA),
  legend.key            = element_rect(fill = "white", colour = NA),
  legend.box.background = element_blank(),
  legend.title = element_text(colour = "black"),
  legend.text  = element_text(colour = "black")
)

combined
########################## SAVE AS TIFF ###################
ggsave(
  filename = "fig2.tiff",
  plot = combined,
  width = 12, height = 4, units = "in", dpi = 300,
  device = ragg::agg_tiff, compression = "lzw"
)



##############################################################
######## FIGURE 3: THREE WAY INTERACTION  ###################
##############################################################
# Set colors for the graph
line_colors <- c("coral3", "lightskyblue3", "olivedrab4")
ribbon_colors <- c("coral", "lightskyblue", "olivedrab3")

# ---- Common additions: boxed panels + Arial font ----
boxed_arial <- theme(
  text = element_text(family = "Arial"),
  panel.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black")
)

# Legend + title overrides (Arial + black legend background)
legend_arial_white <- theme(
  plot.title   = element_text(family = "Arial"),
  legend.title = element_text(family = "Arial" , colour="black"),
  legend.text  = element_text(family = "Arial", colour="black"),
  legend.background     = element_rect(fill = "white", colour = "white"),
  legend.key            = element_rect(fill = "white", colour = "white"),
  legend.box.background = element_rect(fill = "white", colour = "white")
)

# Predicted values (model m3 from your EDU file)
a <- plot_model(
  m3, type = "pred",
  terms = c("pgeducation[-3:3]", "ses[-1.1426, -0.2473, 0.7228]", "treatment[0,1]")
)

# Base theming and labels
a <- a +
  theme_pilot(
    axis_title_size = 12,
    axis_text_size  = 12,
    legend_text_size = 12,
    legend_title_size = 12,
    legend_position = "right",
    title_size = 13
  ) +
  labs(
    x = "PGI",
    y = "Predicted years of education"
  ) +
  ggtitle(" ") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  scale_color_manual(
    name = " ",
    values = line_colors,
    labels = c("Low SES", "Medium SES", "High SES")
  ) +
  scale_fill_manual(
    name = " ",
    values = ribbon_colors,
    labels = c("Low SES", "Medium SES", "High SES")
  )

# Rename facet labels (0/1 -> Not/Exposed)
a$data$facet <- ifelse(
  a$data$facet == 0, "Not exposed to the reform",
  ifelse(a$data$facet == 1, "Exposed to the reform", NA)
)

# Ensure facet order
a$data$facet <- factor(
  a$data$facet,
  levels = c("Not exposed to the reform", "Exposed to the reform")
)

# Facet title styling (Arial) and box the strip
a <- a +
  theme(
    strip.text = element_text(size = 14, family = "Arial"),
    strip.background = element_rect(fill = "white",  linewidth = 0.6)
  )

# Apply boxed panels + Arial everywhere; add black legend if desired
a <- a + boxed_arial + legend_arial_black
a <- a + theme(
  legend.background     = element_rect(fill = "white", colour = NA),
  legend.key            = element_rect(fill = "white", colour = NA),
  legend.box.background = element_blank(),
  legend.title = element_text(colour = "black"),
  legend.text  = element_text(colour = "black")
)

# Final plot
a

# Save as high-quality TIFF (Arial via ragg)
ggsave(
  filename = "fig3.tiff",
  plot = a,
  width = 10, height = 4, units = "in", dpi = 300,
  device = ragg::agg_tiff, compression = "lzw"
)


