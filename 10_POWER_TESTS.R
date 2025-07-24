
#######################################################
###############  two way interactions  ############### 
######################################################

# Modify the simulate_data function to include a varying beta (interaction effect size)
simulate_data <- function(n = 2132, beta = 0.3) {
  treatment <- rbinom(n, 1, 0.5)  # binary treatment (50% treated)
  ses <- rnorm(n, mean = 0, sd = 1)  # continuous SES (standard normal)
  timetrend <- sample(1:10, n, replace = TRUE)  # random time trend values
  male <- rbinom(n, 1, 0.5)  # binary gender (50% male)
  
  # Generate outcome with only the interaction effect, no main effects of treatment or ses
  yoe <- beta * treatment * ses + rnorm(n, mean = 0, sd = 1)  # Add random noise
  return(data.frame(yoe, treatment, ses, timetrend, male))
}

# Power analysis loop: test power for varying beta (effect sizes)
set.seed(123) 
nsim <- 1000
betas <- seq(0.1, 0.5, by = 0.05)  # Varying interaction effect sizes
power_results <- data.frame(beta = numeric(0), power = numeric(0))

for (beta in betas) {
  significant_results <- 0
  
  for (i in 1:nsim) {
    sim_data <- simulate_data(beta = beta)  # Simulate data for current beta
    model <- feols(yoe ~ treatment * ses + 
                     timetrend * ses + 
                     timetrend * treatment + 
                     male, data = sim_data)
    
    # Check if the p-value for the interaction treatment:ses is below 0.05
    coefs <- summary(model, cluster = "timetrend")$coeftable
    if ("treatment:ses" %in% rownames(coefs)) {
      if (coefs["treatment:ses", 4] < 0.05) {
        significant_results <- significant_results + 1
      }
    }
  }
  
  # Calculate power for this value of beta (effect size)
  power <- significant_results / nsim
  power_results <- rbind(power_results, data.frame(beta = beta, power = power))
}

# Convert power values to percentage
power_results$power_percentage <- power_results$power * 100

# Plot the power curve with percentage scale on the y-axis
two_way<-ggplot(power_results, aes(x = beta, y = power_percentage)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red") +  # Add horizontal line at 80%
  labs(title = "Power curve two-way interaction", 
       x = "Interaction effect size", 
       y = "Power (%)") +  # Change y-axis label to percentage
  scale_x_continuous(breaks = seq(min(power_results$beta), max(power_results$beta), by = 0.05)) +  
  theme_pilot(axis_title_size = 12,
              axis_text_size = 10.5,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position = "bottom")  # Adjust theme if needed

# Optionally, print the power results for each beta
print(power_results)


#######################################################
###############  three way interactions  ############### 
######################################################
# Modify the simulate_data function to include a varying beta (interaction effect size) and simulate PCs
simulate_data <- function(n = 2132, beta = 0.3) {
  treatment <- rbinom(n, 1, 0.5)  # binary treatment (50% treated)
  pgeducation <- rnorm(n, mean = 0, sd = 1)  # continuous pgeducation (standard normal)
  ses <- rnorm(n, mean = 0, sd = 1)  # continuous SES
  timetrend <- sample(1:10, n, replace = TRUE)  # random time trend values
  male <- rbinom(n, 1, 0.5)  # binary gender (50% male)
  
  # Simulate PCs as continuous variables (random normal)
  pc1 <- rnorm(n)
  pc2 <- rnorm(n)
  pc3 <- rnorm(n)
  pc4 <- rnorm(n)
  pc5 <- rnorm(n)
  pc6 <- rnorm(n)
  pc7 <- rnorm(n)
  pc8 <- rnorm(n)
  pc9 <- rnorm(n)
  pc10 <- rnorm(n)
  
  # Simulate outcome with only the triple interaction effect, no main effects
  yoe <- beta * treatment * pgeducation * ses + rnorm(n, mean = 0, sd = 1)  # Add random noise
  return(data.frame(yoe, treatment, pgeducation, ses, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, timetrend, male))
}

# Power analysis loop: test power for varying beta (effect sizes)
set.seed(123) 
nsim <- 1000
betas <- seq(0.1, 0.5, by = 0.05)  # Varying interaction effect sizes
power_results <- data.frame(beta = numeric(0), power = numeric(0))

for (beta in betas) {
  significant_results <- 0
  
  for (i in 1:nsim) {
    sim_data <- simulate_data(beta = beta)  # Simulate data for current beta
    model <- feols(yoe ~ treatment * pgeducation * ses + 
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + 
                     timetrend + 
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation + timetrend * treatment + timetrend * ses + 
                     male, data = sim_data)
    
    # Check if the p-value for the triple interaction treatment:pgeducation:ses is below 0.05
    coefs <- summary(model, cluster = "timetrend")$coeftable
    if ("treatment:pgeducation:ses" %in% rownames(coefs)) {
      if (coefs["treatment:pgeducation:ses", 4] < 0.05) {
        significant_results <- significant_results + 1
      }
    }
  }
  
  # Calculate power for this value of beta (effect size)
  power <- significant_results / nsim
  power_results <- rbind(power_results, data.frame(beta = beta, power = power))
}

# Convert power values to percentage
power_results$power_percentage <- power_results$power * 100
print(power_results)

# Plot the power curve with percentage scale on the y-axis
three_way <-ggplot(power_results, aes(x = beta, y = power_percentage)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red") +  # Add horizontal line at 80%
  labs(title = "Power curve three-way interaction", 
       x = "Interaction effect size", 
       y = "Power (%)") +  # Change y-axis label to percentage
  scale_x_continuous(breaks = seq(min(power_results$beta), max(power_results$beta), by = 0.05)) +  
  theme_pilot(axis_title_size = 12,
              axis_text_size = 10.5,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position = "bottom")  # Adjust theme if needed
three_way


#------------- join the two graphs -------------------#

two_way + three_way
