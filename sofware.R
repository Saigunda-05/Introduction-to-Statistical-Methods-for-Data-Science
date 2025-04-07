# Load libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(reshape2)


# Load the datasets
X <- read_csv("C:/data/r studio data/data/X.csv", col_names = c("x1", "x2"))
y <- read_csv("C:/data/r studio data/data/y.csv", col_names = "y")
time <- read_csv("C:/data/r studio data/data/time.csv", col_names = "time")

# Combine into one dataframe
data <- cbind(time, X, y)

# Convert to numeric (if not already)
data$x1 <- as.numeric(data$x1)
data$x2 <- as.numeric(data$x2)
data$y <- as.numeric(data$y)
data$time <- as.numeric(data$time)


# ============== TASK 1: PRELIMINARY DATA ANALYSIS ===============
# Time series plots of input audio and output MEG signal
p1 <- ggplot(data, aes(x = time, y = x1)) +
  geom_line(color = "#0072B2", size = 0.4) +
  ggtitle("Time Series of Input Audio Signal (x1)") +
  xlab("Time (seconds)") +
  ylab("Amplitude") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

p2 <- ggplot(data, aes(x = time, y = y)) +
  geom_line(color = "#D55E00", size = 0.4) +
  ggtitle("Time Series of MEG Signal (y)") +
  xlab("Time (seconds)") +
  ylab("Amplitude") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display time series plots vertically
grid.arrange(p1, p2, ncol = 1)

# Distribution for each signal
hist_x1 <- ggplot(data, aes(x = x1)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Input Signal x1") +
  theme_minimal()

hist_y <- ggplot(data, aes(x = y)) +
  geom_histogram(bins = 50, fill = "salmon", color = "black") +
  ggtitle("Distribution of MEG Signal y") +
  theme_minimal()

# Display histograms
grid.arrange(hist_x1, hist_y, ncol = 2)

# Correlation and scatter plots
corr_xy <- cor(data$x1, data$y)
scatter_plot <- ggplot(data, aes(x = x1, y = y)) +
  geom_point(alpha = 0.5) +
  ggtitle(paste("Scatter Plot: Input vs Output (Correlation =", round(corr_xy, 3), ")")) +
  xlab("Input Audio Signal (x1)") +
  ylab("MEG Signal (y)") +
  theme_minimal()
print(scatter_plot)

# Boxplots for different sound categories
boxplot_categories <- ggplot(data, aes(x = factor(x2), y = y, fill = factor(x2))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "salmon"), 
                    labels = c("Neutral (x2=0)", "Emotional (x2=1)")) +
  ggtitle("MEG Signal by Sound Category") +
  xlab("Sound Category") +
  ylab("MEG Signal (y)") +
  theme_minimal() +
  labs(fill = "Category")
print(boxplot_categories)

# Separate analysis by sound category
data_neutral <- data[data$x2 == 0, ]
data_emotional <- data[data$x2 == 1, ]

# Time series by category
p1_neutral <- ggplot(data_neutral, aes(x = time, y = x1)) +
  geom_line(color = "#0072B2", size = 0.4) +
  ggtitle("Input Audio Signal (Neutral)") +
  theme_minimal()

p2_neutral <- ggplot(data_neutral, aes(x = time, y = y)) +
  geom_line(color = "#D55E00", size = 0.4) +
  ggtitle("MEG Signal (Neutral)") +
  theme_minimal()

p1_emotional <- ggplot(data_emotional, aes(x = time, y = x1)) +
  geom_line(color = "#0072B2", size = 0.4) +
  ggtitle("Input Audio Signal (Emotional)") +
  theme_minimal()

p2_emotional <- ggplot(data_emotional, aes(x = time, y = y)) +
  geom_line(color = "#D55E00", size = 0.4) +
  ggtitle("MEG Signal (Emotional)") +
  theme_minimal()

# Display category-specific plots
grid.arrange(p1_neutral, p2_neutral, p1_emotional, p2_emotional, ncol = 2)



# ==================== TASK 2: REGRESSION MODELING ====================
library(tidyverse)

# Load datasets
X <- read_csv("C:/data/r studio data/data/X.csv",
              skip = 1,
              col_names = c("x1", "x2"),
              col_types = cols(x1 = col_double(), x2 = col_double()))
y <- read_csv("C:/data/r studio data/data/y.csv",
              skip = 1,
              col_names = "y",
              col_types = cols(y = col_double()))
time <- read_csv("C:/data/r studio data/data/time.csv",
                 skip = 1,
                 col_names = "time",
                 col_types = cols(time = col_double()))




# Combine into one data frame
data <- bind_cols(X, y)

data <- data %>%
  mutate(
    x1_sq = x1^2,
    x2_sq = x2^2,
    sinx1 = sin(x1),
    sinx2 = sin(x2),
    cosx1 = cos(x1),
    cosx2 = cos(x2),
    x1x2 = x1 * x2
  )


# Define and fit all models
model1 <- lm(y ~ x1 + x2, data = data)
model2 <- lm(y ~ x1 + x2 + x1_sq + x2_sq, data = data)
model3 <- lm(y ~ x1 + x2 + sinx1 + sinx2, data = data)
model4 <- lm(y ~ x1 + x2 + cosx1 + cosx2, data = data)
model5 <- lm(y ~ x1 + x2 + x1x2, data = data)

# Store models
models <- list(
  Model1 = model1,
  Model2 = model2,
  Model3 = model3,
  Model4 = model4,
  Model5 = model5
)

# Initialize parameter table
param_table <- tibble(
  Parameter = c("θ1", "θ2", "θ3", "θ4", "θ5", "θbias"),
  Model1 = NA_real_,
  Model2 = NA_real_,
  Model3 = NA_real_,
  Model4 = NA_real_,
  Model5 = NA_real_
)

# Fill in parameters for each model
extract_params <- function(model, terms) {
  coefs <- coef(model)
  out <- rep(NA_real_, 6)
  names(out) <- c("θ1", "θ2", "θ3", "θ4", "θ5", "θbias")
  for (i in seq_along(terms)) {
    out[i] <- coefs[terms[i]]
  }
  out["θbias"] <- coefs["(Intercept)"]
  return(out)
}

param_table$Model1 <- extract_params(model1, c("x1", "x2"))
param_table$Model2 <- extract_params(model2, c("x1", "x2", "x1_sq", "x2_sq"))
param_table$Model3 <- extract_params(model3, c("x1", "x2", "sinx1", "sinx2"))
param_table$Model4 <- extract_params(model4, c("x1", "x2", "cosx1", "cosx2"))
param_table$Model5 <- extract_params(model5, c("x1", "x2", "x1x2"))

print(param_table)

# Task 2.2 – Compute RSS
compute_rss <- function(model) {
  sum(residuals(model)^2)
}

rss_list <- sapply(models, compute_rss)
print(rss_list)



#Task 2.3 – Log-Likelihood Function


# Compute variance and log-likelihood
compute_log_likelihood <- function(rss, n) {
  sigma2 <- rss / (n - 1)
  ll <- -n/2 * log(2 * pi) - n/2 * log(sigma2) - rss / (2 * sigma2)
  return(list(log_likelihood = ll, sigma2 = sigma2))
}

# Calculate for each model
log_likelihoods <- lapply(rss_list, compute_log_likelihood, n = nrow(data))
log_lik_values <- sapply(log_likelihoods, `[[`, "log_likelihood")
sigma2_values <- sapply(log_likelihoods, `[[`, "sigma2")

print(log_lik_values)
print(sigma2_values)
print(log_likelihoods)


#Task 2.4 – AIC & BIC Calculation

# Number of parameters in each model (excluding residual variance)
model_k <- c(
  Model1 = 3,  # x1, x2, intercept
  Model2 = 5,  # x1, x2, x1^2, x2^2, intercept
  Model3 = 5,  # x1, x2, sinx1, sinx2, intercept
  Model4 = 5,  # x1, x2, cosx1, cosx2, intercept
  Model5 = 4   # x1, x2, x1x2, intercept
)

n <- nrow(data)

aic_values <- 2 * model_k - 2 * log_lik_values
bic_values <- model_k * log(n) - 2 * log_lik_values

# Print summary
aic_bic_table <- tibble(
  Model = names(rss_list),
  RSS = rss_list,
  LogLikelihood = log_lik_values,
  Sigma2 = sigma2_values,
  AIC = aic_values,
  BIC = bic_values
)
print(aic_bic_table)

#Task 2.5 – Residual Distribution Check
# Plot residuals and Q-Q plots
library(ggplot2)
par(mfrow = c(2, 5))
for (model_name in names(models)) {
  resids <- residuals(models[[model_name]])
  hist(resids, main = paste(model_name, "Residuals"), xlab = "Residuals", col = "skyblue", breaks = 20)
}

par(mfrow = c(2, 5))
for (model_name in names(models)) {
  resids <- residuals(models[[model_name]])
  qqnorm(resids, main = paste(model_name, "Q-Q Plot"))
  qqline(resids, col = "red")
}

#Task 2.6 – Select Best Model

# Combine results into a summary table
model_selection_table <- tibble(
  Model = names(rss_list),
  RSS = unlist(rss_list),
  LogLikelihood = log_lik_values,
  AIC = aic_values,
  BIC = bic_values
)

print(model_selection_table)

par(mfrow = c(2, 3))  # Set layout to show multiple plots

for (name in names(models)) {
  qqnorm(models[[name]]$residuals, main = paste("Q-Q Plot:", name))
  qqline(models[[name]]$residuals)
}

best_aic_model <- model_selection_table$Model[which.min(model_selection_table$AIC)]
best_bic_model <- model_selection_table$Model[which.min(model_selection_table$BIC)]

cat("Model with lowest AIC:", best_aic_model, "\n")
cat("Model with lowest BIC:", best_bic_model, "\n")


# Task 2.7 – Train-Test Split and Prediction with Confidence Intervals

set.seed(42)
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit the selected model (e.g., Model3) on training data
model_best <- lm(y ~ x1 + x2 + sinx1 + sinx2, data = train_data)

# Predict on test data with confidence intervals
predictions <- predict(model_best, newdata = test_data, interval = "confidence", level = 0.95)

# Combine prediction with actual values
prediction_df <- cbind(test_data, predictions)

# Plot predictions and confidence intervals
ggplot(prediction_df, aes(x = 1:nrow(prediction_df))) +
  geom_point(aes(y = y), color = "blue", size = 1.5) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "gray") +
  labs(title = "Model4 Predictions with 95% CI",
       x = "Test Sample Index",
       y = "MEG Response (y)") +
  theme_minimal()


# ----- Task 3: Approximate Bayesian Computation (ABC) -----
# --- Load libraries ---
library(tidyverse)
library(ggplot2)

# --- Step 0: Ensure numeric data and fix sinx2 ---
data <- data %>%
  mutate(
    x1 = as.numeric(x1),
    x2 = as.numeric(x2),
    sinx1 = sin(x1),
    sinx2 = sin(x2)
  )

# --- Step 1: Fit the best model (Model 4) again ---
model4 <- lm(y ~ x1 + x2 + sin(x1) + sin(x2), data = data)

# --- Step 2: Extract coefficients ---
coefs <- coef(model4)

# Remove NA coefficients
coefs <- coefs[!is.na(coefs)]

# Get top 2 parameters by absolute value (excluding intercept)
top2 <- names(sort(abs(coefs[names(coefs) != "(Intercept)"]), decreasing = TRUE))[1:2]
theta1_name <- top2[1]
theta2_name <- top2[2]

# MLE values
theta1_hat <- coefs[theta1_name]
theta2_hat <- coefs[theta2_name]

# Prior range: ±50% of MLE
range1 <- c(theta1_hat * 0.5, theta1_hat * 1.5)
range2 <- c(theta2_hat * 0.5, theta2_hat * 1.5)

# --- Step 3: ABC Sampling setup ---
set.seed(123)
N <- 10000

# Design matrix and target
X_mat <- model.matrix(model4)
y_vec <- y$y
n <- length(y_vec)

# Fix other coefficients
fixed_coefs <- coefs
non_sampled <- setdiff(names(coefs), c(theta1_name, theta2_name))
fixed_vals <- fixed_coefs[non_sampled]

# Indices in design matrix
term_names <- colnames(X_mat)
idx_theta1 <- which(term_names == theta1_name)
idx_theta2 <- which(term_names == theta2_name)
idx_fixed <- which(term_names %in% non_sampled)

# Prior sampling
theta1_samples <- runif(N, min = range1[1], max = range1[2])
theta2_samples <- runif(N, min = range2[1], max = range2[2])

# --- Step 4: Compute predicted y and RSS ---
rss_values <- map2_dbl(theta1_samples, theta2_samples, function(t1, t2) {
  beta <- numeric(ncol(X_mat))
  beta[idx_theta1] <- t1
  beta[idx_theta2] <- t2
  beta[idx_fixed] <- fixed_vals
  y_pred <- X_mat %*% beta
  sum((y_vec - y_pred)^2)
})

# --- Step 5: Accept top 1% by RSS ---
if (all(is.na(rss_values))) stop("All RSS values are NA. Check model structure and numeric inputs.")
threshold <- quantile(rss_values, 0.01, na.rm = TRUE)
accepted <- tibble(
  theta1 = theta1_samples,
  theta2 = theta2_samples,
  rss = rss_values
) %>% filter(rss < threshold)

cat("Number of accepted samples:", nrow(accepted), "\n")

# --- Step 6: Plot posteriors ---

# Marginal for theta1
ggplot(accepted, aes(x = theta1)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = paste("Posterior of", theta1_name), x = theta1_name)

# Marginal for theta2
ggplot(accepted, aes(x = theta2)) +
  geom_histogram(bins = 50, fill = "salmon", color = "black") +
  labs(title = paste("Posterior of", theta2_name), x = theta2_name)

# Joint posterior
ggplot(accepted, aes(x = theta1, y = theta2)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  labs(title = "Joint Posterior Distribution",
       x = theta1_name, y = theta2_name)
