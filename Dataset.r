# ===============================
# CSC 204 Lab Assessment (Penguins)
# ===============================

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install if not already installed
# install.packages("palmerpenguins")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("corrplot")
# install.packages("writexl")

# Load libraries
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(corrplot)
library(writexl)

# Load dataset
data("penguins")
df <- penguins

write_xlsx(df, "DTS_204_Dataset.xlsx")

# Display the first six rows
cat("--- First Six Rows ---\n")
head(df)

# Check Missing Values
cat("\n--- Missing Values Count ---\n")
colSums(is.na(df))

# Generate summary statistics for numerical variables
cat("\n--- Summary Statistics ---\n")
summary(select(df, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g))

# ========================
# Hypothesis Testing
# ========================

# i. Two-Sample t-Test (Adelie vs Gentoo body mass)
adelie_mass <- df %>% filter(species == "Adelie") %>% pull(body_mass_g)
gentoo_mass <- df %>% filter(species == "Gentoo") %>% pull(body_mass_g)
t_test_result <- t.test(adelie_mass, gentoo_mass)
cat("\n--- Two-Sample t-Test Result ---\n")
print(t_test_result)

# ii. One-sample proportion test (male proportion vs 50%)
male_count <- sum(df$sex == "male", na.rm = TRUE) 
total_count <- sum(!is.na(df$sex)) 
prop_test_result <- prop.test(male_count, total_count, p = 0.5)
prop_test_result

# ========================
# Correlation Analysis
# ========================
# Compute correlation matrix
num_vars <- select(df, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
cor_matrix <- cor(num_vars, use = "complete.obs")
cat("\n--- Correlation Matrix ---\n")
cor_matrix

# Visualize correlation matrix 
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.8)

# ========================
# Regression Analysis
# ========================
# i. Simple Linear Regression
simple_lm <- lm(body_mass_g ~ flipper_length_mm, data = df)
cat("\n--- Simple Linear Regression Summary ---\n")
summary(simple_lm)

# ii. Multiple linear regression
multi_lm <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm, data = df)
cat("\n--- Multiple Linear Regression Summary ---\n")
summary(multi_lm)

# ========================
# Data Visualization
# ========================
# 1. Scatter plot: Body Mass vs Flipper Length
ggplot(df, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species), na.rm = TRUE) + 
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) + 
  theme_minimal() + 
  labs(title = "Body Mass vs Flipper Length", x = "Flipper Length (mm)", y = "Body Mass (g)")

# 2. Boxplot: Body Mass by Species
ggplot(df, aes(x = species, y = body_mass_g, fill = species)) + 
  # Added na.rm = TRUE 
  geom_boxplot(na.rm = TRUE) + 
  theme_minimal() + 
  labs(title = "Body Mass by Species", x = "Species", y = "Body Mass (g)")

# 3. Histogram with normal curve overlay
ggplot(df, aes(x = body_mass_g)) + 
  # Added na.rm = TRUE 
  geom_histogram(aes(y = after_stat(density)), binwidth = 200, fill = "lightblue", color = "black", na.rm = TRUE) +
  stat_function(fun = dnorm, args = list(mean = mean(df$body_mass_g, na.rm = TRUE),
                                         sd = sd(df$body_mass_g, na.rm = TRUE)),
                # Updated line sizing syntax
                color = "red", linewidth = 1, na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Histogram of Body Mass with Normal Curve", x = "Body Mass (g)", y = "Density")