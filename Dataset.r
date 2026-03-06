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

# Display the first six rows
cat("--- First Six Rows ---\n")
head(df)

#  Check for missing values
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
print(t_test_result)  # Displays t-test results, p-value, confidence interval 