# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)

# Load cleaned data
survey <- readRDS("clean_survey.rds")

# 1. Basic Summary Statistics
cat("Dataset Dimensions:", dim(survey), "\n")
cat("Number of Complete Cases:", sum(complete.cases(survey)), "\n")

# Check column names to ensure 'Age' exists
cat("Column names in the dataset:\n")
print(names(survey))

# 2. Handle missing data for 'Age'
if (!"Age" %in% names(survey)) {
  stop("The 'Age' column is not present in the dataset.")
}

# If 'Age' is present, handle missing values
survey <- survey %>% filter(!is.na(Age))  # Remove rows with missing Age data

# 3. Univariate Analysis

# Age Distribution
ggplot(survey, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Age Distribution of Survey Respondents",
       x = "Age",
       y = "Count")
ggsave("age_distribution.png")

# Gender Distribution
ggplot(survey, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count")
ggsave("gender_distribution.png")

# Treatment Seeking Distribution
ggplot(survey, aes(x = treatment, fill = treatment)) +
  geom_bar() +
  labs(title = "Mental Health Treatment Distribution",
       x = "Sought Treatment",
       y = "Count")
ggsave("treatment_distribution.png")

# 4. Bivariate Analysis

# Treatment by Gender
ggplot(survey, aes(x = Gender, fill = treatment)) +
  geom_bar(position = "fill") +
  labs(title = "Treatment Seeking by Gender",
       x = "Gender",
       y = "Proportion")
ggsave("treatment_by_gender.png")

# Work Interference by Treatment
ggplot(survey %>% filter(!is.na(work_interfere)), 
       aes(x = work_interfere, fill = treatment)) +
  geom_bar(position = "fill") +
  labs(title = "Treatment Seeking by Work Interference Level",
       x = "Work Interference",
       y = "Proportion")
ggsave("treatment_by_interference.png")

# 5. Statistical Tests

# Chi-square test for gender and treatment
gender_treatment_test <- chisq.test(table(survey$Gender, survey$treatment))
cat("\nChi-square test for Gender and Treatment:\n")
print(gender_treatment_test)

# Correlation between Age and Work Interference
work_interfere_numeric <- as.numeric(survey$work_interfere)
age_interfere_cor <- cor.test(survey$Age, work_interfere_numeric, 
                             use = "complete.obs")
cat("\nCorrelation between Age and Work Interference:\n")
print(age_interfere_cor)

# 6. Summary Statistics by Tech Company
tech_summary <- survey %>%
  group_by(tech_company) %>%
  summarise(
    n = n(),
    treatment_rate = mean(treatment, na.rm = TRUE),
    remote_work_rate = mean(remote_work, na.rm = TRUE),
    benefits_rate = mean(benefits, na.rm = TRUE)
  )

cat("\nSummary Statistics by Tech Company:\n")
print(tech_summary)
