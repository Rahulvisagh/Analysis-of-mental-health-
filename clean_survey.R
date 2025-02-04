# Load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("magrittr")) install.packages("magrittr")

library(tidyverse)
library(lubridate)
library(magrittr)

# Read the data
survey_raw <- read.csv("survey.csv", stringsAsFactors = FALSE)

# Add print statements for debugging
print("Data loaded successfully")
print(paste("Number of rows in raw data:", nrow(survey_raw)))

# Clean the data
clean_survey <- survey_raw %>%
  # Clean Gender field - standardize to M/F/Other
  mutate(
    Gender = case_when(
      tolower(Gender) %in% c("m", "male", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man", "msle", "mail", "malr", "cis male") ~ "M",
      tolower(Gender) %in% c("f", "female", "female (cis)", "femake", "woman", "cis female") ~ "F",
      TRUE ~ "Other"
    ),
    # Convert Age to numeric
    Age = as.numeric(Age),
    # Clean work_interfere
    work_interfere = factor(work_interfere, levels = c("Never", "Rarely", "Sometimes", "Often")),
    # Convert yes/no fields to logical
    self_employed = ifelse(self_employed == "Yes", TRUE, FALSE),
    remote_work = ifelse(remote_work == "Yes", TRUE, FALSE),
    tech_company = ifelse(tech_company == "Yes", TRUE, FALSE),
    benefits = ifelse(benefits == "Yes", TRUE, FALSE),
    treatment = ifelse(treatment == "Yes", TRUE, FALSE),
    # Convert Timestamp to date
    Timestamp = ymd_hms(Timestamp)
  ) %>%
  # Remove rows with invalid ages
  filter(Age >= 18 & Age <= 100)

# Add print statement to confirm cleaning
print(paste("Number of rows in cleaned data:", nrow(clean_survey)))

# Save cleaned data
saveRDS(clean_survey, "clean_survey.rds")

# Verify file was saved
if (file.exists("clean_survey.rds")) {
  print("clean_survey.rds file was created successfully")
} else {
  print("ERROR: Failed to create clean_survey.rds file")
}
