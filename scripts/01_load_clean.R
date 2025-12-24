library(tidyverse)
library(dplyr)
library(readxl)
library(gtsummary)
library(jtools)

data <- read_excel('Hospital_General_Information_2016_2020.xlsx')

#Recoding response variable into a binary variable
df <- data |>
  mutate(
    High_Readmission_Risk = case_when(
      `Readmission national comparison` == "Below the national average" ~ 1,
      `Readmission national comparison` %in% c(
        "Above the national average",
        "Same as the national average"
      ) ~ 0,
      TRUE ~ NA_real_
    )
  )

#Dropping outcomes which are not NA (not available) in the High_Readmission_Risk Column
df <- df |> filter(!is.na(High_Readmission_Risk))

#Cleaning Predictor variables
#Dropping NA's for Variables
df <- df |> filter(!is.na(`Hospital Ownership`))
df <- df |> filter(!is.na(`Hospital Type`))
df <- df |> filter(!is.na(State))

#Recoding Emergency Service variable to binary
df <- df |>
  mutate(
    Emergency_Service = ifelse(`Emergency Services` == "Yes", 1, 0)
  )

#converting year variable to numeric
df$Year <- as.numeric(df$Year)
is.numeric(df$Year)

#remove hospital duplicates if any
df <- df |> distinct()
