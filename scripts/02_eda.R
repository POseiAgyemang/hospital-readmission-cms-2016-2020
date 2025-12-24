library(tidyverse)
library(dplyr)
library(readxl)
library(gtsummary)
library(jtools)
library(ggplot2)

#Exploratory Data Analayis.
#How many hospitals performed worse than the national average
prop.table(table(df$High_Readmission_Risk))

df <- df |>
  mutate(Readmission_Status = ifelse(
    High_Readmission_Risk == 1,
    "Below the National Average",
    "Better/Same as National Average"
  ))

  ggplot(df, aes(x = Readmission_Status)) +
  geom_bar(aes(y = after_stat(prop), group=1),
           fill = "navy") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribution of Hospital Readmission Performance",
    x = "Readmission Performance Classification",
    y = "Percentage of Hospitals"
  ) +
  theme_minimal()
  
  #Do hospital readmission outcome differ by hospital ownership type?
  ownership_summary <- df |>
    group_by(`Hospital Ownership`) |>
    summarise(
      pct_worse = mean(High_Readmission_Risk) * 100,
      n = n()
    ) |>
    arrange(desc(pct_worse))
  
  ggplot(ownership_summary,
         aes(x = reorder(`Hospital Ownership`, pct_worse),
             y = pct_worse)) +
    geom_col(fill = "green") +
    coord_flip() +
    labs(
      title = "Hospital Readmission Performance by Ownership Type",
      x = "Hospital Ownership",
      y = "Percentage Worse Than National Average"
    ) +
    theme_minimal()

  #Are hospitals that offer emergency services more likely to 
  #have worse readmission performance?
  emergency_summary <- df |>
    group_by(Emergency_Service) |>
    summarise(
      pct_worse = mean(High_Readmission_Risk) * 100,
      n = n()
    )

  ggplot(emergency_summary,
         aes(x = factor(Emergency_Service,
                        labels = c("No Emergency Services",
                                   "Emergency Services")),
             y = pct_worse)) +
    geom_col(fill = "brick-red") +
    labs(
      title = "Hospital Readmission Performance by Emergency Service Availability",
      x = "Emergency Services",
      y = "Percentage of Hospitals Worse Than National Average"
    ) +
    theme_minimal()
  
#Has hospital readmission performance improved, worsened, or remained stable 
#between 2016 and 2020?
  year_summary <- df |>
    group_by(Year) |>
    summarise(
      pct_worse = mean(High_Readmission_Risk) * 100,
      n = n()
    )
  
  ggplot(year_summary, aes(x = Year, y = pct_worse)) +
    geom_line(color = "navy", linewidth = 1.2) +
    geom_point(color = "navy", size = 2) +
    labs(
      title = "Trend in Hospital Readmission Performance (2016–2020)",
      x = "Year",
      y = "Percentage of Hospitals Worse Than National Average"
    ) +
    theme_minimal()
