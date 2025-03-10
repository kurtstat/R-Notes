
# Run charts with rectangular box shading ---------------------------------


# Step 0 - load packages --------------------------------------------------

library(readxl)
library(tidyverse)


# Step 1 - import the data ------------------------------------------------

df_run_chart_data <- 
  read_xlsx("data/inreach_weekly_data_v3.xlsx",
            sheet = "data") |> 
  select(discharge_week,
         percent_discharged_home) |> 
  mutate(median = median(percent_discharged_home))

df_run_chart_runs <- 
  read_xlsx("data/inreach_weekly_data_v3.xlsx",
            sheet = "run_table")


# Step 1a - create the run_table markers ---------------------------

df_run_chart_data_a <-
  df_run_chart_data |>
  arrange(discharge_week) |> 
  mutate(median = median(percent_discharged_home)) |> 
  mutate(above_below_on = case_when(percent_discharged_home > median ~ "above",
                                    percent_discharged_home < median ~ "below",
                                    percent_discharged_home == median ~ "on")) |> 
  mutate(point_no = row_number()) |> 
  mutate(previous = lag(above_below_on)) |> 
  mutate(first_point = case_when(is.na(previous) ~ point_no,
                                 above_below_on!= previous ~ point_no, 
                                 above_below_on == previous ~ NA)) |> 
  fill(first_point)
  


# Step 1c - create the (summarized) run table -----------------------------

df_run_chart_table <-
  df_run_chart_data_a |> 
  group_by(first_point) |> 
  summarize(median = median(median),
            min_week = min(discharge_week) - (3.5*24*60*60),
            max_week = max(discharge_week) + (3.5*24*60*60),
            min_percent = min(percent_discharged_home,
                              median),
            max_percent = max(percent_discharged_home,
                              median))


# Step 2 - draw the run chart ---------------------------------------------

ggplot(data = df_run_chart_data_a) +
  geom_rect (data = df_run_chart_table,
             aes(xmin = min_week,
                 xmax = max_week,
                 ymin = min_percent,
                 ymax = max_percent),
             fill = "#5F5F5F",
             alpha = 0.2) +
  geom_line(aes(x = discharge_week,
                y = percent_discharged_home)) +
  geom_point(aes(x = discharge_week,
                 y = percent_discharged_home)) +
  geom_line(aes(x = discharge_week,
                y = median),
            size = 0.7) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
