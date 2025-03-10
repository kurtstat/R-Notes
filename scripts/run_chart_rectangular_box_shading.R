
# Run charts with rectangular box shading ---------------------------------


# Step 0 - load packages --------------------------------------------------

library(readxl)
library(tidyverse)
library(ggtext)


# Step 1 - import the data ------------------------------------------------

df_run_chart_data <- 
  read_xlsx("data/inreach_weekly_data_v3.xlsx",
            sheet = "data") |> 
  select(discharge_week,
         percent_discharged_home) |> 
  mutate(median = median(percent_discharged_home))


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
  summarize(no_of_points = n(),
            median = median(median),
            min_week = min(discharge_week) - (3.5*24*60*60),
            max_week = max(discharge_week) + (3.5*24*60*60),
            min_percent = min(percent_discharged_home,
                              median),
            max_percent = max(percent_discharged_home,
                              median))
df_run_chart_table_a <-
  df_run_chart_table |> 
  mutate(rule_break = if_else(no_of_points >= 6,
                              "rule_break",
                              "nothing_to_see"))


# Step 2 - draw the run chart ---------------------------------------------

ggplot(data = df_run_chart_data_a) +
  geom_rect (data = df_run_chart_table_a,
             aes(xmin = min_week,
                 xmax = max_week,
                 ymin = min_percent,
                 ymax = max_percent,
                 fill = rule_break),
             alpha = 0.6) +
  geom_line(aes(x = discharge_week,
                y = percent_discharged_home)) +
  geom_point(aes(x = discharge_week,
                 y = percent_discharged_home)) +
  geom_line(aes(x = discharge_week,
                y = median),
            size = 0.5) +
  scale_fill_manual(values = c("lightgrey", "#FF6600")) +
  scale_y_continuous(limits = c(0.15, 0.45),
                     breaks = seq(0.15, 0.45, 0.05),
                     labels = scales::percent) +
  scale_x_datetime(date_breaks = "8 week", date_labels =  "%d-%b") +
  labs(title = "Plot the blocks",
       subtitle = "52 data points | 52 useful observations | 22 runs | <span style='color: #FF6600;'>4 rule breaks</span>",
       x = "",
       y = "") +
  annotate("text", x = as.POSIXct('2014-11-19'), 
           y = 0.165, 
           label = "6 points",
           size = 3,
           colour = "#FF6600") +
  annotate("text", x = as.POSIXct('2015-02-08'), 
           y = 0.19, 
           label = "7 points",
           size = 3,
           colour = "#FF6600") +
  annotate("text", x = as.POSIXct('2015-04-17'), 
           y = 0.425, 
           label = "7 points",
           size = 3,
           colour = "#FF6600") +
  annotate("text", x = as.POSIXct('2015-08-02'), 
           y = 0.415, 
           label = "7 points",
           size = 3,
           colour = "#FF6600") +
  #theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        #axis.line.y = element_line(),
        #axis.ticks.x = 
        legend.position = "none",
        plot.subtitle = element_markdown(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
