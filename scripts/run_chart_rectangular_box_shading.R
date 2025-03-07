
# Run charts with rectangular box shading ---------------------------------


# Step 0 - load packages --------------------------------------------------

library(readxl)
library(tidyverse)


# Step 1 - import the data ------------------------------------------------

df_run_chart_data <- 
  read_xlsx("data/inreach_weekly_data_v2.xlsx")


# Step 2 - draw the run chart ---------------------------------------------

ggplot(data = df_run_chart_data) +
  aes(x = discharge_week) +
  geom_line(aes(y = percent_discharged_home)) +
  geom_point(aes(y = percent_discharged_home)) +
  geom_line(aes(y = median)) +
  geom_rect (aes(xmin = xmin,
                 xmax = xmax,
                 ymin = ymin,
                 ymax = ymax),
             alpha = 0.1) +
  theme_minimal()
