
# AMU Two Speed Length of Stay --------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)
library(scales)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/amu_five_years.xlsx") |> 
  arrange(desc(AMU_LOS_HOURS)) |> 
  filter(AMU_LOS_HOURS <= 1120)


# create a summary table of quantiles by month ----------------------------

df_02 <-
  df_01 |> 
  mutate(new_amu_discharge_month = floor_date(AMU_DISCHARGE_DATETIME,
                                              "month")) |> 
  group_by(new_amu_discharge_month) |> 
  summarize(no_of_amu_discharges = n(),
            mean_los = mean(AMU_LOS_HOURS),
            quantile_05 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.005)),
            quantile_10 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.1)),
            quantile_25 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.25)),
            quantile_50 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.5)),
            quantile_75 = quantile(AMU_LOS_HOURS,
                                    probs = c(0.75)),
            quantile_90 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.9)),
            quantile_95 = quantile(AMU_LOS_HOURS,
                                    probs = c(0.95)))


# create a graph ----------------------------------------------------------

ggplot(data = df_02) +
  aes(x = new_amu_discharge_month) +
  geom_line(aes(y = quantile_05)) +
  geom_line(aes(y = quantile_25)) +
  geom_line(aes(y = quantile_50)) +
  geom_line(aes(y = quantile_75)) +
  geom_line(aes(y = quantile_95)) +
  geom_vline(xintercept = as.POSIXct('2021-06-01'),
             linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct('2022-12-31'),
             linetype = "dashed") +
  annotate("rect",
           xmin = as.POSIXct('2021-06-01'), 
           xmax = as.POSIXct('2022-12-31'), 
           ymin = -Inf, 
           ymax = Inf,  
           fill = "grey", 
           alpha= 0.3) +
  #geom_ribbon(aes(ymin = quantile_05,
                  #ymax = quantile_25),
              #fill = "green",
              #alpha = 0.3) +
  #geom_ribbon(aes(ymin = quantile_75,
                  #ymax = quantile_95),
              #fill = "red",
              #alpha = 0.3) +
  scale_x_datetime(limits = c(as.POSIXct('2014-01-01'),
                              as.POSIXct('2025-01-31')),
                   breaks = "12 month",
                   labels = date_format("%Y"),
                   expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 168),
                     breaks = seq(0, 168, 24),
                     label = comma) +
  labs(title = "Never mind the mean, feel the quantiles",
       subtitle = "Length of stay in AMU (hours): April 2019 to March 2024",
       x = NULL,
       y = NULL,
       caption = "Source: Patient Management System") +
  theme_minimal() +
  theme(axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.caption = element_text(hjust = 0,
                                    colour = "black"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))

