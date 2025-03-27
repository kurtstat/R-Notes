
# AMU Two Speed Length of Stay --------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)
library(scales)
library(ggtext)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/amu_five_years.xlsx") |> 
  mutate(new_amu_discharge_date = floor_date(AMU_DISCHARGE_DATETIME,
                                             "day")) |> 
  arrange(desc(AMU_LOS_HOURS)) |> 
  filter(AMU_LOS_HOURS <= 1120)


# create a summary table of quantiles by day ----------------------------

df_02 <-
  df_01 |> 
  group_by(new_amu_discharge_date) |> 
  summarize(no_of_amu_discharges = n(),
            mean_los = mean(AMU_LOS_HOURS),
            quantile_05 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.05)),
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


# append the relevant quantiles to the raw data ---------------------------

df_03 <-
  df_01 |> 
  left_join(df_02,
            join_by(x$new_amu_discharge_date == y$new_amu_discharge_date)) |> 
  select(new_amu_discharge_date,
         AMU_DISCHARGE_DATETIME,
         AMU_LOS_HOURS,
         quantile_05,
         quantile_25,
         quantile_50,
         quantile_75,
         quantile_95) |> 
  arrange(new_amu_discharge_date)


# filter to just the two slow and fast speed cohorts ----------------------

df_04_fast <-
  df_03 |> 
  filter(AMU_LOS_HOURS >= quantile_05 & AMU_LOS_HOURS <= quantile_25) |> 
  mutate(fast_slow = "fast")

df_04_slow <-
  df_03 |> 
  filter(AMU_LOS_HOURS >= quantile_75 & AMU_LOS_HOURS <= quantile_95) |> 
  mutate(fast_slow = "slow")

df_04_fast_and_slow <-
  rbind(df_04_fast,
        df_04_slow)

# create a graph ----------------------------------------------------------

ggplot(data = df_04_fast_and_slow) +
  annotate("rect",
           xmin = as.POSIXct('2021-06-01'), 
           xmax = as.POSIXct('2022-12-31'), 
           ymin = -Inf, 
           ymax = Inf,  
           fill = "grey", 
           alpha= 0.2) +
  aes(x = AMU_DISCHARGE_DATETIME) +
  geom_point(aes(y = AMU_LOS_HOURS,
                 colour = fast_slow),
             size = 0.1,
             alpha = 0.1) +
  #geom_line(aes(y = quantile_05)) +
  #geom_line(aes(y = quantile_25)) +
  geom_vline(xintercept = as.POSIXct('2021-06-01'),
             linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct('2022-12-31'),
             linetype = "dashed") +
  scale_x_datetime(limits = c(as.POSIXct('2014-01-01'),
                              as.POSIXct('2025-01-31')),
                   breaks = "12 month",
                   labels = date_format("%Y"),
                   expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 168),
                     breaks = seq(0, 168, 24),
                     label = comma) +
  scale_colour_manual(values = c("#1a9641", "#d7191c")) +
  labs(title = "Flowing, <span style='color: #1a9641;'>Fast</span> and <span style='color: #d7191c;'>Slow</span>",
       subtitle = "Length of stay in AMU (hours): April 2019 to March 2024",
       x = NULL,
       y = NULL,
       caption = "Source: Patient Management System") +
  theme(legend.position = "none",
        axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major.y = element_line(colour = "grey",
                                          linetype = "dotted"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.caption = element_text(hjust = 0,
                                    colour = "black"),
        plot.title = element_markdown(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))

