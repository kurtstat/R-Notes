
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
  filter(AMU_LOS_HOURS <= 336)


# create a summary table of quantiles by day ----------------------------

df_02 <-
  df_01 |> 
  group_by(new_amu_discharge_date) |> 
  summarize(no_of_amu_discharges = n(),
            mean_los = mean(AMU_LOS_HOURS),
            quantile_00 = quantile(AMU_LOS_HOURS,
                                   probs = c(0)),
            quantile_25 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.25)),
            quantile_50 = quantile(AMU_LOS_HOURS,
                                   probs = c(0.5)),
            quantile_75 = quantile(AMU_LOS_HOURS,
                                    probs = c(0.75)),
            quantile_100 = quantile(AMU_LOS_HOURS,
                                    probs = c(1)))


# append the relevant quantiles to the raw data ---------------------------

df_03 <-
  df_01 |> 
  left_join(df_02,
            join_by(x$new_amu_discharge_date == y$new_amu_discharge_date)) |> 
  select(new_amu_discharge_date,
         AMU_ADMISSION_DATETIME,
         AMU_DISCHARGE_DATETIME,
         AMU_LOS_HOURS,
         quantile_00,
         quantile_25,
         quantile_50,
         quantile_75,
         quantile_100) |> 
  arrange(new_amu_discharge_date)


# assign the los_quartiles to each discharge ----------------------

df_04_q1 <-
  df_03 |> 
  filter(AMU_LOS_HOURS >= quantile_00 & AMU_LOS_HOURS <= quantile_25) |> 
  mutate(los_quartile = "q1")

df_04_q2 <-
  df_03 |> 
  filter(AMU_LOS_HOURS > quantile_25 & AMU_LOS_HOURS <= quantile_50) |> 
  mutate(los_quartile = "q2")

df_04_q3 <-
  df_03 |> 
  filter(AMU_LOS_HOURS > quantile_50 & AMU_LOS_HOURS <= quantile_75) |> 
  mutate(los_quartile = "q3")

df_04_q4 <-
  df_03 |> 
  filter(AMU_LOS_HOURS > quantile_75 & AMU_LOS_HOURS <= quantile_100) |> 
  mutate(los_quartile = "q4")

df_04 <-
  rbind(df_04_q1,
        df_04_q2,
        df_04_q3,
        df_04_q4) |> 
  arrange(AMU_DISCHARGE_DATETIME)

# create a graph ----------------------------------------------------------

ggplot(data = df_04) +
  annotate("rect",
           xmin = as.POSIXct('2021-06-01'), 
           xmax = as.POSIXct('2022-12-31'), 
           ymin = -Inf, 
           ymax = Inf,  
           fill = "grey", 
           alpha= 0.2) +
  aes(x = AMU_DISCHARGE_DATETIME) +
  geom_point(aes(y = AMU_LOS_HOURS,
                 colour = los_quartile),
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
  scale_colour_manual(values = c("#1a9641", "#C0C0C0", "#C0C0C0", "#d7191c")) +
  labs(title = "Flowing, <span style='color: #1a9641;'>Fast</span>, <span style='color: #C0C0C0;'>medium</span> and <span style='color: #d7191c;'>Slow</span>",
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


# create a dataframe with 1827 x 24 = 43,848 snapshot hours ---------------

dataframe_43848 <-
  data.frame(date_hour = seq(as.POSIXct("2019-04-01 00:01"),
                             as.POSIXct("2024-03-31 23:01"),
                             60*60))


# do the inner_join() -----------------------------------------------------

dataframe_05 <-
  dataframe_43848 |> 
  inner_join(df_04,
             join_by(date_hour >= AMU_ADMISSION_DATETIME,
                     date_hour < AMU_DISCHARGE_DATETIME)) |> 
  group_by(los_quartile,
           date_hour) |> 
  summarize(no_of_occupied_beds = n())


# convert hourly snapshots to daily average snapshots ---------------------

df_06 <-
  dataframe_05 |> 
  mutate(snapshot_date = floor_date(date_hour,
                                    "day")) |> 
  group_by(los_quartile,
           snapshot_date) |> 
  summarize(mean_amu_daily_fullness = mean(no_of_occupied_beds))



# let pivot_wider() work its magic ----------------------------------------

df_07 <-
  df_06 |> 
  pivot_wider(names_from = los_quartile,
              values_from = mean_amu_daily_fullness) |> 
  filter(snapshot_date <= '2024-03-24 23:59')


# draw the four jagged lines of fullness ----------------------------------

ggplot(data = df_07) +
  annotate("rect",
           xmin = as.POSIXct('2021-06-01'), 
           xmax = as.POSIXct('2022-12-31'), 
           ymin = -Inf, 
           ymax = Inf,  
           fill = "grey", 
           alpha= 0.2) +
  aes(x = snapshot_date) +
  geom_line(aes(y = q4),
            colour = "#d7191c",
            alpha = 1) +
  #geom_line(aes(y = q3)) +
  #geom_line(aes(y = q2)) +
  geom_line(aes(y = q1),
            colour = "#1a9641",
            alpha = 1) +
  geom_vline(xintercept = as.POSIXct('2021-06-01'),
             linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct('2022-12-31'),
             linetype = "dashed") +
  scale_x_datetime(limits = c(as.POSIXct('2014-01-01'),
                              as.POSIXct('2025-01-31')),
                   breaks = "12 month",
                   labels = date_format("%Y"),
                   expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, 10),
                     label = comma) +
  scale_colour_manual(values = c("#1a9641", "#d7191c")) +
  labs(title = "Fullness, <span style='color: #1a9641;'>Fast</span> and <span style='color: #d7191c;'>Slow</span>",
       subtitle = "Beds occupied in AMU by <span style='color: #1a9641;'>lower quartile</span> length of stay patients  \n and <span style='color: #d7191c;'>upper quartile</span> length of stay patients: April 2019 to March 2024",
       x = NULL,
       y = NULL,
       caption = "Source: Patient Management System") +
  theme(legend.position = "none",
        axis.line.x = element_line(colour = "black"),
        #axis.line.y = element_line(colour = "black"),
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
        plot.subtitle = element_markdown(size = 9),
        #plot.subtitle = element_textbox_simple(size = 9),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
