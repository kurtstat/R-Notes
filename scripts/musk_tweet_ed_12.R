
# the musk tweet thing ----------------------------------------------------


# load packages -----------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(hms)
library(scales)


# import data -------------------------------------------------------------

df_01 <-
  read.xlsx("https://www.kurtosis.co.uk/data/ed_12_hour_stays.xlsx") |>
  mutate(new_arrival_datetime = convertToDateTime(arrival_datetime)) |>
  select(seqno,
         new_arrival_datetime,
         los_mins)


# separate date and time --------------------------------------------------

df_02 <-
  df_01 |>
  mutate(arrival_date = floor_date(new_arrival_datetime,
                                       "day")) |>
  mutate(arrival_time = hms::as_hms(new_arrival_datetime))


# draw the scatterplot ----------------------------------------------------
ggplot(data = df_02) +
  aes(x = arrival_date,
      y = arrival_time) +
  geom_point(size = 0.01,
             colour = "red",
             alpha = 0.3) +
  #geom_vline(xintercept = as.POSIXct('2021-11-30')) +
  #scale_y_reverse(labels = function(x) hms::as_hms(x),
  scale_y_reverse(labels = function(x) substring(format(hms::hms(x),"%H:%M"),1,5),
                  breaks = seq(as.numeric(hms::as_hms("00:00:00")),
                               as.numeric(hms::as_hms("24:00:00")),
                               21600),
                  position = "right") +

  scale_x_datetime(limits = c(as.POSIXct('2014-01-01'),
                          as.POSIXct('2024-06-01')),
                   breaks = "12 month",
                   labels = date_format("%Y"),
                   expand = c(0, 0)) +
  labs(title = "12-hour stays, Elon Musk Tweet style",
       subtitle = "Arrival times of patients who spent longer than 12 hours \nin the Emergency Department",
       caption = "Source: Patient Management System",
       x = NULL,
       y = NULL) +
  theme(axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.caption = element_text(hjust = 0),
        axis.title = element_text(colour = "black"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))




