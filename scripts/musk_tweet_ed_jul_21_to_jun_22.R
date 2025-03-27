
# the musk tweet thing ----------------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)
library(hms)
library(scales)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/rie_ed_jul_21_to_jun_22.xlsx") |> 
  rename(ed_arrival_datetime = ed_arrival_date) |> 
  rename(ed_los_mins = ed_los)


# delete improbably long (>48 hours) stays --------------------------------

df_02 <-
  df_01 |>
  filter(ed_los_mins < 2880) |> 
  filter(ed_los_mins > 720)


# separate date and time --------------------------------------------------

df_03 <-
  df_02 |>
  mutate(ed_arrival_date = floor_date(ed_arrival_datetime,
                                       "day")) |>
  mutate(ed_arrival_time = hms::as_hms(ed_arrival_datetime))


# draw the scatterplot ----------------------------------------------------
ggplot(data = df_03) +
  aes(x = ed_arrival_date,
      y = ed_arrival_time) +
  geom_point(size = 0.1,
             colour = "#cb181d") +
  #geom_vline(xintercept = as.POSIXct('2021-11-30')) +
  #scale_y_reverse(labels = function(x) hms::as_hms(x),
  scale_y_reverse(labels = function(x) substring(format(hms::hms(x),"%H:%M"),1,5),
                  breaks = seq(as.numeric(hms::as_hms("00:00:00")),
                               as.numeric(hms::as_hms("24:00:00")),
                               21600),
                  position = "right") +
  scale_x_datetime(limits = c(as.POSIXct('2021-07-01',
                                         tz = "UTC"),
                          as.POSIXct('2022-06-30',
                                     tz = "UTC")),
                   breaks = "91 days",
                   labels = date_format("%d-%b-%y"),
                   expand = c(0, 0)) +
  labs(title = "The year it fell apart",
       subtitle = "ED arrivals with stays >12 hours: 1 July 2021 to 30 June 2022",
       caption = "Source: Patient Management System",
       x = NULL,
       y = NULL) +
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.caption = element_text(hjust = 0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))



