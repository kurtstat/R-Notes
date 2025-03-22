
# The W graphic -------------------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/01_ed_duration_extract.xlsx")


# restrict to one day's arrivals ------------------------------------------

df_02 <-
  df_01 |> 
  filter(arrival_datetime >= as.POSIXct('2014-10-01 00:00',
                                        tz = "UTC") &
           arrival_datetime <= as.POSIXct('2014-10-01 23:59',
                                          tz =  "UTC")) |> 
  arrange(arrival_datetime) |> 
  mutate(dest = case_when(adm_ward == "Ward_02" | adm_ward == "Ward_CDU" ~ "AMU",
                          adm_ward == "Ward_04" ~ "SAU",
                          adm_ward != "Ward_02" | adm_ward != "Ward_CDU" | adm_ward != "Ward_04" ~ "other",
                          is.na(adm_ward) ~ "home"))


# draw the graph using geom_col() ---------------------------

ggplot(data = df_02) +
  aes(x = arrival_datetime,
      y = los_mins,
      fill = dest) +
  geom_col(stat = "identity") +
  scale_fill_manual(values = c("darkgreen",
                               "darkgrey",
                               "blue",
                               "darkgreen")) +
  scale_x_datetime(limits = c(as.POSIXct('2014-10-01 00:00',
                                       tz = "UTC-1"),
                              as.POSIXct('2014-10-02 00:00',
                                         tz = "UTC")),
                   breaks = "3 hour",
                   date_labels = "%H:%M") +
  scale_y_continuous(limits = c(0, 1440),
                     breaks = seq(0, 1440, 240)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


# draw a graph using geom_segment() ---------------------------------------

ggplot(data = df_02) +
  geom_segment(aes(x = arrival_datetime,
                   xend = arrival_datetime,
                   y = 0,
                   yend = los_mins,
                   colour = dest)) +
  scale_colour_manual(values = c("red",
                               "darkgrey",
                               "red",
                               "red")) +
  scale_x_datetime(limits = c(as.POSIXct('2014-10-01 00:00',
                                         tz = "UTC-1"),
                              as.POSIXct('2014-10-02 00:00',
                                         tz = "UTC")),
                   breaks = "3 hour",
                   date_labels = "%H:%M") +
  scale_y_continuous(limits = c(0, 1440),
                     breaks = seq(0, 1440, 240)) +
  labs(title = "How many and how long?",
       subtitle = "ED attendances (n = 163) by time of arrival and length of stay (mins): 1 October 2014",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

