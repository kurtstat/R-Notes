
# The Coloured Dots Graphic (with added transparency) ---------------------


# load packages -----------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggtext)


# import data -------------------------------------------------------------

df_01 <-
  read.xlsx("https://www.kurtosis.co.uk/data/01_ed_duration_extract.xlsx")


# fix the datetimes ------------------------------------------------------

df_02 <-
  df_01 |> 
  mutate(new_arrival_datetime = convertToDateTime(arrival_datetime,
                                                  tz = "UTC")) |> 
  mutate(new_departure_datetime = convertToDateTime(departure_datetime,
                                                    tz = "UTC")) |>
  select(patient_id,
         new_arrival_datetime,
         new_departure_datetime,
         los_mins,
         adm_ward)


# select one day ----------------------------------------------------------

df_03 <-
  df_02 |> 
  filter(new_arrival_datetime <= '2014-10-01 23:59' &
           new_departure_datetime >= '2014-10-01 00:00')


# isolate the arrivals ----------------------------------------------------

df_03_arrivals <-
  df_03 |> 
  filter(new_arrival_datetime >= '2014-10-01 00:00')


# isolate the departures --------------------------------------------------

df_03_departures <-
  df_03 |> 
  filter(new_departure_datetime <= '2014-10-01 23:59')


# create the movement_type variables --------------------------------------

# first, for the arrivals:

df_03_arrivals_a <-
  df_03_arrivals |> 
  mutate(movement_datetime = new_arrival_datetime) |> 
  mutate(movement_15 = floor_date(movement_datetime,"15 minutes")) |>  
  mutate(movement_type = "arrival") |> 
  mutate(in_out = 1) |> 
  select(patient_id,
         los_mins,
         movement_datetime,
         movement_15,
         movement_type,
         in_out)

# second, for the departures:

df_03_departures_a <-
  df_03_departures |> 
  mutate(movement_datetime = new_departure_datetime) |> 
  mutate(movement_15 = floor_date(movement_datetime,"15 minutes")) |>  
  mutate(movement_type = ifelse(is.na(adm_ward),"departure","transfer")) |> 
  mutate(in_out = -1) |> 
  select(patient_id,
         los_mins,
         movement_datetime,
         movement_15,
         movement_type,
         in_out)

# bind the arrivals and departures dataframes together --------------------

df_04 <- 
  rbind(df_03_arrivals_a, 
        df_03_departures_a)


# calculate the movement_15_seqno using cumsum() --------------------------

df_05 <- 
  df_04 |> 
  group_by(in_out,
           movement_15) |>  
  mutate(movement_15_seqno = cumsum(in_out)) |> 
  arrange(movement_datetime)


# draw a scatterplot ------------------------------------------------------

ggplot(data = df_05) +
  aes(x = movement_15,
      y = movement_15_seqno,
      fill = movement_type,
      alpha = los_mins) +
  geom_point(pch = 21,
             size = 2,
             stroke = NA) +
  scale_fill_manual(values = c("lightgrey", "lightgrey", "blue")) +
  scale_x_datetime(limits = c(as.POSIXct('2014-10-0100:00',
                                         tz = "UTC-2"),
                              as.POSIXct('2014-10-02 00:00',
                                         tz = "UTC")),
                   date_labels = "%H:%M",
                   date_breaks = "3 hour") +
  scale_y_continuous(limits = c(-7, 8)) +
  labs(title = "Comings and goings",
       subtitle = "Emergency Department <span style='color: darkgrey;'>arrivals</span>, <span style='color: darkgrey;'>departures</span> and <span style='color: blue;'>admissions</span>: Wed 1 October 2014",
       x = NULL,
       y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "grey"),
        plot.title = element_text(),
        plot.subtitle = element_markdown(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
