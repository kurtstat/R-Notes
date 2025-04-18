
# the musk tweet thing ----------------------------------------------------


# load packages -----------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(hms)
library(scales)


# import data -------------------------------------------------------------

df_01 <-
  read.xlsx("https://www.kurtosis.co.uk/data/amu_five_years.xlsx") |> 
  mutate(new_amu_start_datetime = convertToDateTime(AMU_ADMISSION_DATETIME)) |> 
  select(UNIQUE_ADMISSION_IDENTIFIER,
         new_amu_start_datetime,
         AMU_LOS_HOURS)


# restrict to >120 hour stays ----------------------------------------------

df_02 <-
  df_01 |> 
  filter(AMU_LOS_HOURS > 120)


# separate date and time --------------------------------------------------

df_03 <-
  df_02 |> 
  mutate(amu_arrival_date = floor_date(new_amu_start_datetime,
                                       "day")) |> 
  mutate(amu_arrival_time = hms::as_hms(new_amu_start_datetime))


# draw the scatterplot ----------------------------------------------------
ggplot(data = df_03) +
  aes(x = amu_arrival_date,
      y = amu_arrival_time) +
  geom_point(size = 0.7,
             colour = "red") +
  geom_vline(xintercept = as.POSIXct('2021-11-30')) +
  scale_y_reverse(labels = function(x) hms::as_hms(x),
                  breaks = seq(as.numeric(hms::as_hms("00:00:00")), 
                               as.numeric(hms::as_hms("24:00:00")),
                               21600),
                  position = "right") +
  scale_x_datetime(limits = c(as.POSIXct('2019-04-01'),
                          as.POSIXct('2024-03-31')),
                   breaks = "year",
                   labels = date_format("%Y"),
                   expand = c(0, 0)) +
  labs(title = "Did Elon Musk buy AMU in October 2021?",
       subtitle = "Patients spending longer than a week in the AMU",
       x= NULL,
       y = NULL) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
                  