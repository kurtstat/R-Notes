# amu five years - the geom_segment() thing for AMU length of stay


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/amu_five_years.xlsx")


# grab a day --------------------------------------------------------------

# FOURTH MONDAY IN MARCH
# MON 23 MARCH 2020
# MON 22 MARCH 2021
# MON 28 MARCH 2022
# MON 27 MARCH 2023
# MON 25 MARCH 2024

df_02 <-
  df_01 |> 
  filter(AMU_DISCHARGE_DATETIME >= '2024-03-25 00:00' &
           AMU_ADMISSION_DATETIME <= '2024-03-25 23:59') |> 
  arrange(AMU_ADMISSION_DATETIME) |> 
  mutate(new_uai = paste0("A-", UNIQUE_ADMISSION_IDENTIFIER)) |> 
  mutate(f_start = if_else(AMU_ADMISSION_DATETIME < '2024-03-25 00:00', 
                           as.POSIXct('2024-03-25 00:00',
                                      tz = "UTC"), 
                           as.POSIXct(AMU_ADMISSION_DATETIME,
                                      tz = "UTC"))) |> 
  mutate(f_end = if_else(AMU_DISCHARGE_DATETIME > '2024-03-25 23:59', 
                           as.POSIXct('2024-03-25 23:59',
                                      tz = "UTC"), 
                           as.POSIXct(AMU_DISCHARGE_DATETIME,
                                      tz = "UTC"))) |> 
  arrange(desc(f_start))

amu_mean_los <- mean(df_02$AMU_LOS_HOURS)

# draw a graph ------------------------------------------------------------

ggplot(data = df_02) +
  aes(y = reorder(new_uai,
                  desc(AMU_ADMISSION_DATETIME))) +
  geom_segment(aes(x = as.POSIXct(AMU_ADMISSION_DATETIME),
                   xend = as.POSIXct(AMU_DISCHARGE_DATETIME)),
               colour = "blue") +
  scale_x_datetime(limits = c(as.POSIXct('2024-03-20 00:00'),
                              as.POSIXct('2024-03-31 00:00')),
                   breaks = c(as.POSIXct('2024-03-25 00:00'),
                              as.POSIXct('2024-03-25 23:59')),
                   date_labels = "%H:%M") +
  labs(title = "Mon 25 March 2025",
       subtitle = "92 AMU inpatients overlapped that day at some point",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.line.x = element_line(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

               