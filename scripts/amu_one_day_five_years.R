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
  filter(AMU_ADMISSION_DATETIME >= '2020-03-23 00:00' &
           AMU_ADMISSION_DATETIME <= '2020-03-29 23:59') |> 
  arrange(AMU_ADMISSION_DATETIME)

amu_mean_los <- mean(df_02$AMU_LOS_HOURS)

# draw a graph ------------------------------------------------------------

ggplot(data = df_02) +
  geom_segment(aes(x = AMU_ADMISSION_DATETIME,
                   xend = AMU_ADMISSION_DATETIME,
                   y = 0,
                   yend = AMU_LOS_HOURS),
               #linewidth = 0.5,
               colour = "blue") +
  geom_hline(yintercept = amu_mean_los,
            linetype = "dashed",
            colour = "blue") +
  scale_y_continuous(limits = c(0, 168),
                     breaks = seq(0, 168, 24)) +
  scale_x_datetime(limits = c(as.POSIXct('2020-03-23 00:00',
                                         tz = "UTC"),
                              as.POSIXct('2020-03-29 23:59',
                                         tz = "UTC")),
                   breaks = "1 day",
                   date_labels = "%d-%b") +
  labs(title = "Mon 23 March to Sun 29 March 2020",
       subtitle = "174 AMU admissions | average length of stay = 25 hours",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_line(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

               