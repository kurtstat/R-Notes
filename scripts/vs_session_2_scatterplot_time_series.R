
# AMU Five Year Timeline --------------------------------------------------


# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)


# Import ------------------------------------------------------------------

amu_data <-
  read_xlsx("data/amu_five_years.xlsx")


# Scatterplot -------------------------------------------------------------

ggplot(data = amu_data) +
  aes(x = AMU_DISCHARGE_MONTH,
      y = AMU_LOS_HOURS) +
  geom_point()


# Summary table -----------------------------------------------------------

summary_table <-
  amu_data |> 
  mutate(amu_discharge_date = floor_date(AMU_DISCHARGE_DATETIME,
                                         "days")) |> 
  group_by(amu_discharge_date) |> 
  summarize(mean_los = mean(AMU_LOS_HOURS)) |> 
  mutate(amu_discharge_month = floor_date(amu_discharge_date,
                                          "months"))


# New scatterplot ---------------------------------------------------------

ggplot(data = summary_table) +
  aes(x = as.Date(amu_discharge_month),
      y = mean_los) +
  geom_point(pch = 21,
             fill = "red",
             colour = "red",
             alpha = 0.1,
             size = 2.5) +
  annotate("text", 
           x = as.Date('2020-08-15'),
           y = 94, 
           label = "Hiatus No.1",
           size = 2.5,
           hjust = 0,
           colour = "black") +
  annotate("rect", 
           xmin = as.Date('2023-04-01'), 
           xmax = as.Date('2023-04-01'), 
           ymin = 0, 
           ymax = 96,
           colour = "black",
           linetype = "dashed") +
  annotate("text",
           x = as.Date('2022-08-15'),
           y = 94, 
           label = "Hiatus No.2",
           size = 2.5,
           hjust = 0,
           colour = "black") +
  annotate("rect", 
                      xmin = as.Date('2021-04-01'), 
                      xmax = as.Date('2021-04-01'), 
                      ymin = 0, 
                      ymax = 96,
                      colour = "black",
                      linetype = "dashed",
                      size = 0.5) +
  scale_x_date(date_breaks = "6 month", 
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0, 96),
                     breaks = seq(0, 96, 24)) +
  labs(title = "Something must have happened (don't know what, though)",
       subtitle = "Daily mean AMU length of stay (in hours) Apr 2019 to Mar 2024",
       x = "",
       y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
