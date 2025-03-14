
# awn_scatterplot_time_series ---------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)



# import data -------------------------------------------------------------

df_01 <- 
  read_xlsx("data/amu_52_weeks.xlsx") |> 
  filter(Ward_Stay_LOS_Hours <= 168)


# summary table -----------------------------------------------------------

df_02 <-
  df_01 |> 
  group_by(WardStayEndDate) |> 
  summarize(mean_los_hours = mean(Ward_Stay_LOS_Hours)) |> 
  rename(amu_end_date = WardStayEndDate)


# append quarter to the summary table -------------------------------------

df_03 <-
  df_02 |> 
  left_join(df_01,
            join_by(amu_end_date == WardStayEndDate)) |> 
  select(amu_end_date,
         WardStayEndWeek,
         quarter,
         mean_los_hours) |> 
  distinct()


# draw scatterplot --------------------------------------------------------

ggplot(data = df_03) +
  aes(x = WardStayEndWeek,
      y = mean_los_hours,
      fill = quarter,
      colour = quarter) +
  geom_point(pch = 21,
             size = 3,
             alpha = 0.2) +
  geom_segment(aes(x = as.POSIXct('2014-12-25'),
                   y = 0,
                   xend = as.POSIXct('2014-12-25'),
                   yend = 60),
               colour = "black",
               linetype = "dashed") +
  geom_segment(aes(x = as.POSIXct('2015-06-25'),
                   y = 0,
                   xend = as.POSIXct('2015-06-25'),
                   yend = 60),
               colour = "black",
               linetype = "dashed") +
  scale_fill_manual(values = c("darkorange", "grey", "grey", "blue")) +
  scale_colour_manual(values = c("darkorange", "grey", "grey", "blue")) +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 12)) +
  scale_x_datetime(limits = c(as.POSIXct('2014-09-29'),
                              as.POSIXct('2015-09-27')),
                   breaks = c(as.POSIXct('2014-09-29'),
                              as.POSIXct('2014-12-28'),
                              as.POSIXct('2015-03-28'),
                              as.POSIXct('2015-06-28'),
                              as.POSIXct('2015-09-27')), 
                   date_labels = "%d-%b-%y") +
  labs(title = "Length of stay reduced in the AMU",
       subtitle = "And here's the granularity",
       x = "",
       y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
