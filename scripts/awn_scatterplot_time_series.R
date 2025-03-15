
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


# calculate weekly averages -----------------------------------------------

df_04 <-
  df_01 |> 
  group_by(WardStayEndWeek) |> 
  summarize(weekly_mean = mean(Ward_Stay_LOS_Hours))


# append weekly averages to df_03 -----------------------------------------

df_03a <-
  df_03 |> 
  left_join(df_04,
            join_by(WardStayEndWeek == WardStayEndWeek)) |> 
  select(amu_end_date,
         WardStayEndWeek,
         quarter,
         mean_los_hours,
         weekly_mean)

# calculate the two means -------------------------------------------------

mean_autumn <-
  df_03 |> 
  filter(quarter == "q1") |> 
  summarize(mean_autumn = mean(mean_los_hours))
  
mean_summer <-
  df_03 |> 
  filter(quarter == "q4") |> 
  summarize(mean_summer = mean(mean_los_hours))

# draw scatterplot --------------------------------------------------------

ggplot(data = df_03a) +
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
  geom_segment(aes(x = as.POSIXct('2014-09-29'),
                   y = mean_autumn$mean_autumn,
                   xend = as.POSIXct('2014-12-27'),
                   yend = mean_autumn$mean_autumn),
               colour ="black",
               linetype = "solid",
               linewidth = 0.5) +
  annotate("text", 
           x = as.POSIXct('2014-09-29'),
           y = 57.5, 
           label = "ALoS = 29.9 hours",
           size = 2.8,
           hjust = 0,
           colour = "darkorange",
           fontface = 2) +
  geom_segment(aes(x = as.POSIXct('2015-06-25'),
                   y = 0,
                   xend = as.POSIXct('2015-06-25'),
                   yend = 60),
               colour = "black",
               linetype = "dashed") +
  geom_segment(aes(x = as.POSIXct('2015-06-28'),
                   y = mean_summer$mean_summer,
                   xend = as.POSIXct('2015-09-27'),
                   yend = mean_summer$mean_summer),
               colour ="black",
               linewidth = 0.5,
               linetype = "solid") +
  annotate("text", 
           x = as.POSIXct('2015-06-28'),
           y = 57.5, 
           label = "ALoS = 27.0 hours",
           size = 2.8,
           hjust = -0.05,
           colour = "blue",
           fontface = 2) +
  geom_line(aes(y = weekly_mean),
            colour = "black",
            linewidth = 0.5) +
  geom_point(aes(y = weekly_mean),
            colour = "black",
            size = 1) +
  scale_fill_manual(values = c("darkorange", "grey", "grey", "blue")) +
  scale_colour_manual(values = c("darkorange", "grey", "grey", "blue")) +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 12)) +
  scale_x_datetime(limits = c(as.POSIXct('2014-09-29'),
                              as.POSIXct('2015-09-27')),
                   breaks = c(as.POSIXct('2014-09-29', tz = "UTC"),
                              as.POSIXct('2014-12-28', tz = "UTC"),
                              as.POSIXct('2015-03-28', tz = "UTC"),
                              as.POSIXct('2015-06-28', tz = "UTC"),
                              as.POSIXct('2015-09-27', tz = "UTC")), 
                   date_labels = "%d-%b-%y") +
  labs(title = "AMU length of stay reduced by ten percent",
       subtitle = "29 September 2014 to 27 September 2015",
       x = "",
       y = "") +
  #theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey",
                                          linewidth = 0.5,
                                          linetype = "dotted"),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.1, 0.5), "cm"))
