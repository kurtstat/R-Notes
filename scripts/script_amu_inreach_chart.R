
# Draw an annotated chart of the weekly AMU in-reach data -----------------


# Step 0 - load the packages ----------------------------------------------

library(tidyverse)
library(readxl)
library(scales)


# Step 1 - import the data ------------------------------------------------

df_01 <-
  read_xlsx("data/inreach_weekly_data.xlsx")


# Step 2 - calculate the weekly percentages -------------------------------

df_01a <-
  df_01 |> 
  mutate(weekly_percentage = discharged_home / total)


# Step 3 - calculate the constants ----------------------------------------

mean_autumn_2014 <-
  df_01a |> 
  filter(discharge_week <= '2014-12-22') |> 
  summarize(mean_autumn_2014 = sum(discharged_home) / sum(total))

sd_autumn_2014 <-
  df_01a |> 
  filter(discharge_week <= '2014-12-22') |> 
  summarize(sd_autumn_24 = sd(weekly_percentage))

lcl_autumn_2014 <-
  mean_autumn_2014 - (1.96 * sd_autumn_2014)

ucl_autumn_2014 <-
  mean_autumn_2014 + (1.96 * sd_autumn_2014)

mean_summer_2015 <-
  df_01a |> 
  filter(discharge_week >= '2015-06-29') |> 
  summarize(mean_summer_2015 = sum(discharged_home) / sum(total))

sd_summer_2015 <-
  df_01a |> 
  filter(discharge_week >= '2015-06-29') |> 
  summarize(sd_summer_2015 = sd(weekly_percentage))

lcl_summer_2015 <-
  mean_summer_2015 - (1.96 * sd_summer_2015)

ucl_summer_2015 <-
  mean_summer_2015 + (1.96 * sd_summer_2015)


# Step 4 - add the constants to the dataframe -----------------------------

df_01b <-
  df_01a |> 
  mutate(first_mean = if_else(discharge_week <= '2014-12-22',
                              mean_autumn_2014,
                              NA)) |> 
  mutate(first_lcl = if_else(discharge_week <= '2014-12-22',
                              lcl_autumn_2014,
                              NA)) |> 
  mutate(first_ucl = if_else(discharge_week <= '2014-12-22',
                              ucl_autumn_2014,
                              NA)) |> 
  mutate(second_mean = if_else(discharge_week >= '2015-06-29',
                              mean_summer_2015,
                              NA)) |> 
  mutate(second_lcl = if_else(discharge_week >= '2015-06-29',
                             lcl_summer_2015,
                             NA)) |> 
  mutate(second_ucl = if_else(discharge_week >= '2015-06-29',
                             ucl_summer_2015,
                             NA))


# Step 5 - draw the chart -------------------------------------------------

ggplot(data = df_01b) +
  annotate("rect", 
           xmin = as.POSIXct('2014-09-27'), 
           xmax = as.POSIXct('2014-12-22'), 
           ymin = -Inf, 
           ymax = Inf,
           fill = "#EAEAEA",
           alpha = 0.25) +
  annotate("rect", 
           xmin = as.POSIXct('2015-06-29'), 
           xmax = as.POSIXct('2015-09-21'), 
           ymin = -Inf, 
           ymax = Inf,
           fill = "#EAEAEA",
           alpha = 0.25) +
  aes(x = discharge_week) +
  geom_line(aes(y = weekly_percentage)) +
  geom_point(aes(y = weekly_percentage)) +
  geom_line(aes(y = first_mean$mean_autumn_2014),
            colour = "darkorange",
            linewidth = 1) +
  #geom_line(aes(y = first_lcl$mean_autumn_2014),
            #linetype = "dashed",
            #colour = "grey") +
  #geom_line(aes(y = first_ucl$mean_autumn_2014),
            #linetype = "dashed",
            #colour = "grey") +
  geom_line(aes(y = second_mean$mean_summer_2015),
            colour = "darkorange",
            linewidth = 1) +
  #geom_line(aes(y = second_lcl$mean_summer_2015),
            #linetype = "dashed",
            #colour = "grey") +
  #geom_line(aes(y = second_ucl$mean_summer_2015),
            #linetype = "dashed",
            #colour = "grey") +
  annotate("text", 
           x = as.POSIXct('2014-10-01'),
           y = 0.5, 
           label = "Autumn 2014 = 28%",
           size = 2.8,
           hjust = 0,
           colour = "darkorange") +
  annotate("text", 
           x = as.POSIXct('2015-07-02'),
           y = 0.5, 
           label = "Summer 2014 = 34%",
           size = 2.8,
           hjust = 0,
           colour = "darkorange") +
  scale_y_continuous(limits = c(0, 0.5),
                     breaks = seq(0, 0.5, 0.1),
                     labels = percent) +
  labs(title = "More patients went home from the Acute Medical Unit",
       subtitle = "Percentage of direct discharges | Anytown General Hospital",
       x = "",
       y = "",
       caption = "Source: Patient Management System") +
  theme_classic()
  
