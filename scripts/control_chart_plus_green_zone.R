# Jagged line of fullness with control limits AND green zone
# What was PLUS what should've been


# Step 0 - load the packages ----------------------------------------------

library(tidyverse)
library(readxl)
library(scales)


# Step 1 - import the data ------------------------------------------------

dataframe_01 <-
  read_xlsx("data/gm_duration_extract.xlsx")


# Step 2 - create an 8784 dataframe ---------------------------------------

dataframe_8784 <-
  data.frame(date_hour = seq(as.POSIXct("2023-04-01 00:01"),
                             as.POSIXct("2024-03-31 23:01"),
                             60*60))


# Step 3 - create the 8784 time series table ------------------------------

dataframe_01a <-
  dataframe_8784 |> 
  inner_join(dataframe_01,
             join_by(date_hour >= START_DATE,
                     date_hour < END_DATE)) |> 
  group_by(date_hour) |> 
  summarize(no_of_occupied_beds = n()) |> 
  mutate(new_date = floor_date(date_hour,
                               "days")) |> 
  group_by(new_date) |> 
  summarize(mean_occupied_beds = mean(no_of_occupied_beds)) |> 
  mutate(new_week = floor_date(new_date,
                               "weeks",
                               1)) |>
  group_by(new_week) |> 
  summarize(new_mean_occupied_beds = mean(mean_occupied_beds))


# Step 4 - create the control chart constants -----------------------------

overall_mean <- mean(dataframe_01a$new_mean_occupied_beds) 
sd <- sd(dataframe_01a$new_mean_occupied_beds)
lcl <- overall_mean - (3 * sd) 
ucl <- overall_mean + (3 * sd)
bed_allocation = 55
green_lcl = bed_allocation - (ucl - lcl)
green_mean = bed_allocation - (green_lcl / 2)


# Step 5 - create the summary table for the control chart -----------------

dataframe_01b <-
  dataframe_01a |> 
  mutate(lcl = lcl) |> 
  mutate(ucl = ucl)


# Step 6 - draw the control chart -----------------------------------------

ggplot(data = dataframe_01b) +
  aes(x = new_week) +
  #geom_line(aes(y = lcl),
            #linetype = "dashed") +
  #geom_line(aes(y = ucl),
            #linetype = "dashed") +
  #geom_ribbon(aes(ymax=`ucl`,
                  #ymin=`lcl`), 
              #fill="grey", 
              #alpha=0.25) +
  geom_ribbon(aes(ymax=`bed_allocation`,
                  ymin=`green_lcl`), 
              fill="darkgreen", 
              alpha=0.1) +
  geom_line(aes(y = new_mean_occupied_beds)) +
  geom_point(aes(y = new_mean_occupied_beds)) +
  #geom_line(aes(y = overall_mean)) +
  geom_line(aes(y = green_mean),
            colour = "darkgreen") +
  geom_line(aes(y = green_lcl),
            colour = "darkgreen",
            linetype = "dashed") +
  geom_line(aes(y = bed_allocation),
            colour = "darkgreen",
            linetype = "dashed") +
  scale_y_continuous(limits = c(0, 80),
                     breaks = seq(0, 80, 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="grey"),
        axis.line.y = element_line(color="grey"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(title = "Average no. of occupied beds per week",
       subtitle = "Anytown General Hospital | General Medicine (bed allocation = 55)",
       x = element_blank(),
       y = element_blank())
