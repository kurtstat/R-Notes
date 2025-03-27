
# The monthly count line chart --------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)
library(scales)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/rie_monthly_720_counts.xlsx")


# draw the line chart -----------------------------------------------------

ggplot(data = df_01) +
  aes(x = month_ending,
      y = scotland_12) +
  geom_line(colour = "red",
            linewidth = 0.7) +
  geom_point(colour = "red", 
             size = 0) +
  geom_vline(xintercept = as.POSIXct('2021-06-01'),
             linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct('2022-12-31'),
             linetype = "dashed") +
  annotate("rect",
           xmin = as.POSIXct('2021-06-01'), 
           xmax = as.POSIXct('2022-12-31'), 
           ymin = -Inf, 
           ymax = Inf,  
           fill = "grey", 
           alpha= 0.3) +
  scale_x_datetime(limits = c(as.POSIXct('2014-01-01'),
                              as.POSIXct('2025-01-31')),
                   breaks = "12 month",
                   labels = date_format("%Y"),
                   expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10000),
                     breaks = seq(0, 10000, 2000),
                     label = comma) +
  labs(title = "A&E attendances with stays longer than 12 hours",
       subtitle = "NHS Scotland (all hospitals): January 2014 to January 2025",
       x = NULL,
       y = NULL,
       caption = "Source: Public Health Scotland") +
  theme_minimal() +
  theme(axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.caption = element_text(hjust = 0,
                                    colour = "black"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
