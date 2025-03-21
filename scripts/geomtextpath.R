
# load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(geomtextpath)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/2025_03_20_normal_data.xlsx")


# plot graph --------------------------------------------------------------

ggplot(data = df_01) +
  aes(x = no_of_attendances,
      y = no_of_days,
      label = "This is what a Normal distribution with a mean of 330 and a standard deviation of 31 looks like") +
  geom_textline(colour = "blue") +
  labs(title = "Experimenting with {geomtextpath}",
       subtitle = "Trying to integrate text and image",
       x = "No. of attendances per day",
       y = "No. of days") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey",
                                          linetype = "dotted"),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
  
