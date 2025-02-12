# Drawing a funnel plot with two sets of control limits (and maybe a shaded zone)


# Step 0 - load the packages ----------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)


# Step 1 - import the data ------------------------------------------------

df_01 <-
  read_xlsx("data/gm_cons_funnel.xlsx") |> 
  clean_names()


# Step 2 - calculate the overall (all consultants) proportion -------------

overall_proportion <- sum(df_01$no_of_stays_longer_than_14_days) / sum(df_01$no_of_stays_in_2023_24)


# Step 3 - append the individual and overall proportions to the main summary table --------

df_01a <-
  df_01 |> 
  mutate(individual_proportion = no_of_stays_longer_than_14_days / no_of_stays_in_2023_24) |> 
  mutate(overall_proportion = overall_proportion)


# Step 4 - calculate the standard error for each consultant ---------------

df_01b <-
  df_01a |> 
  mutate(standard_error = sqrt(overall_proportion * (1 - overall_proportion) / no_of_stays_in_2023_24))


# Step 5 - calculate the control limits -------------------------------

df_01c <-
  df_01b |> 
  mutate(lcl_95 = overall_proportion - (1.96 * standard_error)) |> 
  mutate(ucl_95 = overall_proportion + (1.96 * standard_error)) |>
  mutate(lcl_99_7 = overall_proportion - (3 * standard_error)) |>
  mutate(ucl_99_7 = overall_proportion + (3 * standard_error)) |> 
  mutate(green_docs = if_else(between(individual_proportion, lcl_95, ucl_95),
                              individual_proportion,
                              NA)) |> 
  mutate(lower_amber_docs = if_else(between(individual_proportion, lcl_95, lcl_99_7),
                                    individual_proportion,
                                    NA)) |> 
  mutate(upper_amber_docs = if_else(between(individual_proportion, ucl_95, ucl_99_7),
                                    individual_proportion,
                                    NA)) |> 
  mutate(lower_red_docs = if_else(individual_proportion < lcl_99_7,
                                  individual_proportion,
                                  NA)) |> 
  mutate(upper_red_docs = if_else(individual_proportion > ucl_99_7,
                                  individual_proportion,
                                  NA))
  
# Step 6 - draw the funnel plot -------------------------------------------

ggplot(data = df_01c) +
  aes(x = no_of_stays_in_2023_24) +
  geom_point(aes(y = green_docs),
             pch = 21,
             fill = "#91bfdb",
             colour = "darkgrey",
             size = 3) +
  geom_point(aes(y = lower_amber_docs),
             pch = 21,
             fill = "#ffffbf",
             colour = "darkgrey",
             size = 3) +
  geom_point(aes(y = upper_amber_docs),
             pch = 21,
             fill = "#ffffbf",
             colour = "darkgrey",
             size = 3) +
  geom_point(aes(y = lower_red_docs),
             pch = 21,
             fill = "#fc8d59",
             colour = "darkgrey",
             size = 3) +
  geom_point(aes(y = upper_red_docs),
             pch = 21,
             fill = "#fc8d59",
             colour = "darkgrey",
             size = 3) +
  geom_line(aes(y = overall_proportion)) +
  geom_line(aes(y = lcl_95),
            linetype = "dotted",
            colour = "darkgrey") +
  geom_line(aes(y = ucl_95),
            linetype = "dotted",
            colour = "darkgrey") +
  geom_line(aes(y = lcl_99_7),
            linetype = "dashed",
            colour = "darkgrey") +
  geom_line(aes(y = ucl_99_7),
            linetype = "dashed",
            colour = "darkgrey") +
  scale_x_continuous(limits = c(100, 400),
                     breaks = seq(100, 400, 50)) +
  scale_y_continuous(limits = c(0.05, 0.35),
                     breaks = seq(0.05, 0.35, 0.05),
                     labels = scales::percent) +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Variation in length of stay by consultant",
       subtitle = "Percentage of inpatients with length of stay > 14 days",
       x = "No. of discharges in 2023-24",
       y = "")
  
