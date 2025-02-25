
# Two Fullness Histograms -------------------------------------------------


# Step 1 - load the packages ----------------------------------------------

library(tidyverse)
library(readxl)
library(ggtext)

# Step 2 - import the data ------------------------------------------------

df_01 <-
  read_xlsx("data/ari_ward_duration_extract_2023_24.xlsx")
df_ed_366 <-
  read_xlsx("data/2025_02_24_ed_366.xlsx")
df_spec_table <-
  read_xlsx("data/ari_spec_table.xlsx")


# Step 2a - replace specialty_code with new_specialty_code ----------------

df_01_new <-
  df_01 |> 
  inner_join(df_spec_table,
             join_by(specialty_code == specialty_code)) |> 
  mutate(new_start_datetime = as.POSIXct(start_datetime,
                                         tz = "GMT")) |>
  mutate(new_end_datetime = as.POSIXct(end_datetime,
                                       tz = "GMT")) |> 
  select(seq_no,
         new_specialty_code,
         ward_code,
         new_start_datetime,
         new_end_datetime)

# Step 3 - create an 8784 dataframe ---------------------------------------

df_8784 <-
  data.frame(date_hour = seq(as.POSIXct("2023-04-01 00:01",
                                        tz = "GMT"),
                             as.POSIXct("2024-03-31 23:01",
                                        tz = "GMT"),
                             60*60))

# Step 4 - create the 8784 time series table ------------------------------

df_01_new_a <-
  df_8784 |> 
  inner_join(df_01_new,
             join_by(date_hour >= new_start_datetime,
                     date_hour < new_end_datetime)) |> 
  group_by(date_hour) |> 
  summarize(no_of_occupied_beds = n()) |> 
  mutate(new_date = floor_date(date_hour,
                           unit = "day")) |> 
  group_by(new_date) |> 
  summarize(mean_occupied_beds = mean(no_of_occupied_beds))


# Step 5 - append the ED rank value ---------------------------------------

df_01_new_b <-
  df_01_new_a |> 
  left_join(df_ed_366,
            join_by(new_date == ed_date)) |> 
  select(new_date,
         mean_occupied_beds,
         compliance_rank)


# Step 6 - create a worst/best variable ---------------------------

df_01_new_c <-
  df_01_new_b |> 
  mutate(quartile = case_when(compliance_rank <= 91 ~ "lq",
                              compliance_rank > 91 & compliance_rank <= 183 ~ "q2",
                              compliance_rank > 183 & compliance_rank <= 275 ~ "q3",
                              compliance_rank > 275 ~ "uq")) |> 
  filter(quartile == "lq" | quartile == "uq")


# Step 6a - identify the two mean values ----------------------------------

mean_best <-
  df_01_new_c |> 
  filter(quartile == "lq") |> 
  summarize(mean_best = mean(mean_occupied_beds))

mean_worst <-
  df_01_new_c |> 
  filter(quartile == "uq") |> 
  summarize(mean_worst = mean(mean_occupied_beds))


# Step 7 - draw the histograms --------------------------------------------

ggplot(data = df_01_new_c) +
  aes(x = mean_occupied_beds,
      colour = quartile) +
  geom_density(linewidth = 0.8) +
  scale_x_continuous(limits = c(600, 800),
                     breaks = seq(600, 800, 20)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("darkgreen", "darkorange")) +
  geom_vline(xintercept = 699,
             linewidth = 0.8,
             linetype = "dotted",
             colour = "darkgreen") +
  geom_vline(xintercept = 705,
             linewidth = 0.8,
             linetype = "dotted",
             colour = "darkorange") +
  labs(title = "The Paradox of Small Differences",
       subtitle = "No. of beds occupied on the <span style='color: darkgreen;'>91 best</span> and <span style='color: darkorange;'>91 worst</span> days",
       x = "No. of beds occupied (all specialties)",
       y = "",
       caption = "* best (61%) and worst (39%) defined by performance against the four-hour target") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(plot.subtitle = element_markdown()) +
  theme(legend.position = "none")
  




