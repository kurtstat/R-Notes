
# The Red Curtains of Catastrophe -----------------------------------------


# (1) load packages -------------------------------------------------------

library(readxl)
library(tidyverse)
library(scales)


# (2) import the heatmap --------------------------------------------------

df_heatmap <-
  read_xlsx("data/2025_03_26_heatmap.xlsx",
            "percentages")


# (3) keep the important rows in the heatmap dataframe --------------------

df_heatmap <- 
  df_heatmap[2:25,] |> 
  rename("arrival_hour" = 'Hour of arrival') |> 
  rename("heatmap_percent" = 'Total')

  
# (4) keep the important columns in the heatmap dataframe -----------------

df_heatmap <-
  df_heatmap |> 
  mutate(new_arrival_hour = as.integer(arrival_hour)) |> 
  select(new_arrival_hour,
         heatmap_percent)


# (5) import the monthly attendance numbers -------------------------------

df_monthly_count <-
  read_xlsx("data/rie_monthly_720_counts.xlsx") |> 
  select(month_ending,
         attendances_greater_12hrs) |> 
  filter(month_ending != '2025-01-31')


# (6) find out the number of >12-hour stays in the period ----------------

no_of_rows <- sum(df_monthly_count$attendances_greater_12hrs)


# (7) create the 3168 dataframe ------------------------------------------

df_01 <-
  df_monthly_count |> 
  cross_join(df_heatmap)


# (8) create a  variable with the sample numbers -------------------------

df_02 <- 
  df_01 |>
  mutate(hour_sample_size = round(attendances_greater_12hrs * heatmap_percent,0))


# (9) find out the max sample size ---------------------------------------

max_sample_size <- max(df_02$hour_sample_size)


# (10) find out the number of >12-hour stays captured by this method -----

no_of_rows_samples <- sum(df_02$hour_sample_size)


# (11) Create a new dataframe with repeated rows using uncount() ---------

df_03 <-
  df_02 |> 
  uncount(hour_sample_size)


# (12) append hour_sample_size to df_03 ----------------------------------

df_04 <-
  df_03 |> 
  left_join(df_02,
            join_by(month_ending == month_ending,
                    attendances_greater_12hrs == attendances_greater_12hrs,
                    new_arrival_hour == new_arrival_hour,
                    heatmap_percent == heatmap_percent)) |> 
  select(month_ending,
         attendances_greater_12hrs,
         new_arrival_hour,
         heatmap_percent,
         hour_sample_size) |> 
  arrange(hour_sample_size)


###  replacement for Excel ###

# (13) generate random numbers --------------------------------------------


df_05 <- 
  df_04 |>
  mutate(month = lubridate::days_in_month(month_ending))|>
  mutate(month_hours = month * 60) |>
  mutate(rowid = row_number()) |>
  group_by(rowid) |>
  mutate(random_number = floor(runif(1, min = 1, max = month_hours))) |>
  ungroup()

### end of excel replacement ### 


# (14) Need to add two variables to df_05 ---------------------------------

df_05a <-
  df_05 |> 
  mutate(year_month = str_sub(month_ending, 1, 7)) |> 
  mutate(month_heatmap_score = paste(year_month,
                                     heatmap_percent,
                                     sep = "-"))


# now to work on the really long dataframe --------------------------------

# (15) create a dataframe with 5.8m minutes -------------------------------

df_06_minutes <-
  data_frame(musk_min = seq(as.POSIXct('2014-01-01 00:00',
                                       tz = "UTC"),
                            as.POSIXct('2024-12-31 23:59',
                                       tz = "UTC"),
                            by = "1 min"))


# (16) append first_day_of_month, weekday and hour variables to this dataframe

df_06_minutes_x <-
  df_06_minutes |> 
  mutate(first_day_of_month = floor_date(musk_min,
                                         "month")) |> 
  mutate(musk_day = wday(musk_min,
                         label = TRUE)) |> 
  mutate(musk_hour = hour(musk_min)) |> 
  mutate(musk_hour_min = hms::as_hms(musk_min))


# (17) concatenate the weekday and hour variables ------------------------

df_06_minutes_y <-
  df_06_minutes_x |> 
  mutate(musk_day_hour = paste(as.character(musk_day),
                               as.character(musk_hour),
                               sep = "_"))


# (18) append the heatmap_category to the big dataframe ------------------

df_06_minutes_z <-
  df_06_minutes_y |> 
  left_join(df_heatmap,
            join_by(musk_hour == new_arrival_hour)) |> 
  arrange(first_day_of_month,
          musk_hour) |> 
  mutate(musk_3168 = consecutive_id(first_day_of_month,
                                   musk_hour)) |> 
  mutate(musk_row_number = seq.int(nrow(df_06_minutes_y))) |> 
  mutate(musk_year_month = format(first_day_of_month,
                                  "%Y-%m")) |> 
  mutate(zz_month_heatmap_score = paste(musk_year_month,
                                        heatmap_percent,
                                        sep = "-"))


# (19) the cumsum() counter step -----------------------------------------

df_06_minutes_zz <-
  df_06_minutes_z |>  
  mutate(one = 1) |> 
  group_by(zz_month_heatmap_score) |> 
  mutate(musk_seq_no = cumsum(one)) |> 
  ungroup() |> 
  arrange(musk_3168,
          musk_seq_no) |> 
  select(musk_seq_no,
         musk_year_month,
         zz_month_heatmap_score,
         musk_3168,
         musk_min)


# (20) I need to append a datetime to these random numbers ---------------

df_07 <-
  df_05a |> 
  left_join(df_06_minutes_zz,
            join_by(month_heatmap_score == zz_month_heatmap_score,
                    random_number == musk_seq_no,
                    year_month == musk_year_month)) |> 
  select(month_heatmap_score,
         random_number,
         musk_min) |> 
  arrange(musk_min)


# (21) separate date and time -------------------------------------------

df_08 <-
  df_07 |>
  mutate(arrival_date = floor_date(musk_min,
                                   "day")) |>
  mutate(arrival_time = hms::as_hms(musk_min)) |> 
  mutate(heatmap_score = substr(month_heatmap_score, 9, 26))


# (22) draw the scatterplot ----------------------------------------------

ggplot(data = df_08) +
  aes(x = arrival_date,
      y = arrival_time) +
  geom_point(size = 0.01,
             colour = "red",
             alpha = 0.1) +
  #scale_y_reverse(labels = function(x) hms::as_hms(x),
  scale_y_reverse(labels = function(x) substring(format(hms::hms(x),"%H:%M"),1,5),
                  breaks = seq(as.numeric(hms::as_hms("00:00:00")),
                               as.numeric(hms::as_hms("24:00:00")),
                               21600),
                  position = "right") +
  scale_x_datetime(limits = c(as.POSIXct('2014-01-01'),
                              as.POSIXct('2025-01-01')),
                   breaks = "12 month",
                   labels = date_format("%Y"),
                   expand = c(0, 0)) +
  labs(title = "The Red Curtains of Catastrophe",
       subtitle = "Arrival times of patients who spent longer than 12 hours in A&E",
       caption = "Source: Public Health Scotland",
       x = NULL,
       y = NULL) +
  theme(axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 20),
        axis.title = element_text(colour = "black"),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))









