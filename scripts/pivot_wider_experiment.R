
# AGH 8736 Fullness -------------------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)


# import data -------------------------------------------------------------

df_01 <-
  read_xlsx("data/2025_03_21_agh_ip_data_v4.xlsx",
            "FINAL")


# create 8736 dataframe ---------------------------------------------------

df_8736 <-
  data_frame(snapshot_datetime = seq(as.POSIXct('2014-09-29 00:01',
                                                tz = "UTC"),
                                     as.POSIXct('2015-09-27 23:01',
                                                tz = "UTC"),
                                     by = "hour"))


# restrict the dataframe to inpatients only -------------------------------

df_02 <-
  df_01 |> 
  filter(derived_patient_type_code == "I") |> 
  filter(ward_code != "H02" & ward_code != "HCDU" & ward_code != "H04")



# do the non-equi join ----------------------------------------------------

df_03 <-
  df_02 |> 
  left_join(df_8736,
            join_by(ward_stay_start_datetime <= snapshot_datetime,
                    f_movement_end_datetime > snapshot_datetime)) |> 
  group_by(snapshot_datetime,
           ward_code) |> 
  summarize(no_of_beds_occupied = n()) |> 
  drop_na(snapshot_datetime)



# try pivot_wider() -------------------------------------------------------

df_04 <-
  df_03 |> 
  pivot_wider(names_from = ward_code, 
              values_from = no_of_beds_occupied)



