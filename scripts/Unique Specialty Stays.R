# Trying to get to a dataframe with one row per unique specialty stay


# Step 0 - load the packages ----------------------------------------------

library(tidyverse)
library(readxl)


# Step 1 - import the data ------------------------------------------------

df_01 <-
  read_xlsx("data/ari_ward_duration_extract_2023_24.xlsx") |> 
  drop_na(end_datetime)
df_spec_table <-
  read_xlsx("data/ari_spec_table.xlsx")


# Step 1a - replace specialty_code with new_specialty_code ----------------

df_01_new <-
  df_01 |> 
  inner_join(df_spec_table,
            join_by(specialty_code == specialty_code)) |> 
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime) |> 
  filter(start_datetime != end_datetime)


# Step 1b - create a los_days variable ------------------------------------

df_01_new_a <-
  df_01_new |> 
  mutate(los_days = difftime(end_datetime,
                             start_datetime,
                             units = "days")) |> 
  arrange(desc(los_days))


# Step 1c - add up the los_days numbers -----------------------------------

los_days_total <-
  df_01_new_a |> 
  summarize(los_days_total = sum(los_days))

write_csv(x = df_01_new_a,
          "ward_duration_extract.csv")


# Step 2 - Create a duplicate dataframe -----------------------------------

df_01_copy <-
  df_01_new |> 
  rename(copy_seq_no = seq_no,
         copy_new_specialty_code = new_specialty_code,
         copy_ward_code = ward_code,
         copy_start_datetime = start_datetime,
         copy_end_datetime = end_datetime)


# Step 3 - Use anti_join() to get FIRST ward stays -----------------------

df_01_first <-
  df_01_new |> 
  anti_join(df_01_copy,
            join_by(start_datetime == copy_end_datetime,
                    new_specialty_code == copy_new_specialty_code))


# Step 4 - Use inner_join() to get SECOND ward stays --------------------

df_01_second <-
  df_01_first |> 
  inner_join(df_01_copy,
            relationship = "many-to-many",
            join_by(end_datetime == copy_start_datetime,
                    new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
       new_specialty_code,
       ward_code,
       start_datetime,
       end_datetime,
       copy_end_datetime) |> 
  rename(second_end_datetime = copy_end_datetime)


# Step 5 - Use inner_join() to get THIRD ward stays ------------------

df_01_third <-
  df_01_second |> 
  inner_join(df_01_copy,
            relationship = "many-to-many",
            join_by(second_end_datetime == copy_start_datetime,
                    new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime,
         second_end_datetime,
         copy_end_datetime) |> 
  rename(third_end_datetime = copy_end_datetime)


# Step 6 - Use inner_join() to get FOURTH ward stays ------------------

df_01_fourth <-
  df_01_third |> 
  inner_join(df_01_copy,
             relationship = "many-to-many",
             join_by(third_end_datetime == copy_start_datetime,
                     new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime,
         second_end_datetime,
         third_end_datetime,
         copy_end_datetime) |> 
  rename(fourth_end_datetime = copy_end_datetime)


# Step 7 - Use inner_join() to get FIFTH ward stays ------------------

df_01_fifth <-
  df_01_fourth |> 
  inner_join(df_01_copy,
             relationship = "many-to-many",
             join_by(fourth_end_datetime == copy_start_datetime,
                     new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime,
         second_end_datetime,
         third_end_datetime,
         fourth_end_datetime,
         copy_end_datetime) |> 
  rename(fifth_end_datetime = copy_end_datetime)


# Step 8 - Use inner_join() to get SIXTH ward stays ------------------

df_01_sixth <-
  df_01_fifth |> 
  inner_join(df_01_copy,
             relationship = "many-to-many",
             join_by(fifth_end_datetime == copy_start_datetime,
                     new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime,
         second_end_datetime,
         third_end_datetime,
         fourth_end_datetime,
         fifth_end_datetime,
         copy_end_datetime) |> 
  rename(sixth_end_datetime = copy_end_datetime)


# Step 9 - Use inner_join() to get SEVENTH ward stays ------------------

df_01_seventh <-
  df_01_sixth |> 
  inner_join(df_01_copy,
             relationship = "many-to-many",
             join_by(sixth_end_datetime == copy_start_datetime,
                     new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime,
         second_end_datetime,
         third_end_datetime,
         fourth_end_datetime,
         fifth_end_datetime,
         sixth_end_datetime,
         copy_end_datetime) |> 
  rename(seventh_end_datetime = copy_end_datetime)

# Step 10 - Use inner_join() to get EIGHTH ward stays ------------------

df_01_eighth <-
  df_01_seventh |> 
  inner_join(df_01_copy,
             relationship = "many-to-many",
             join_by(seventh_end_datetime == copy_start_datetime,
                     new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime,
         second_end_datetime,
         third_end_datetime,
         fourth_end_datetime,
         fifth_end_datetime,
         sixth_end_datetime,
         seventh_end_datetime,
         copy_end_datetime) |> 
  rename(eighth_end_datetime = copy_end_datetime)

# Step 11 - join first to second ------------------------------------------

df_01_final <-
  df_01_first |> 
  left_join(df_01_second,
            relationship = "many-to-many",
            join_by(end_datetime == end_datetime,
                    new_specialty_code == new_specialty_code)) |> 
  select(seq_no.x,
         new_specialty_code,
         ward_code.x,
         start_datetime.x,
         end_datetime,
         second_end_datetime) |> 
  group_by(seq_no.x) |> 
  summarize(earliest_start_datetime = min(start_datetime.x),
            latest_end_datetime = max(end_datetime),
            latest_second_datetime = max(second_end_datetime))

# Step 12 - join second to third ------------------------------------------

df_01_final_third <-
  df_01_final |> 
  left_join(df_01_third,
            relationship = "many-to-many",
            join_by(latest_end_datetime == end_datetime)) |> 
  select(seq_no.x,
         earliest_start_datetime,
         latest_end_datetime,
         third_end_datetime) |> 
  group_by(seq_no.x) |> 
  summarize(earliest_start_datetime = min(earliest_start_datetime),
            latest_end_datetime = max(latest_end_datetime),
            latest_third_datetime = max(third_end_datetime))

# Step 13 - join third to fourth ------------------------------------------

df_01_final_fourth <-
  df_01_final |> 
  left_join(df_01_fourth,
            relationship = "many-to-many",
            join_by(latest_end_datetime == end_datetime)) |> 
  select(seq_no.x,
         earliest_start_datetime,
         latest_end_datetime,
         fourth_end_datetime) |> 
  group_by(seq_no.x) |> 
  summarize(earliest_start_datetime = min(earliest_start_datetime),
            latest_end_datetime = max(latest_end_datetime),
            latest_fourth_datetime = max(fourth_end_datetime))
