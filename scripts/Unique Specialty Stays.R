# Trying to get to a dataframe with one row per unique specialty stay


# Step 0 - load the packages ----------------------------------------------

library(tidyverse)
library(readxl)


# Step 1 - import the data ------------------------------------------------

df_01 <-
  read_xlsx("data/ari_ward_duration_extract_2023_24.xlsx")
df_spec_table <-
  read_xlsx("data/ari_spec_table.xlsx")



# Step 1a - replace specialty_code with new_specialty_code ----------------

df_01_new <-
  df_01 |> 
  left_join(df_spec_table,
            join_by(specialty_code == specialty_code)) |> 
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime)

# Step 2 - Create a duplicate dataframe -----------------------------------

df_01_copy <-
  df_01_new |> 
  rename(copy_seq_no = seq_no,
         copy_new_specialty_code = new_specialty_code,
         copy_ward_code = ward_code,
         copy_start_datetime = start_datetime,
         copy_end_datetime = end_datetime)


# Step 3 - Use anti_join() to get unique admissions -----------------------

df_01a <-
  df_01_new |> 
  anti_join(df_01_copy,
            join_by(start_datetime == copy_end_datetime))


# Step 4 - append second ward stay end_datetime to df_01a -----------------

df_01b <-
  df_01a |> 
  left_join(df_01_copy,
            relationship = "many-to-many",
            join_by(end_datetime == copy_start_datetime,
                    new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
       new_specialty_code,
       ward_code,
       start_datetime,
       end_datetime,
       copy_end_datetime)


# Step 5 - append third ward stay end_datetime to df_01a ------------------

df_01c <-
  df_01b |> 
  left_join(df_01_copy,
            relationship = "many-to-many",
            join_by(copy_end_datetime == copy_start_datetime,
                    new_specialty_code == copy_new_specialty_code)) |>
  select(seq_no,
         new_specialty_code,
         ward_code,
         start_datetime,
         end_datetime,
         copy_end_datetime)

