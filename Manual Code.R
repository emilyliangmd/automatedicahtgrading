### Install and load packages
install.packages("here")
install.packages("openxlsx")
install.packages("janitor")
install.packages("tidyverse")
library(here)
library(openxlsx)
library(janitor)
library(tidyverse)

### Import data
df <-
  read.xlsx(here("dfs", "Mock ANC Data for heatwaveR.xlsx"),
            detectDates = TRUE) %>% 
  clean_names()

### Data wrangling/formatting
# Create time_post_inf
df <- df %>%
  mutate(time_post_inf = as.numeric(date - cart_date)) %>%
  filter(time_post_inf %in% 0:30)

# Keep unique combinations of patient_id, time_post_inf, and anc
# For each combination of patient_id and time_post_inf, keep the lowest anc
df <- df %>%
  distinct(patient_id, time_post_inf, anc, .keep_all = TRUE) %>%
  group_by(patient_id, time_post_inf) %>%
  slice_min(anc) %>% # Keep lowest ANC per day
  ungroup()

### Apply code
# Create df_mod to track duration of neutropenia below the ICAHT thresholds
# "Severe" neutropenia refers to ANC ≤ 500 cells/µL
# "Profound" neutropenia refers to ANC ≤ 100 cells/µL
df_mod <- df %>%
  arrange(patient_id, date) %>%
  group_by(patient_id) %>%
  mutate(profound = anc <= 100,
         severe = anc <= 500) %>%
  mutate(
    gap_severe = ifelse(
      severe &
        lag(severe),
      ifelse(
        time_post_inf - lag(time_post_inf) < time_post_inf,
        time_post_inf - lag(time_post_inf),
        time_post_inf
      ),
      ifelse(
        !severe &
          lag(severe) &
          lead(severe),
        ifelse(
          lead(time_post_inf) - lag(time_post_inf) <= 3,
          time_post_inf - lag(time_post_inf),
          0
        ),
        ifelse(
          severe &
            !lag(severe) &
            lag(severe, 2),
          ifelse(
            time_post_inf - lag(time_post_inf, 2) <= 3,
            time_post_inf - lag(time_post_inf),
            0
          ),
          ifelse(
            lag(severe) &
              !(severe) &
              !lead(severe) &
              lead(severe, 2),
            ifelse(
              lead(time_post_inf, 2) - lag(time_post_inf) <= 3,
              time_post_inf - lag(time_post_inf),
              0
            ),
            ifelse(
              lag(severe, 2) &
                !lag(severe) &
                !(severe) &
                lead(severe),
              ifelse(
                lead(time_post_inf) - lag(time_post_inf, 2) <= 3,
                time_post_inf - lag(time_post_inf),
                0
              ),
              ifelse(
                severe &
                  !lag(severe) &
                  !lag(severe, 2) &
                  lag(severe, 3),
                ifelse(
                  time_post_inf - lag(time_post_inf, 3) <= 3,
                  time_post_inf - lag(time_post_inf),
                  0
                ),
                ifelse(row_number() ==
                         n(), 0, 0)
              )
            )
          )
        )
      )
    ),
    gap_profound = ifelse(
      profound &
        lag(profound),
      ifelse(
        time_post_inf - lag(time_post_inf) < time_post_inf,
        time_post_inf - lag(time_post_inf),
        time_post_inf
      ),
      ifelse(
        !profound &
          lag(profound) &
          lead(profound),
        ifelse(
          lead(time_post_inf) - lag(time_post_inf) <= 3,
          time_post_inf - lag(time_post_inf),
          0
        ),
        ifelse(
          profound &
            !lag(profound) &
            lag(profound, 2),
          ifelse(
            time_post_inf - lag(time_post_inf, 2) <= 3,
            time_post_inf - lag(time_post_inf),
            0
          ),
          ifelse(
            lag(profound) &
              !(profound) &
              !lead(profound) &
              lead(profound, 2),
            ifelse(
              lead(time_post_inf, 2) - lag(time_post_inf) <= 3,
              time_post_inf - lag(time_post_inf),
              0
            ),
            ifelse(
              lag(profound, 2) &
                !lag(profound) &
                !(profound) &
                lead(profound),
              ifelse(
                lead(time_post_inf) - lag(time_post_inf, 2) <= 3,
                time_post_inf - lag(time_post_inf),
                0
              ),
              ifelse(
                profound &
                  !lag(profound) &
                  !lag(profound, 2) &
                  lag(profound, 3),
                ifelse(
                  time_post_inf - lag(time_post_inf, 3) <= 3,
                  time_post_inf - lag(time_post_inf),
                  0
                ),
                ifelse(row_number() ==
                         n(), 0, 0)
              )
            )
          )
        )
      )
    )
  ) %>%
  filter(time_post_inf >= 0 & time_post_inf <= 30) %>%
  mutate(
    severe_dur = 0,
    profound_dur = 0,
    num = row_number(),
    first = row_number() == 1,
    last = row_number() == n()
  )

df_mod$gap_severe[is.na(df_mod$gap_severe)] = 0
df_mod$gap_profound[is.na(df_mod$gap_profound)] = 0

# Remove rows with NAs
df_mod <- df_mod %>% 
  filter(!is.na(anc))

# Calculate duration of neutropenia
for (i in 2:nrow(df_mod)) {
  if (df_mod$first[i] & df_mod$severe[i]) {
    df_mod$severe_dur[i] = df_mod$gap_severe[i] + 1
    
  } else if (!df_mod$first[i] &
             df_mod$severe[i] &
             !df_mod$severe[i - 1] &
             df_mod$gap_severe[i] == 0) {
    df_mod$severe_dur[i] = 1
    
  } else if (!df_mod$first[i] &
             df_mod$severe[i] & df_mod$severe[i - 1]) {
    df_mod$severe_dur[i] = df_mod$severe_dur[i] + df_mod$severe_dur[i - 1] + df_mod$gap_severe[i]
    
  } else if (!df_mod$first[i] &
             df_mod$gap_severe[i] > 0 &
             df_mod$severe_dur[i - 1] > 0) {
    df_mod$severe_dur[i] = df_mod$severe_dur[i] + df_mod$severe_dur[i - 1] + df_mod$gap_severe[i]
    
  }
  
  if (df_mod$first[i] &
      df_mod$profound[i]) {
    df_mod$profound_dur[i] = df_mod$gap_profound[i] + 1
    
  } else if (!df_mod$first[i] &
             df_mod$profound[i] &
             !df_mod$profound[i - 1] &
             df_mod$gap_profound[i] == 0) {
    df_mod$profound_dur[i] = 1
    
  } else if (!df_mod$first[i] &
             df_mod$profound[i] &
             df_mod$profound[i - 1]) {
    df_mod$profound_dur[i] = df_mod$profound_dur[i] + df_mod$profound_dur[i - 1] + df_mod$gap_profound[i]
    
  } else if (!df_mod$first[i] &
             df_mod$gap_profound[i] > 0 &
             df_mod$profound_dur[i - 1] > 0) {
    df_mod$profound_dur[i] = df_mod$profound_dur[i] + df_mod$profound_dur[i - 1] + df_mod$gap_profound[i]
    
  }
}

# Assign early ICAHT grades
df_dur <- df_mod %>%
  group_by(patient_id) %>%
  summarise(
    max_profound_dur = max(profound_dur),
    max_severe_dur = max(severe_dur)
  ) %>%
  mutate(
    ICAHT = case_when(
      max_severe_dur == 31 | max_profound_dur >= 14 ~ 4,
      (max_severe_dur >= 14 &
         max_severe_dur < 31) |
        (max_profound_dur >= 7 & max_profound_dur < 14) ~ 3,
      max_severe_dur >= 7 &
        max_severe_dur < 14 ~ 2,
      max_severe_dur > 0 &
        max_severe_dur < 7 ~ 1,
      max_severe_dur == 0 ~ 0
    )
  )

### Export df_dur
write.xlsx(df_dur, "Automated Early ICAHT Grades by Manual Code.xlsx")