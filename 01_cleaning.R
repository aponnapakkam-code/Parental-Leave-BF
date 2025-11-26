#Cleaning

# ---- packages ----
library(readxl)
library(tidyverse)
library(janitor)
library(DescTools)

path <- "raw_data.xlsx"
#Data originally had 2 separate sheets, one with demographic information, other with feeding information at 2, 4, 6 months

# ---- 1) Read & clean sheets ----
demo <- read_excel(path, sheet = "Mother_Infant_Demographics") |> clean_names()
feed_raw <- read_excel(path, sheet = "Breastmilk_feeding_Infant_encou") |> clean_names()


# keep ONLY the needed columns from the feeding sheet and coerce to numeric
feed <- feed_raw |>
  select(infant_patient, age_at_encntr_mo, feeding_type) |>
  mutate(
    infant_patient   = suppressWarnings(as.integer(infant_patient)),
    age_at_encntr_mo = suppressWarnings(as.integer(age_at_encntr_mo)),
    feeding_type     = suppressWarnings(as.integer(feeding_type))
  ) |>
  # strict filters: valid patient id, valid month, valid feeding code
  filter(!is.na(infant_patient),
         age_at_encntr_mo %in% c(2,4,6),
         feeding_type %in% c(1,2,3))

# standardize the join key in BOTH data frames
demo <- demo %>%
  mutate(
    infant_patient = parse_integer(as.character(infant_patient), na = c("", "NA", "NaN", "NULL"))
  )

feed <- feed %>%
  mutate(
    infant_patient = parse_integer(as.character(infant_patient), na = c("", "NA", "NaN", "NULL"))
  )


# ---- 2) Add Epoch on DEMO by row index (1–143 = Pre; 144–310 = Post) ----
demo <- demo |>
  mutate(.row = row_number(),
         epoch = case_when(
           .row >= 1   & .row <= 143 ~ "Pre",
           .row >= 144 & .row <= 310 ~ "Post",
           TRUE ~ NA_character_
         )) |>
  select(-.row) |>
  mutate(
    # normalize rank
    officer_enlisted = case_when(
      officer_enlisted %in% c(1, "1") ~ "Officer",
      officer_enlisted %in% c(2, "2") ~ "Enlisted",
      TRUE ~ as.character(officer_enlisted)
    ),
    officer_enlisted = factor(officer_enlisted, levels = c("Officer","Enlisted")),
    epoch = factor(epoch, levels = c("Pre","Post"))
  )

# ---- 3) Join feeding -> demographics by infant_patient ----
dat <- feed |>
  left_join(demo |> select(infant_patient, epoch, officer_enlisted),
            by = "infant_patient") |>
  # build month + outcomes (1=breast, 2=combo, 3=formula)
  mutate(
    month   = factor(age_at_encntr_mo, levels = c(2,4,6), labels = c("2mo","4mo","6mo")),
    any_bf  = if_else(feeding_type %in% c(1,2), 1L,
                      if_else(feeding_type == 3, 0L, NA_integer_)),
    excl_bf = if_else(feeding_type == 1, 1L,
                      if_else(feeding_type %in% c(2,3), 0L, NA_integer_))
  ) |>
  # final guardrails: must have epoch, month, and valid rank
  filter(!is.na(epoch), !is.na(month), officer_enlisted %in% c("Officer","Enlisted"))

# ---- 4) Sanity checks ----
cat("Rows kept in dat:", nrow(dat),
    " | unique infants:", n_distinct(dat$infant_patient), "\n")
print(dat |> count(month, officer_enlisted))
# Any duplicate infant × month?
dups <- dat |> count(infant_patient, month) |> filter(n > 1)
if (nrow(dups) > 0) {
  cat("Duplicates detected (infant × month). Showing first few:\n")
  print(head(dups, 10))
  # If desired, keep first per infant×month:
dat <- dat |> arrange(infant_patient, month) |> distinct(infant_patient, month, .keep_all = TRUE)
}

