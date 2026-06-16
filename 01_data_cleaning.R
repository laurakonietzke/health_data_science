# =============================================================================
# 01_data_cleaning.R
# NHANES (cycle "L") -> analysis-ready diabetes dataset
#
# Rewrite of the original Diabetes_Dataset_Cleaning.R. The original applied a
# single missing-code rule (7/9/77/.../9999 -> NA) to EVERY column, which
# deleted legitimate values (e.g. age 7/9/77, sleep 7h/9h). See
# docs/data_quality_issue.md. This version handles missing codes PER VARIABLE.
#
# Run from the project root (it auto-detects its own location):
#   Rscript 01_data_cleaning.R
# =============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
})

# ---- Resolve project paths (script lives in the repo root) ------------------
args     <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  getwd()
}
raw_dir <- file.path(root, "data", "Rohdaten")
out_dir <- file.path(root, "data", "Datensatz")
stopifnot(dir.exists(raw_dir))

# ---- Helper: set specific sentinel codes to NA for given columns ------------
na_codes <- function(df, cols, codes) {
  cols <- intersect(cols, names(df))
  df %>% mutate(across(all_of(cols), ~ replace(.x, .x %in% codes, NA)))
}

# =============================================================================
# 1. Import raw NHANES modules
# =============================================================================
rx <- function(f) read_xpt(file.path(raw_dir, f))

demo   <- rx("DEMO_L.xpt")
diq    <- rx("DIQ_L.xpt")
bmx    <- rx("BMX_L.xpt")
paq    <- rx("PAQ_L.xpt")
dr1tot <- rx("DR1TOT_L.xpt")
smq    <- rx("SMQ_L.xpt")
alq    <- rx("ALQ_L.xpt")
slq    <- rx("SLQ_L.xpt")
trigly <- rx("TRIGLY_L.xpt")
hdl    <- rx("HDL_L.xpt")
bpq    <- rx("BPQ_L.xpt")
mcq    <- rx("MCQ_L.xpt")

# =============================================================================
# 2. Select needed variables from each module
# =============================================================================
demo_sel   <- demo   %>% select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3, INDFMPIR,
                                DMDEDUC2, WTMEC2YR, SDMVSTRA, SDMVPSU)
diq_sel    <- diq    %>% select(SEQN, DIQ010)
bmx_sel    <- bmx    %>% select(SEQN, BMXBMI)
paq_sel    <- paq    %>% select(SEQN, PAD790Q, PAD790U, PAD800,
                                PAD810Q, PAD810U, PAD820)
dr1tot_sel <- dr1tot %>% select(SEQN, DR1TKCAL, DR1TSUGR)
smq_sel    <- smq    %>% select(SEQN, SMQ020, SMQ040, SMD650)
alq_sel    <- alq    %>% select(SEQN, ALQ130)
slq_sel    <- slq    %>% select(SEQN, SLD012)
trigly_sel <- trigly %>% select(SEQN, LBDLDL, LBXTLG)
hdl_sel    <- hdl    %>% select(SEQN, LBDHDD)
bpq_sel    <- bpq    %>% select(SEQN, BPQ020)
mcq_sel    <- mcq    %>% select(SEQN, MCQ160E, MCQ160F)

# =============================================================================
# 3. Merge all modules on the respondent id (SEQN)
# =============================================================================
nhanes <- demo_sel %>%
  left_join(diq_sel,    by = "SEQN") %>%
  left_join(bmx_sel,    by = "SEQN") %>%
  left_join(paq_sel,    by = "SEQN") %>%
  left_join(dr1tot_sel, by = "SEQN") %>%
  left_join(smq_sel,    by = "SEQN") %>%
  left_join(alq_sel,    by = "SEQN") %>%
  left_join(slq_sel,    by = "SEQN") %>%
  left_join(trigly_sel, by = "SEQN") %>%
  left_join(hdl_sel,    by = "SEQN") %>%
  left_join(bpq_sel,    by = "SEQN") %>%
  left_join(mcq_sel,    by = "SEQN")

# =============================================================================
# 4. PER-VARIABLE missing-code handling
#    (NHANES "Refused"/"Don't know" codes are column specific)
# =============================================================================
nhanes <- nhanes %>%
  # Yes/No & education items: 7 = Refused, 9 = Don't know
  na_codes(c("DMDEDUC2", "BPQ020", "MCQ160E", "MCQ160F", "SMQ020", "SMQ040"),
           c(7, 9)) %>%
  # Count items: 777 = Refused, 999 = Don't know
  na_codes(c("ALQ130", "SMD650"), c(777, 999)) %>%
  # Physical-activity duration/frequency: 7777 / 9999
  na_codes(c("PAD790Q", "PAD800", "PAD810Q", "PAD820"), c(7777, 9999))
# NOTE: continuous measures (RIDAGEYR, SLD012, BMXBMI, INDFMPIR, DR1TKCAL,
# DR1TSUGR, LBDLDL, LBXTLG, LBDHDD) carry NO sentinel codes and are left as-is.

# =============================================================================
# 5. Standardise physical activity to "per week", then minutes/week
#    PAD790U / PAD810U units: D = day, W = week, M = month, Y = year
# =============================================================================
to_per_week <- function(q, u) {
  case_when(
    u == "D" ~ q * 7,
    u == "W" ~ q,
    u == "M" ~ q / 4.3,
    u == "Y" ~ q / 52,
    TRUE     ~ NA_real_
  )
}

nhanes <- nhanes %>%
  mutate(
    moderate_freq_week = to_per_week(PAD790Q, PAD790U),
    vigorous_freq_week = to_per_week(PAD810Q, PAD810U),
    moderate_min_week  = moderate_freq_week * PAD800,
    vigorous_min_week  = vigorous_freq_week * PAD820
  )

# =============================================================================
# 6. Recode outcome, factors, binaries, and derive smoking status
# =============================================================================
nhanes <- nhanes %>%
  mutate(
    # Diabetes outcome: 1 = yes, 0 = no (3 borderline / 9 don't know -> NA)
    diabetes = case_when(
      DIQ010 == 1 ~ 1,
      DIQ010 == 2 ~ 0,
      TRUE        ~ NA_real_
    ),

    RIAGENDR = factor(RIAGENDR, levels = c(1, 2),
                      labels = c("Male", "Female")),

    RIDRETH3 = factor(RIDRETH3, levels = c(1, 2, 3, 4, 6, 7),
                      labels = c("Mexican American", "Other Hispanic",
                                 "Non-Hispanic White", "Non-Hispanic Black",
                                 "Non-Hispanic Asian", "Other / Multi-Racial")),

    DMDEDUC2 = factor(DMDEDUC2, levels = c(1, 2, 3, 4, 5),
                      labels = c("<9th grade", "9-11th grade",
                                 "High school/GED", "College/Associate",
                                 "College graduate+")),

    # Smoking status from SMQ020 (>=100 cigs lifetime) + SMQ040 (now smoke)
    smoking_status = factor(case_when(
      SMQ020 == 2                    ~ "Never",
      SMQ020 == 1 & SMQ040 %in% 1:2  ~ "Current",
      SMQ020 == 1 & SMQ040 == 3      ~ "Former",
      TRUE                           ~ NA_character_
    ), levels = c("Never", "Former", "Current")),

    # Binary yes/no -> 1/0
    BPQ020  = if_else(BPQ020  == 1, 1, if_else(BPQ020  == 2, 0, NA_real_)),
    MCQ160E = if_else(MCQ160E == 1, 1, if_else(MCQ160E == 2, 0, NA_real_)),
    MCQ160F = if_else(MCQ160F == 1, 1, if_else(MCQ160F == 2, 0, NA_real_))
  )

# =============================================================================
# 7. Build the final analysis dataset (drop intermediates, rename, add id)
# =============================================================================
final <- nhanes %>%
  filter(!is.na(diabetes)) %>%               # drop rows with unknown outcome
  select(
    ID              = SEQN,
    age             = RIDAGEYR,
    gender          = RIAGENDR,
    origin          = RIDRETH3,
    income          = INDFMPIR,
    education_level = DMDEDUC2,
    bmi             = BMXBMI,
    calorie_intake  = DR1TKCAL,
    sugar_intake    = DR1TSUGR,
    smoking_status,
    cigarettes_day  = SMD650,
    alcohol_week    = ALQ130,
    sleep_duration  = SLD012,
    LDL_cholesterol = LBDLDL,
    triglycerides   = LBXTLG,
    HDL_cholesterol = LBDHDD,
    hypertension    = BPQ020,
    heart_attacks   = MCQ160E,
    stroke          = MCQ160F,
    diabetes,
    moderate_min_week,
    vigorous_min_week
  ) %>%
  mutate(patient_id = row_number()) %>%
  select(patient_id, everything())

# =============================================================================
# 8. Back up the old (corrupted) outputs, then save the new ones
# =============================================================================
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
old <- c("nhanes_cleaned.csv", "nhanes_cleaned.rds", "nhanes_cleaned_variablen.csv")
old <- old[file.exists(file.path(out_dir, old))]
if (length(old)) {
  bdir <- file.path(out_dir, "backup_corrupted")
  if (!dir.exists(bdir)) dir.create(bdir)
  file.copy(file.path(out_dir, old), file.path(bdir, old), overwrite = TRUE)
  message("Backed up old outputs to: ", bdir)
}

write.csv(nhanes, file.path(out_dir, "nhanes_cleaned.csv"), row.names = FALSE)
saveRDS(nhanes,   file.path(out_dir, "nhanes_cleaned.rds"))
write.csv(final,  file.path(out_dir, "nhanes_cleaned_variablen.csv"), row.names = FALSE)

# =============================================================================
# 9. Verification — confirm the corruption is fixed
# =============================================================================
cat("\n================ VERIFICATION ================\n")
cat("Final dataset:", nrow(final), "rows x", ncol(final), "cols\n")
cat("\nAges 7 / 9 / 77 present (were 0 in corrupted data):\n")
print(table(factor(final$age, levels = c(7, 9, 77))))
cat("\nSleep 7h / 9h present (were absent in corrupted data):\n")
print(table(factor(final$sleep_duration, levels = c(7, 9))))
cat("\nSmoking status:\n"); print(table(final$smoking_status, useNA = "ifany"))
cat("\nDiabetes outcome:\n"); print(table(final$diabetes, useNA = "ifany"))
cat("\nMissing % per column (final):\n")
print(round(100 * colMeans(is.na(final)), 1))
cat("\nDone.\n")
