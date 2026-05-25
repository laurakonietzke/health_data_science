nhanes <- read.csv("C:/Users/vanes/Documents/Master/2. Semester/Health Data Science/Alzheimer_Projekt/health_data_science/data/Datensatz/nhanes_cleaned.csv")
# ============================================================
# Variablen entfernen & Datensatz bereinigen
# ============================================================

library(dplyr)

# Datensatz einlesen
nhanes <- read.csv("C:/Users/vanes/Documents/Master/2. Semester/Health Data Science/Alzheimer_Projekt/health_data_science/data/Datensatz/nhanes_cleaned.csv")

# ============================================================
# 1. Variablen entfernen
# ============================================================
nhanes <- nhanes %>%
  select(-WTMEC2YR, -SDMVSTRA, -SDMVPSU, -DIQ010,
         -PAD790Q, -PAD790U, -PAD800,
         -PAD810Q, -PAD810U, -PAD820,
         -moderate_freq_week, -vigorous_freq_week)

# ============================================================
# 2. Zeilen entfernen wo diabetes == NA
# ============================================================
nhanes <- nhanes %>%
  filter(!is.na(diabetes))

# ============================================================
# 3. Ergebnis prüfen
# ============================================================
dim(nhanes)
colnames(nhanes)
head(nhanes)

# ============================================================
# 4. Bereinigten Datensatz speichern
# ============================================================
write.csv(nhanes, "C:/Users/vanes/Documents/Master/2. Semester/Health Data Science/Alzheimer_Projekt/health_data_science/data/Datensatz/nhanes_cleaned_variablen.csv", row.names = FALSE)

# ============================================================
# Spaltennamen umbenennen
# ============================================================

nhanes <- nhanes %>%
  rename(
    ID                = SEQN,
    age               = RIDAGEYR,
    gender            = RIAGENDR,
    origin            = RIDRETH3,
    income            = INDFMPIR,
    education_level   = DMDEDUC2,
    bmi               = BMXBMI,
    calorie_intake    = DR1TKCAL,
    sugar_intake      = DR1TSUGR,
    cigarettes_day    = SMD650,
    alcohol_week      = ALQ130,
    sleep_duration    = SLD012,
    LDL_cholesterol   = LBDLDL,
    triglycerides     = LBXTLG,
    HDL_cholesterol   = LBDHDD,
    hypertension      = BPQ020,
    heart_attacks     = MCQ160E,
    stroke            = MCQ160F
  )

# Prüfen ob es geklappt hat
colnames(nhanes)

# Datensatz speichern
write.csv(nhanes, "C:/Users/vanes/Documents/Master/2. Semester/Health Data Science/Alzheimer_Projekt/health_data_science/data/Datensatz/nhanes_cleaned_variablen.csv", row.names = FALSE)

# ============================================================
# Spalte patient_id hinzufügen
# ============================================================

nhanes <- nhanes %>%
  mutate(patient_id = row_number()) %>%
  select(patient_id, everything())

# Prüfen ob es geklappt hat
head(nhanes)

# Datensatz speichern
write.csv(nhanes, "C:/Users/vanes/Documents/Master/2. Semester/Health Data Science/Alzheimer_Projekt/health_data_science/data/Datensatz/nhanes_cleaned_variablen.csv", row.names = FALSE)