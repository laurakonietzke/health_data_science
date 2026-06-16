
# =========================================================
# 1. Pakete laden
# =========================================================

install.packages(c("tidyverse", "haven", "janitor"))

library(tidyverse)
library(haven)
library(janitor)


# =========================================================
# 2. Arbeitsordner setzen
# (Ordner mit allen .xpt-Dateien)
# =========================================================

setwd("C:/Users/jsche/Documents/Uni/Master (HS Aalen)/Semester 2/Health Data Science/health_data_science/data/Rohdaten")


# =========================================================
# 3. NHANES-Datensätze importieren
# =========================================================

demo    <- read_xpt("DEMO_L.xpt")
diq     <- read_xpt("DIQ_L.xpt")
bmx     <- read_xpt("BMX_L.xpt")
paq     <- read_xpt("PAQ_L.xpt")
dr1tot  <- read_xpt("DR1TOT_L.xpt")
smq     <- read_xpt("SMQ_L.xpt")
alq     <- read_xpt("ALQ_L.xpt")
slq     <- read_xpt("SLQ_L.xpt")
trigly  <- read_xpt("TRIGLY_L.xpt")
hdl     <- read_xpt("HDL_L.xpt")
bpq     <- read_xpt("BPQ_L.xpt")
mcq     <- read_xpt("MCQ_L.xpt")


# =========================================================
# 4. Nur benötigte Variablen auswählen
# =========================================================

demo_sel <- demo %>%
  select(
    SEQN,
    RIDAGEYR,
    RIAGENDR,
    RIDRETH3,
    INDFMPIR,
    DMDEDUC2,
    WTMEC2YR,
    SDMVSTRA,
    SDMVPSU
  )

diq_sel <- diq %>%
  select(
    SEQN,
    DIQ010
  )

bmx_sel <- bmx %>%
  select(
    SEQN,
    BMXBMI
  )

paq_sel <- paq %>%
  select(
    SEQN,
    PAD790Q,
    PAD790U,
    PAD800,
    PAD810Q,
    PAD810U,
    PAD820
  )

dr1tot_sel <- dr1tot %>%
  select(
    SEQN,
    DR1TKCAL,
    DR1TSUGR
  )

smq_sel <- smq %>%
  select(
    SEQN,
    SMD650
  )

alq_sel <- alq %>%
  select(
    SEQN,
    ALQ130
  )

slq_sel <- slq %>%
  select(
    SEQN,
    SLD012
  )

trigly_sel <- trigly %>%
  select(
    SEQN,
    LBDLDL,
    LBXTLG
  )

hdl_sel <- hdl %>%
  select(
    SEQN,
    LBDHDD
  )

bpq_sel <- bpq %>%
  select(
    SEQN,
    BPQ020
  )

mcq_sel <- mcq %>%
  select(
    SEQN,
    MCQ160E,
    MCQ160F
  )


# =========================================================
# 5. Alle Datensätze über SEQN zusammenführen
# =========================================================

nhanes <- demo_sel %>%
  left_join(diq_sel, by = "SEQN") %>%
  left_join(bmx_sel, by = "SEQN") %>%
  left_join(paq_sel, by = "SEQN") %>%
  left_join(dr1tot_sel, by = "SEQN") %>%
  left_join(smq_sel, by = "SEQN") %>%
  left_join(alq_sel, by = "SEQN") %>%
  left_join(slq_sel, by = "SEQN") %>%
  left_join(trigly_sel, by = "SEQN") %>%
  left_join(hdl_sel, by = "SEQN") %>%
  left_join(bpq_sel, by = "SEQN") %>%
  left_join(mcq_sel, by = "SEQN")


# =========================================================
# 6. NHANES Missing-Codes in NA umwandeln
# =========================================================

nhanes <- nhanes %>%
  mutate(across(
    everything(),
    ~replace(
      .,
      . %in% c(7, 9, 77, 99, 777, 999, 7777, 9999),
      NA
    )
  ))


# =========================================================
# 7. Moderate Aktivität auf "pro Woche" standardisieren
# =========================================================

nhanes <- nhanes %>%
  mutate(
    moderate_freq_week = case_when(
      PAD790U == "D" ~ PAD790Q * 7,
      PAD790U == "W" ~ PAD790Q,
      PAD790U == "M" ~ PAD790Q / 4.3,
      PAD790U == "Y" ~ PAD790Q / 52,
      TRUE ~ NA_real_
    )
  )


# =========================================================
# 8. Intensive Aktivität auf "pro Woche" standardisieren
# =========================================================

nhanes <- nhanes %>%
  mutate(
    vigorous_freq_week = case_when(
      PAD810U == "D" ~ PAD810Q * 7,
      PAD810U == "W" ~ PAD810Q,
      PAD810U == "M" ~ PAD810Q / 4.3,
      PAD810U == "Y" ~ PAD810Q / 52,
      TRUE ~ NA_real_
    )
  )


# =========================================================
# 9. Variablen sinnvoll recodieren
# =========================================================

nhanes <- nhanes %>%
  mutate(
    
    # Diabetes Outcome: 1 = Diabetes, 0 = kein Diabetes
    diabetes = case_when(
      DIQ010 == 1 ~ 1,
      DIQ010 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Geschlecht als Faktor
    RIAGENDR = factor(
      RIAGENDR,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    # Ethnizität als Faktor
    RIDRETH3 = factor(
      RIDRETH3,
      levels = c(1, 2, 3, 4, 6, 7),
      labels = c(
        "Mexican American",
        "Other Hispanic",
        "Non-Hispanic White",
        "Non-Hispanic Black",
        "Non-Hispanic Asian",
        "Other / Multi-Racial"
      )
    ),
    
    # Bildungsstand als Faktor
    DMDEDUC2 = factor(
      DMDEDUC2,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "<9th grade",
        "9-11th grade",
        "High school/GED",
        "College/Associate",
        "College graduate+"
      )
    ),
    
    # Binäre Variablen auf 0/1 umcodieren
    BPQ020 = case_when(
      BPQ020 == 1 ~ 1,
      BPQ020 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    MCQ160E = case_when(
      MCQ160E == 1 ~ 1,
      MCQ160E == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    MCQ160F = case_when(
      MCQ160F == 1 ~ 1,
      MCQ160F == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ===========================================================================
# Moderate bzw. intensive Minuten körperliche Aktivität pro Woche berechnen
# ===========================================================================

nhanes <- nhanes %>%
  mutate(
    
    # Moderate Minuten pro Woche
    moderate_min_week =
      moderate_freq_week * PAD800,
    
    # Intensive Minuten pro Woche
    vigorous_min_week =
      vigorous_freq_week * PAD820
  )


# =========================================================
# 10. Überblick über den bereinigten Datensatz
# =========================================================

glimpse(nhanes)

summary(nhanes)


# =========================================================
# 11. Fehlende Werte pro Variable anzeigen
# =========================================================

colSums(is.na(nhanes))


# =========================================================
# 12. Bereinigten Datensatz speichern
# =========================================================

# CSV-Datei
write.csv(nhanes, "nhanes_cleaned.csv", row.names = FALSE)

# R-Datei (empfohlen)
saveRDS(nhanes, "nhanes_cleaned.rds")

glimpse(nhanes)
