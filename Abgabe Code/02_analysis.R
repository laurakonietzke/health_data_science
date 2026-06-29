library(dplyr)
library(broom)
library(pROC)
library(ggplot2)

# Einlesen des Datensatzes
path <- file.path("data", "Datensatz", "nhanes_cleaned_variablen.csv")
raw  <- read.csv(path, stringsAsFactors = FALSE)

# Definition der Analysepopulation (Erwachsene ≥18 Jahre) und finale Aufbereitung der
# Variablen für die statistische Analyse (Filterung und analysespezifische Faktor-Formatierung)
# -> Ergebnisdatensatz = ad (analysis dataset)
ad <- raw %>%
  filter(age >= 18) %>%
  mutate(
    diabetes       = factor(diabetes, levels = c(0, 1), labels = c("No", "Yes")),
    gender         = factor(gender),
    origin         = factor(origin),
    education_level= factor(education_level),
    smoking_status = factor(smoking_status, levels = c("Never", "Former", "Current")),
    hypertension   = factor(hypertension,  levels = c(0, 1), labels = c("No", "Yes")),
    heart_attacks  = factor(heart_attacks, levels = c(0, 1), labels = c("No", "Yes")),
    stroke         = factor(stroke,        levels = c(0, 1), labels = c("No", "Yes"))
  )

# Kleine Statistik mit Anzahl Erwachsene, Anzahl Diabetiker und Anteil in Prozent
cat("Adults (age >= 18):", nrow(ad),
    "| Diabetes = Yes:", sum(ad$diabetes == "Yes"),
    sprintf("(%.1f%%)", 100 * mean(ad$diabetes == "Yes")))

# Gruppierung der Variablen in nicht beeinflussbare Faktoren (nonmod), beeinflussbare
# Lebensstilfaktoren (lifestyle), kontinuierliche Variablen (cont_vars) und
# kategoriale Variablen (cat_vars)
nonmod   <- c("age", "gender", "origin")                  # Block 1 (non-modifiable)
lifestyle<- c("bmi", "smoking_status", "sleep_duration",
              "moderate_min_week", "sugar_intake")          # Block 2 (modifiable)

cont_vars <- c("age", "bmi", "sleep_duration", "sugar_intake",
               "calorie_intake", "alcohol_week", "moderate_min_week")
cat_vars  <- c("gender", "origin", "education_level", "smoking_status", "hypertension")

# Berechnung von Mittelwert und Standardabweichung für jede kontinuierliche Variable
# getrennt nach: Diabetes = Nein und Diabetes = Ja
# -> Ausgabe als Tabelle im Format "Variable | No: mean (sd) | Yes: mean (sd)"
summ_cont <- function(v) {
  s <- ad %>% group_by(diabetes) %>%
    summarise(mean = mean(.data[[v]], na.rm = TRUE),
              sd   = sd(.data[[v]],   na.rm = TRUE),
              n    = sum(!is.na(.data[[v]])), .groups = "drop")
  data.frame(variable = v,
             No_mean_sd  = sprintf("%.1f (%.1f)", s$mean[s$diabetes=="No"],  s$sd[s$diabetes=="No"]),
             Yes_mean_sd = sprintf("%.1f (%.1f)", s$mean[s$diabetes=="Yes"], s$sd[s$diabetes=="Yes"]))
}
do.call(rbind, lapply(cont_vars, summ_cont))

# Berechnung der Häufigkeiten und prozentualen Anteile für jede kategoriale Variable 
# getrennt nach: Diabetes = Nein und Diabetes = Ja
# -> Ausgabe als Tabelle im Format „Variable | level | No: n (%) | Yes: n (%)“ 
cat_pct <- function(v) {
  t <- table(ad[[v]], ad$diabetes)
  p <- prop.table(t, margin = 2) * 100
  data.frame(variable = v, level = rownames(t),
             No  = sprintf("%d (%.1f%%)", t[, "No"],  p[, "No"]),
             Yes = sprintf("%d (%.1f%%)", t[, "Yes"], p[, "Yes"]),
             row.names = NULL)
}
# Zusammenführen der Einzeltabellen zu einer großen Übersicht
do.call(rbind, lapply(cat_vars, cat_pct))


## T-Test je Variable: vergleicht den Mittelwert zwischen Diabetikern und Nicht-Diabetikern

# Definition der Funktion ttest_row: 
# baut für eine beliebige Variable v die passende Formel (z.B. "bmi ~ diabetes"), 
# führt den t-Test aus und gibt das Ergebnis als saubere Tabellenzeile zurück
ttest_row <- function(v) {
  f  <- as.formula(paste(v, "~ diabetes"))
  tt <- t.test(f, data = ad)
  data.frame(variable = v,
             mean_No  = round(tt$estimate[1], 2),
             mean_Yes = round(tt$estimate[2], 2),
             t        = round(tt$statistic, 2),
             df       = round(tt$parameter, 0),
             p_value  = signif(tt$p.value, 3),
             row.names = NULL)
}

# Ausführung: ttest_row wird einmal pro Variable aus cont_vars aufgerufen,
# alle Ergebniszeilen werden zu einer Tabelle zusammengefügt; anschließend
# wird markiert, welche Ergebnisse statistisch signifikant sind (p < 0,05)
tt_tab <- do.call(rbind, lapply(cont_vars, ttest_row))
tt_tab$signif <- ifelse(tt_tab$p_value < 0.05, "*", "")
tt_tab

## Chi-Quadrat-Test je kategorialer Variable: 
# Im Gegensatz zum t-Test geht es hier um Kategorien (z.B. Raucherstatus, Geschlecht),deshalb wird hier die
# Verteilung der Kategorien zwischen den beiden Diabetes-Gruppen verglichen

# Definition der Funktion chi_row:
# baut für eine beliebige kategoriale Variable v eine Kreuztabelle im Vergleich zu Diabetes,
# führt den Chi-Quadrat-Test aus und gibt das Ergebnis als saubere Tabellenzeile zurück
chi_row <- function(v) {
  tab <- table(ad[[v]], ad$diabetes)
  ct  <- suppressWarnings(chisq.test(tab))
  data.frame(variable = v,
             X_squared = round(ct$statistic, 2),
             df        = ct$parameter,
             p_value   = signif(ct$p.value, 3),
             row.names = NULL)
}
# Ausführung: chi_row wird einmal pro Variable aus cat_vars aufgerufen,
# alle Ergebniszeilen werden zu einer Tabelle zusammengefügt; anschließend
# wird markiert, welche Ergebnisse statistisch signifikant sind (p < 0,05)
chi_tab <- do.call(rbind, lapply(cat_vars, chi_row))
chi_tab$signif <- ifelse(chi_tab$p_value < 0.05, "*", "")
chi_tab

## Logistische Regression: 
# berechnet aus allen Variablen gleichzeitig eine Diabetes-Wahrscheinlichkeit pro Person
# bestimmt Richtung UND Stärke jedes Effekts

# Vorbereitung: nur Personen mit vollständigen Angaben zu allen Modellvariablen
# behalten (glm() kann mit fehlenden Werten nicht rechnen)
model_vars <- c("diabetes", nonmod, lifestyle)
md <- ad[complete.cases(ad[, model_vars]), ]
cat("Complete-case modelling sample:", nrow(md),
    "| Diabetes = Yes:", sum(md$diabetes == "Yes"),
    sprintf("(%.1f%%)", 100 * mean(md$diabetes == "Yes")))

# Baut die Modellformel aus den Variablennamen und berechnet das Modell
full_fml <- as.formula(
  paste("diabetes ~", paste(c(nonmod, lifestyle), collapse = " + ")))
m_full <- glm(full_fml, data = md, family = binomial)

# Odds ratios with 95% CI
# Wandelt die unbearbeiteten Ergebnisse in lesbare Odds Ratios mit Konfidenzintervall um; 
# OR > 1 = Risikofaktor, OR < 1 = Schutzfaktor;
# Intercept wird entfernt, da inhaltlich nicht relevant; anschließend wird
# markiert, welche Effekte statistisch signifikant sind (p < 0,05)
or_tab <- tidy(m_full, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(term,
            OR       = round(estimate, 3),
            CI_low   = round(conf.low, 3),
            CI_high  = round(conf.high, 3),
            p_value  = signif(p.value, 3),
            signif   = ifelse(p.value < 0.05, "*", ""))
or_tab

## Hierarchische Regression: bringt der Lifestyle-Block einen zusätzlichen Erklärungswert
## gegenüber Alter/Geschlecht/Herkunft allein?
# Vergleicht zwei verschachtelte Modelle (Block 2 enthält alle Variablen von
# Block 1 plus zusätzlich die Lifestyle-Variablen)

# Modell 1 (nur nicht-veränderbare Faktoren) und Modell 2 (volles Modell von oben)
m1 <- glm(as.formula(paste("diabetes ~", paste(nonmod, collapse = " + "))),
          data = md, family = binomial)                       # Block 1
m2 <- m_full                                                   # Block 2 (full)

# Hilfsfunktion: berechnet, wie viel des Diabetes-Status ein Modell erklären kann
# höherer Wert = besseres Modell (Pseudo-R²)
mcfadden <- function(m) 1 - as.numeric(logLik(m) / logLik(update(m, . ~ 1)))

# Likelihood-Ratio-Test: 
# Testet, ob Modell 2 wirklich besser ist als Modell 1, oder ob der Unterschied
# auch nur Zufall sein könnte
lrt <- anova(m1, m2, test = "LRT")

# Vergleichstabelle: Anzahl Variablen, AIC (niedriger = besser) und
# Pseudo-R² (höher = besser) für beide Modelle nebeneinander
data.frame(
  model      = c("Block 1: non-modifiable", "Block 2: + lifestyle"),
  predictors = c(length(nonmod), length(c(nonmod, lifestyle))),
  AIC        = round(c(AIC(m1), AIC(m2)), 1),
  pseudoR2   = round(c(mcfadden(m1), mcfadden(m2)), 4)
)

# Gibt das Testergebnis aus: 
# p < 0,05 heißt, Block 2 ist wirklich besser,
# nicht nur durch Zufall
cat("Likelihood-ratio test (Block 1 vs Block 2):\n")
cat(sprintf("  Chi2 = %.2f, df = %d, p = %.3g\n",
            lrt$Deviance[2], lrt$Df[2], lrt$`Pr(>Chi)`[2]))

# Zeigt zusätzlich, wie viel zusätzliche erklärte Variation der Lifestyle-Block
# konkret bringt (Differenz der Pseudo-R²-Werte beider Modelle)
cat(sprintf("  Delta McFadden pseudo-R2 = %.4f\n", mcfadden(m2) - mcfadden(m1)))

md$p1 <- predict(m1, type = "response")
md$p2 <- predict(m2, type = "response")

roc1 <- roc(md$diabetes, md$p1, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
roc2 <- roc(md$diabetes, md$p2, levels = c("No", "Yes"), direction = "<", quiet = TRUE)

data.frame(
  model   = c("Block 1 (base)", "Block 2 (+ lifestyle)"),
  AUC     = round(c(auc(roc1), auc(roc2)), 3),
  CI_low  = round(c(ci.auc(roc1)[1], ci.auc(roc2)[1]), 3),
  CI_high = round(c(ci.auc(roc1)[3], ci.auc(roc2)[3]), 3)
)

dt <- roc.test(roc1, roc2, method = "delong")
cat(sprintf("DeLong test (base vs full): Z = %.2f, p = %.3g\n",
            dt$statistic, dt$p.value))

ggroc(list(`Base` = roc1, `+ Lifestyle` = roc2), legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  labs(title = "ROC: base vs. full model", x = "1 - Specificity",
       y = "Sensitivity", colour = "Model") +
  coord_equal() + theme_minimal()

conf_metrics <- function(prob, actual, thr) {
  pred <- factor(ifelse(prob >= thr, "Yes", "No"), levels = c("No", "Yes"))
  cm <- table(Predicted = pred, Actual = actual)
  TP <- cm["Yes","Yes"]; TN <- cm["No","No"]
  FP <- cm["Yes","No"];  FN <- cm["No","Yes"]
  list(cm = cm, stats = c(
    Threshold   = round(thr, 3),
    Sensitivity = round(TP/(TP+FN), 3),
    Specificity = round(TN/(TN+FP), 3),
    Accuracy    = round((TP+TN)/sum(cm), 3),
    Precision   = round(TP/(TP+FP), 3)))
}

thr_youden <- as.numeric(coords(roc2, "best", best.method = "youden",
                                ret = "threshold", transpose = TRUE))

r_05  <- conf_metrics(md$p2, md$diabetes, 0.5)
r_yj  <- conf_metrics(md$p2, md$diabetes, thr_youden)

cat("Threshold = 0.5\n");  print(r_05$cm)
cat("\nYouden-optimal threshold =", round(thr_youden, 3), "\n"); print(r_yj$cm)

rbind(`0.5` = r_05$stats, `Youden` = r_yj$stats)
