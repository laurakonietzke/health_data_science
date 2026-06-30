library(dplyr)
library(broom)
library(pROC)
library(ggplot2)
library(glmnet)

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


## ROC Kurve und AUC
# Für jede Person im Datensatz wird die vom Modell vorhergesagte Wahrscheinlichkeit für Diabetes berechnet (type = "response" gibt Wahrscheinlichkeiten statt Log-Odds zurück),
# einmal mit Modell 1 und einmal mit Modell 2.
md$p1 <- predict(m1, type = "response")
md$p2 <- predict(m2, type = "response")

# ROC-Kurven für beide Modelle erstellen

# zur Bewertung und zum Vergleich der Vorhersagequalität beider Modelle.
# Verglichen werden die vorhergesagten Diabetes-Wahrscheinlichkeiten mit dem tatsächlichen Diabetes-Status.
roc1 <- roc(md$diabetes, md$p1, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
roc2 <- roc(md$diabetes, md$p2, levels = c("No", "Yes"), direction = "<", quiet = TRUE)

# Erstellung einer Übersichtstabelle zum Vergleich der Modellgüte anhand der AUC-Werte
# inklusive 95%-Konfidenzintervallen der beiden Modelle.
data.frame(
  model   = c("Block 1 (base)", "Block 2 (+ lifestyle)"),
  AUC     = round(c(auc(roc1), auc(roc2)), 3),
  CI_low  = round(c(ci.auc(roc1)[1], ci.auc(roc2)[1]), 3),
  CI_high = round(c(ci.auc(roc1)[3], ci.auc(roc2)[3]), 3))

# Statistischer Vergleich der AUC-Werte beider Modelle mittels DeLong-Test,
# um zu prüfen, ob sich die Vorhersageleistung signifikant unterscheidet.
dt <- roc.test(roc1, roc2, method = "delong")
cat(sprintf("DeLong test (base vs full): Z = %.2f, p = %.3g\n",
            dt$statistic, dt$p.value))

# Grafischer Vergleich beider ROC-Kurven
# legacy.axes = TRUE sorgt für die klassische Achsenbeschriftung (1 - Spezifität statt Spezifität)
# Diagonale Linie = Referenzlinie eines zufälligen Klassifikators (AUC = 0.5)
ggroc(list(`Base` = roc1, `+ Lifestyle` = roc2), legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  labs(title = "ROC: base vs. full model", x = "1 - Specificity",
       y = "Sensitivity", colour = "Model") +
  coord_equal() + theme_minimal()


## Konfusionsmatrix
# Funktion zur Berechnung von Konfusionsmatrix und Gütemaßen für einen gegebenen Schwellenwert (thr)
conf_metrics <- function(prob, actual, thr) {
  # Klassifikation: Wahrscheinlichkeit >= Schwellenwert -> "Yes", sonst "No"
  pred <- factor(ifelse(prob >= thr, "Yes", "No"), levels = c("No", "Yes"))
  cm <- table(Predicted = pred, Actual = actual)
  # Einzelne Zellen der Konfusionsmatrix extrahieren
  TP <- cm["Yes","Yes"]; TN <- cm["No","No"]
  FP <- cm["Yes","No"];  FN <- cm["No","Yes"]
  # Gütemaße berechnen: Sensitivität, Spezifität, Accuracy, Precision
  list(cm = cm, stats = c(
    Threshold   = round(thr, 3),
    Sensitivity = round(TP/(TP+FN), 3),
    Specificity = round(TN/(TN+FP), 3),
    Accuracy    = round((TP+TN)/sum(cm), 3),
    Precision   = round(TP/(TP+FP), 3)))
}
# Optimalen Schwellenwert nach Youden-Index bestimmen (maximiert Sensitivität + Spezifität - 1)
thr_youden <- as.numeric(coords(roc2, "best", best.method = "youden",
                                ret = "threshold", transpose = TRUE))
# Konfusionsmatrix und Gütemaße für Standard-Schwellenwert 0.5 berechnen
r_05  <- conf_metrics(md$p2, md$diabetes, 0.5)
# Konfusionsmatrix und Gütemaße für den Youden-optimalen Schwellenwert berechnen
r_yj  <- conf_metrics(md$p2, md$diabetes, thr_youden)

# Ergebnisse ausgeben: Konfusionsmatrizen für beide Schwellenwerte
cat("Threshold = 0.5\n");  print(r_05$cm)
cat("\nYouden-optimal threshold =", round(thr_youden, 3), "\n"); print(r_yj$cm)

# Direkter Vergleich der Gütemaße beider Schwellenwerte in einer Tabelle
rbind(`0.5` = r_05$stats, `Youden` = r_yj$stats)


## LASSO-Regression zur datengesteuerten Variablenselektion aus einem breiten Prädiktorenpool.
# Durch die Regularisierung (alpha = 1) werden irrelevante Variablen auf 0 geschätzt,
# sodass Auswahl und Schätzung gleichzeitig erfolgen.
# Im Gegensatz zur hierarchischen Regression erfolgt keine Blockstruktur, sondern eine freie Auswahl relevanter Prädiktoren.
# Der verwendete Variablenpool umfasst soziodemografische und Lebensstilvariablen,
# während potenzielle Folgeerkrankungen (Herzinfarkt, Schlaganfall) und Biomarker (LDL, HDL,
# Triglyceride) ausgeschlossen wurden,
# da diese eher Konsequenzen als Ursachen von Diabetes sind.
lasso_vars <- c("age", "gender", "origin",
                "bmi", "smoking_status", "sleep_duration",
                "moderate_min_week", "vigorous_min_week", "sugar_intake",
                "calorie_intake", "alcohol_week",
                "education_level", "hypertension", "income")

lasso_fml <- as.formula(paste("diabetes ~", paste(lasso_vars, collapse = " + ")))

# Eigene Complete-Case-Stichprobe für LASSO: mehr Variablen bedeuten ggf.
# andere fehlende Werte als in md (Block 1/2)
md_lasso <- ad[complete.cases(ad[, c("diabetes", lasso_vars)]), ]
cat("LASSO modelling sample:", nrow(md_lasso),
    "| Diabetes = Yes:", sum(md_lasso$diabetes == "Yes"),
    sprintf("(%.1f%%)\n", 100 * mean(md_lasso$diabetes == "Yes")))

# Designmatrix: model.matrix() kodiert kategoriale Variablen als Dummies;
# Intercept wird entfernt (glmnet fügt ihn intern hinzu)
X <- model.matrix(lasso_fml, data = md_lasso)[, -1]

# Outcome als binäre Variable (0 = No, 1 = Yes)
y <- as.integer(md_lasso$diabetes == "Yes")

# 10-fold-Kreuzvalidierung zur Wahl des optimalen Lambda-Werts
# set.seed() sorgt für reproduzierbare Fold-Aufteilung
set.seed(42)
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 10)

# Zwei typische Lambda-Werte:
# lambda.min = bestes Vorhersage-Lambda (mehr Variablen)
# lambda.1se = sparsameres Modell (besser für Interpretation)
cat(sprintf("lambda.min = %.5f  |  lambda.1se = %.5f\n",
            cv_lasso$lambda.min, cv_lasso$lambda.1se))

# Koeffizienten beim sparsamen Modell (lambda.1se)
# Variablen mit Koeffizient = 0 werden nicht selektiert
lasso_coef <- coef(cv_lasso, s = "lambda.1se")

lasso_df <- data.frame(
  term        = rownames(lasso_coef),
  coefficient = round(as.numeric(lasso_coef), 4),
  row.names   = NULL
  )

lasso_df$selected <- lasso_df$coefficient != 0

# Ausgabe der Koeffizienten (ohne Intercept)
cat("\nLASSO-Koeffizienten (lambda.1se):\n")
print(lasso_df[lasso_df$term != "(Intercept)", ])

# Anzahl selektierter Variablen
cat(sprintf(
  "\nVariablen selektiert (Koeffizient != 0): %d von %d\n",
  sum(lasso_df$selected & lasso_df$term != "(Intercept)"),
  nrow(lasso_df) - 1
))

# Vorhersagewahrscheinlichkeiten und AUC für das LASSO-Modell
md_lasso$p_lasso <- as.numeric(
  predict(cv_lasso, newx = X, s = "lambda.1se", type = "response"))
roc_lasso <- roc(md_lasso$diabetes, md_lasso$p_lasso,
                 levels = c("No", "Yes"), direction = "<", quiet = TRUE)

# Modellvergleich: AUC aller drei Modelle nebeneinander
# Hinweis: LASSO-Stichprobe (md_lasso) kann von Block 1/2 (md) abweichen,
# da mehr Variablen ggf. mehr fehlende Werte erzeugen -> n-Spalte beachten
data.frame(
  model   = c("Block 1 (base)", "Block 2 (+ lifestyle)", "LASSO (breiter Pool)"),
  n       = c(nrow(md), nrow(md), nrow(md_lasso)),
  AUC     = round(c(auc(roc1), auc(roc2), auc(roc_lasso)), 3),
  CI_low  = round(c(ci.auc(roc1)[1], ci.auc(roc2)[1], ci.auc(roc_lasso)[1]), 3),
  CI_high = round(c(ci.auc(roc1)[3], ci.auc(roc2)[3], ci.auc(roc_lasso)[3]), 3)
)

# ROC-Kurven aller drei Modelle im Vergleich
ggroc(list(`Base (Block 1)` = roc1,
           `+ Lifestyle (Block 2)` = roc2,
           `LASSO (breiter Pool)` = roc_lasso),
      legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  labs(title = "ROC: base vs. full model vs. LASSO",
       x = "1 - Specificity", y = "Sensitivity", colour = "Model") +
  coord_equal() + theme_minimal()
