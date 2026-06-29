library(dplyr)
library(ggplot2)
library(pROC)
library(scales)

# Farbpalette (konsistent durch alle Grafiken)
farben <- c("No" = "#185FA5", "Yes" = "#D85A30")

# Daten laden und vorbereiten (wie im Analyse-Skript)
path <- file.path("data", "Datensatz", "nhanes_cleaned_variablen.csv")
raw  <- read.csv(path, stringsAsFactors = FALSE)

ad <- raw %>%
  filter(age >= 18) %>%
  mutate(
    diabetes        = factor(diabetes, levels = c(0,1), labels = c("No","Yes")),
    gender          = factor(gender),
    origin          = factor(origin),
    education_level = factor(education_level),
    smoking_status  = factor(smoking_status, levels = c("Never","Former","Current")),
    hypertension    = factor(hypertension, levels = c(0,1), labels = c("No","Yes")),
    heart_attacks   = factor(heart_attacks, levels = c(0,1), labels = c("No","Yes")),
    stroke          = factor(stroke, levels = c(0,1), labels = c("No","Yes"))
  )

# Komplettes Modell-Dataset
nonmod    <- c("age","gender","origin")
lifestyle <- c("bmi","smoking_status","sleep_duration","moderate_min_week","sugar_intake")
model_vars <- c("diabetes", nonmod, lifestyle)
md <- ad[complete.cases(ad[, model_vars]), ]

# Modelle
m1 <- glm(as.formula(paste("diabetes ~", paste(nonmod, collapse=" + "))),
          data = md, family = binomial)
m_full <- glm(as.formula(paste("diabetes ~", paste(c(nonmod, lifestyle), collapse=" + "))),
              data = md, family = binomial)

md$p1 <- predict(m1, type = "response")
md$p2 <- predict(m_full, type = "response")

# Theme für alle Grafiken
pres_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title        = element_text(face = "bold", size = 16, margin = margin(b = 6)),
    plot.subtitle     = element_text(size = 13, color = "grey40", margin = margin(b = 12)),
    plot.caption      = element_text(size = 10, color = "grey55", hjust = 0),
    axis.title        = element_text(size = 12),
    axis.text         = element_text(size = 11),
    legend.title      = element_text(size = 12),
    legend.text       = element_text(size = 11),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey92"),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.position   = "top"
  )


# ================================================================
# 1 Altersverteilung nach Diabetes-Status
# ================================================================
ggplot(ad, aes(x = age, fill = diabetes, color = diabetes)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  scale_fill_manual(values  = farben, name = "Diabetes") +
  scale_color_manual(values = farben, name = "Diabetes") +
  scale_x_continuous(breaks = seq(20, 80, 10)) +
  labs(
    title    = "Altersverteilung nach Diabetes-Status",
    subtitle = "Erwachsene ≥ 18 Jahre (NHANES Zyklus L)",
    x        = "Alter (Jahre)",
    y        = "Dichte",
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme


# ================================================================
# 2 BMI-Vergleich nach Diabetes-Status (Boxplot + Jitter)
# ================================================================
ggplot(ad, aes(x = diabetes, y = bmi, fill = diabetes)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5, linewidth = 0.7) +
  geom_jitter(aes(color = diabetes), width = 0.15, alpha = 0.08, size = 0.8) +
  scale_fill_manual(values  = farben, guide = "none") +
  scale_color_manual(values = farben, guide = "none") +
  scale_x_discrete(labels = c("No" = "Kein Diabetes", "Yes" = "Diabetes")) +
  labs(
    title    = "BMI nach Diabetes-Status",
    subtitle = "Boxplot mit individuellen Datenpunkten",
    x        = NULL,
    y        = "BMI (kg/m²)",
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme


# ================================================================
# 3 Odds Ratios (Forest Plot)
# ================================================================
library(broom)

or_tab <- tidy(m_full, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    signif = p.value < 0.05,
    term   = recode(term,
                    "age"                         = "Alter (pro Jahr)",
                    "genderFemale"                = "Geschlecht: Weiblich",
                    "originNon-Hispanic White"    = "Ethnie: Nicht-Hisp. Weiß",
                    "originNon-Hispanic Black"    = "Ethnie: Nicht-Hisp. Schwarz",
                    "originNon-Hispanic Asian"    = "Ethnie: Nicht-Hisp. Asiatisch",
                    "originOther Hispanic"        = "Ethnie: Andere Hispanisch",
                    "originOther / Multi-Racial"  = "Ethnie: Sonstige/Mehrrassig",
                    "bmi"                         = "BMI (pro Einheit)",
                    "smoking_statusFormer"        = "Rauchen: Ex-Raucher",
                    "smoking_statusCurrent"       = "Rauchen: Aktiv",
                    "sleep_duration"              = "Schlafdauer (h)",
                    "moderate_min_week"           = "Moderate Aktivität (min/Wo.)",
                    "sugar_intake"                = "Zuckeraufnahme (g/Tag)"
    )
  )

ggplot(or_tab, aes(x = estimate, y = reorder(term, estimate), color = signif)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("TRUE" = "#D85A30", "FALSE" = "#888780"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
    name   = "Signifikanz"
  ) +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2, 2.5),
                labels  = number_format(accuracy = 0.01)) +
  labs(
    title    = "Odds Ratios – Multiple Logistische Regression",
    subtitle = "Adjustierte ORs mit 95%-Konfidenzintervall (log-Skala)",
    x        = "Odds Ratio (log-Skala)",
    y        = NULL,
    caption  = "Referenz: OR = 1 (gestrichelte Linie)"
  ) +
  pres_theme +
  theme(legend.position = "bottom")


# ================================================================
# 4 ROC-Kurven (Basis- vs. Vollmodell)
# ================================================================
roc1 <- roc(md$diabetes, md$p1, levels = c("No","Yes"), direction = "<", quiet = TRUE)
roc2 <- roc(md$diabetes, md$p2, levels = c("No","Yes"), direction = "<", quiet = TRUE)

roc_df <- bind_rows(
  data.frame(
    fpr   = 1 - roc1$specificities,
    tpr   = roc1$sensitivities,
    Modell = sprintf("Basis (AUC = %.3f)", auc(roc1))
  ),
  data.frame(
    fpr   = 1 - roc2$specificities,
    tpr   = roc2$sensitivities,
    Modell = sprintf("Vollmodell (AUC = %.3f)", auc(roc2))
  )
)

ggplot(roc_df, aes(x = fpr, y = tpr, color = Modell, linetype = Modell)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted",
              color = "grey60", linewidth = 0.7) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("#185FA5", "#D85A30")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  coord_equal() +
  labs(
    title    = "ROC-Kurven: Basis- vs. Vollmodell",
    subtitle = "DeLong-Test: Vollmodell signifikant besser (p < 0.001)",
    x        = "1 – Spezifität (False Positive Rate)",
    y        = "Sensitivität (True Positive Rate)",
    color    = NULL, linetype = NULL,
    caption  = "Quelle: NHANES Cycle L, logistische Regression"
  ) +
  pres_theme


# ================================================================
# 5 Confusion Matrix als Heatmap
# ================================================================
library(tidyr)

thr_youden <- as.numeric(coords(roc2, "best", best.method = "youden",
                                ret = "threshold", transpose = TRUE))

pred_yj  <- factor(ifelse(md$p2 >= thr_youden, "Yes", "No"), levels = c("No","Yes"))
cm_df    <- as.data.frame(table(Vorhergesagt = pred_yj, Tatsächlich = md$diabetes))

ggplot(cm_df, aes(x = Tatsächlich, y = Vorhergesagt, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq), size = 8, fontface = "bold",
            color = "white") +
  scale_fill_gradient(low = "#B5D4F4", high = "#0C447C",
                      name = "Anzahl") +
  scale_x_discrete(labels = c("No" = "Kein Diabetes", "Yes" = "Diabetes")) +
  scale_y_discrete(labels = c("No" = "Kein Diabetes", "Yes" = "Diabetes")) +
  labs(
    title    = "Konfusionsmatrix (Youden-Schwellenwert)",
    subtitle = sprintf("Schwellenwert = %.3f", thr_youden),
    x        = "Tatsächliche Klasse",
    y        = "Vorhergesagte Klasse",
    caption  = "Quelle: NHANES Cycle L, Vollmodell"
  ) +
  pres_theme +
  theme(legend.position = "right")


# ================================================================
# 6 Rauchstatus nach Diabetes-Status (gruppiertes Balkendiagramm)
# ================================================================
ad %>%
  filter(!is.na(smoking_status)) %>%
  count(diabetes, smoking_status) %>%
  group_by(diabetes) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = smoking_status, y = pct, fill = diabetes)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            position = position_dodge(0.7), vjust = -0.4,
            size = 3.5, color = "grey30") +
  scale_fill_manual(values = farben, name = "Diabetes") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Rauchstatus nach Diabetes-Gruppe",
    subtitle = "Anteil innerhalb jeder Gruppe",
    x        = "Rauchstatus",
    y        = "Anteil (%)",
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme



# DESKRIPTIVE STATISTIK

# ================================================================
# Diabetes-Prävalenz (Donut-Chart)
# ================================================================
ad %>%
  count(diabetes) %>%
  mutate(
    pct   = n / sum(n),
    label = sprintf("%s\n%d (%.1f%%)", diabetes, n, pct * 100),
    ypos  = cumsum(pct) - pct / 2
  ) %>%
  ggplot(aes(x = 2, y = pct, fill = diabetes)) +
  geom_col(width = 1, color = "white", linewidth = 1.2) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = farben, guide = "none") +
  labs(
    title    = "Diabetes-Prävalenz in der Stichprobe",
    subtitle = "Erwachsene ≥ 18 Jahre (NHANES Zyklus L)",
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


# ================================================================
# Verteilung aller kontinuierlichen Variablen (Facet-Histogramme)
# ================================================================
cont_long <- ad %>%
  select(diabetes, age, bmi, sleep_duration, sugar_intake,
         calorie_intake, moderate_min_week) %>%
  pivot_longer(-diabetes, names_to = "variable", values_to = "wert") %>%
  mutate(variable = recode(variable,
                           "age"               = "Alter (Jahre)",
                           "bmi"               = "BMI (kg/m²)",
                           "sleep_duration"    = "Schlafdauer (h)",
                           "sugar_intake"      = "Zuckeraufnahme (g)",
                           "calorie_intake"    = "Kalorienaufnahme (kcal)",
                           "moderate_min_week" = "Moderate Aktivität (min/Wo.)"
  ))

ggplot(cont_long, aes(x = wert, fill = diabetes)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = farben, name = "Diabetes") +
  labs(
    title    = "Verteilung kontinuierlicher Variablen",
    subtitle = "Nach Diabetes-Status überlagert",
    x        = NULL,
    y        = "Anzahl",
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "top"
  )


# ================================================================
# Alters- und Geschlechts-Pyramide
# ================================================================
ad %>%
  filter(!is.na(gender)) %>%
  mutate(age_group = cut(age,
                         breaks = c(17, 29, 39, 49, 59, 69, 79, Inf),
                         labels = c("18–29","30–39","40–49","50–59","60–69","70–79","80+")
  )) %>%
  count(age_group, gender) %>%
  mutate(n_plot = if_else(gender == "Male", -n, n)) %>%
  ggplot(aes(x = n_plot, y = age_group, fill = gender)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
  scale_fill_manual(values = c("Male" = "#185FA5", "Female" = "#D85A30"),
                    name = "Geschlecht") +
  scale_x_continuous(
    labels = function(x) scales::comma(abs(x)),
    breaks = seq(-800, 800, 200)
  ) +
  labs(
    title    = "Alters- und Geschlechtspyramide",
    subtitle = "Erwachsene ≥ 18 Jahre (NHANES Zyklus L)",
    x        = "Anzahl Personen",
    y        = "Altersgruppe",
    caption  = "Links = Männer, Rechts = Frauen"
  ) +
  pres_theme +
  theme(legend.position = "top")


# ================================================================
# Bildungsgrad nach Diabetes-Status
# ================================================================
ad %>%
  filter(!is.na(education_level)) %>%
  count(education_level, diabetes) %>%
  group_by(education_level) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = education_level, y = pct, fill = diabetes)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            position = position_dodge(0.7),
            vjust = -0.5, size = 3.5, color = "grey30") +
  scale_fill_manual(values = farben, name = "Diabetes") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Bildungsgrad nach Diabetes-Status",
    subtitle = "Anteil innerhalb jeder Bildungsgruppe",
    x        = "Bildungsabschluss",
    y        = "Anteil (%)",
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme +
  theme(legend.position = "top")



# ================================================================
# Einfache Histogramme – eine Variable pro Plot
# ================================================================
plot_hist <- function(var, label, binw = NULL) {
  p <- ggplot(ad %>% filter(!is.na(.data[[var]])),
              aes(x = .data[[var]], fill = diabetes)) +
    geom_histogram(binwidth = binw, bins = if (is.null(binw)) 25 else NULL,
                   alpha = 0.75, position = "identity", color = "white",
                   linewidth = 0.3) +
    scale_fill_manual(values = farben, name = "Diabetes") +
    labs(
      title = label,
      x     = label,
      y     = "Anzahl",
      caption = "Quelle: NHANES Cycle L"
    ) +
    pres_theme +
    theme(legend.position = "top")
  print(p)
}

plot_hist("age",               "Alter (Jahre)",              binw = 2)
plot_hist("bmi",               "BMI (kg/m²)",                binw = 1)
plot_hist("sleep_duration",    "Schlafdauer (h)",             binw = 0.5)
plot_hist("sugar_intake",      "Zuckeraufnahme (g/Tag)",      binw = 20)
plot_hist("moderate_min_week", "Moderate Aktivität (min/Wo.)",binw = 30)


# ================================================================
# Violin-Plot – Verteilungsform auf einen Blick
# ================================================================
ad %>%
  select(diabetes, age, bmi, sleep_duration, sugar_intake, moderate_min_week) %>%
  pivot_longer(-diabetes, names_to = "variable", values_to = "wert") %>%
  mutate(variable = recode(variable,
                           "age"               = "Alter (Jahre)",
                           "bmi"               = "BMI (kg/m²)",
                           "sleep_duration"    = "Schlafdauer (h)",
                           "sugar_intake"      = "Zuckeraufnahme (g)",
                           "moderate_min_week" = "Mod. Aktivität (min/Wo.)"
  )) %>%
  ggplot(aes(x = diabetes, y = wert, fill = diabetes)) +
  geom_violin(trim = TRUE, alpha = 0.6, linewidth = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = NA,
               fill = "white", linewidth = 0.6) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = farben, guide = "none") +
  scale_x_discrete(labels = c("No" = "Kein\nDiabetes", "Yes" = "Diabetes")) +
  labs(
    title    = "Verteilungsform nach Diabetes-Status",
    subtitle = "Violin = Verteilungsdichte | Box = Median + IQR",
    x        = NULL,
    y        = "Wert",
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme +
  theme(strip.text = element_text(face = "bold", size = 11))


# ================================================================
# Anteile kategorischer Variablen – einfache Übersicht
# ================================================================
ad %>%
  select(diabetes,
         "Geschlecht"   = gender,
         "Rauchstatus"  = smoking_status,
         "Hypertonie"   = hypertension,
         "Herzinfarkt"  = heart_attacks,
         "Schlaganfall" = stroke) %>%
  pivot_longer(-diabetes, names_to = "variable", values_to = "auspraegung") %>%
  filter(!is.na(auspraegung)) %>%
  count(variable, auspraegung, diabetes) %>%
  group_by(variable, auspraegung) %>%
  mutate(pct = n / sum(n)) %>%
  filter(diabetes == "Yes") %>%
  ggplot(aes(x = pct,
             y = reorder(paste(variable, "–", auspraegung), pct),
             fill = pct)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            hjust = -0.15, size = 3.8, color = "grey25") +
  scale_fill_gradient(low = "#B5D4F4", high = "#0C447C",
                      labels = percent_format(), name = "Diabetes-\nAnteil") +
  scale_x_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.12)),
                     limits = c(0, 1)) +
  labs(
    title    = "Diabetes-Anteil je Kategorie",
    subtitle = "Anteil der Diabetes-Ja-Gruppe innerhalb jeder Ausprägung",
    x        = "Anteil mit Diabetes (%)",
    y        = NULL,
    caption  = "Quelle: NHANES Cycle L"
  ) +
  pres_theme +
  theme(legend.position = "right")


# ================================================================
# Hilfsfunktion um Ausreißer zu entfernen
# ================================================================
# Werte außerhalb von Q1 - 1.5*IQR bzw. Q3 + 1.5*IQR werden auf NA gesetzt
remove_outliers <- function(x, factor = 1.5) {
  q  <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  if_else(x < q[1] - factor * iqr | x > q[2] + factor * iqr, NA_real_, x)
}


# ================================================================
# Facet-Histogramme (ohne Ausreißer)
# ================================================================
ad %>%
  select(diabetes, age, bmi, sleep_duration, sugar_intake,
         calorie_intake, moderate_min_week) %>%
  mutate(across(-diabetes, remove_outliers)) %>%
  pivot_longer(-diabetes, names_to = "variable", values_to = "wert") %>%
  mutate(variable = recode(variable,
                           "age"               = "Alter (Jahre)",
                           "bmi"               = "BMI (kg/m²)",
                           "sleep_duration"    = "Schlafdauer (h)",
                           "sugar_intake"      = "Zuckeraufnahme (g)",
                           "calorie_intake"    = "Kalorienaufnahme (kcal)",
                           "moderate_min_week" = "Moderate Aktivität (min/Wo.)"
  )) %>%
  ggplot(aes(x = wert, fill = diabetes)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity",
                 color = "white", linewidth = 0.3) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = farben, name = "Diabetes") +
  labs(
    title    = "Verteilung kontinuierlicher Variablen",
    subtitle = "Nach Diabetes-Status überlagert | Ausreißer entfernt (IQR × 1.5)",
    x        = NULL,
    y        = "Anzahl",
    caption  = "Quelle: NHANES Cycle L | Werte außerhalb Q1/Q3 ± 1.5×IQR ausgeschlossen"
  ) +
  pres_theme +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "top"
  )


# ================================================================
# Violin-Plot (ohne Ausreißer)
# ================================================================
ad %>%
  select(diabetes, age, bmi, sleep_duration, sugar_intake, moderate_min_week) %>%
  mutate(across(-diabetes, remove_outliers)) %>%
  pivot_longer(-diabetes, names_to = "variable", values_to = "wert") %>%
  filter(!is.na(wert)) %>%
  mutate(variable = recode(variable,
                           "age"               = "Alter (Jahre)",
                           "bmi"               = "BMI (kg/m²)",
                           "sleep_duration"    = "Schlafdauer (h)",
                           "sugar_intake"      = "Zuckeraufnahme (g)",
                           "moderate_min_week" = "Mod. Aktivität (min/Wo.)"
  )) %>%
  ggplot(aes(x = diabetes, y = wert, fill = diabetes)) +
  geom_violin(trim = TRUE, alpha = 0.6, linewidth = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = NA,
               fill = "white", linewidth = 0.6) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "grey20") +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = farben, guide = "none") +
  scale_x_discrete(labels = c("No" = "Kein\nDiabetes", "Yes" = "Diabetes")) +
  labs(
    title    = "Verteilungsform nach Diabetes-Status",
    subtitle = "Violin = Dichte | Box = Median + IQR | Raute = Mittelwert | Ausreißer entfernt (IQR × 1.5)",
    x        = NULL,
    y        = "Wert",
    caption  = "Quelle: NHANES Cycle L | Werte außerhalb Q1/Q3 ± 1.5×IQR ausgeschlossen"
  ) +
  pres_theme +
  theme(strip.text = element_text(face = "bold", size = 11))