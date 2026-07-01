library(dplyr)
library(ggplot2)
library(pROC)
library(scales)
library(glmnet)
library(broom)
library(tidyr)

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
# Zusätzliche Tabellen/Modelle, die die späteren Grafiken benötigen
# (siehe Abgabe Code/02_analysis.R für die Herleitung)
# ================================================================

cont_vars <- c("age", "bmi", "sleep_duration", "sugar_intake",
               "calorie_intake", "alcohol_week", "moderate_min_week")
cat_vars  <- c("gender", "origin", "education_level", "smoking_status", "hypertension")

# t-Tests je kontinuierlicher Variable (für die t-Test-Grafik)
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
tt_tab <- do.call(rbind, lapply(cont_vars, ttest_row))
tt_tab$signif <- ifelse(tt_tab$p_value < 0.05, "*", "")

# Chi-Quadrat-Tests je kategorialer Variable (für die Chi-Quadrat-Grafik)
chi_row <- function(v) {
  tab <- table(ad[[v]], ad$diabetes)
  ct  <- suppressWarnings(chisq.test(tab))
  data.frame(variable = v,
             X_squared = round(ct$statistic, 2),
             df        = ct$parameter,
             p_value   = signif(ct$p.value, 3),
             row.names = NULL)
}
chi_tab <- do.call(rbind, lapply(cat_vars, chi_row))
chi_tab$signif <- ifelse(chi_tab$p_value < 0.05, "*", "")

# Odds Ratios mit 95%-CI (für den Forest Plot)
or_tab <- tidy(m_full, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(term,
            OR      = round(estimate, 3),
            CI_low  = round(conf.low, 3),
            CI_high = round(conf.high, 3),
            p_value = signif(p.value, 3),
            signif  = ifelse(p.value < 0.05, "*", ""))

# Hierarchische Regression: Block 1 vs. Block 2 (für AIC/Pseudo-R²-Grafik)
m2 <- m_full
mcfadden <- function(m) 1 - as.numeric(logLik(m) / logLik(update(m, . ~ 1)))
lrt <- anova(m1, m2, test = "LRT")

# ROC (Block 2) + Konfusionsmatrix am Youden-optimalen Schwellenwert
roc2 <- roc(md$diabetes, md$p2, levels = c("No", "Yes"), direction = "<", quiet = TRUE)

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
r_yj <- conf_metrics(md$p2, md$diabetes, thr_youden)

# LASSO-Regression (für Lambda-Plot und Koeffizientenplot)
lasso_vars <- c("age", "gender", "origin",
                "bmi", "smoking_status", "sleep_duration",
                "moderate_min_week", "vigorous_min_week", "sugar_intake",
                "calorie_intake", "alcohol_week",
                "education_level", "hypertension", "income")
lasso_fml <- as.formula(paste("diabetes ~", paste(lasso_vars, collapse = " + ")))
md_lasso  <- ad[complete.cases(ad[, c("diabetes", lasso_vars)]), ]
X <- model.matrix(lasso_fml, data = md_lasso)[, -1]
y <- as.integer(md_lasso$diabetes == "Yes")
set.seed(42)
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 10)

lasso_coef <- coef(cv_lasso, s = "lambda.1se")
lasso_df <- data.frame(
  term        = rownames(lasso_coef),
  coefficient = round(as.numeric(lasso_coef), 4),
  row.names   = NULL
)
lasso_df$selected <- lasso_df$coefficient != 0

# Farb-/Theme-Helfer, die die weiter unten stehenden Grafiken erwarten
col_no             <- unname(farben["No"])
col_yes            <- unname(farben["Yes"])
col_sig            <- col_yes
col_nonsig         <- "grey70"
theme_presentation <- function() pres_theme

out_dir <- "Plots"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ================================================================
# Diabetes-Prävalenz (Donut-Chart)
# ================================================================
p_donut <- ad %>%
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

print(p_donut)
ggsave(file.path(out_dir, "Diabetes Prävalenz.png"), p_donut,
       width = 7, height = 7, dpi = 300, bg = "white")

# ================================================================
# Alters- und Geschlechts-Pyramide
# ================================================================
p_pyramide <- ad %>%
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

print(p_pyramide)
ggsave(file.path(out_dir, "Alters_und_Geschlechtspyramide.png"), p_pyramide,
       width = 8, height = 6, dpi = 300, bg = "white")

# ================================================================
# Altersverteilung nach Diabetes-Status
# ================================================================
p_age_density <- ggplot(ad, aes(x = age, fill = diabetes, color = diabetes)) +
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

print(p_age_density)
ggsave(file.path(out_dir, "Altersverteilung_nach_Diabetes Status.png"), p_age_density,
       width = 8, height = 5.5, dpi = 300, bg = "white")

# ================================================================
# BMI-Vergleich nach Diabetes-Status (Boxplot + Jitter)
# ================================================================
p_bmi_box <- ggplot(ad, aes(x = diabetes, y = bmi, fill = diabetes)) +
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

print(p_bmi_box)
ggsave(file.path(out_dir, "BMI_nach_Diabetes Status.png"), p_bmi_box,
       width = 6.5, height = 6, dpi = 300, bg = "white")



# =============================================================================
# Grafik: Chi-Quadrat-Ergebnisse als Balkendiagramm 
# =============================================================================
chi_plot_df <- chi_tab %>%
  mutate(
    sig_label = ifelse(signif == "*", "signifikant (p < 0.05)", "nicht signifikant"),
    p_label   = ifelse(p_value < 0.001,
                       paste0("p = ", format(p_value, scientific = TRUE, digits = 2)),
                       paste0("p = ", round(p_value, 3)))
  )

p_chi <- ggplot(chi_plot_df, aes(x = reorder(variable, X_squared), y = X_squared,
                                 fill = sig_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = p_label), hjust = -0.1, size = 3.8, fontface = "italic") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("signifikant (p < 0.05)" = col_sig,
                               "nicht signifikant"      = col_nonsig)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(
    title    = "Chi-Quadrat-Test: Welche kategorialen Faktoren hängen mit Diabetes zusammen?",
    subtitle = "Chi-Quadrat-Statistik je Variable (nur bei gleichen Freiheitsgraden direkt vergleichbar)",
    x        = NULL, y = "Chi-Quadrat (X\u00b2)", fill = NULL
  ) +
  theme_presentation() +
  theme(panel.grid.major.y = element_blank())

print(p_chi)
ggsave("plot_02_chiquadrat.png", p_chi, width = 9, height = 5.5, dpi = 300, bg = "white")

# =============================================================================
# Grafik: Odds Ratios als Forest Plot
# =============================================================================
or_plot_df <- or_tab %>%
  mutate(
    risk_type = ifelse(OR > 1, "Risikofaktor (OR > 1)", "Schutzfaktor (OR < 1)"),
    term      = factor(term, levels = term[order(OR)])
  )

p_or <- ggplot(or_plot_df, aes(x = OR, y = term, color = risk_type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, linewidth = 1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Risikofaktor (OR > 1)" = col_yes,
                                "Schutzfaktor (OR < 1)" = col_no)) +
  scale_x_log10() +
  labs(
    title    = "Logistische Regression: Welche Faktoren erhöhen oder senken das Diabetes-Risiko?",
    subtitle = "Odds Ratios mit 95%-Konfidenzintervall \u2014 adjustiert für alle anderen Modellvariablen",
    x        = "Odds Ratio (log-Skala)", y = NULL, color = NULL,
    caption  = "Gestrichelte Linie = kein Effekt (OR = 1). Rechts davon = erhöhtes Risiko, links davon = schützend."
  ) +
  theme_presentation() +
  theme(panel.grid.major.y = element_blank())

print(p_or)
ggsave("plot_03_odds_ratios.png", p_or, width = 9, height = 6.5, dpi = 300, bg = "white")

# =============================================================================
# Grafik: AIC & Pseudo-R² nebeneinander 
# =============================================================================
plot_comp <- data.frame(
  model    = c("Block 1\n(Basis)", "Block 2\n(+ Lifestyle)"),
  AIC      = c(AIC(m1), AIC(m2)),
  pseudoR2 = c(mcfadden(m1), mcfadden(m2))
) %>%
  mutate(model = factor(model, levels = model))

p_aic <- ggplot(plot_comp, aes(x = model, y = AIC, fill = model)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.6, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c(col_no, col_yes)) +
  coord_cartesian(ylim = c(min(plot_comp$AIC) * 0.97, max(plot_comp$AIC) * 1.03)) +
  labs(title = "AIC", subtitle = "Niedriger = besser", x = NULL, y = "AIC") +
  theme_presentation() + theme(legend.position = "none")

p_r2 <- ggplot(plot_comp, aes(x = model, y = pseudoR2, fill = model)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = percent(pseudoR2, accuracy = 0.1)),
            vjust = -0.6, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c(col_no, col_yes)) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Pseudo-R\u00b2 (McFadden)", subtitle = "Höher = besser", x = NULL, y = "Pseudo-R\u00b2") +
  theme_presentation() + theme(legend.position = "none")

if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(patchwork)

p_value_lrt <- signif(lrt$`Pr(>Chi)`[2], 3)
p_hierarch <- (p_aic + p_r2) +
  plot_annotation(
    title    = "Hierarchische Regression: Bringt der Lifestyle-Block einen Mehrwert?",
    subtitle = sprintf("Likelihood-Ratio-Test: Chi\u00b2 = %.1f, df = %d, p = %s",
                       lrt$Deviance[2], lrt$Df[2], format(p_value_lrt, scientific = TRUE)),
    theme    = theme(plot.title    = element_text(face = "bold", size = 18),
                     plot.subtitle = element_text(color = "grey40", size = 11))
  )

print(p_hierarch)
ggsave("plot_04_hierarchische_regression.png", p_hierarch,
       width = 10, height = 5.5, dpi = 300, bg = "white")

# =============================================================================
# Grafik: t-Test-Ergebnisse als facettierte Balkendiagramme 
# =============================================================================
tt_long <- tt_tab %>%
  mutate(
    sig_label = ifelse(signif == "*", "(*)", "(n.s.)"),
    facet_lab = paste(variable, sig_label)
  ) %>%
  select(variable, facet_lab, mean_No, mean_Yes) %>%
  pivot_longer(cols = c(mean_No, mean_Yes),
               names_to  = "Gruppe",
               values_to = "Mittelwert") %>%
  mutate(Gruppe = recode(Gruppe, mean_No = "No", mean_Yes = "Yes"),
         Gruppe = factor(Gruppe, levels = c("No", "Yes")))

# Reihenfolge der Panels wie im Originalvektor cont_vars beibehalten
tt_long$facet_lab <- factor(
  tt_long$facet_lab,
  levels = unique(tt_long$facet_lab[match(cont_vars, tt_long$variable)])
)

p_ttest <- ggplot(tt_long, aes(x = Gruppe, y = Mittelwert, fill = Gruppe)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = Mittelwert),
            vjust = -0.5, fontface = "bold", size = 4) +
  facet_wrap(~ facet_lab, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("No" = col_no, "Yes" = col_yes)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Vergleich der Mittelwerte: No vs. Yes",
    subtitle = "(*) = statistisch signifikanter Unterschied (p < 0.05), n.s. = nicht signifikant",
    x        = NULL, y = "Mittelwert", fill = "Gruppe"
  ) +
  theme_presentation() +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text          = element_text(face = "bold", size = 11),
    strip.background    = element_rect(fill = "grey95", color = NA)
  )

print(p_ttest)
ggsave("plot_01_ttest.png", p_ttest, width = 12, height = 7, dpi = 300, bg = "white")



# =============================================================================
# ROC-Kurve Block 1 vs. Block 2 │ bereits vorhanden │
# ROC-Kurve alle drei Modelle  │ bereits vorhanden │
# =============================================================================
# 1. Konfusionsmatrix-Heatmap (Youden-Schwellenwert)
# =============================================================================

cm_df <- as.data.frame(r_yj$cm)

# Typ jeder Zelle bestimmen (TP, TN, FP, FN)
cm_df$type <- case_when(
  cm_df$Predicted == "Yes" & cm_df$Actual == "Yes" ~ "True Positive (TP)",
  cm_df$Predicted == "No"  & cm_df$Actual == "No"  ~ "True Negative (TN)",
  cm_df$Predicted == "Yes" & cm_df$Actual == "No"  ~ "False Positive (FP)",
  cm_df$Predicted == "No"  & cm_df$Actual == "Yes" ~ "False Negative (FN)"
)

# Achsen so ordnen, dass Yes oben/rechts steht (Standard-Darstellung)
cm_df$Predicted <- factor(cm_df$Predicted, levels = c("Yes", "No"))
cm_df$Actual    <- factor(cm_df$Actual,    levels = c("No",  "Yes"))

p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = type)) +
  geom_tile(colour = "white", linewidth = 2) +
  geom_text(aes(label = paste0(type, "\n\n", Freq)),
            size = 5.5, fontface = "bold", colour = "white") +
  scale_fill_manual(values = c(
    "True Positive (TP)"  = "#2166ac",
    "True Negative (TN)"  = "#4dac26",
    "False Positive (FP)" = "#d01c8b",
    "False Negative (FN)" = "#f1a340"
  )) +
  labs(
    title    = "Konfusionsmatrix (Youden-Schwellenwert)",
    subtitle = paste0("Threshold = ", round(thr_youden, 3),
                      " | Modell: Block 2 (+ Lifestyle)"),
    x = "Tatsächliche Diagnose",
    y = "Vorhergesagte Diagnose"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "none",
    panel.grid         = element_blank(),
    plot.title         = element_text(face = "bold"),
    axis.text          = element_text(size = 13, face = "bold")
  )

print(p_cm)
ggsave(file.path(out_dir, "01_konfusionsmatrix.png"), p_cm,
       width = 6, height = 5.5, dpi = 300)

# =============================================================================
# 2. Lambda-Plot: CV-Fehler über Lambda (Kreuzvalidierung des LASSO)
# =============================================================================

lambda_df <- data.frame(
  log_lambda = log(cv_lasso$lambda),
  cvm        = cv_lasso$cvm,    # mittlerer CV-Fehler
  cvup       = cv_lasso$cvup,   # oberes 1-SE-Band
  cvlo       = cv_lasso$cvlo    # unteres 1-SE-Band
)

p_lambda <- ggplot(lambda_df, aes(x = log_lambda, y = cvm)) +
  geom_ribbon(aes(ymin = cvlo, ymax = cvup), alpha = 0.15, fill = "#2166ac") +
  geom_line(colour = "#2166ac", linewidth = 1) +
  geom_point(colour = "#2166ac", size = 1.5) +
  geom_vline(xintercept = log(cv_lasso$lambda.min),
             linetype = "dashed", colour = "#d73027", linewidth = 0.9) +
  geom_vline(xintercept = log(cv_lasso$lambda.1se),
             linetype = "dashed", colour = "#fc8d59", linewidth = 0.9) +
  annotate("text",
           x     = log(cv_lasso$lambda.min),
           y     = max(lambda_df$cvup),
           label = "lambda.min",
           colour = "#d73027", hjust = 1.1, size = 4.5) +
  annotate("text",
           x     = log(cv_lasso$lambda.1se),
           y     = max(lambda_df$cvup),
           label = "lambda.1se",
           colour = "#fc8d59", hjust = -0.1, size = 4.5) +
  labs(
    title    = "LASSO: Kreuzvalidierung zur Wahl von Lambda",
    subtitle = "Blau = mittlerer CV-Fehler mit 1-SE-Band",
    x        = "log(Lambda)  ←  weniger Strafe  |  mehr Strafe  →",
    y        = "Mittlerer CV-Fehler"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(p_lambda)
ggsave(file.path(out_dir, "02_lambda_plot.png"), p_lambda,
       width = 8, height = 5, dpi = 300)

# =============================================================================
# 3. LASSO-Koeffizientenplot
# =============================================================================

coef_df <- lasso_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    status = case_when(
      coefficient > 0 ~ "Risikoerhöhend",
      coefficient < 0 ~ "Risikosenkend",
      TRUE            ~ "Nicht selektiert (= 0)"
    ),
    term = reorder(term, coefficient)
  )

p_coef <- ggplot(coef_df, aes(x = coefficient, y = term, fill = status)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.8, colour = "grey30") +
  scale_fill_manual(values = c(
    "Risikoerhöhend"        = "#d73027",
    "Risikosenkend"         = "#4575b4",
    "Nicht selektiert (= 0)" = "grey80"
  )) +
  labs(
    title    = "LASSO-Koeffizienten (lambda.1se)",
    subtitle = "Grau = auf null gesetzt → Variable nicht selektiert",
    x        = "Koeffizient",
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(p_coef)
ggsave(file.path(out_dir, "03_lasso_koeffizienten.png"), p_coef,
       width = 8, height = 7, dpi = 300)

cat("\nAlle 3 Grafiken gespeichert in:", normalizePath(out_dir, mustWork = FALSE), "\n")


