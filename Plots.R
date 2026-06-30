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
# Altersverteilung nach Diabetes-Status
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
# BMI-Vergleich nach Diabetes-Status (Boxplot + Jitter)
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
