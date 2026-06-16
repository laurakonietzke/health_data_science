# Das Paket 'tidyverse' stellt u.a. 'dplyr' bereit,
# mit dem wir komfortabel Spalten auswählen und entfernen können.
# Falls nicht installiert: install.packages("tidyverse")
library(tidyverse)

# NEU: 'spalten_entfernen' = Vektor mit den zu entfernenden Spaltennamen
# Bitte die Namen exakt so schreiben, wie sie in names(df_raw) erscheinen!
variablen_entfernen <- c(
  "Education Level",
  "Employment Status",
  "Cognitive Test Score",
  "Dietary Habits",        # Im Original als "Dietry Habbits" – exakten Namen mit names(df_raw) prüfen!
  "Marital Status",
  "Social Engagement Level",
  "Income Level"
)

# NEU: 'df_clean' (data frame clean) = bereinigter Datensatz ohne die 7 Variablen
# select() aus dplyr wählt Spalten aus; mit -all_of() werden Spalten ausgeschlossen.
# all_of() stellt sicher, dass ein Fehler ausgegeben wird,
# wenn ein Spaltenname nicht gefunden wird (sicherer als einfaches '-c(...)')
ds_bereinigt <- alzheimers_prediction_dataset %>%
  select(-all_of(variablen_entfernen))

# Fehlende Werte prüfen
colSums(is.na(ds_bereinigt))  # Anzahl NAs je Spalte

#'europaeische_laender' = Vektor mit allen europäischen Ländern,
# die im Datensatz vorhanden sind und behalten werden sollen.
europaeische_laender <- c(
  "Germany",
  "Italy",
  "Spain",
  "France",
  "Norway",
  "Sweden",
  "UK"          
)

# NEU: 'ds_europa' = Datensatz gefiltert auf europäische Länder
# filter() behält nur die Zeilen (Tupel), bei denen der Wert
# in der Spalte "Country" in unserem Vektor 'europaeische_laender' vorkommt.
# %in% prüft dabei für jede Zeile, ob der Wert im Vektor enthalten ist.

ds_europa <- ds_bereinigt %>%
  filter(Country %in% europaeische_laender)

# Wie viele Tupel sind nach dem Filtern noch vorhanden?
nrow(ds_europa)


# NEU: 'Patient ID' = neue Spalte zur eindeutigen Identifikation
# jedes Tupels (jeder Patient bekommt eine aufsteigende Nummer).
# nrow(ds_europa) gibt die Gesamtanzahl der Zeilen zurück → 25.985
# seq_len() erzeugt eine aufsteigende Zahlenfolge von 1 bis 25.985

ds_alzheimer <- ds_europa %>%
  mutate(`Patient ID` = seq_len(nrow(ds_europa)))%>%
  relocate(`Patient ID`)

# Datensatz als CSV-Datei speichern
# row.names = FALSE → keine zusätzliche Zeilennummerierung in der Datei
write.csv(ds_alzheimer,
          "C:/Users/vanes/Documents/Master/ds_alzheimer.csv",
          row.names = FALSE)

# Abschlussmeldung
cat("Datensatz erfolgreich gespeichert.\n",
    "Dateiname: ds_alzheimer.csv\n",
    "Anzahl Tupel:", nrow(ds_alzheimer), "\n",
    "Anzahl Variablen:", ncol(ds_alzheimer), "\n")

