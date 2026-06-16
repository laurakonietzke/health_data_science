# Health Data Science — Lifestyle Factors & Diabetes (NHANES)

Analysis of how modifiable lifestyle factors relate to diabetes, using NHANES
(cycle "L") data. The outcome is binary (`diabetes`: yes/no).

## What's in here

```
01_data_cleaning.R          Raw .xpt -> corrected, analysis-ready dataset (run once)
02_analysis.qmd             Analysis report (renders to HTML)
02_analysis_preview.md      Knitted preview of the report
health_data_science.Rproj   RStudio project file
data/
  Rohdaten/                 Raw NHANES modules (.xpt)
  Datensatz/                Cleaned datasets (output of 01_data_cleaning.R)
```

The analysis (in `02_analysis.qmd`) covers adults (age ≥ 18) and proceeds from simple to
complex:

1. Descriptive statistics ("Table 1") by diabetes status
2. Univariate screening — Chi² tests (categorical) and t-tests (continuous)
3. Multiple logistic regression with odds ratios (95% CI)
4. Hierarchical regression — non-modifiable block vs. + lifestyle block (LRT, AIC, pseudo-R²)
5. ROC curves & AUC, with the DeLong test
6. Confusion matrix at the 0.5 and Youden-optimal thresholds

## How to start

1. **Install R packages** (once per machine):

   ```r
   install.packages(c("haven", "dplyr", "janitor", "tidyr",
                      "pROC", "broom", "ggplot2"))
   ```

   To render the report you also need RStudio (it bundles Quarto + Pandoc).

2. **Clean the data** — only needed if the raw files change or `data/Datensatz/` is
   missing (the cleaned files are already included):

   ```sh
   Rscript 01_data_cleaning.R
   ```

   Reads `data/Rohdaten/*.xpt` and writes the cleaned datasets to `data/Datensatz/`.

3. **Render the report** — open `health_data_science.Rproj` in RStudio, open
   `02_analysis.qmd`, and click **Render** (Cmd/Ctrl+Shift+K). It produces
   `02_analysis.html`.

   From the console instead:

   ```r
   quarto::quarto_render("02_analysis.qmd")
   ```
