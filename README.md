# DATASCI 100 — Lab Materials
### Introduction to Statistical Inference
**Department of Data and Decision Sciences · Emory University**

---

This repository contains lab materials for **DATASCI 100: Introduction to Statistical Inference** at Emory University. Labs are designed to complement lecture content with hands-on data analysis in R, emphasizing the `tidyverse` ecosystem and conceptual understanding alongside computation.

---

## 📁 Repository Structure

```
DATASCI-100-Lab/
├── Lab-01/          # Introduction to R and the tidyverse
├── Lab-02/          # Exploratory data analysis
├── Lab-03/          # Probability fundamentals
├── Lab-04/          # Sampling distributions
├── Lab-05/          # Confidence intervals
├── Lab-06/          # Hypothesis testing (proportions)
├── Lab-07/          # Chi-square tests
├── Lab-08/          # Inference for means (one-sample t-test)
├── Lab-09/          # Paired and two-sample t-tests
├── Lab-10/          # ANOVA
├── Lab-Exam/        # Lab exam using the gapminder dataset
└── data/            # Datasets used across labs
```

> **Note:** Each lab folder contains an `.Rmd` source file, a compiled `.pdf` or `.html` output, and any supplementary datasets.

---

## 🧪 Topics Covered

| Lab | Topic |
|-----|-------|
| 1–2 | R fundamentals, `ggplot2`, `dplyr`, EDA |
| 3–4 | Probability, sampling distributions, Central Limit Theorem |
| 5–6 | Confidence intervals, hypothesis tests for proportions |
| 7   | Chi-square goodness-of-fit and tests of independence |
| 8   | One-sample and one-proportion *t*-tests |
| 9   | Paired *t*-tests and independent two-sample *t*-tests |
| 10  | One-way ANOVA and post-hoc testing |
| Exam | Integrative analysis using the `gapminder` dataset |

---

## 🛠️ Prerequisites

- **R** (≥ 4.0) and **RStudio**
- The following R packages:

```r
install.packages("tidyverse")
```

---

## 🚀 Getting Started

Clone the repository and open any `.Rmd` file in RStudio:

```bash
git clone https://github.com/JustinEloriaga/DATASCI-100-Lab.git
```

Labs are self-contained — each `.Rmd` includes instructions, code scaffolding, and discussion questions. Students fill in the blanks and knit to produce their submission.

---

## 📝 Pedagogical Approach

These labs follow a **`tidyverse`-first** philosophy:
- Data wrangling with `dplyr` and `tidyr`
- Visualization with `ggplot2`
- Simulation-based inference with `infer` where appropriate
- Interpretation-first: code supports statistical reasoning, not the other way around

---

## 👤 Instructor

**Justin Eloriaga**  
Instructor, Department of Data and Decision Sciences  
PhD Candidate in Economics, Emory University  
[justineloriaga.com](https://justineloriaga.com) · [@EloriagaJustin](https://twitter.com/EloriagaJustin)

---

## 📄 License

These materials are shared for educational use. If you adapt them for your own course, attribution is appreciated.
