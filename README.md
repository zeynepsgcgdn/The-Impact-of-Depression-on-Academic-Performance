# The-Impact-of-Depression-on-Academic-Performance
# Student Mental Health and Academic Performance Analysis

## 📌 Overview
This repository contains an R-based exploratory data analysis (EDA) and statistical testing project focused on understanding the factors affecting student mental health. The analysis investigates the relationships between academic pressure, daily habits (sleep, diet, study hours), financial stress, and depression.

## 📊 Dataset Features
The dataset includes various demographic, academic, and lifestyle variables:
* **Demographics:** Age (`yas`), Gender (`cinsiyet`), City (`sehir`)
* **Academic Factors:** Degree (`derece`), GPA (`ortalama_not`), Daily Study Hours (`gunluk_calisma_saati`), Academic Pressure (`akademik_baski`), Student Satisfaction (`ogrenci_tatmini`)
* **Lifestyle & Health:** Sleep Duration (`uyku_durumu`), Dietary Habits (`diyet_aliskanlik`), Sedative Use (`sakinlestirici_ilac`)
* **Psychological Factors:** Financial Stress (`finansal_stres`), Family History of Mental Illness (`ailede_mental_hastalik_gecmisi`), Depression Status (`depresyon_durumu`)

## 🛠️ Technologies & Libraries Used
The analysis is entirely conducted in **R**, utilizing the following key libraries:
* **Data Manipulation:** `dplyr`, `tidyverse`, `reshape2`
* **Data Visualization:** `ggplot2`, `GGally`
* **Descriptive Statistics:** `skimr`, `psych`, `moments`
* **Data Import:** `readxl`

## 🔬 Statistical Methodology
This project heavily utilizes non-parametric statistical tests to evaluate hypotheses, given the categorical and ordinal nature of many variables. The statistical methods include:
* **Mann-Whitney U Test (Wilcoxon Rank-Sum):** Used for comparing differences between two independent groups (e.g., academic pressure vs. depression status, financial stress vs. depression).
* **Kruskal-Wallis Test:** Used for comparing more than two independent groups (e.g., academic pressure across different degree levels and sleep durations).
* **Spearman Correlation:** Applied to measure the strength and direction of associations between ordinal/continuous variables (e.g., academic pressure vs. student satisfaction, academic pressure vs. GPA).
* **Chi-Square Test of Independence:** Used to determine significant relationships between categorical variables (e.g., study hour groups vs. depression, dietary habits vs. depression).
* **Fisher's Exact Test:** Utilized for categorical data when sample sizes are small (e.g., family mental health history vs. depression).

## 💡 Key Questions Addressed
The script explores 15 specific research questions, including but not limited to:
1.  Is there a significant difference in academic pressure between students with and without depression?
2.  How does academic pressure correlate with overall student satisfaction and GPA?
3.  Does the length of daily study hours impact the likelihood of experiencing depression?
4.  Which gender experiences higher levels of academic pressure?
5.  What are the shared characteristics of resilient students (those with high academic pressure but no depression)?
6.  How do dietary habits and sleep duration affect financial stress and sedative use?

## 🚀 How to Run the Project
1.  Clone this repository to your local machine.
2.  Ensure you have R and RStudio installed.
3.  Install the required packages by running:
    ```R
    install.packages(c("readxl", "skimr", "tidyverse", "dplyr", "ggplot2", "GGally", "reshape2", "psych", "moments", "e1071"))
    ```
4.  Open the `ist347odev_41.R` script in RStudio.
5.  Run the script. A file prompt will appear (`file.choose()`); select the dataset (`ist347_odev_41.xlsx`) to load the data into the environment.

## 📈 Visualizations
The script generates numerous insightful visualizations, including:
* Bar charts for categorical distributions.
* Boxplots and violin plots for group comparisons.
* Scatter plots with regression lines for correlation analysis.
* Density plots for age distributions.
* A comprehensive correlation matrix heatmap.
