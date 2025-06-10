# maihda-sheffield-presentation
R code for MAIHDA analysis presented at Sheffield

# MAIHDA Analysis - Sheffield Presentation

This repository contains materials for my presentation on Multilevel Analysis of Individual Heterogeneity and Discriminatory Accuracy (MAIHDA) at the University of Sheffield.

## Contents

### 📊 Interactive Presentation
- `MAIHDA_Presentation.html` - Interactive slides demonstrating three MAIHDA applications
- View online: [https://YOUR-USERNAME.github.io/maihda-sheffield-presentation/MAIHDA_Presentation.html](https://YOUR-USERNAME.github.io/maihda-sheffield-presentation/MAIHDA_Presentation.html)

### 📈 R Analysis
- `MAIHDA_Full_Analysis.Rmd` - Complete R Markdown document with synthetic data analysis
- `MAIHDA_Full_Analysis.html` - Knitted HTML output with results

### 🎯 Three Applications Demonstrated

1. **Spatial MAIHDA** - School Segregation Analysis
   - 48 intersectional strata: Ethnicity × SES × School Type
   - Monte Carlo spatial clustering analysis
   - Key finding: Pakistani × Low SES × Faith School segregation index = 142.5 [95% CI: 135.2-149.8]

2. **Longitudinal MAIHDA** - Teacher Retention
   - 200 intersectional strata: Ethnicity × Gender × ITT × Region
   - Survival analysis with time-varying hazards
   - Key finding: Black male non-ITT teachers in London face Year 2 crisis (HR = 2.8)

3. **Policy Evaluation MAIHDA** - Transport Access
   - 60 intersectional strata: Ethnicity × Income × Car Access × Zone
   - Policy simulation with differential effects
   - Key finding: Free transport reduces inequality by 42% [38%-46%]

## Navigation

The presentation includes:
- Interactive navigation (arrow keys or buttons)
- 10 slides optimized for 10-minute presentation
- Visual demonstrations of Monte Carlo methods
- Responsive design for different screen sizes

## Author

**Dr Yiyang Gao**  
Evidence Centre for Education  
Durham University  
Contact: [yiyang.gao@durham.ac.uk]

## Usage

1. **View the presentation**: Open `MAIHDA_Presentation.html` in any modern web browser
2. **Navigate**: Use arrow keys or on-screen buttons
3. **Run the analysis**: Open `MAIHDA_Full_Analysis.Rmd` in RStudio

## Requirements

For running the R analysis:
```r
install.packages(c("tidyverse", "knitr", "kableExtra", "survival", "survminer"))
