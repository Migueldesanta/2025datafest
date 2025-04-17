# 🏢 City Leasing Forecast & Market Explorer

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange)
![Year: 2025](https://img.shields.io/badge/year-2025-lightgrey)
![Status: new app](https://img.shields.io/badge/lifecycle-newapp-brightgreen)

An interactive Shiny dashboard for analyzing U.S. office leasing trends and forecasting 2025 Q1 leasing activity using machine learning.
This project was developed as part of the **2025 ASA DataFest at Penn State University**, a national data science competition organized by the American Statistical Association.

---

## 📌 Project Objective

To identify the most competitive and investment-ready U.S. cities in the post-COVID leasing market using trend scoring and forecasting models.

---

## 📁 Structure

The dashboard includes four interactive pages:

1. **Home** – Project overview and team  
2. **Method** – Data, feature engineering, and model methodology  
3. **Market Overview** – Heatmap and 3D trend score visualizations  
4. **Trend Forecast** – Leasing growth forecast for 2025 Q1

📁 Root Directory
│
├── 📂 docs/                # Project documentation and screenshots
│   └── page.png           # Page-level screenshots
│
├── 📂 R/                   # Custom R scripts used in data processing and modeling
│   ├── build_features.R    # Feature engineering pipeline
│   ├── train_model.R       # Model training and forecasting
│   └── trend_scoring.R     # Market trend score calculation
│
├── app.R                  # Main Shiny app UI + server code
├── DESCRIPTION            # App metadata (title, author, license, etc.)
├── CODEOWNERS             # GitHub file to auto-assign code reviewers
├── .gitignore             # Files and folders to ignore in version control
├── .lintr                 # Linting rules for R code style

---

## 🧠 Methods Summary

- **Data Period:** 2021 Q4 to 2024 Q4  
- **Trend Score:** Composite Z-score using rent, vacancy, occupancy, unemployment, and leased SF  
- **Model:** XGBoost regression on lagged features  
- **Target:** Forecast 2025 Q1 leased square footage

---

## 📸 Screenshots

### 🏠 Page 1 – Home
![Page 1](page1.png)

### 🛠️ Page 2 – Methodology
![Page 2](page2.png)

### 🌍 Page 3 – Market Overview
![Page 3](page3.png)

### 📈 Page 4 – Trend Forecast
![Page 4](page4.png)

---

## 👥 Team

- [MichaelYun](https://github.com/Migueldesanta)
- @RunyiZhang  
- [JingchunZhang](https://github.com/Jingchunz)
- @ZhaoyuHou

---

## 🔒 Data Availability

Due to data licensing agreements and privacy considerations, the original leasing dataset used in this project has been **removed from the repository**.

The app and code remain accessible for reference, but full functionality (e.g., trend scoring and forecasting) requires the proprietary source data, which is not publicly distributed.

If you are a competition judge, instructor, or reviewer seeking access, please contact the project team directly.

---

## 📄 License

This project is licensed under the **Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)** license.

You are **free to**:
- Share — copy and redistribute the material in any medium or format  
- Adapt — remix, transform, and build upon the material  

**Under the following terms**:
- **Attribution (BY):** You must give appropriate credit and indicate if changes were made.  
- **NonCommercial (NC):** You may not use the material for commercial purposes.  
- **ShareAlike (SA):** If you remix, transform, or build upon the material, you must distribute your contributions under the same license as the original.

🔗 License details: [https://creativecommons.org/licenses/by-nc-sa/4.0/](https://creativecommons.org/licenses/by-nc-sa/4.0/)

---


