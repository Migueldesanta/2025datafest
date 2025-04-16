# 🏢 City Leasing Forecast & Market Explorer

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange)
![Year: 2025](https://img.shields.io/badge/year-2025-lightgrey)
![Status: new app](https://img.shields.io/badge/lifecycle-newapp-brightgreen)

An interactive Shiny dashboard built to evaluate U.S. city competitiveness and forecast leasing activity for 2025 Q1 using post-pandemic real estate and labor market data.

---

## 📌 Project Objective

> To identify competitive U.S. office markets and support post-COVID investment decisions using trend scores and machine learning.

---

## 🏆 Competition

This project was developed as part of the 2025 ASA DataFest at Penn State University, a weekend-long data analysis competition organized by the American Statistical Association. Participants worked intensively to analyze a complex dataset and present their findings to a panel of judges.

---

## 📁 App Structure

The dashboard includes four main pages:

1. **Home** – Project intro, background, and team  
2. **Method** – Data pipeline, trend scoring, and model workflow  
3. **Market Overview** – Heatmap and 3D trend score visualizations  
4. **Trend Forecast** – 2025 Q1 leasing forecast and growth rate comparison

---

## 🧠 Methodology Summary

- **Data Range:** 2021 Q4 – 2024 Q4  
- **Trend Score:** Composite Z-score of 5 indicators:  
  leased SF, rent, vacancy, occupancy, unemployment  
- **Forecast Model:** XGBoost (target = log-leased SF for 2025 Q1)  
- **Evaluation:** RMSE, \(R^2\), growth rate from 2024 Q4 to 2025 Q1  

---

## 📸 Screenshots

![Heatmap](screenshots/heatmap.png)  
*Market Trend Heatmap*

![3D Plot](screenshots/3d_trend_score.png)  
*3D Trend Score Distribution*

![Forecast](screenshots/forecast.png)  
*Leasing Forecast for 2025 Q1*

---

## 👥 Team Members

- @MichaelYun  
- @RunyiZhang  
- @JingchunZhang  
- @ZhaoyuHou  

---

## 🔧 Setup Instructions

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/city-leasing-forecast.git

