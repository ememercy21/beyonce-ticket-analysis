# beyonce-ticket-analysis
A data-driven analysis of the secondary ticket market for Beyoncé's 2023 Renaissance World Tour. This project explores when and where fans can find the best deals on resale tickets.

## Overview

Concert ticket prices have surged in recent years, driven by dynamic pricing, industry monopolization, and inflated resale markets. This study uses real resale listing data to answer: **Is there an optimal time to buy Beyoncé tickets to minimize price while maximizing seat quality?**

## Key Findings

- **Timing matters more than market size.** The "middle window" (2–4 months before the show) offers the most stable chance for good deals.
- **Premium sections (B-Hive, Floor, VIP) have the highest deal probability**—up to 54% if purchased early.
- **Market size has minimal impact.** Big, medium, and small cities all show roughly 21–22% good deal rates.
- **Act fast on low-priced listings.**

## Data

- **Source:** [SeatData.io](https://seatdata.io) (third-party resale listings)
- **Scope:** 66,704 ticket listings across 25 North American cities
- **Variables:** Event date, city, section, listing price, days before event, and derived metrics (Deal Score, Seat Quality Score)

## Methods

- **Regression modeling:** Compared OLS, Ridge, and LASSO to estimate fair market value (LASSO selected)
- **Probability modeling:** Generalized Additive Model (GAM) to capture non-linear timing effects on deal probability
- **Languages/Tools:** R (tidyverse, mgcv), C (for KDE comparison)

## Repository Structure
- **data/**: # Raw and cleaned datasets
- **scripts/**: # R scripts for analysis and modeling
- **figures/**: # Plots and visualizations
- **presentation/**: # Slide deck (PDF)
- **paper/**: # Written report (PDF)

## Visualizations

Includes KDE plots, boxplots by section and market size, GAM smooth effect curves, and probability heatmaps by section and timing.

## Limitations

- Focused on one artist/tour; results may not generalize
- Only resale data (no primary sales or presales)
- "Good deal" threshold (top 20%) is subjective

## Future Work

- Expand to other artists and genres
- Incorporate external factors (competing events, artist popularity trends)
- Explore random forest or other ML models for improved prediction

## Author

**Mercy Eme**

## Acknowledgments

Data sourced from SeatData.io. Inspired by the chaos of trying to get Beyoncé tickets like a normal person.
