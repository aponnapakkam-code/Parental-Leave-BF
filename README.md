# Parental-Leave-BF

This repository contains code for a retrospective cohort study assessing breastfeeding rate differences between **officer** and **enlisted** active duty mothers across **pre-** and **post-intervention** epochs. 
The intervention is the introduction of the 2022 parental leave policy. This is at a single military hospital. The analysis focuses on:

- Any breastfeeding at 2, 4, and 6 months  
- Exclusive breastfeeding at 2, 4, and 6 months  
- Differences between officer and enlisted families (risk differences)  
- Changes in these differences between epochs (rank × epoch interaction)  
- Adjusted risk differences using bias-reduced logistic regression and bootstrap CIs

All code is written in R, with a focus on transparency and reproducibility.

---

## Repository Structure

├── data/
│   ├── raw/
│      └── dat_raw.csv           # Original de-identified data (NOT in repo- contact for access)
├── R/
│   ├── 01_cleaning.R             # Data cleaning, factor coding, variable derivation
│   ├── 02_unadjusted_RD.R        # Unadjusted officer vs enlisted RDs + CIs + Fisher tests
│   ├── 03_interaction_check.R    # Unadjusted rank × epoch interaction tests
│   ├── 04_adjusted_RD_brglm.R    # Adjusted RDs via brglm + bootstrap CIs
| 
├── output/
│   ├── tables/
│   │   ├── unadjusted_epoch_RD.csv
│   │   ├── adjusted_RD_pooled.csv
│   │   └── adjusted_RD_4mo_by_epoch.csv
│   └── figures/
│       └── (optional plots)
├── README.md
