# Economic Interconnectedness

This repository contains the scripts, data, and outputs from an ongiong project to perform large scale network analysis on bilateral trade flows and international financial position and their economic significance. 

Using data from the Inaternational Monetary Fund (IMF)'s [Diectory of Trade Satatistics (DOTS)](https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85), [Coordinated Direct Investment Survey (CDIS)](https://data.imf.org/?sk=40313609-f037-48c1-84b1-e1f1ce54d6d5) and [Coordinated Protfolio Investment Survey (CPIS)](https://data.imf.org/?sk=b981b4e3-4e58-467e-9b90-9de0c3367363), as well as the Bank of International Settlements (BIS)'s [Location Banking Statistics (LBS)](https://data.bis.org/topics/LBS) we construct a composite index of systemic interconnectedness using 16 centrality based metrics, weigthed after filtering through a Principal Component Analysis (PAC). Afterwards we perform a cross-country time series panel Vector Autoregression (VAR) to evaluate for links of tarde and financial interconnectedness on macroeconomic performance.

All modelling and analysis is performed in [R](https://www.r-project.org/), with reproducible documentation written in R Markdown and final formatting handled in Overleaf.

---

## Analytical Pipeline

| Step | Script | Description |
|------|---------|-------------|
| S0 | `CS_S0_Packages.R` | Load and manage all required R packages. |
| S1 | `CS_S1_API_Codes_Countries.R` | Set API connections and define country-level metadata. |
| S2 | `CS_S2_Download_Data.R` | Download and assemble IMF/BIS datasets. |
| S3 | `CS_S3_Network_Analysis.R` | Construct and analyze trade and financial networks. |
| S4 | `CS_S4_Empirical_Model_Unit_Roots.R` | Pre-test series and estimate empirical model structure. |
| S5 | `CS_S5_P_VAR.R` | Estimate panel VAR models. |
| S6 | `CS_S6_Figures_Tables.R` | Generate final figures, tables, and output objects. |

Each script can be run sequentially to reproduce the paper’s analytical results.

---

## References


## Version history

### 1.0

##### XX-XXX-202X

Inititial release
