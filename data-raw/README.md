# Data Provenance

This folder documents how raw data for *proj-trade-finance-networks* is obtained and prepared.

| Dataset | Source | Script | Output (in /data) | Notes |
|----------|---------|--------|------------------|--------|
| trade_finance_network_data | Custom API / internal dataset | scripts/CS_S2_Download_Data.R | trade_finance_network_data.csv | Created automatically via API query |
| country_set | Manual metadata | - | country_set.csv | Provides list of countries for analysis |

## Notes
- All scripts in this folder generate datasets saved into `/data/`.
- The `/data/` folder is local-only (ignored in Git).
- This file records provenance for reproducibility and transparency.

data-raw/README.md