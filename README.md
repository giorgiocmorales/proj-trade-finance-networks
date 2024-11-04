# Economic Interconnectedness

This repository contains the scripts, data, and outputs from an ongiong project to perform large scale network analysis on bilateral trade flows and international financial position and their economic significance. 

Using data from the Inaternational Monetary Fund (IMF)'s [Diectory of Trade Satatistics (DOTS)](https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85), [Coordinated Direct Investment Survey (CDIS)](https://data.imf.org/?sk=40313609-f037-48c1-84b1-e1f1ce54d6d5) and [Coordinated Protfolio Investment Survey (CPIS)](https://data.imf.org/?sk=b981b4e3-4e58-467e-9b90-9de0c3367363), as well as the Bank of International Settlements (BIS)'s [Location Banking Statistics (LBS)](https://data.bis.org/topics/LBS) we construct a composite index of systemic interconnectedness using 16 centrality based metrics, weigthed after filtering through a Principal Component Analysis (PAC). Afterwards we perform a cross-country time series panel Vector Autoregression (VAR) to evaluate for links of tarde and financial interconnectedness on macroeconomic performance.

The model's acripts, it's analytical pipeline, and outputs are all done using mostly [R](https://www.r-project.org/), using R Markdown for drafting a reproducible research document and Overleaf for formatting a submiteable document.

## Version history

### 1.0

##### XX-XXX-202X

Intitial release
