{\rtf1\ansi\ansicpg1252\cocoartf2822
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # Replication Package: "Concentrated Hate and Diffused Love"\
\
## Citation\
"Concentrated Hate and Diffused Love: Attention Distribution in Competitive Fields." Under review. [Author information withheld for peer review]\
\
## Abstract\
How do communities distribute emotional attention across competing targets? Using survey data from 36,223 German football (soccer) fans, this study measures the concentration of positive and negative affect across rivals and allies. We find that negative affect is systematically more concentrated than positive affect, both in outgoing and incoming ties. Club attributes predict incoming but not outgoing concentration, revealing different mechanisms for attention allocation versus attraction. These patterns demonstrate that communities organize emotional attention through distinct distributional logics, revealing distinct logics of conflict and coalition in competitive fields. Results are robust across alternative concentration measures (Gini coefficient, Shannon entropy, top-3 concentration) and fan subgroups.\
\
## Requirements\
- R version 4.0.0 or higher\
- Required packages: tidyverse, readxl, igraph, betareg, broom, effsize, ineq, patchwork, fuzzyjoin, ggrepel\
- Runtime: approximately 5-10 minutes\
\
## Instructions\
1. Download all files maintaining the folder structure\
2. Set working directory to package root\
3. Ensure all required packages are installed\
4. Run: `source("code/replication_script.R")`\
5. All outputs will be saved to figures/ and results/ folders\
\
## File Structure\
\
### Data Files (`data/` folder)\
- **`hate_network.csv`** - Aggregated hate relationship matrix between football clubs. Rows represent source clubs, columns represent target clubs, values indicate weighted nomination counts from fan survey\
- **`love_network.csv`** - Aggregated love relationship matrix between football clubs. Same structure as hate network but for positive relationships\
- **`clubs_master.csv`** - Club attributes including geographic location, league tier, stadium capacity, membership numbers, founding year, and regional characteristics\
- **`market_values.xlsx`** - Player market valuations from Transfermarkt for 2018 season, used as proxy for club competitive status\
\
### Code (`code/` folder)\
- **`replication_script.R`** - Complete replication script that reproduces all analyses, figures, and tables from the paper. Includes data loading, network construction, HHI calculations, statistical tests, beta regression models, and robustness checks\
\
### Output Folders (created by script)\
- **`figures/`** - Contains all figures from the paper:\
 - `figure_1_hhi_boxplots.png` - Main concentration asymmetry visualization\
 - `figure_2_correlations.png` - Love vs hate concentration correlations\
- **`results/`** - Contains analysis outputs:\
 - `hhi_concentration_results.csv` - HHI measures for all clubs\
 - `analysis_dataset.csv` - Complete merged dataset used in regressions\
 - `beta_models.RData` - Saved beta regression model objects\
 - `summary_statistics.rds` - Key statistical results and effect sizes\
\
### Documentation\
- **`README.md`** - This file with package overview and instructions\
\
## Key Variables\
- **HHI measures**: Normalized Herfindahl-Hirschman Index values (0-1 scale) measuring attention concentration\
- **Outgoing ties**: How each club's fanbase distributes its own attention\
- **Incoming ties**: How attention from all other clubs focuses on each target club\
- **Club attributes**: League tier, market value, stadium capacity, geographic and political variables\
\
## Data Sources\
- Fan survey data: Der Spiegel's Fanatlas 2018 (36,223 respondents)\
- Market valuations: Transfermarkt.com\
- Regional economic data: German Federal Statistical Office\
- Electoral data: German Federal Returning Officer\
\
## Reproducibility Notes\
- Package versions recorded in session info\
- Runtime may vary based on system specifications\
- All paths use relative references for cross-platform compatibility\
\
## License\
Creative Commons Attribution 4.0\
\
## Contact\
Available from corresponding author upon request during review process to maintain anonymity protocols. Will be made publicly available upon publication.\
\
## Version History\
- v1.0: Initial replication package for peer review}