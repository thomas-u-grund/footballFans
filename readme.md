Replication Package: "Negative emotions concentrate while positive emotions diffuse in football fan rivalries"

## Citation
"Negative emotions concentrate while positive emotions diffuse in football fan rivalries." Under review. [Author information withheld for peer review]\

## Abstract
Background: Research on negativity bias demonstrates that negative information receives disproportionate cognitive attention at the individual level, but how this psychological mechanism shapes collective emotional patterns remains unclear. We investigate whether communities systematically concentrate negative emotions on fewer targets while distributing positive emotions more broadly.

Methods: We analyze 36,223 German football fans' reports of loved and hated clubs, measuring attention concentration using the Herfindahl-Hirschman Index (HHI). We distinguish between outgoing concentration (how communities allocate their own attention) and incoming concentration (how attention from others focuses on particular targets), testing whether concentration differs between positive and negative affect.

Results: Negative attention was significantly more concentrated than positive attention for both outgoing (M=0.36 vs M=0.23, p$<$0.001) and incoming ties (M=0.41 vs M=0.33, p=0.011). Love and hate concentration are weakly correlated (r=0.26--0.32), indicating independent processes. Structural factors predict incoming but not outgoing concentration (Pseudo $R^2$=0.14--0.35 vs $R^2$=0.47--0.49), revealing asymmetric mechanisms of attention allocation versus attraction.

Conclusions: Communities face cognitive constraints that produce systematic asymmetries in how they distribute emotional attention. These findings highlight how cognitive constraints shape collective emotional structures, with implications for political polarization, consumer behavior, and conflict dynamics.

## Requirements
- R version 4.0.0 or higher\
- Required packages: tidyverse, readxl, igraph, betareg, broom, effsize, ineq, patchwork, fuzzyjoin, ggrepel\

## Instructions
1. Download all files maintaining the folder structure\
2. Set working directory to package root\
3. Ensure all required packages are installed\
4. Run: `source("code/replication_script.R")`\
5. All outputs will be saved to figures/ and results/ folders\

## File Structure

### Data Files (`data/` folder)
- **`hate_network.csv`** - Aggregated hate relationship matrix between football clubs. Rows represent source clubs, columns represent target clubs, values indicate weighted nomination counts from fan survey\
- **`love_network.csv`** - Aggregated love relationship matrix between football clubs. Same structure as hate network but for positive relationships\
- **`clubs_master.csv`** - Club attributes including geographic location, league tier, stadium capacity, membership numbers, founding year, and regional characteristics\
- **`market_values.xlsx`** - Player market valuations from Transfermarkt for 2018 season, used as proxy for club competitive status\

### Code (`code/` folder)
- **`replication_script.R`** - Complete replication script that reproduces all analyses, figures, and tables from the paper. Includes data loading, network construction, HHI calculations, statistical tests, beta regression models, and robustness checks\

### Output Folders (created by script)
- **`figures/`** - Contains all figures from the paper:\
 - `figure_1_hhi_boxplots.png` - Main concentration asymmetry visualization\
 - `figure_2_correlations.png` - Love vs hate concentration correlations\
- **`results/`** - Contains analysis outputs:\
 - `hhi_concentration_results.csv` - HHI measures for all clubs\
 - `analysis_dataset.csv` - Complete merged dataset used in regressions\
 - `beta_models.RData` - Saved beta regression model objects\
 - `summary_statistics.rds` - Key statistical results and effect sizes\

### Documentation
- **`README.md`** - This file with package overview and instructions\

## Key Variables
- **HHI measures**: Normalized Herfindahl-Hirschman Index values (0-1 scale) measuring attention concentration\
- **Outgoing ties**: How each club's fanbase distributes its own attention\
- **Incoming ties**: How attention from all other clubs focuses on each target club\
- **Club attributes**: League tier, market value, stadium capacity, geographic and political variables\

## Data Sources
- Fan survey data: Der Spiegel's Fanatlas 2018 (36,223 respondents)\
- Market valuations: Transfermarkt.com\
- Regional economic data: German Federal Statistical Office\
- Electoral data: German Federal Returning Officer\

## Reproducibility Notes
- Package versions recorded in session info\
- Runtime may vary based on system specifications\
- All paths use relative references for cross-platform compatibility\

## License
Creative Commons Attribution 4.0\

## Version History
- v1.0: Initial replication package for peer review
