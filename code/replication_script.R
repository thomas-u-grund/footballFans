# ==============================================================================
# REPLICATION SCRIPT: "Negative emotions concentrate while positive emotions diffuse in football fan rivalries"
# ==============================================================================
#
# Thomas U. Grund, RWTH Aachen University
# tgrund@soziologie.rwth-aachen.de
#
# This script replicates all main analyses from the paper.
#
# Required data files (should be in 'data/' folder):
# - hate_network.csv (hate network)
# - love_network.csv (love network)
# - clubs_master.csv (club attributes)
# - market_values.xlsx (market value data)
#
# Output:
# - All figures from the paper (saved to 'figures/' folder)
# - All tables and statistics reported in the paper
# - Robustness check results
#
# ==============================================================================

rm(list=ls())

# Load required packages
library(tidyverse)     # Data manipulation and visualization
library(readxl)        # Reading Excel files
library(igraph)        # Network analysis
library(betareg)       # Beta regression
library(broom)         # Model tidying
library(effsize)       # Effect size calculations
library(ineq)          # Gini coefficient
library(patchwork)     # Combining plots
library(fuzzyjoin)     # Fuzzy string matching
library(ggrepel)       # Text labels in plots

# Create output directories
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================

cat("Loading and preparing data...\n")

hate_adj_raw<- read.csv("data/hate_network.csv")
love_adj_raw<- read.csv("data/love_network.csv")

club_names_hate <- hate_adj_raw[,1]
club_names_love <- love_adj_raw[,1]

hate_adj<-as.matrix(hate_adj_raw)[,-1]
love_adj<-as.matrix(love_adj_raw)[,-1]

# Convert to numeric (this is the key fix)
hate_adj <- apply(hate_adj, 2, as.numeric)
love_adj <- apply(love_adj, 2, as.numeric)

rownames(hate_adj)<-club_names_hate
rownames(love_adj)<-club_names_love
colnames(hate_adj)<-club_names_hate
colnames(love_adj)<-club_names_love

# ==============================================================================
# 2. NETWORK CONSTRUCTION
# ==============================================================================

cat("Constructing attention networks...\n")

# Normalize to proportions (row sums = 1)
love_weighted <- t(apply(love_adj, 1, function(x) {
  row_total <- sum(x, na.rm = TRUE)
  if(row_total > 0) x / row_total else x
}))

hate_weighted <- t(apply(hate_adj, 1, function(x) {
  row_total <- sum(x, na.rm = TRUE)
  if(row_total > 0) x / row_total else x
}))

# Preserve row and column names
rownames(love_weighted) <- rownames(love_adj)
colnames(love_weighted) <- colnames(love_adj)
rownames(hate_weighted) <- rownames(hate_adj)
colnames(hate_weighted) <- colnames(hate_adj)

# ==============================================================================
# 3. HHI CONCENTRATION MEASURES
# ==============================================================================

cat("Calculating HHI concentration measures...\n")

# Function to calculate normalized HHI (as used in the paper)
calculate_hhi <- function(x) {
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  if(n == 0) return(NA)

  # Standard HHI calculation
  standard_hhi <- sum((x/sum(x))^2)

  # Normalize: (HHI - 1/n) / (1 - 1/n)
  normalized_hhi <- (standard_hhi - 1/n) / (1 - 1/n)
  return(normalized_hhi)
}

# Calculate HHI for all clubs
# Outgoing attention (how each club distributes its attention)
love_out_hhi <- apply(love_weighted, 1, calculate_hhi)
hate_out_hhi <- apply(hate_weighted, 1, calculate_hhi)

# Incoming attention (how attention from others focuses on each club)
love_in_hhi <- apply(t(love_weighted), 1, calculate_hhi)
hate_in_hhi <- apply(t(hate_weighted), 1, calculate_hhi)

# Create results data frame
hhi_results <- data.frame(
  club = rownames(love_weighted),
  love_out_hhi = love_out_hhi,
  hate_out_hhi = hate_out_hhi,
  love_in_hhi = love_in_hhi[rownames(love_weighted)],
  hate_in_hhi = hate_in_hhi[rownames(love_weighted)]
)

# ==============================================================================
# 4. MAIN RESULTS (Section 4.1): CONCENTRATION ASYMMETRY
# ==============================================================================

cat("Testing H1: Concentration Asymmetry...\n")

# Calculate descriptive statistics reported in paper
outgoing_stats <- list(
  hate_mean = mean(hhi_results$hate_out_hhi, na.rm = TRUE),
  hate_sd = sd(hhi_results$hate_out_hhi, na.rm = TRUE),
  love_mean = mean(hhi_results$love_out_hhi, na.rm = TRUE),
  love_sd = sd(hhi_results$love_out_hhi, na.rm = TRUE)
)

incoming_stats <- list(
  hate_mean = mean(hhi_results$hate_in_hhi, na.rm = TRUE),
  hate_sd = sd(hhi_results$hate_in_hhi, na.rm = TRUE),
  love_mean = mean(hhi_results$love_in_hhi, na.rm = TRUE),
  love_sd = sd(hhi_results$love_in_hhi, na.rm = TRUE)
)

# Paired t-tests for differences
outgoing_test <- t.test(hhi_results$hate_out_hhi, hhi_results$love_out_hhi,
                        paired = TRUE)
incoming_test <- t.test(hhi_results$hate_in_hhi, hhi_results$love_in_hhi,
                        paired = TRUE)

# Print results as reported in paper
cat("\n=== SECTION 4.1 RESULTS ===\n")
cat("Outgoing ties:\n")
cat(sprintf("  Hate: M = %.2f, SD = %.2f\n", outgoing_stats$hate_mean, outgoing_stats$hate_sd))
cat(sprintf("  Love: M = %.2f, SD = %.2f\n", outgoing_stats$love_mean, outgoing_stats$love_sd))
cat(sprintf("  Difference: Δ = %.2f, p = %.3g\n",
            outgoing_stats$hate_mean - outgoing_stats$love_mean,
            outgoing_test$p.value))

cat("Incoming ties:\n")
cat(sprintf("  Hate: M = %.2f, SD = %.2f\n", incoming_stats$hate_mean, incoming_stats$hate_sd))
cat(sprintf("  Love: M = %.2f, SD = %.2f\n", incoming_stats$love_mean, incoming_stats$love_sd))
cat(sprintf("  Difference: Δ = %.2f, p = %.3g\n",
            incoming_stats$hate_mean - incoming_stats$love_mean,
            incoming_test$p.value))

# ==============================================================================
# 5. FIGURE 1: HHI BOXPLOTS
# ==============================================================================

cat("Creating Figure 1: HHI Boxplots...\n")

# Prepare data for plotting
plot_data <- bind_rows(
  hhi_results %>% transmute(Level = "Outgoing", Type = "Love", HHI = love_out_hhi),
  hhi_results %>% transmute(Level = "Outgoing", Type = "Hate", HHI = hate_out_hhi),
  hhi_results %>% transmute(Level = "Incoming", Type = "Love", HHI = love_in_hhi),
  hhi_results %>% transmute(Level = "Incoming", Type = "Hate", HHI = hate_in_hhi)
) %>%
  mutate(Level = factor(Level, levels = c("Outgoing", "Incoming")))

# Create annotation data for statistical tests
ann_df <- tibble(
  Level = factor(c("Outgoing", "Incoming"), levels = c("Outgoing", "Incoming")),
  label = c(
    sprintf("Δ = %.02f\np < 0.001", outgoing_stats$hate_mean - outgoing_stats$love_mean),
    sprintf("Δ = %.02f\np = %.3g", incoming_stats$hate_mean - incoming_stats$love_mean,
            incoming_test$p.value)
  ),
  x = 1.05,
  y = 0.98
)

# Create Figure 1
figure1 <- ggplot(plot_data, aes(x = Type, y = HHI, fill = Type)) +
  geom_boxplot(alpha = 0.65, width = 0.7, outlier.alpha = 0.9) +
  stat_summary(fun = median, geom = "crossbar", width = 0.6,
               linewidth = 0.7, colour = "black") +
  facet_wrap(~ Level, nrow = 1) +
  geom_text(data = ann_df,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, vjust = 1, size = 5) +
  labs(x = NULL, y = "HHI") +
  theme_classic(base_size = 16) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 20, face = "bold"),
    axis.text.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.background = element_rect(fill = "white", colour = "white")
  )

ggsave("figures/figure_1_hhi_boxplots.png", figure1,
       width = 8, height = 4.5, dpi = 300, bg = "white")

# ==============================================================================
# 6. CORRELATIONS (Section 4.2): INDEPENDENCE OF PROCESSES
# ==============================================================================

cat("Testing H2: Independence of Processes...\n")

# Calculate correlations as reported in paper
cor_out <- cor.test(hhi_results$love_out_hhi, hhi_results$hate_out_hhi,
                    use = "complete.obs")
cor_in <- cor.test(hhi_results$love_in_hhi, hhi_results$hate_in_hhi,
                   use = "complete.obs")

cat("\n=== SECTION 4.2 RESULTS ===\n")
cat(sprintf("Outgoing concentration: r = %.2f, p = %.3f\n",
            cor_out$estimate, cor_out$p.value))
cat(sprintf("Incoming concentration: r = %.2f, p = %.3f\n",
            cor_in$estimate, cor_in$p.value))

# Create Figure 2: Correlation plots
p1 <- ggplot(hhi_results, aes(x = love_out_hhi, y = hate_out_hhi)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Love Concentration (Outgoing)",
       y = "Hate Concentration (Outgoing)",
       title = sprintf("Outgoing: r = %.2f, p = %.3f", cor_out$estimate, cor_out$p.value)) +
  theme_classic()

p2 <- ggplot(hhi_results, aes(x = love_in_hhi, y = hate_in_hhi)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Love Concentration (Incoming)",
       y = "Hate Concentration (Incoming)",
       title = sprintf("Incoming: r = %.2f, p = %.3f", cor_in$estimate, cor_in$p.value)) +
  theme_classic()

figure2 <- p1 + p2
ggsave("figures/figure_2_correlations.png", figure2,
       width = 10, height = 4, dpi = 300, bg = "white")

# ==============================================================================
# 7. CLUB ATTRIBUTES DATA PREPARATION
# ==============================================================================

cat("Loading and preparing club attributes...\n")

# Load club attributes
clubs_master <- read_csv("data/clubs_master.csv")
market_values <- read_xlsx("data/market_values.xlsx", sheet = 1)

# Function to simplify club names for matching
simplify_name <- function(name) {
  name %>%
    str_to_lower() %>%
    str_remove_all("^(1\\.\\s*fc|sv|fc|bsc|sc|tsv|vfl|sg|spvgg|msv|fsv|ssv|kfc)\\s+") %>%
    str_trim()
}

# Fuzzy match club names with market values
clubs_master <- clubs_master %>%
  mutate(club_name_simple = simplify_name(club_name))

market_values <- market_values %>%
  mutate(name_simple = simplify_name(name))

matched <- stringdist_left_join(
  clubs_master, market_values,
  by = c("club_name_simple" = "name_simple"),
  method = "jw", max_dist = 0.15, distance_col = "dist"
) %>%
  arrange(dist) %>%
  distinct(club_name, .keep_all = TRUE)

# Clean and prepare variables
clubs_clean <- matched %>%
  mutate(
    # Convert market values to numeric (millions)
    market_value_numeric = case_when(
      str_detect(market_value, "m") ~ as.numeric(str_remove_all(market_value, "[^0-9\\.]")),
      str_detect(market_value, "k") ~ as.numeric(str_remove_all(market_value, "[^0-9\\.]")) / 1000,
      TRUE ~ NA_real_
    ),
    log_market_value = log(market_value_numeric),

    # League tier coding
    liga_numeric = case_when(
      str_detect(Liga, "1\\. Liga") ~ 4,
      str_detect(Liga, "2\\. Liga") ~ 3,
      str_detect(Liga, "3\\. Liga") ~ 2,
      str_detect(Liga, "Regionalliga") ~ 1,
      TRUE ~ NA_real_
    ),

    # East/West coding (with precise word boundaries to avoid Niedersachsen issue)
    east_west_binary = case_when(
      str_detect(tolower(LAND_NAME), "\\bsachsen\\b|\\bthüringen\\b|\\bthã¼ringen\\b|\\bbrandenburg\\b|\\bmecklenburg\\b|\\bsachsen-anhalt\\b|\\bberlin\\b") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(
    club_name, city, log_market_value, liga_numeric, club_age,
    plaetze_clean, mitglieder_clean, WKR_NAME, gdp_per_capita,
    unemployment_rate, LAND_NAME, perc_afd_2017, east_west_binary
  ) %>%
  rename(
    stadium_capacity = plaetze_clean,
    members = mitglieder_clean,
    region = LAND_NAME,
    gdp = gdp_per_capita
  ) %>%
  mutate(
    # Convert to numeric and handle missing values
    club_age = as.numeric(club_age),
    stadium_capacity = as.numeric(stadium_capacity),
    members = as.numeric(members),
    gdp = as.numeric(gdp),
    log_members = log(ifelse(is.na(members) | members <= 0, 5000, members))
  )

# Correct Berlin clubs to West
clubs_clean$east_west_binary[clubs_clean$WKR_NAME == "Berlin-Mitte"] <- 0

# Merge club attributes with HHI concentration measures
cat("Merging club attributes with HHI measures...\n")

# Check dimensions before merge
cat("clubs_clean dimensions:", nrow(clubs_clean), "x", ncol(clubs_clean), "\n")
cat("hhi_results dimensions:", nrow(hhi_results), "x", ncol(hhi_results), "\n")

# Perform the merge
merged_data <- clubs_clean %>%
  left_join(hhi_results, by = c("club_name" = "club"))

# Check merge results
cat("merged_data dimensions after join:", nrow(merged_data), "x", ncol(merged_data), "\n")
cat("Clubs with missing HHI data:", sum(is.na(merged_data$love_out_hhi)), "\n")

# Ensure consistent ordering (alphabetical by club name)
merged_data <- merged_data %>%
  arrange(club_name)

cat("Final merged_data dimensions:", nrow(merged_data), "x", ncol(merged_data), "\n")
cat("Club order (first 5):", paste(head(merged_data$club_name, 5), collapse = ", "), "\n")

# ==============================================================================
# 8.1 BETA REGRESSION ANALYSIS
# ==============================================================================

cat("Running beta regression models...\n")

# Prepare analysis dataset (remove cases with missing values)
analysis_data <- merged_data %>%
  drop_na(love_out_hhi, hate_out_hhi, love_in_hhi, hate_in_hhi,
          gdp, unemployment_rate, east_west_binary, region, stadium_capacity) %>%
  mutate(
    liga_factor = factor(liga_numeric,
                         levels = c(4, 3, 2, 1),
                         labels = c("Bundesliga", "2nd", "3rd", "Regional"))
  ) %>%
  arrange(club_name)  # Ensure consistent ordering

cat("Analysis sample size:", nrow(analysis_data), "clubs\n")

# Beta transformation (as in original code)
transform_for_beta <- function(y, n = length(y)) {
  (y * (n - 1) + 0.5) / n
}

# Apply beta transformation to HHI values
analysis_data_beta <- analysis_data %>%
  mutate(
    love_out_hhi_beta = ifelse(love_out_hhi == 0 | love_out_hhi == 1,
                               transform_for_beta(love_out_hhi), love_out_hhi),
    hate_out_hhi_beta = ifelse(hate_out_hhi == 0 | hate_out_hhi == 1,
                               transform_for_beta(hate_out_hhi), hate_out_hhi),
    love_in_hhi_beta = ifelse(love_in_hhi == 0 | love_in_hhi == 1,
                              transform_for_beta(love_in_hhi), love_in_hhi),
    hate_in_hhi_beta = ifelse(hate_in_hhi == 0 | hate_in_hhi == 1,
                              transform_for_beta(hate_in_hhi), hate_in_hhi)
  )

# Beta regression models (using transformed variables)
beta_m1 <- betareg(love_out_hhi_beta ~ scale(gdp) + scale(unemployment_rate) +
                     east_west_binary + scale(stadium_capacity) +
                     scale(log_members) + scale(club_age) +
                     scale(log_market_value) + liga_factor +
                     scale(perc_afd_2017),
                   data = analysis_data_beta, link = "logit")

beta_m2 <- betareg(hate_out_hhi_beta ~ scale(gdp) + scale(unemployment_rate) +
                     east_west_binary + scale(stadium_capacity) +
                     scale(log_members) + scale(club_age) +
                     scale(log_market_value) + liga_factor +
                     scale(perc_afd_2017),
                   data = analysis_data_beta, link = "logit")

beta_m3 <- betareg(love_in_hhi_beta ~ scale(gdp) + scale(unemployment_rate) +
                     east_west_binary + scale(stadium_capacity) +
                     scale(log_members) + scale(club_age) +
                     scale(log_market_value) + liga_factor +
                     scale(perc_afd_2017),
                   data = analysis_data_beta, link = "logit")

beta_m4 <- betareg(hate_in_hhi_beta ~ scale(gdp) + scale(unemployment_rate) +
                     east_west_binary + scale(stadium_capacity) +
                     scale(log_members) + scale(club_age) +
                     scale(log_market_value) + liga_factor +
                     scale(perc_afd_2017),
                   data = analysis_data_beta, link = "logit")

# Extract key results for H3 test
cat("\n=== SECTION 4.3 RESULTS (H3: Agency-Structure Asymmetry) ===\n")
cat("Pseudo R-squared values:\n")
cat(sprintf("  Love Outgoing: %.2f\n", summary(beta_m1)$pseudo.r.squared))
cat(sprintf("  Hate Outgoing: %.2f\n", summary(beta_m2)$pseudo.r.squared))
cat(sprintf("  Love Incoming: %.2f\n", summary(beta_m3)$pseudo.r.squared))
cat(sprintf("  Hate Incoming: %.2f\n", summary(beta_m4)$pseudo.r.squared))

# Key coefficients mentioned in paper
east_love_coef <- coef(beta_m3)["east_west_binary"]
afd_love_coef <- coef(beta_m3)["scale(perc_afd_2017)"]
unemp_hate_coef <- coef(beta_m4)["scale(unemployment_rate)"]

cat("\nKey coefficients:\n")
cat(sprintf("  East Germany → Love In: β = %.2f\n", east_love_coef))
cat(sprintf("  AfD Support → Love In: β = %.2f\n", afd_love_coef))
cat(sprintf("  Unemployment → Hate In: β = %.2f\n", unemp_hate_coef))

# Save model results for Table 1
save(beta_m1, beta_m2, beta_m3, beta_m4, file = "results/beta_models.RData")

# ==============================================================================
# 8.2 GENERATE LATEX TABLES FOR PAPER
# ==============================================================================

library(xtable)
library(stargazer)

cat("Generating LaTeX tables...\n")

# ==============================================================================
# 8.2.1 TABLE 1: DESCRIPTIVE STATISTICS
# ==============================================================================

# Calculate descriptive statistics for all variables used in the analysis
desc_stats <- analysis_data %>%
  summarise(
    # Concentration Measures (HHI)
    love_out_mean = mean(love_out_hhi, na.rm = TRUE),
    love_out_sd = sd(love_out_hhi, na.rm = TRUE),
    love_out_min = min(love_out_hhi, na.rm = TRUE),
    love_out_max = max(love_out_hhi, na.rm = TRUE),

    hate_out_mean = mean(hate_out_hhi, na.rm = TRUE),
    hate_out_sd = sd(hate_out_hhi, na.rm = TRUE),
    hate_out_min = min(hate_out_hhi, na.rm = TRUE),
    hate_out_max = max(hate_out_hhi, na.rm = TRUE),

    love_in_mean = mean(love_in_hhi, na.rm = TRUE),
    love_in_sd = sd(love_in_hhi, na.rm = TRUE),
    love_in_min = min(love_in_hhi, na.rm = TRUE),
    love_in_max = max(love_in_hhi, na.rm = TRUE),

    hate_in_mean = mean(hate_in_hhi, na.rm = TRUE),
    hate_in_sd = sd(hate_in_hhi, na.rm = TRUE),
    hate_in_min = min(hate_in_hhi, na.rm = TRUE),
    hate_in_max = max(hate_in_hhi, na.rm = TRUE),

    # Structural Factors
    stadium_mean = mean(log(stadium_capacity), na.rm = TRUE),
    stadium_sd = sd(log(stadium_capacity), na.rm = TRUE),
    stadium_min = min(log(stadium_capacity), na.rm = TRUE),
    stadium_max = max(log(stadium_capacity), na.rm = TRUE),

    age_mean = mean(club_age, na.rm = TRUE),
    age_sd = sd(club_age, na.rm = TRUE),
    age_min = min(club_age, na.rm = TRUE),
    age_max = max(club_age, na.rm = TRUE),

    market_mean = mean(log_market_value, na.rm = TRUE),
    market_sd = sd(log_market_value, na.rm = TRUE),
    market_min = min(log_market_value, na.rm = TRUE),
    market_max = max(log_market_value, na.rm = TRUE),

    members_mean = mean(log_members, na.rm = TRUE),
    members_sd = sd(log_members, na.rm = TRUE),
    members_min = min(log_members, na.rm = TRUE),
    members_max = max(log_members, na.rm = TRUE),

    # Geographic Factors
    gdp_mean = mean(gdp, na.rm = TRUE),
    gdp_sd = sd(gdp, na.rm = TRUE),
    gdp_min = min(gdp, na.rm = TRUE),
    gdp_max = max(gdp, na.rm = TRUE),

    unemp_mean = mean(unemployment_rate, na.rm = TRUE),
    unemp_sd = sd(unemployment_rate, na.rm = TRUE),
    unemp_min = min(unemployment_rate, na.rm = TRUE),
    unemp_max = max(unemployment_rate, na.rm = TRUE),

    # Political Factor
    afd_mean = mean(perc_afd_2017, na.rm = TRUE),
    afd_sd = sd(perc_afd_2017, na.rm = TRUE),
    afd_min = min(perc_afd_2017, na.rm = TRUE),
    afd_max = max(perc_afd_2017, na.rm = TRUE)
  )

# Categorical variables
liga_table <- table(analysis_data$liga_factor)
east_table <- table(analysis_data$east_west_binary)

# Create LaTeX table for descriptive statistics
desc_latex <- paste0(
  "\\begin{table}[!ht]\n",
  "\\centering\n",
  "\\caption{\\textbf{Descriptive statistics.}}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\hline\n",
  "\\textbf{Variable} & \\textbf{Mean} & \\textbf{SD} & \\textbf{Min} & \\textbf{Max} \\\\\n",
  "\\hline\n",
  "\\textbf{Concentration Measures (HHI)} & & & & \\\\\n",
  sprintf("Love Concentration (Outgoing) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$love_out_mean, desc_stats$love_out_sd,
          desc_stats$love_out_min, desc_stats$love_out_max),
  sprintf("Hate Concentration (Outgoing) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$hate_out_mean, desc_stats$hate_out_sd,
          desc_stats$hate_out_min, desc_stats$hate_out_max),
  sprintf("Love Concentration (Incoming) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$love_in_mean, desc_stats$love_in_sd,
          desc_stats$love_in_min, desc_stats$love_in_max),
  sprintf("Hate Concentration (Incoming) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$hate_in_mean, desc_stats$hate_in_sd,
          desc_stats$hate_in_min, desc_stats$hate_in_max),
  "& & & & \\\\\n",
  "\\textbf{Structural Factors} & & & & \\\\\n",
  sprintf("Stadium Capacity (log) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$stadium_mean, desc_stats$stadium_sd,
          desc_stats$stadium_min, desc_stats$stadium_max),
  sprintf("Club Age (years) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$age_mean, desc_stats$age_sd,
          desc_stats$age_min, desc_stats$age_max),
  sprintf("Market Value (log) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$market_mean, desc_stats$market_sd,
          desc_stats$market_min, desc_stats$market_max),
  sprintf("Members (log) & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$members_mean, desc_stats$members_sd,
          desc_stats$members_min, desc_stats$members_max),
  "& & & & \\\\\n",
  "\\textbf{Geographic Factors} & & & & \\\\\n",
  sprintf("GDP per Capita & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$gdp_mean, desc_stats$gdp_sd,
          desc_stats$gdp_min, desc_stats$gdp_max),
  sprintf("Unemployment Rate & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$unemp_mean, desc_stats$unemp_sd,
          desc_stats$unemp_min, desc_stats$unemp_max),
  "& & & & \\\\\n",
  "\\textbf{Historical/Political} & & & & \\\\\n",
  sprintf("AfD Vote Share & %.2f & %.2f & %.2f & %.2f \\\\\n",
          desc_stats$afd_mean, desc_stats$afd_sd,
          desc_stats$afd_min, desc_stats$afd_max),
  "\\hline\n",
  "\\end{tabular}\n",
  "\\begin{flushleft}\n",
  "\\textbf{Categorical Variables:} \\\\\n",
  sprintf("League Tier: Bundesliga (n=%d), 2nd Division (n=%d), 3rd Division (n=%d), Regional (n=%d) \\\\\n",
          liga_table["Bundesliga"], liga_table["2nd"], liga_table["3rd"], liga_table["Regional"]),
  sprintf("East Germany: Yes (n=%d), No (n=%d) \\\\\n",
          east_table["1"], east_table["0"]),
  sprintf("Total N=%d clubs (8 clubs excluded due to insufficient data)\n", nrow(analysis_data)),
  "\\end{flushleft}\n",
  "\\label{table_descriptives}\n",
  "\\end{table}\n"
)

# Write descriptive statistics table
writeLines(desc_latex, "results/table_descriptives.tex")
cat("Descriptive statistics table saved to results/table_descriptives.tex\n")

# ==============================================================================
# 8.2.2 TABLE 2: BETA REGRESSION RESULTS
# ==============================================================================

# Function to format coefficients with significance stars
format_coef <- function(coef, se, p_value) {
  stars <- ifelse(p_value < 0.001, "^{***}",
                  ifelse(p_value < 0.01, "^{**}",
                         ifelse(p_value < 0.05, "^*",
                                ifelse(p_value < 0.10, "^\\dagger", ""))))
  sprintf("%.2f%s (%.2f)", coef, stars, se)
}

# Extract coefficients and statistics for all models
extract_beta_results <- function(model) {
  summ <- summary(model)
  coefs <- summ$coefficients$mean

  # Return data frame with coefficient names, estimates, SEs, and p-values
  data.frame(
    var = rownames(coefs),
    estimate = coefs[, "Estimate"],
    se = coefs[, "Std. Error"],
    p_value = coefs[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
}

m1_results <- extract_beta_results(beta_m1)
m2_results <- extract_beta_results(beta_m2)
m3_results <- extract_beta_results(beta_m3)
m4_results <- extract_beta_results(beta_m4)

# Create mapping for variable names to LaTeX labels
var_labels <- c(
  "scale(stadium_capacity)" = "Stadium Capacity",
  "scale(club_age)" = "Club Age",
  "scale(log_market_value)" = "Market Value",
  "scale(log_members)" = "Members (log)",
  "scale(gdp)" = "GDP per Capita",
  "scale(unemployment_rate)" = "Unemployment Rate",
  "east_west_binary" = "East Germany",
  "scale(perc_afd_2017)" = "AfD Vote Share",
  "liga_factor2nd" = "2nd Division",
  "liga_factor3rd" = "3rd Division",
  "liga_factorRegional" = "Regional"
)

# Order of variables in table (matching paper)
var_order <- c(
  "scale(stadium_capacity)",
  "scale(club_age)",
  "scale(log_market_value)",
  "scale(log_members)",
  "scale(gdp)",
  "scale(unemployment_rate)",
  "east_west_binary",
  "scale(perc_afd_2017)",
  "liga_factor2nd",
  "liga_factor3rd",
  "liga_factorRegional"
)

# Build table rows
build_table_row <- function(var_name, m1, m2, m3, m4) {
  get_formatted <- function(results, var) {
    row <- results[results$var == var, ]
    if(nrow(row) == 0) return("---")
    format_coef(row$estimate, row$se, row$p_value)
  }

  sprintf("%s & %s & %s & %s & %s \\\\\n",
          var_labels[var_name],
          get_formatted(m1, var_name),
          get_formatted(m2, var_name),
          get_formatted(m3, var_name),
          get_formatted(m4, var_name))
}

# Create LaTeX table
beta_latex <- paste0(
  "\\begin{table}[!ht]\n",
  "\\begin{adjustwidth}{-2.25in}{0in}\n",
  "\\centering\n",
  "\\caption{\\textbf{Beta regression results for attention concentration.}}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\hline\n",
  "& \\multicolumn{2}{c}{\\textbf{Outgoing}} & \\multicolumn{2}{c}{\\textbf{Incoming}} \\\\\n",
  "\\cline{2-3} \\cline{4-5}\n",
  "& \\textbf{Love} & \\textbf{Hate} & \\textbf{Love} & \\textbf{Hate} \\\\\n",
  "\\hline\n",
  "\\textbf{Structural Factors} & & & & \\\\\n",
  build_table_row("scale(stadium_capacity)", m1_results, m2_results, m3_results, m4_results),
  build_table_row("scale(club_age)", m1_results, m2_results, m3_results, m4_results),
  build_table_row("scale(log_market_value)", m1_results, m2_results, m3_results, m4_results),
  build_table_row("scale(log_members)", m1_results, m2_results, m3_results, m4_results),
  "& & & & \\\\\n",
  "\\textbf{Geographic Factors} & & & & \\\\\n",
  build_table_row("scale(gdp)", m1_results, m2_results, m3_results, m4_results),
  build_table_row("scale(unemployment_rate)", m1_results, m2_results, m3_results, m4_results),
  "& & & & \\\\\n",
  "\\textbf{Historical/Political} & & & & \\\\\n",
  build_table_row("east_west_binary", m1_results, m2_results, m3_results, m4_results),
  build_table_row("scale(perc_afd_2017)", m1_results, m2_results, m3_results, m4_results),
  "& & & & \\\\\n",
  "\\textbf{League Tier (ref: Bundesliga)} & & & & \\\\\n",
  build_table_row("liga_factor2nd", m1_results, m2_results, m3_results, m4_results),
  build_table_row("liga_factor3rd", m1_results, m2_results, m3_results, m4_results),
  build_table_row("liga_factorRegional", m1_results, m2_results, m3_results, m4_results),
  "& & & & \\\\\n",
  sprintf("Pseudo $R^2$ & %.2f & %.2f & %.2f & %.2f \\\\\n",
          summary(beta_m1)$pseudo.r.squared,
          summary(beta_m2)$pseudo.r.squared,
          summary(beta_m3)$pseudo.r.squared,
          summary(beta_m4)$pseudo.r.squared),
  sprintf("Log-likelihood & %.2f & %.2f & %.2f & %.2f \\\\\n",
          logLik(beta_m1)[1],
          logLik(beta_m2)[1],
          logLik(beta_m3)[1],
          logLik(beta_m4)[1]),
  sprintf("N & %d & %d & %d & %d \\\\\n",
          nrow(analysis_data_beta),
          nrow(analysis_data_beta),
          nrow(analysis_data_beta),
          nrow(analysis_data_beta)),
  "\\hline\n",
  "\\end{tabular}\n",
  "\\begin{flushleft}\n",
  "Table notes: Coefficients from beta regression models with logit link shown with standard errors in parentheses. ",
  "$^\\dagger p < 0.10$, $^* p < 0.05$, $^{**} p < 0.01$, $^{***} p < 0.001$. ",
  "Eight clubs were excluded due to insufficient data for HHI calculation.\n",
  "\\end{flushleft}\n",
  "\\label{table1}\n",
  "\\end{adjustwidth}\n",
  "\\end{table}\n"
)

# Write beta regression table
writeLines(beta_latex, "results/table_beta_regression.tex")
cat("Beta regression table saved to results/table_beta_regression.tex\n")

cat("\nLaTeX tables generated successfully!\n")
cat("  - results/table_descriptives.tex\n")
cat("  - results/table_beta_regression.tex\n")

# ==============================================================================
# 9. ROBUSTNESS CHECKS
# ==============================================================================

cat("Running robustness checks...\n")

# Alternative concentration measures
calculate_gini <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if(length(x) == 0) return(NA)
  return(ineq(x, type = "Gini"))
}

calculate_shannon_entropy <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if(length(x) == 0) return(NA)
  p <- x / sum(x)
  return(-sum(p * log(p)))
}

calculate_top3_concentration <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if(length(x) == 0) return(NA)
  total <- sum(x)
  if(total == 0) return(NA)
  x_sorted <- sort(x, decreasing = TRUE)
  top_3_sum <- sum(x_sorted[1:min(3, length(x_sorted))])
  return(top_3_sum / total)
}

# Calculate alternative measures
love_out_gini <- apply(love_weighted, 1, calculate_gini)
hate_out_gini <- apply(hate_weighted, 1, calculate_gini)
love_out_shannon <- apply(love_weighted, 1, calculate_shannon_entropy)
hate_out_shannon <- apply(hate_weighted, 1, calculate_shannon_entropy)
love_out_top3 <- apply(love_weighted, 1, calculate_top3_concentration)
hate_out_top3 <- apply(hate_weighted, 1, calculate_top3_concentration)

# Calculate alternative measures for INCOMING ties as well
love_in_gini <- apply(t(love_weighted), 1, calculate_gini)
hate_in_gini <- apply(t(hate_weighted), 1, calculate_gini)
love_in_shannon <- apply(t(love_weighted), 1, calculate_shannon_entropy)
hate_in_shannon <- apply(t(hate_weighted), 1, calculate_shannon_entropy)
love_in_top3 <- apply(t(love_weighted), 1, calculate_top3_concentration)
hate_in_top3 <- apply(t(hate_weighted), 1, calculate_top3_concentration)

# Test differences
gini_test <- t.test(hate_out_gini, love_out_gini, paired = TRUE)
shannon_test <- t.test(love_out_shannon, hate_out_shannon, paired = TRUE)  # Reversed for interpretation
top3_test <- t.test(hate_out_top3, love_out_top3, paired = TRUE)

# Test differences for incoming ties
gini_in_test <- t.test(hate_in_gini, love_in_gini, paired = TRUE)
shannon_in_test <- t.test(love_in_shannon, hate_in_shannon, paired = TRUE)
top3_in_test <- t.test(hate_in_top3, love_in_top3, paired = TRUE)

cat("\n=== ROBUSTNESS CHECKS ===\n")
cat("Alternative measures (outgoing ties):\n")
cat(sprintf("  Gini: %.3f vs %.3f, p < 0.001\n",
            mean(hate_out_gini, na.rm=T), mean(love_out_gini, na.rm=T)))
cat(sprintf("  Shannon Entropy: %.3f vs %.3f, p < 0.001\n",
            mean(hate_out_shannon, na.rm=T), mean(love_out_shannon, na.rm=T)))
cat(sprintf("  Top-3: %.1f%% vs %.1f%%, p < 0.001\n",
            mean(hate_out_top3, na.rm=T)*100, mean(love_out_top3, na.rm=T)*100))
cat(sprintf("  Gini: %.3f vs %.3f, p < 0.001\n",
            mean(hate_in_gini, na.rm=T), mean(love_in_gini, na.rm=T)))
cat("  Shannon Entropy (incoming): %.3f vs %.3f, p < 0.001\n",
    mean(hate_in_shannon, na.rm=T), mean(love_in_shannon, na.rm=T))
cat("  Top-3 (incoming): %.1f%% vs %.1f%%, p < 0.001\n",
    mean(hate_in_top3, na.rm=T)*100, mean(love_in_top3, na.rm=T)*100)
# ==============================================================================
# 10. SAVE RESULTS
# ==============================================================================

cat("Saving results...\n")

# Save main results dataset
write_csv(hhi_results, "results/hhi_concentration_results.csv")
write_csv(analysis_data, "results/analysis_dataset.csv")

# Save summary statistics
summary_stats <- list(
  outgoing_stats = outgoing_stats,
  incoming_stats = incoming_stats,
  outgoing_test_p = outgoing_test$p.value,
  incoming_test_p = incoming_test$p.value,
  correlation_out = cor_out$estimate,
  correlation_in = cor_in$estimate,
  east_effect = east_love_coef,
  afd_effect = afd_love_coef,
  unemployment_effect = unemp_hate_coef
)

saveRDS(summary_stats, "results/summary_statistics.rds")

cat("\n=== REPLICATION COMPLETE ===\n")
cat("Key files generated:\n")
cat("  - figures/figure_1_hhi_boxplots.png\n")
cat("  - figures/figure_2_correlations.png\n")
cat("  - results/hhi_concentration_results.csv\n")
cat("  - results/analysis_dataset.csv\n")
cat("  - results/beta_models.RData\n")
cat("  - results/summary_statistics.rds\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
