############################################
# FINAL FINAL FINAL READY-TO-RUN CODE
# WVS Wave 7 (v6.0) + GDP per capita + HDI + Life expectancy
# Focus: Q169 & Q170
#
# Satisfies required items:
# - Data visualization
# - 1 variable: weighted bar + descriptive statistics table
# - 2 variables: scatter + fit line
# - 3+ variables: multiple OLS regression + regression table + residual plot
# - Time series line graph + coverage line graph
# - Choropleth map
# - Prediction method
# - Standard OLS (lm)
############################################

# =========================
# 0) Packages
# =========================
pkgs <- c(
  "tidyverse","janitor","stringr","broom","sf",
  "rnaturalearth","rnaturalearthdata","scales",
  "sandwich","lmtest"
)
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(janitor)
library(stringr)
library(broom)
library(sf)
library(rnaturalearth)
library(scales)
library(sandwich)
library(lmtest)

# =========================
# 1) Paths (files in working dir)
# =========================
path_wvs <- "WVS_Cross-National_Wave_7_csv_v6_0.csv"
path_gdp <- "GDP_per_capita.csv"
path_hdi <- "hdi-data.csv"
path_le  <- "Life expectancy at birth.csv"

# =========================
# 2) Helpers
# =========================
wvs_na_codes <- c(-1, -2, -4, -5)
safe_numeric <- function(x) suppressWarnings(as.numeric(x))

likert_labels_en <- c(
  "1" = "Strongly disagree",
  "2" = "Disagree",
  "3" = "Agree",
  "4" = "Strongly agree"
)

plot_bar_share_1to4 <- function(df, var, title, weight_var = NULL, labels_map = NULL) {
  d <- df %>%
    filter(!is.na(.data[[var]]), .data[[var]] %in% 1:4) %>%
    mutate(value = factor(.data[[var]], levels = 1:4))
  
  if(!is.null(weight_var) && weight_var %in% names(d)) {
    d_sum <- d %>%
      group_by(value) %>%
      summarise(w = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop") %>%
      mutate(share = w / sum(w))
  } else {
    d_sum <- d %>% count(value, name = "w") %>% mutate(share = w / sum(w))
  }
  
  p <- ggplot(d_sum, aes(x = value, y = share)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = title, x = var, y = "Share")
  
  if(!is.null(labels_map)) p <- p + scale_x_discrete(labels = labels_map)
  p
}

# =========================
# 3) Load data
# =========================
wvs_raw <- readr::read_csv(path_wvs, show_col_types = FALSE) %>% clean_names()
gdp_raw <- readr::read_csv(path_gdp, show_col_types = FALSE) %>% clean_names()
hdi_raw <- readr::read_csv(path_hdi, show_col_types = FALSE) %>% clean_names()
le_raw  <- readr::read_csv(path_le,  show_col_types = FALSE) %>% clean_names()

# =========================
# 4) Prepare WVS (Q169 & Q170)
# =========================
stopifnot("q169" %in% names(wvs_raw), "q170" %in% names(wvs_raw))
stopifnot("b_country_alpha" %in% names(wvs_raw), "a_year" %in% names(wvs_raw))

weight_col <- if("pwght" %in% names(wvs_raw)) "pwght" else if("w_weight" %in% names(wvs_raw)) "w_weight" else NA_character_

wvs <- wvs_raw %>%
  transmute(
    country_alpha = b_country_alpha,
    year          = as.integer(a_year),
    weight        = if(!is.na(weight_col)) safe_numeric(.data[[weight_col]]) else NA_real_,
    q169          = safe_numeric(q169),
    q170          = safe_numeric(q170)
  ) %>%
  mutate(
    q169 = ifelse(q169 %in% wvs_na_codes, NA_real_, q169),
    q170 = ifelse(q170 %in% wvs_na_codes, NA_real_, q170),
    weight = ifelse(is.na(weight) | weight <= 0, 1, weight)
  ) %>%
  filter(!is.na(country_alpha), !is.na(year))

cat("\nWVS year counts:\n")
print(wvs %>% count(year, sort = TRUE))

# =========================
# 5) GDP per capita (wide -> long, pick one series)
# =========================
gdp_series_candidates <- c(
  "NY.GDP.PCAP.CD",    # GDP per capita (current US$)
  "NY.GDP.PCAP.PP.CD", # PPP current
  "NY.GDP.PCAP.PP.KD"  # PPP constant
)

gdp_one <- gdp_raw %>% filter(series_code %in% gdp_series_candidates)
if(nrow(gdp_one) == 0) {
  warning("GDP: none of preferred series_code found. Using the file as-is (may duplicate).")
  gdp_one <- gdp_raw
} else {
  gdp_one <- gdp_one %>%
    mutate(series_code = factor(series_code, levels = gdp_series_candidates)) %>%
    arrange(series_code) %>%
    group_by(country_code) %>%
    slice(1) %>%
    ungroup()
}

gdp_year_cols <- names(gdp_one)[str_detect(names(gdp_one), "(19|20)\\d{2}")]
if(length(gdp_year_cols) == 0) stop("GDP: no year columns found.")

gdp_long <- gdp_one %>%
  select(country_code, all_of(gdp_year_cols)) %>%
  pivot_longer(
    cols = all_of(gdp_year_cols),
    names_to = "year",
    values_to = "gdp_pc"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "(19|20)\\d{2}")),
    gdp_pc = as.character(gdp_pc),
    gdp_pc = na_if(gdp_pc, ".."),
    gdp_pc = safe_numeric(gdp_pc),
    log_gdp_pc = log(gdp_pc)
  )

# =========================
# 6) Life expectancy (wide -> long)
# =========================
le_year_cols <- names(le_raw)[str_detect(names(le_raw), "^x(19|20)\\d{2}$")]
if(length(le_year_cols) == 0) stop("Life expectancy: no columns like xYYYY found.")

le_long <- le_raw %>%
  select(country_code, all_of(le_year_cols)) %>%
  pivot_longer(
    cols = all_of(le_year_cols),
    names_to = "year",
    values_to = "life_exp"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "(19|20)\\d{2}")),
    life_exp = safe_numeric(life_exp)
  )

# =========================
# 7) HDI (country-level, not by year)
# =========================
stopifnot(all(c("country_iso_code","index_code","indicator_code","value") %in% names(hdi_raw)))

hdi_country <- hdi_raw %>%
  filter(index_code == "HDI", indicator_code == "hdi") %>%
  transmute(country_code = country_iso_code, hdi = safe_numeric(value)) %>%
  group_by(country_code) %>%
  summarise(hdi = max(hdi, na.rm = TRUE), .groups = "drop")

# =========================
# 8) Merge WVS + macro (ISO3 + year), HDI by country
# =========================
wvs_macro <- wvs %>%
  left_join(gdp_long %>% select(country_code, year, gdp_pc, log_gdp_pc),
            by = c("country_alpha" = "country_code", "year" = "year")) %>%
  left_join(le_long %>% select(country_code, year, life_exp),
            by = c("country_alpha" = "country_code", "year" = "year")) %>%
  left_join(hdi_country, by = c("country_alpha" = "country_code"))

cat("\nMerge missingness:\n")
print(wvs_macro %>% summarise(
  n = n(),
  gdp_missing = mean(is.na(gdp_pc)),
  le_missing  = mean(is.na(life_exp)),
  hdi_missing = mean(is.na(hdi))
))

# =========================
# 9) Country-year aggregation (weighted means)
# =========================
country_year <- wvs_macro %>%
  group_by(country_alpha, year) %>%
  summarise(
    n = n(),
    q169_mean = weighted.mean(q169, weight, na.rm = TRUE),
    q170_mean = weighted.mean(q170, weight, na.rm = TRUE),
    gdp_pc = first(gdp_pc),
    log_gdp_pc = first(log_gdp_pc),
    life_exp = first(life_exp),
    hdi = first(hdi),
    .groups = "drop"
  ) %>%
  filter(is.finite(q169_mean), is.finite(q170_mean))

# =========================
# 10) REQUIRED 1-variable:
#     (a) weighted bar shares + (b) descriptive statistics table
# =========================
print(plot_bar_share_1to4(wvs_macro, "q169", "Q169 distribution (weighted share, 1–4)", "weight", likert_labels_en))
print(plot_bar_share_1to4(wvs_macro, "q170", "Q170 distribution (weighted share, 1–4)", "weight", likert_labels_en))

desc_tbl <- wvs_macro %>%
  summarise(
    q169_n = sum(!is.na(q169)),
    q169_mean = mean(q169, na.rm = TRUE),
    q169_sd = sd(q169, na.rm = TRUE),
    q169_min = min(q169, na.rm = TRUE),
    q169_max = max(q169, na.rm = TRUE),
    q170_n = sum(!is.na(q170)),
    q170_mean = mean(q170, na.rm = TRUE),
    q170_sd = sd(q170, na.rm = TRUE),
    q170_min = min(q170, na.rm = TRUE),
    q170_max = max(q170, na.rm = TRUE)
  )
cat("\nDescriptive statistics (individual-level):\n")
print(desc_tbl)

# =========================
# 11) REQUIRED 2-variable: scatter + fit line
# =========================
scatter_df <- country_year %>%
  drop_na(log_gdp_pc, q169_mean) %>%
  filter(is.finite(log_gdp_pc), is.finite(q169_mean))

print(
  ggplot(scatter_df, aes(x = log_gdp_pc, y = q169_mean)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = "Mean Q169 vs log(GDP per capita) (country-year)",
      x = "log(GDP per capita)",
      y = "Mean Q169"
    )
)

# =========================
# 12) REQUIRED 3+ variables:
#     stable country cross-section regression + residual plot + regression table
#     Pick, for each country, the year with MAX respondents.
# =========================
country_cs <- wvs_macro %>%
  group_by(country_alpha, year) %>%
  summarise(
    n = n(),
    q169_mean = weighted.mean(q169, weight, na.rm = TRUE),
    q170_mean = weighted.mean(q170, weight, na.rm = TRUE),
    log_gdp_pc = first(log_gdp_pc),
    life_exp = first(life_exp),
    hdi = first(hdi),
    .groups = "drop"
  ) %>%
  group_by(country_alpha) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  drop_na(q169_mean, q170_mean, log_gdp_pc, life_exp, hdi) %>%
  filter(is.finite(q169_mean), is.finite(q170_mean), is.finite(log_gdp_pc), is.finite(life_exp), is.finite(hdi))

cat("\nCountry cross-section size:", nrow(country_cs), "countries\n")

mod_cs_q169 <- lm(q169_mean ~ q170_mean + log_gdp_pc + life_exp + hdi, data = country_cs)
mod_cs_q170 <- lm(q170_mean ~ q169_mean + log_gdp_pc + life_exp + hdi, data = country_cs)

cat("\nOLS summary (Q169 dependent):\n")
print(summary(mod_cs_q169))
cat("\nOLS summary (Q170 dependent):\n")
print(summary(mod_cs_q170))

cat("\nRobust SE (HC1) (Q169 dependent):\n")
print(coeftest(mod_cs_q169, vcov = vcovHC(mod_cs_q169, type = "HC1")))
cat("\nRobust SE (HC1) (Q170 dependent):\n")
print(coeftest(mod_cs_q170, vcov = vcovHC(mod_cs_q170, type = "HC1")))

cat("\nRegression tables (broom::tidy):\n")
print(broom::tidy(mod_cs_q169))
print(broom::tidy(mod_cs_q170))

print(
  country_cs %>%
    mutate(fitted = fitted(mod_cs_q169), resid = residuals(mod_cs_q169)) %>%
    ggplot(aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Residuals vs fitted (country-level model for mean Q169)",
      x = "Fitted values",
      y = "Residuals"
    )
)

cat("\nCorrelation matrix (country_cs):\n")
print(
  country_cs %>%
    select(q169_mean, q170_mean, log_gdp_pc, life_exp, hdi) %>%
    cor(use = "pairwise.complete.obs")
)

# =========================
# 13) REQUIRED: Time series line graph + coverage
# =========================
global_ts <- country_year %>%
  group_by(year) %>%
  summarise(
    q169_global = mean(q169_mean, na.rm = TRUE),
    q170_global = mean(q170_mean, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  ) %>%
  arrange(year)

print(
  ggplot(global_ts, aes(x = year, y = q169_global)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = sort(unique(global_ts$year))) +
    labs(
      title = "Global trend: mean Q169 (unbalanced; composition varies)",
      x = "Year",
      y = "Mean Q169"
    )
)

print(
  ggplot(global_ts, aes(x = year, y = n_countries)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = sort(unique(global_ts$year))) +
    labs(
      title = "Coverage: number of countries per year (WVS Wave 7)",
      x = "Year",
      y = "# countries"
    )
)

# =========================
# 14) REQUIRED: Choropleth map (best-covered year)
# =========================
target_year <- country_year %>% count(year, sort = TRUE) %>% slice(1) %>% pull(year)
cat("\nChosen map year (max countries):", target_year, "\n")

map_df <- country_year %>%
  filter(year == target_year) %>%
  select(iso_a3 = country_alpha, q169_mean)

world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_make_valid()

print(
  world %>%
    left_join(map_df, by = c("iso_a3" = "iso_a3")) %>%
    ggplot() +
    geom_sf(aes(fill = q169_mean), color = NA) +
    scale_fill_continuous(labels = number_format(accuracy = 0.01), na.value = "grey90") +
    labs(
      title = paste0("Mean Q169 by country (", target_year, ")"),
      fill = "Mean Q169"
    ) +
    theme_void()
)

# =========================
# 15) REQUIRED: Prediction method (illustrative)
# =========================
pred_mod <- lm(q169_global ~ year, data = global_ts)
future_years <- tibble(year = seq(max(global_ts$year) + 1, max(global_ts$year) + 5))
pred_df <- bind_rows(global_ts %>% select(year, q169_global), future_years) %>%
  mutate(q169_pred = predict(pred_mod, newdata = .))

print(
  ggplot(pred_df, aes(x = year, y = q169_pred)) +
    geom_line() +
    geom_point(data = global_ts, aes(x = year, y = q169_global)) +
    scale_x_continuous(breaks = sort(unique(pred_df$year))) +
    labs(
      title = "Prediction: global mean Q169 (linear trend; illustrative)",
      x = "Year",
      y = "Predicted mean Q169"
    )
)

############################################
# END
############################################
