
# Run WHO data processing script to get dt_deaths_who_long
cat("Running 020 WHO deaths data processing script...\n")

source(paste0(wd_code,"020_get_deaths_who_ghe2023.R"))

# Run GBD data processing script to get baseline_rates
cat("Running 021 GBD baseline rates processing script...\n")
source(paste0(wd_code,"021_get_base_rates.R"))

# Run TPS transition probabilities script to get dt_tps
cat("Running 022 TPS transition probabilities processing script...\n")
source(paste0(wd_code,"022_get_tps.R"))

# Run script to make tps and bgmx trend forecast
cat("Running 023 TPS and BGMX trend forecast script...\n")
source(paste0(wd_code,"023_get_tps_bgmx.R"))

