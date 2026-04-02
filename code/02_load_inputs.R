
# Run WHO data processing script to get dt_deaths_who_long

source("020_get_deaths_who.R")

# Run GBD data processing script to get baseline_rates
source("021_get_base_rates.R")

# Run TPS transition probabilities script to get dt_tps
source("022_get_tps.R")

# Run script to make tps and bgmx trend forecast

source("023_get_tps_bgmx.R")

