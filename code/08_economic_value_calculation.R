
#Source
# https://github.com/achangdias/HLI_evam/blob/main/economic_value_calculation.R
# https://www.nature.com/articles/s41591-024-03253-7#code-availability

# Alternative
# https://www.nature.com/articles/s41591-024-03248-4?fromPaywallRec=false#code-availability
# https://github.com/sbolongaita/HLI_Cause-Specific-EVAM


rm(list = ls())

#### 0) setup #### 

library(data.table)
library(ggplot2)
library(tidyr)
library(wesanderson)
library(countrycode)
library(gridExtra)
library(grid)
library(dplyr)
library(viridis)
library(patchwork)

library(countrycode) # To convert map names to ISO3 codes

my_palette <- wes_palette("Zissou1", n=5, type="discrete")
my_palette2 <- wes_palette("Cavalcanti1", n=5, type="discrete")
my_palette3 <- wes_palette("IsleofDogs1", n=6, type="discrete")

SAVETAB <- FALSE # If false, running the code will not export any tables
SAVEFIG <- TRUE # If false, running the code will not export any graphs

#### 1) Definitions ####

# Baseline income elasticities for High-income (HIC) and low/middle-income (LMIC) countries

VSL_income_elast_above_USA <- 0.8
VSL_income_elast_below_USA <- 1.2

# Income elasticities for sensitivity analysis (applied to all countries)

VSL_income_elast_sensitivity_low <- 1
VSL_income_elast_sensitivity_high <- 1.5

# We consider 4 cases of annual discount rates: 0 (no discounting), 1%, 3%, and 5%.

DISCOUNT_RATES <- c(0, 0.01, 0.03, 0.05)

# The reference age for a representative individual a_hat is 40 

a_hat = 40

#### 2) Data preparation #### 

## 2.1 load mortality and frontier data ####  

mortality.data <- fread("avoidable_deaths.csv")

frontier.data <- fread("frontier_life_tables_N.csv")

data_map <- fread("cih_groups_wbinc20.csv")

# For this illustration, we are going to examine 3 years: 2000, 2019, and 2050.

years <- c(2000, 2019, 2021, 2050)
year_gap <- 2050 - 2019
mortality.data <- mortality.data[year %in% years]

# Extract list of countries

country.list <- unique(mortality.data$iso3)


## 2.2 Mortality calculations #### 

# Next, we calculate annual survival probabilities within age groups by assuming they are uniform

mortality.data[, S_a := npx^(1/Ageintervaln)]
mortality.data[age == 85, S_a := 1-mxn] # For the last age group, use raw mortality rates

    # npx: probability of survival of a person between the age x to x + n (i.e., 1-nqx = npx)


# Next, we do the same for frontier probabilities

mortality.data[, frontier_npx := 1 - frontier_nqx]
mortality.data[, frontier_S_a := frontier_npx^(1/Ageintervaln)]
mortality.data[age == 85, frontier_S_a := 1-frontier_mxn] # For the last age group, use raw mortality rates

# Calculate population weights for each age group for total population and for each sex.

mortality.data[,pop.weight:=p/sum(p), by=.(iso3, year)]
mortality.data[,pop.sex.weight:=p/sum(p), by=.(iso3, year, sex)]

# Calculate share of avoidable mortality

mortality.data[, share_avm:=av_deaths/deaths]

# Eliminating columns that will not be used anymore

mortality.data <- mortality.data[,.(country = iso3, year, sex, age, n=Ageintervaln, pop=p, pop.weight, pop.sex.weight, S_a, frontier_S_a, share_avm,
                                    av_deaths, deaths)]

# Calculate survival relative to the representative agent Sâ .
# Since we have values for each sex, the representative agent will be a weighted average of these populations.

mortality.data[age==a_hat, S_a_hat:=weighted.mean(S_a, pop), by=.(country, year)] # why age==a_hat?
mortality.data[, S_a_hat:=mean(S_a_hat, na.rm=T), by=.(country, year, sex)] # shouldn't this be S_a_hat_sex? 

## 2.3 economic data ####  

# The main source is WB data for GNI per capita, PPP (constant 2017 international $)
# new: https://data.worldbank.org/indicator/NY.GNP.PCAP.PP.KD
income.data <- fread("API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_5363529.csv", skip = 4, header = T)
income.data <- income.data %>%
  select(-`Country Name`, -`Indicator Name`, -`Indicator Code`, -V67) %>%
  rename(country = `Country Code`) %>%
  filter(country %in% c(country.list, "OED")) %>% ## Add OECD countries as a group (to get income for sensitivity analysis)
  pivot_longer(cols = -country,
               names_to = "year",
               values_to = "y_bar") %>%
  mutate(year = as.integer(year))

# Seven countries have no GNI data at all (also applies to GDP data)
no.income.data <- income.data %>%
  group_by(country) %>%
  summarise(
    obs = n(),
    missing = sum(is.na(y_bar))) %>%
  filter(obs == missing)

no.income.data

# We drop these countries due to insufficient data for the analysis.

income.data <- income.data %>%
  filter( !(country %in% no.income.data$country) ) %>%
  data.table()

rm(no.income.data)

# For those with missing data in 2000, look for the closest available data point

missing.2000 <- income.data %>%
  filter(year == 2000, is.na(y_bar)) %>%
  group_by(country) %>%
  select(country)

# Calculate closest available dates.

closest.dates.2000 <- income.data %>%
  filter(country %in% missing.2000$country) %>%
  filter(!is.na(y_bar)) %>%
  mutate(dist = 2000 - year) %>%
  group_by(country) %>%
  mutate(closest.dist = min(abs(dist))) %>%
  filter(closest.dist == abs(dist)) %>%
  data.table()

length(missing.2000$country) == nrow(closest.dates.2000)

# Note that there are no countries with equidistant available points (in fact, only Hungary has closest available date before 2000).

for (cty in closest.dates.2000$country) {
  imputed_value <- closest.dates.2000[country == cty, y_bar]
  income.data[country == cty & year == 2000, y_bar := imputed_value]
}

# Now do the same with 2019 and 2021 data

missing.2019 <- income.data %>%
  filter(year == 2019, is.na(y_bar)) %>%
  group_by(country) %>%
  select(country)

closest.dates.2019 <- income.data %>%
  filter(country %in% missing.2019$country) %>%
  filter(!is.na(y_bar)) %>%
  mutate(dist = 2019 - year) %>%
  group_by(country) %>%
  mutate(closest.dist = min(abs(dist))) %>%
  filter(closest.dist == abs(dist)) %>%
  data.table()

length(missing.2019$country) == nrow(closest.dates.2019)

for (cty in closest.dates.2019$country) {
  imputed_value <- closest.dates.2019[country == cty, y_bar]
  income.data[country == cty & year == 2019, y_bar := imputed_value]
}

missing.2021 <- income.data %>%
  filter(year == 2021, is.na(y_bar)) %>%
  group_by(country) %>%
  select(country)

closest.dates.2021 <- income.data %>%
  filter(country %in% missing.2021$country) %>%
  filter(!is.na(y_bar)) %>%
  mutate(dist = 2021 - year) %>%
  group_by(country) %>%
  mutate(closest.dist = min(abs(dist))) %>%
  filter(closest.dist == abs(dist)) %>%
  data.table()

length(missing.2021$country) == nrow(closest.dates.2021)

for (cty in closest.dates.2021$country) {
  imputed_value <- closest.dates.2021[country == cty, y_bar]
  income.data[country == cty & year == 2021, y_bar := imputed_value]
}

rm(missing.2000, missing.2019, closest.dates.2000, closest.dates.2019, closest.dates.2021, closest.dates.2021)

# Keep only 2000 and 2019 and 2021.
income.data <- income.data[year %in% c(2000, 2019, 2021)]


# For 2050 projections, we will use OECD growth % between 2021 and 2050 for OECD countries, and 
# the world average % for remaining 

# First, load OECD data. source: https://data.oecd.org/gdp/real-gdp-long-term-forecast.htm
data_oecd <- fread("oecd_gdp.csv")
names(data_oecd) <- c("iso3","indicator","subject","measure","freq","year","value", "flag")

oecd_iso3 <- data_oecd[, unique(iso3)]

data_oecd <- data_oecd[,.(iso3, year, value)]
data_oecd <- data_oecd[year == 2021 | year == 2050, ]

data_oecd_rate <- data.table(oecd_iso3)
names(data_oecd_rate) <- c("iso3")

for (i in oecd_iso3){
  dat <- data_oecd[iso3 == i, ]
  v1 <- dat[year == 2021, as.numeric(value)]
  v2 <- dat[year == 2050, as.numeric(value)]
  data_oecd_rate[iso3 == i, rate := (v2-v1)/v1]
  print(i)
}
data_oecd_rate
names(data_oecd_rate) <- c("country","rate")

# merge OECD data rates to GNI per capita 

data_gni_2050 <- income.data[year == 2021, ]
data_gni_2050[, year := 2050]
data_gni_2050 <- merge(data_gni_2050, data_oecd_rate, by="country", all.x = TRUE)
data_gni_2050[is.na(rate), unique(country)]
data_gni_2050[is.na(rate), rate := data_oecd_rate[country == "WLD", .(rate)]]
data_gni_2050[, gni_2050 := y_bar * (1+rate) ]

# merge 2050 back to income.data
data_gni_2050[, y_bar := NULL] ; data_gni_2050[, rate := NULL]
setnames(data_gni_2050, "gni_2050","y_bar")
income.data <- rbind(income.data, data_gni_2050)

income.groups <- fread("cih_groups_wbinc20.csv")
income.data <- merge(income.data,
                     income.groups[, .(country = iso3, income.group = wbinc20, wb.region = cihgroup)],
                     by="country")


#### 3) Pre-calculations #### 

## 3.1 Discounted life expectancy #### 

# Here, we calculate discounted life expectancy for each age group, based on the current and frontier mortality, 
# and for each β

# Since we will need to make a Cartesian product with β cases, we start by
# Cartesian product of mortality data and betas

mortality.data[,VAR:=1] # Auxiliary variable just for the product
mortality.data <- merge(mortality.data, data.table(disc.rate=DISCOUNT_RATES, VAR=1), by="VAR", allow.cartesian = T)
mortality.data[,VAR:=NULL]
mortality.data[, beta:=1/(1+disc.rate)] # note: beta is the discount factor

# We start with the last age group and proceed recursively. For group 85+, we have

mortality.data[age == 85, L_a:=1/(1-beta*S_a)]

# Then, recursively using the relation

setorder(mortality.data, country, year, beta, sex, -age)

# We cannot vectorize this because each age depends on a write-read-write cycle, so we need to iterate recursively

for (a in unique(mortality.data$age)) {
  mortality.data[, L_a:=ifelse(age == a & age != 85,
                               S_a * (1 - beta^n*S_a^n)/(1-beta*S_a) + beta^n*S_a^n * shift(L_a, 1),
                               L_a),
                 by=.(country, year, beta, sex)]
}

# Next, we do the same for the frontier.
# In some cases, certain age groups in specific countries might have a survival probability above the frontier.
# In these cases, we need to consider the actual survival probability so that we do not account for negative avoidable 
# mortality (or we’d be valuing increase in mortality). To do so, we set the survival probability for that age group to the frontier

mortality.data[S_a > frontier_S_a, S_a:=frontier_S_a]

mortality.data[age == 85, frontier_L_a:=1/(1-beta*frontier_S_a)]

setorder(mortality.data, country, year, beta, sex, -age)

# We cannot vectorize this because each age depends on a write-read-write cycle, so we need to iterate recursively
for (a in unique(mortality.data$age)) {
  mortality.data[, frontier_L_a:=ifelse(age == a & age != 85,
                                        frontier_S_a * (1 - beta^n*frontier_S_a^n)/(1-beta*frontier_S_a) + beta^n*frontier_S_a^n * shift(frontier_L_a, 1),
                                        frontier_L_a),
                 by=.(country, year, beta, sex)]
}

# Then, we calculate the decomposed effect only for the immediate mortality changes within an age group, calculated as

mortality.data[, dot_L_a:=frontier_S_a/S_a*L_a]

# To calculate La−n,a decomposition, it will be useful to store S0,a for all ages so that we can compute Sa−n,a=S0,a/S0,a=n

setorder(mortality.data, country, year, beta, sex, age)

mortality.data[age == 0, S_0a:=1] # Prob. of reaching age 0 is 1 by default
# We cannot vectorize this because each age depends on a write-read-write cycle, so we need to iterate recursively
for (a in unique(mortality.data$age)) {
  mortality.data[, S_0a:=ifelse(age == a & age > 0,
                                shift(S_a, 1)^shift(n, 1) * shift(S_0a, 1),
                                S_0a),
                 by=.(country, year, beta, sex)]
}

# Set the discounted life expectancy relative to the representative agent, Lâ , for each value of β
# Since we have values for each sex, the representative agent will be a weighted average of these populations.

mortality.data[age==a_hat, L_a_hat:=weighted.mean(L_a, pop), by=.(country, year, beta)]
mortality.data[, L_a_hat:=mean(L_a_hat, na.rm=T), by=.(country, year, sex, beta )]

## 3.2 VSL #### 

# We will expand income data to include 3 cases of elasticity. The baseline and two sensitivity cases. 
# The baseline has separate elasticity for countries with income above and below the USA.

US_GNIpc <- income.data[country == "USA", .(US_GNIpc=mean(y_bar)), by=year]

income.data <- merge(income.data, US_GNIpc, by="year")

baseline.income.data <- copy(income.data)

baseline.income.data[y_bar >= US_GNIpc ,inc.elasticity:=VSL_income_elast_above_USA]
baseline.income.data[y_bar < US_GNIpc ,inc.elasticity:=VSL_income_elast_below_USA]
baseline.income.data[,inc.elasticity.case:="B"]
baseline.income.data[,base.income.case:="US"]

# The two other cases have fixed elasticities for everyone.

low_elast.income.data <- copy(income.data)
low_elast.income.data[, inc.elasticity:=VSL_income_elast_sensitivity_low]
low_elast.income.data[,inc.elasticity.case:="L"]
low_elast.income.data[,base.income.case:="US"]

high_elast.income.data <- copy(income.data)
high_elast.income.data[, inc.elasticity:=VSL_income_elast_sensitivity_high]
high_elast.income.data[,inc.elasticity.case:="H"]
high_elast.income.data[,base.income.case:="US"]

# Stack these together to form the complete income data.

income.data <- rbind(baseline.income.data,
                     low_elast.income.data,
                     high_elast.income.data)

rm(baseline.income.data, low_elast.income.data, high_elast.income.data)

# Reorder regions for plotting

income.data$wb.region <- factor(income.data$wb.region,
                                levels = c("world",
                                           "ssa", 
                                           "india",
                                           "euroasia",
                                           "china",
                                           "lac",
                                           "high.income"),
                                labels = c("World",
                                           "Sub-Saharan\nAfrica",
                                           "India",
                                           "Euroasia &\nMediterranean",
                                           "China",
                                           "Latin America &\nCaribbean",
                                           "High-income"))
# To adjust the VSL, the reference value uses USA data and the ratio of VSL to income. Then, to get v̂ /y¯,
# we divide that ratio by Lâ according to the discount rate case.
# But first, we need to merge with discount rate cases now. Thus, the valuation dataset merges mortality and income data.

valuation.data <- merge(income.data, mortality.data, by=c("country", "year"), allow.cartesian = T)

# Reference ratios of VSL to income
US_VSLr <- 160
OECD_VSLr <- 100  

# Reference life expectancies
US_L_a_hat <- valuation.data[country == "USA", .(US_L_a_hat=mean(L_a_hat)), by=.(year, disc.rate)]
# OECD_L_a_hat <- valuation.data[country %in% oecd.country.list, .(OECD_L_a_hat=weighted.mean(L_a_hat, pop)), by=.(year, disc.rate)]

valuation.data <- merge(valuation.data, US_L_a_hat, by=c("year", "disc.rate"))
# valuation.data <- merge(valuation.data, OECD_L_a_hat, by=c("year", "disc.rate"))

# Calculate the elasticity-adjusted VSL, imputing minimum VLS/y of 20.

valuation.data[base.income.case=="US", v_hat:= (US_VSLr/US_L_a_hat) * y_bar * (y_bar/US_GNIpc)^(inc.elasticity-1)]
valuation.data[base.income.case=="OECD", v_hat:= (OECD_VSLr/OECD_L_a_hat) * y_bar * (y_bar/OECD_GNIpc)^(inc.elasticity-1)]

# Enforce minimum VLS_r >= 20 
valuation.data[base.income.case=="US", VSLr_hat:= US_VSLr * (y_bar/US_GNIpc)^(inc.elasticity-1)]
valuation.data[base.income.case=="OECD", VSLr_hat:= OECD_VSLr * (y_bar/OECD_GNIpc)^(inc.elasticity-1)]
valuation.data[VSLr_hat < 20 & base.income.case=="US", v_hat:= 20/US_L_a_hat * y_bar]
valuation.data[VSLr_hat < 20 & base.income.case=="OECD", v_hat:= 20/OECD_L_a_hat * y_bar]
valuation.data[VSLr_hat < 20, VSLr_hat:= 20]

valuation.data[, US_GNIpc:=NULL] # We won't need these columns anymore
valuation.data[, US_L_a_hat:=NULL] 
valuation.data[, OECD_GNIpc:=NULL]
valuation.data[, OECD_L_a_hat:=NULL] 


#### 4) Economic valuation #### 

# Here, we define the valuation functions. If “rho” is 0, it calculates linear approximation of this value 
# (or the traditional method). Otherwise, it calculates the value of mortality risk change with constant RRA, 
# relative to annual income under a new vector of survival probabilities.

calculate.b.lifecycle <- function(L_i, L_f, y_bar, v_hat, S_a_hat, rho) {
  if (rho == 0) {
    b <- v_hat/(L_f * y_bar) * (L_f - L_i)
  }
  
  else {
    # Share of annual income with risk neutrality
    b_0 <- v_hat / y_bar * (L_f - L_i) / L_f * (S_a_hat^2)
    
    if (rho == 1) { # Log utility
      b <- 1 - exp(-b_0)
    } else { # CRRA with rho != 1
      b <- 1 - ( 1 - (1-rho)*b_0 )^(1/(1-rho))  
    }
  }
  return(b)
}

# Alternatively, for the decomposition, we assume that the individual can only trade off current income for a current risk change

calculate.b.oneyear <- function(S_i, S_f, L_i, y_bar, v_hat, S_a_hat, rho) {
  if (rho == 0) {
    b <- v_hat/y_bar * (S_f/S_i*L_i - L_i) # The term within brackets is L_f - L_i
  }
  else {
    b_0_dot <- v_hat / y_bar * (S_a_hat^2) * L_i/S_i * (S_f - S_i)/S_f  
    
    if (rho == 1) { # Log utility
      b <- 1 - exp(-b_0_dot)
    } else { # CRRA with rho != 1
      b <- 1 - ( 1 - (1-rho)*b_0_dot )^(1/(1-rho))  
    }
  }
}

# Finally, we define a generic function that can call either of these based on “method” (lifecycle or oneyear) 
# and “rho” (0 for linear extrapolation, 1 for log, 2 for crra)

calculate.b <- function(method, S_i, S_f, L_i, L_f, y_bar, v_hat, S_a_hat, rho) {
  if(method == "lifecycle") calculate.b.lifecycle(L_i, L_f, y_bar, v_hat, S_a_hat, rho)
  else if(method == "oneyear") calculate.b.oneyear(S_i, S_f, L_i, y_bar, v_hat, S_a_hat, rho)
  else NULL
}

# All of the functions above calculate the value for a specific age. 
# Since we are dealing with age groups, we need to average them before aggregating to the all ages within that group. 
# This average needs to be weighted by the relative representation of each age within the group, 
# which we construct using the uniform one-year survival probabilities.

calculate.b.weighted <- function(n, beta, method, S_i, S_f, L_ai, L_af, y_bar, v_hat, S_a_hat, rho) {
  # If age group has n=1, no weighting is needed
  if (n == 1) {
    value <- calculate.b(method, S_i, S_f, L_ai, L_af, y_bar, v_hat, S_a_hat, rho)
  }
  else {
    # Calculate weight denominator
    w.denominator <- 0
    for(i in 1:n) w.denominator <- w.denominator + S_i^(i-1)
    
    # Iterate over ages and accumulate value
    value <- 0
    
    # First, for age a
    w <- 1/w.denominator
    L_i <- L_ai 
    L_f <- L_af
    
    value <- value + w*calculate.b(method, S_i, S_f, L_i, L_f, y_bar, v_hat, S_a_hat, rho)
    
    for(i in 2:n) {
      # Use recursive relations
      w <- w * S_i
      L_i <- (L_i - S_i)/(beta * S_i)
      L_f <- (L_f - S_f)/(beta * S_f)
      
      value <- value + w*calculate.b(method, S_i, S_f, L_i, L_f, y_bar, v_hat, S_a_hat, rho)
    }
  }
  return(value)
}

## 4.1 by country and age groups ####

# We use the function above to calculate the values, as shares of annual income (b), for each age group.

valuation.data[, b_constant:= calculate.b.lifecycle(L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 0)]
valuation.data[, b_log:= calculate.b.lifecycle(L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 1)]
valuation.data[, b_crra:= calculate.b.lifecycle(L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 2)]

# To obtain the value in terms of discounted income years (ϕ), we multiply by L̃ a

valuation.data[, phi_constant:=b_constant*frontier_L_a]
valuation.data[, phi_log:=b_log*frontier_L_a]
valuation.data[, phi_crra:=b_crra*frontier_L_a]

# Next, we calculate the one-year effects (decomposition)

valuation.data[, b_constant_dot:= calculate.b.oneyear(S_a, frontier_S_a, L_a, y_bar, v_hat, S_a_hat, rho = 0)]
valuation.data[, b_log_dot:= calculate.b.oneyear(S_a, frontier_S_a, L_a, y_bar, v_hat, S_a_hat, rho = 1)]
valuation.data[, b_crra_dot:= calculate.b.oneyear(S_a, frontier_S_a, L_a, y_bar, v_hat, S_a_hat, rho = 2)]

# Weighted average values within age groups. 
# We have to do case by case with “n” because it needs to be a scalar inside the function to iterate over. 
# For this reason, we need to do one pass for each different n and method. 
# As a convention, we consider age group 85+ as having length one 
# (this is, we give the same value for everyone in this age group as if they are 85. 
# Extending this length unreasonably increases the value because of truncation)

valuation.data[n==1 | n==-1, b_constant_1yr:= calculate.b.weighted(1, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 0)]
valuation.data[n==4, b_constant_1yr:= calculate.b.weighted(4, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 0)]
valuation.data[n==5, b_constant_1yr:= calculate.b.weighted(5, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 0)]

valuation.data[n==1 | n==-1, b_log_1yr:= calculate.b.weighted(1, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 1)]
valuation.data[n==4, b_log_1yr:= calculate.b.weighted(4, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 1)]
valuation.data[n==5, b_log_1yr:= calculate.b.weighted(5, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 1)]


valuation.data[n==1 | n==-1, b_crra_1yr:= calculate.b.weighted(1, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 2)]
valuation.data[n==4, b_crra_1yr:= calculate.b.weighted(4, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 2)]
valuation.data[n==5, b_crra_1yr:= calculate.b.weighted(5, beta, "oneyear", S_a, frontier_S_a, L_a, frontier_L_a, y_bar, v_hat, S_a_hat, rho = 2)]


## 4.2 by country ####

# Aggregate for the whole population without separating by sex.

agg.values <- valuation.data[,.(phi_constant = sum(phi_constant*pop.weight),
                                phi_log = sum(phi_log*pop.weight),
                                phi_crra = sum(phi_crra*pop.weight),
                                b_constant_1yr = sum(b_constant_1yr*pop.weight),
                                b_log_1yr = sum(b_log_1yr*pop.weight),
                                b_crra_1yr = sum(b_crra_1yr*pop.weight),
                                y_bar = mean(y_bar),
                                pop = sum(pop)), # To aggregate by region
                             by=.(country, income.group, wb.region, year, base.income.case, inc.elasticity.case, disc.rate)]

# Next, we aggregate the values by country, sex, income elasticity, and beta.

agg.values.bysex <- valuation.data[,.(phi_constant = sum(phi_constant*pop.sex.weight),
                                      phi_log = sum(phi_log*pop.sex.weight),
                                      phi_crra = sum(phi_crra*pop.sex.weight),
                                      b_constant_1yr = sum(b_constant_1yr*pop.sex.weight),
                                      b_log_1yr = sum(b_log_1yr*pop.sex.weight),
                                      b_crra_1yr = sum(b_crra_1yr*pop.sex.weight),
                                      y_bar = mean(y_bar),
                                      pop = sum(pop)), # To aggregate by region
                                   by=.(country, income.group, wb.region, year, base.income.case, inc.elasticity.case, disc.rate, sex)]

## 4.4 by WB region, weighted by country population 

agg.values.region <- agg.values[,.(phi_constant = weighted.mean(phi_constant, pop),
                                   phi_log = weighted.mean(phi_log, pop),
                                   phi_crra =weighted.mean(phi_crra, pop),
                                   b_constant_1yr = weighted.mean(b_constant_1yr, pop),
                                   b_log_1yr = weighted.mean(b_log_1yr, pop),
                                   b_crra_1yr = weighted.mean(b_crra_1yr, pop),
                                   y_bar = weighted.mean(y_bar, pop),
                                   pop = sum(pop)),
                                by=.(wb.region, year, base.income.case, inc.elasticity.case, disc.rate)]

agg.values.region.bysex <- agg.values.bysex[,.(phi_constant = weighted.mean(phi_constant, pop),
                                               phi_log = weighted.mean(phi_log, pop),
                                               phi_crra =weighted.mean(phi_crra, pop),
                                               b_constant_1yr = weighted.mean(b_constant_1yr, pop),
                                               b_log_1yr = weighted.mean(b_log_1yr, pop),
                                               b_crra_1yr = weighted.mean(b_crra_1yr, pop),
                                               y_bar = weighted.mean(y_bar, pop),
                                               pop = sum(pop)),
                                            by=.(wb.region, year, sex, base.income.case, inc.elasticity.case, disc.rate)]

# Add world aggregate as additional region
agg.values.region <- rbind(agg.values.region, 
                           agg.values[,.(wb.region = "World",
                                         phi_constant = weighted.mean(phi_constant, pop),
                                         phi_log = weighted.mean(phi_log, pop),
                                         phi_crra =weighted.mean(phi_crra, pop),
                                         b_constant_1yr = weighted.mean(b_constant_1yr, pop),
                                         b_log_1yr = weighted.mean(b_log_1yr, pop),
                                         b_crra_1yr = weighted.mean(b_crra_1yr, pop),
                                         y_bar = weighted.mean(y_bar, pop),
                                         pop = sum(pop)),
                                      by=.(year, base.income.case, inc.elasticity.case, disc.rate)])

agg.values.region.bysex <- rbind(agg.values.region.bysex, 
                                 agg.values.bysex[,.(wb.region = "World",
                                                     phi_constant = weighted.mean(phi_constant, pop),
                                                     phi_log = weighted.mean(phi_log, pop),
                                                     phi_crra =weighted.mean(phi_crra, pop),
                                                     b_constant_1yr = weighted.mean(b_constant_1yr, pop),
                                                     b_log_1yr = weighted.mean(b_log_1yr, pop),
                                                     b_crra_1yr = weighted.mean(b_crra_1yr, pop),
                                                     y_bar = weighted.mean(y_bar, pop),
                                                     pop = sum(pop)),
                                                  by=.(year, sex, base.income.case, inc.elasticity.case, disc.rate)])
write.csv(agg.values.region, "agg.values.region.csv")
write.csv(agg.values.region.bysex, "agg.values.region.bysex.csv")


# by income group 
agg.values.income <- agg.values[,.(phi_constant = weighted.mean(phi_constant, pop),
                                   phi_log = weighted.mean(phi_log, pop),
                                   phi_crra =weighted.mean(phi_crra, pop),
                                   b_constant_1yr = weighted.mean(b_constant_1yr, pop),
                                   b_log_1yr = weighted.mean(b_log_1yr, pop),
                                   b_crra_1yr = weighted.mean(b_crra_1yr, pop),
                                   y_bar = weighted.mean(y_bar, pop),
                                   pop = sum(pop)),
                                by=.(income.group, year, base.income.case, inc.elasticity.case, disc.rate)]

# by income and age 
agg.values.income_age <- valuation.data[,.(phi_constant = sum(phi_constant*pop.weight),
                  phi_log = sum(phi_log*pop.weight),
                  phi_crra = sum(phi_crra*pop.weight),
                  b_constant_1yr = sum(b_constant_1yr*pop.weight),
                  b_log_1yr = sum(b_log_1yr*pop.weight),
                  b_crra_1yr = sum(b_crra_1yr*pop.weight),
                  y_bar = mean(y_bar),
                  pop = sum(pop)), # To aggregate by region
               by=.(age, income.group, wb.region, year, base.income.case, inc.elasticity.case, disc.rate)]

dcast(
  agg.values.income_age[base.income.case == "US" & inc.elasticity.case=="B" & disc.rate==0.03 & year == 2019,
                    .(age, income.group, year, b_log_1yr = round(b_log_1yr,3)*100)],
  income.group~age,
  value.var="b_log_1yr"
)




#### 5) baseline graphs #### 

baseline_caption <- "Baseline parameters (3% discount, income elasticity of 0.8 or 1.2, base VSL is USA, exponential VSL decay)"

agg.values.region[, unique(wb.region)]
agg.values.region[wb.region == "Euroasia &\nMediterranean", wb.region := "Eurasia &\nMediterranean"]
agg.values.region[, wb.region := factor(wb.region, levels = c("World", "Sub-Saharan\nAfrica",
                                                              "India", "Eurasia &\nMediterranean",
                                                              "China","Latin America &\nCaribbean",
                                                              "High-income"))]


agg.values.region.bysex[wb.region == "Euroasia &\nMediterranean", wb.region := "Eurasia &\nMediterranean"]
agg.values.region.bysex[, wb.region := factor(wb.region, levels = c("World", "Sub-Saharan\nAfrica",
                                                              "India", "Eurasia &\nMediterranean",
                                                              "China","Latin America &\nCaribbean",
                                                              "High-income"))]

## 5.2 bar chart by region and year  ####
my_fill_no_title<-scale_fill_gradientn(
  name = "",
  breaks  = seq(5, 55, 5), #c(5, 8, 12, 15, 18),
  limits  = c(qmin, qmax),
  labels  = seq(5, 55, 5), #c(5, 8, 12, 15, 18),
  colours = my_palette, 
  na.value = "gray")

for (y in years) {
  g <- ggplot(agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == y]) + 
    #geom_col(aes(y = wb.region, x =phi_log), fill = my_palette[1]) +
    #scale_x_continuous(limits = c(0,16), breaks=c(0, 5, 10, 15)) +
    geom_col(aes(y = wb.region, x = b_log_1yr*100), fill = my_palette[1]) +
    scale_x_continuous(limits = c(0,50), breaks=seq(0, 50, 10)) +
    scale_y_discrete(limits = rev) +
    #labs(x = "Present value / income", y = NULL,
    labs(x = "Percentage of annual of income (%)", y = NULL,
         title = paste0("Value of avoidable mortality, year ",  y),
         caption = baseline_caption) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 7),
          axis.text.x = element_text(size = 10))
  
  if(SAVEFIG) ggsave(paste0("output/agg_bdot_by_region_baseline_",y,".pdf"),
                     width = 6.5, height = 4)
}

for (y in years) {
  g <- ggplot(agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == y]) + 
    geom_col(aes(y = wb.region, x = b_log_1yr*100), fill = my_palette[1]) +
    #scale_x_continuous(limits = c(0,16), breaks=c(0, 5, 10, 15)) +
    scale_y_discrete(limits = rev) +
    labs(x = "Percent of annual income (%)", y = NULL,
         title = paste0("Value of avoidable mortality, year ",  y),
         caption = baseline_caption) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 7),
          axis.text.x = element_text(size = 10))
  
  if(SAVEFIG) ggsave(paste0("output/agg_b_by_region_baseline_",y,".pdf"),
                     width = 6.5, height = 4)
}

# Bar chart with values by region and sex
for (y in years) {
  g <- ggplot(agg.values.region.bysex[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == y]) + 
    geom_bar(aes(x = as.factor(sex), y = b_log_1yr*100, fill=as.factor(sex)), width=0.9, position=position_dodge(width=0.75), stat="identity") +
    scale_y_continuous(breaks=seq(0, 50, 10)) +
    scale_x_discrete(limits = rev) +
    scale_fill_manual(name="", values = c(my_palette3[1], my_palette3[3]), labels = c("Male", "Female")) +
    #labs(x = "Present value / income", y = NULL,
    labs(y = "Percent of annual of income (%)", x = NULL) +
    theme_minimal() +
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.major.x = element_blank(),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          strip.text = element_text(size = 6),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 9),
          axis.text.x = element_blank()) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    facet_wrap(~wb.region, ncol = 7, scales = "fixed")
  # if(SAVEFIG) ggsave(paste0("output/graphs/agg_value_by_region_sex_baseline_",y,".pdf"),
  if(SAVEFIG) ggsave(paste0("output/agg_bdot_by_region_sex_baseline_",y,".pdf"),
                     width = 6.5, height = 4)
}

g1 <- ggplot(agg.values.region.bysex[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 &
                                      year == 2000, ]) + 
  geom_bar(aes(x = as.factor(sex), y = b_log_1yr*100, fill=as.factor(sex)), width=0.9, position=position_dodge(width=0.75), stat="identity") +
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(name="", values = c(my_palette3[1], my_palette3[3]), labels = c("Male", "Female")) +
  #labs(x = "Present value / income", y = NULL,
  labs(y = "Percent of annual of income (%)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  facet_wrap(~wb.region, ncol = 7, scales = "fixed")
g1

g1 <- ggplot(agg.values.region.bysex[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 &
                                       year == 2000, ]) + 
  geom_bar(aes(x = as.factor(sex), y = b_log_1yr*100, fill=as.factor(sex)), width=0.9, position=position_dodge(width=0.75), stat="identity") +
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  ylim(0,50) + 
  scale_x_discrete(limits = rev) +
  scale_fill_manual(name="", values = c(my_palette3[1], my_palette3[3]), labels = c("Male", "Female")) +
  #labs(x = "Present value / income", y = NULL,
  labs(y = "Percent of annual of income (%)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  facet_wrap(~wb.region, ncol = 7, scales = "fixed")+ 
  ggtitle("A: 2000")

g1

g2 <- ggplot(agg.values.region.bysex[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 &
                                       year == 2019, ]) + 
  geom_bar(aes(x = as.factor(sex), y = b_log_1yr*100, fill=as.factor(sex)), width=0.9, position=position_dodge(width=0.75), stat="identity") +
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  ylim(0,50) + 
  scale_x_discrete(limits = rev) +
  scale_fill_manual(name="", values = c(my_palette3[1], my_palette3[3]), labels = c("Male", "Female")) +
  #labs(x = "Present value / income", y = NULL,
  labs(y = "Percent of annual of income (%)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  facet_wrap(~wb.region, ncol = 7, scales = "fixed")+ 
  ggtitle("B: 2019")
g2
g3 <- ggplot(agg.values.region.bysex[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 &
                                       year == 2050, ]) + 
  geom_bar(aes(x = as.factor(sex), y = b_log_1yr*100, fill=as.factor(sex)), width=0.9, position=position_dodge(width=0.75), stat="identity") +
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  ylim(0,50) + 
  scale_x_discrete(limits = rev) +
  scale_fill_manual(name="", values = c(my_palette3[1], my_palette3[3]), labels = c("Male", "Female")) +
  #labs(x = "Present value / income", y = NULL,
  labs(y = "Percent of annual of income (%)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  facet_wrap(~wb.region, ncol = 7, scales = "fixed") + 
  ggtitle("C: 2050")

## 5.3 range chart by region and year ####
year.pallete <-  hcl.colors(7, "BluYl")

# exclude 2021 here
g <- ggplot(agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03]) + 
  #geom_bar(aes(y = wb.region, x = phi_log, fill=as.factor(year)), width=0.7, position=position_dodge(width=0.75), stat="identity") +
  geom_bar(aes(x = as.factor(year), y = b_log_1yr*100, fill=as.factor(year)), width=0.85, position=position_dodge(width=0.85), stat="identity") +
  #scale_x_continuous(limits = c(0,16), breaks=c(0, 5, 10, 15)) +
  scale_y_continuous(limits = c(0,47), breaks=seq(0, 40, 10)) +
  #scale_x_discrete(limits = rev) +
  labs(y = "Percentage of annual of income (%)", x = NULL) +
  scale_fill_manual(name = "Year", values =year.pallete[c(6,1,4,2)], labels=c(2000, 2019, 2021, 2040)) +
  theme_minimal() + 
  theme(legend.position = "hide",
        strip.text = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9)) +
  facet_grid(~wb.region)

#if(SAVEFIG) ggsave(paste0("output/graphs/agg_value_by_region_year_baseline.pdf"),
if(SAVEFIG) ggsave(paste0("output/agg_bdot_by_region_year_baseline.pdf"),
                   width = 7.5, height = 3)

## 5.4 column chart by age and region ####
# Aggregate data to region with separate ages

agg.values.region.byage <- rbind(valuation.data[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03,
                                                .(phi_log = weighted.mean(phi_log, pop),
                                                  b_log_1yr = weighted.mean(b_log_1yr, pop),
                                                  share_avm = weighted.mean(share_avm, pop),
                                                  S_a = weighted.mean(S_a, pop),
                                                  frontier_S_a = weighted.mean(frontier_S_a, pop),
                                                  age_pop = sum(pop)), # To aggregate to world
                                                by=.(wb.region, age, year)],
                                 valuation.data[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03,
                                                .(wb.region = "World", 
                                                  phi_log = weighted.mean(phi_log, pop),
                                                  b_log_1yr = weighted.mean(b_log_1yr, pop),
                                                  S_a = weighted.mean(S_a, pop),
                                                  frontier_S_a = weighted.mean(frontier_S_a, pop),
                                                  share_avm = weighted.mean(share_avm, pop),
                                                  age_pop = sum(pop)), # To aggregate to world
                                                by=.(age, year)])

agg.values.region.byage[, SMU_change:=(frontier_S_a - S_a)*10000]

agg.values.region.byage[wb.region == "Euroasia &\nMediterranean", wb.region := "Eurasia &\nMediterranean"]
agg.values.region.byage[, wb.region := factor(wb.region, levels = c("World", "Sub-Saharan\nAfrica",
                                                                    "India", "Eurasia &\nMediterranean",
                                                                    "China","Latin America &\nCaribbean",
                                                                    "High-income"))]

# Discretize the SMU change for plotting
agg.values.region.byage[, SMU_change_disc:=(SMU_change %/% 250) * 250] # Bracket into groups of five]
smu.breaks <- seq(250, 1500, 250)

for (y in years){
  g <- ggplot(agg.values.region.byage[wb.region != "World" & year == y]) + 
    geom_bar(aes(x = as.factor(age), y = b_log_1yr*100, fill = as.factor(SMU_change_disc)), width=0.75, stat="identity") +
    scale_x_discrete(breaks=c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85)) +
    labs(x = "Group (starting age)", y = "Percent of annual income (%)") +
    scale_fill_manual(
      name = "Change in survival probability (SMUs)",
      labels = smu.breaks,
      values = hcl.colors(n=7, palette="Reds-3", rev=T)[2:7],
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70 / length(smu.breaks), units = "mm"),
        title.position = 'left',
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        label.position = "bottom"
      )) +
    theme_minimal() + 
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 7),
          axis.text.x = element_text (size = 8)) +
    facet_wrap(~wb.region, ncol = 3, scales = "free_x")
  
  if(SAVEFIG) ggsave(paste0("output/agg_bdot_by_region_age_baseline_",y,".pdf"),
                     width = 6.5, height = 5)
  
  g <- ggplot(agg.values.region.byage[wb.region == "World" & year == y]) + 
    geom_bar(aes(x = as.factor(age), y = b_log_1yr*100, fill =  as.factor(SMU_change_disc)), width=0.75, stat="identity") +
    scale_x_discrete(breaks=c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85)) +
    labs(x = "Group (starting age)", y = "Percent of annual income (%)") +
    scale_fill_manual(
      name = "Change in survival probability (SMUs)",
      labels = smu.breaks,
      values = hcl.colors(n=7, palette="Reds-3", rev=T)[2:7],
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70 / length(smu.breaks), units = "mm"),
        title.position = 'left',
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        label.position = "bottom"
      )) +
    theme_minimal() + 
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 7),
          axis.text.x = element_text (size = 8))
  
  if(SAVEFIG) ggsave(paste0("output/agg_value_world_by_age_baseline_",y,".pdf"),
                     width = 6.5, height = 5)
}

g <- ggplot(agg.values.region.byage[wb.region == "World"]) + 
  geom_bar(aes(x = as.factor(age), y = b_log_1yr*100, fill = share_avm*100), width=0.75, stat="identity") +
  scale_x_discrete(breaks=c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85)) +
  #  scale_y_continuous(limits = c(0,15), breaks=c(0, 5, 10, 15)) +
  labs(x = "Age", y = "Percentage of annual income",
       title = "Value of avoidable mortality, 2019",
       subtitle = "World, population-weighted averages",
       caption = baseline_caption) +
  scale_fill_gradientn(
    name = "Percentage of deaths in age group that are avoidable",
    colours = wes_palette("Zissou1", n=20, type="continuous")) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 8))

if(SAVEFIG) ggsave(paste0("output/agg_bdot_world_by_age_baseline.pdf"),
                   width = 6.5, height = 5)


## 5.5 contribution by age group ####
# Stack age contributions to the average value. Need to group ages more for a reasonable plot
hli.age.groups <- data.table(
  age = c(0, 1, seq(5, 85, 5)),
  group2 = factor(c(rep("0",1),
                    rep("01-09",2),
                    rep("10-19", 2),
                    rep("20-39", 4),
                    rep("40-59", 4),
                    rep("60-79", 4),
                    rep("80+", 2)))
)

agg.values.region.bygroup <- merge(agg.values.region.byage, hli.age.groups, by="age")

agg.values.region.bygroup <- agg.values.region.bygroup[, .(phi_log = weighted.mean(phi_log, age_pop),
                                                           b_log_1yr = weighted.mean(b_log_1yr, age_pop),
                                                           group_pop = sum(age_pop)),
                                                       by=.(wb.region, group2, year)]


agg.values.region.bygroup[, group.pop.weight:=group_pop/sum(group_pop), by=.(wb.region, year)]
agg.values.region.bygroup[, group.pop.weight.cum:=cumsum(group.pop.weight), by=.(wb.region, year)]

agg.values.region.bygroup[, contrib:=b_log_1yr*group.pop.weight]
agg.values.region.bygroup[, contrib.share:=contrib/sum(contrib), by=.(wb.region, year)]
agg.values.region.bygroup[, contrib.share.cum:=cumsum(contrib.share), by=.(wb.region, year)]

# Leverage
agg.values.region.bygroup[, contrib.leverage:=contrib.share/group.pop.weight]
write.csv(agg.values.region.bygroup, "fig8_2023sep.csv")
g <- ggplot(agg.values.region.bygroup[year == 2019]) + 
  #geom_bar(aes(y = wb.region, x = phi_log*group.pop.weight, fill=group2), width=0.8, stat="identity") +
  geom_bar(aes(x = wb.region, y = contrib*100, fill=group2), width=0.8, stat="identity") +
  #scale_x_continuous(limits = c(0,16), breaks=c(0, 5, 10, 15)) +
  #scale_x_discrete(limits = rev) +
  #labs(y = "", x = "Present value / income",
  labs(x = NULL, y = "Percentage of annual of income (%)") +
  scale_fill_manual(limits = rev,
                    name = "Age group",
                    values = hcl.colors(7, "Viridis")) +
  theme_minimal() + 
  theme(legend.position = "right",
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 8, angle = -60, hjust = -0.2, vjust = 0.6))

#if(SAVEFIG) ggsave(paste0("output/graphs/agg_value_region_age_stacked_baseline.pdf"),
if(SAVEFIG) ggsave(paste0("output/agg_bdot_region_age_stacked_baseline.pdf"),
                   width = 6, height = 5)

melt.contribs.table <- melt(agg.values.region.bygroup[, .(group2, wb.region, year, contrib.share, group.pop.weight)],
                            id.vars = c("wb.region", "group2", "year"))

melt.contribs.table[variable == "contrib.share", variable:= "Contribution to total value"]
melt.contribs.table[variable == "group.pop.weight", variable:= "Share of population"]
write.csv(melt.contribs.table, "fig8_2023sep.csv")
for (y in years) {
  g <- ggplot(agg.values.region.bygroup[year == y]) + 
    geom_bar(aes(x = "Value", y = contrib.share*100, fill=group2), width=0.7, stat="identity") +
    geom_bar(aes(x = "Population", y = group.pop.weight*100, fill=group2), width=0.7, stat="identity") +
    geom_segment(aes(x = 1+0.35, xend=2-0.35, y = (1-contrib.share.cum)*100, yend = (1-group.pop.weight.cum)*100),
                 size = 0.3, color = "gray60") +
    scale_y_continuous(limits = c(0,101), breaks=seq(0,100,20)) +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Percentage (%)") +
    scale_fill_manual(limits = rev,
                      name = "Age group",
                      values = hcl.colors(7, "Viridis")) +
    theme_minimal() + 
    theme(legend.position = "right",
          panel.grid.major.x = element_blank(),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          strip.text = element_text(size = 6),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 9),
          axis.text.x = element_text (size = 7, angle = -45, hjust = 0, vjust = 0.5, margin=unit(c(-0.3,0,0,0), 'cm'))) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    facet_wrap(~wb.region, ncol = 7, scales = "fixed")
  
  
  if(SAVEFIG) ggsave(paste0("output/agg_bdot_regions_contribution_population_by_age_baseline_",y,".pdf"),
                     width = 6.5, height = 4)
}

contrib.palette <- hcl.colors(5, "Earth")

g <- ggplot(melt(agg.values.region.bygroup[wb.region == "World", .(group2, contrib.share, group.pop.weight)],
                 id.vars = c("group2")),
            aes(x = group2)) + 
  geom_bar(aes(y = value*100, fill=variable), width=0.75, stat="identity", position="dodge") +
  scale_x_discrete(labels = c("0-04", "05-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) +
  scale_y_continuous(limits = c(0,25), breaks=seq(0, 30, 10)) +
  labs(x = "Age", y = "Percentages (%)") +
  scale_fill_manual(
    name = "",
    labels = c("Contribution to value", "Share of population"),
    values = c("contrib.share" = contrib.palette[5], "group.pop.weight" = contrib.palette[2])) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 8),
        panel.grid.major.x = element_blank())

if(SAVEFIG) ggsave(paste0("output/agg_bdot_world_contribution_population_by_age_baseline.pdf"),
                   width = 6.5, height = 5)

g <- ggplot(melt(agg.values.region.bygroup[wb.region != "World", .(group2, wb.region, contrib.share, group.pop.weight)],
                 id.vars = c("wb.region", "group2")),
            aes(x = group2)) + 
  geom_bar(aes(y = value*100, fill=variable), width=0.75, stat="identity", position="dodge") +
  scale_x_discrete(labels = c("0-04", "05-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) +
  scale_y_continuous(breaks=seq(0, 30, 10)) +
  labs(x = "Age", y = "Percentages (%)") +
  scale_fill_manual(
    name = "",
    labels = c("Contribution to value", "Share of population"),
    values = c("contrib.share" = contrib.palette[5], "group.pop.weight" = contrib.palette[2])) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        legend.text = element_text(size = 8),
        axis.text.x = element_text (size = 7),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~wb.region, ncol = 2, scales = "free")

if(SAVEFIG) ggsave(paste0("output/agg_bdot_regions_contribution_population_by_age_baseline2.pdf"),
                   width = 6.5, height = 5)


#### 6) sensitivity analyses #### 

agg.values.region[base.income.case == "US" & inc.elasticity.case == "B" & 
                    disc.rate == 0.03 & year == 2019, .(wb.region, year, b_log_1yr, b_constant_1yr, b_crra_1yr)]

agg.values.region[wb.region == "Euroasia &\nMediterranean", wb.region := "Eurasia &\nMediterranean"]
# 5.1 VSL decay (utility function)

method.palette <- hcl.colors(5, "Heat")

g <- ggplot(data=agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == 2019],  aes(y=wb.region)) +
  geom_errorbar(aes(xmin=phi_crra, xmax=phi_constant), size = 0.3, width = 0, color = 'gray30') +
  geom_point(aes(x=phi_constant, color="Linear", shape="Linear"), size=3) +
  geom_point(aes(x=phi_log, color="Exponential", shape="Exponential"), size=3) +
  geom_point(aes(x=phi_crra, color="Hyperbolic", shape="Hyperbolic"), size=3) +
  labs(y = "", x = "Present value / income",
       title = "Value of avoidable mortality",
       subtitle = "Changes from the baseline for different VSL decay methods",
       caption = "Parameters: 2019 data, 3% discount, income elasticity of 0.8 or 1.2, base VSL is USA") +
  scale_color_manual(name = "VSL decay method",
                     values = c("Hyperbolic" = method.palette[3],
                                "Exponential" = 'black',
                                "Linear" = method.palette[1])) +
  scale_shape_manual(name = "VSL decay method",
                     values = c("Hyperbolic" = 18,
                                "Exponential" = 19,
                                "Linear" = 18)) +
  #scale_x_continuous(limits = c(7,19.5), breaks=seq(8, 18, 2)) +
  scale_x_continuous(limits = c(0,33), breaks=seq(0, 30, 5)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))
g
if(SAVEFIG) ggsave("sensitivity analysis/SA_method.pdf",
                   width = 6.5, height = 4)
# jpeg("sensitivity analysis/SA_method.jpg", width = 6.5, height = 4, units = 'in', res = 300)
# g
# dev.off


# B dot
method.palette <- hcl.colors(5, "Heat")

g1 <- ggplot(data=agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == 2019],  aes(y=wb.region)) +
  geom_errorbar(aes(xmin=b_crra_1yr*100, xmax=b_constant_1yr*100), size = 0.3, width = 0, color = 'gray30') +
  geom_point(aes(x=b_constant_1yr*100, color="None (fixed VSL)", shape="None (fixed VSL)"), size=3) +
  geom_point(aes(x=b_log_1yr*100, color="Logarithmic", shape="Logarithmic"), size=3) +
  geom_point(aes(x=b_crra_1yr*100, color="Reciprocal", shape="Reciprocal"), size=3) +
  labs(y = "", x = "Percent of annual income (%)",
       subtitle = "A: VSL decay methods") +
  scale_color_manual(name = "Risk size adjustment",
                     values = c("Reciprocal" = method.palette[3],
                                "Logarithmic" = 'black',
                                "None (fixed VSL)" = method.palette[1])) +
  scale_shape_manual(name = "Risk size adjustment",
                     values = c("Reciprocal" = 18,
                                "Logarithmic" = 19,
                                "None (fixed VSL)" = 18)) +
  scale_x_continuous(limits = c(0,60), breaks=seq(0, 60, 10)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))

if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_bdot_method.pdf"),
                   width = 6.5, height = 4)

# 5.2 discount rate 
disc.palette <- hcl.colors(5, "Heat")

g <- ggplot() +
  geom_errorbar(data=agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" &  year == 2019, .(Vmin = min(phi_log), Vmax = max(phi_log)), by=wb.region],
                aes(y=wb.region, xmin = Vmin, xmax = Vmax), size = 0.3, width = 0, color = 'gray30') +
  geom_point(data=agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" &  year == 2019],
             aes(y=wb.region, x=phi_log, color=as.factor(disc.rate), shape=as.factor(disc.rate)), size=3) +
  labs(y = "", x = "Present value / income",
       title = "Value of avoidable mortality",
       subtitle = "Changes from the baseline for different discount rates",
       caption = "Parametes: 2019 data, income elasticity of 0.8 or 1.2, base VSL is USA, exponential VSL decay") +
  scale_color_manual(name = "Discount rate", labels = c("0%", "1%", "3%", "5%"),
                     values = c(disc.palette[1], disc.palette[2], 'black', disc.palette[3])) +
  scale_shape_manual(name = "Discount rate", labels = c("0%", "1%", "3%", "5%"),
                     values = c(18, 18, 19, 18)) +
  #scale_x_continuous(limits = c(5,33), breaks=seq(5, 30, 5)) +
  scale_x_continuous(limits = c(0,33), breaks=seq(0, 30, 5)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))

if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_discount_rate.pdf"),
                   width = 6.5, height = 4)

disc.palette <- hcl.colors(5, "Heat")

g2 <- ggplot() +
  geom_errorbar(data=agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" &  year == 2019, .(Vmin = min(b_log_1yr*100), Vmax = max(b_log_1yr*100)), by=wb.region],
                aes(y=wb.region, xmin = Vmin, xmax = Vmax), size = 0.3, width = 0, color = 'gray30') +
  geom_point(data=agg.values.region[inc.elasticity.case == "B" & base.income.case == "US" &  year == 2019],
             aes(y=wb.region, x=b_log_1yr*100, color=as.factor(disc.rate), shape=as.factor(disc.rate)), size=3) +
  labs(y = "", x = "Percent of annual income (%)",
       subtitle = "B: Discount rates") +
  scale_color_manual(name = "Discount rate", labels = c("0%", "1%", "3%", "5%"),
                     values = c(disc.palette[1], disc.palette[2], 'black', disc.palette[3])) +
  scale_shape_manual(name = "Discount rate", labels = c("0%", "1%", "3%", "5%"),
                     values = c(18, 18, 19, 18)) +
  scale_x_continuous(limits = c(0,60), breaks=seq(0, 60, 10)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))
g2
if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_bdot_discount_rate.pdf"),
                   width = 6.5, height = 4)


# 5.3 elasticity

elast.palette <- hcl.colors(5, "Heat")

g <- ggplot() +
  geom_errorbar(data=agg.values.region[base.income.case == "US" & disc.rate == 0.03 &  year == 2019, .(Vmin = min(phi_log), Vmax = max(phi_log)), by=wb.region],
                aes(y=wb.region, xmin = Vmin, xmax = Vmax), size = 0.3, width = 0, color = 'gray30') +
  geom_point(data=agg.values.region[base.income.case == "US" & disc.rate == 0.03 &  year == 2019],
             aes(y=wb.region, x=phi_log, color=as.factor(inc.elasticity.case), shape=as.factor(inc.elasticity.case)), size=3) +
  labs(y = "", x = "Present value / income",
       title = "Value of avoidable mortality",
       subtitle = "Changes from the baseline for different VSL income elasticities",
       caption = "Parametes: 2019 data, 3% discount, base VSL is USA, exponential VSL decay") +
  scale_color_manual(name = "Income elasticity", labels = c("H"="1.5", "B"="0.8 (below USA) or 1.2 (above USA)", "L"="1.0"),
                     values = c("H"=elast.palette[3], "B"='black', "L"=elast.palette[1])) +
  scale_shape_manual(name = "Income elasticity", labels = c("H"="1.5", "B"="0.8 (below USA) or 1.2 (above USA)", "L"="1.0"),
                     values = c("H"=18, "B"=19, "L"=18)) +
  #scale_x_continuous(limits = c(5,20), breaks=seq(5, 20, 5)) +
  scale_x_continuous(limits = c(0,33), breaks=seq(0, 30, 5)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))

if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_income_elasticity.pdf"),
                   width = 6.5, height = 4)

elast.palette <- hcl.colors(5, "Heat")

g3 <- ggplot() +
  geom_errorbar(data=agg.values.region[base.income.case == "US" & disc.rate == 0.03 &  year == 2019, .(Vmin = min(b_log_1yr*100), Vmax = max(b_log_1yr*100)), by=wb.region],
                aes(y=wb.region, xmin = Vmin, xmax = Vmax), size = 0.3, width = 0, color = 'gray30') +
  geom_point(data=agg.values.region[base.income.case == "US" & disc.rate == 0.03 &  year == 2019],
             aes(y=wb.region, x=b_log_1yr*100, color=as.factor(inc.elasticity.case), shape=as.factor(inc.elasticity.case)), size=3) +
  labs(y = "", x = "Percent of annual income (%)",
       subtitle = "C: Income elasticities") +  
  scale_color_manual(name = "Income elasticity", labels = c("H"="1.5", "B"="0.8 (above USA) or 1.2 (below USA)", "L"="1.0"),
                                                                         values = c("H"=elast.palette[3], "B"='black', "L"=elast.palette[1])) +
  scale_shape_manual(name = "Income elasticity", labels = c("H"="1.5", "B"="0.8 (above USA) or 1.2 (below USA)", "L"="1.0"),
                     values = c("H"=18, "B"=19, "L"=18)) +
  scale_x_continuous(limits = c(0,60), breaks=seq(0, 60, 10)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))

if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_bdot_income_elasticity.pdf"),
                   width = 6.5, height = 4)

# 5.4 base VSL
basevsl.palette <- hcl.colors(5, "Heat")

g <- ggplot() +
  geom_errorbar(data=agg.values.region[inc.elasticity.case == "B" & disc.rate == 0.03 &  year == 2019, .(Vmin = min(phi_log), Vmax = max(phi_log)), by=wb.region],
                aes(y=wb.region, xmin = Vmin, xmax = Vmax), size = 0.3, width = 0, color = 'gray30') +
  geom_point(data=agg.values.region[inc.elasticity.case == "B" & disc.rate == 0.03 &  year == 2019],
             aes(y=wb.region, x=phi_log, color=as.factor(base.income.case), shape=as.factor(base.income.case)), size=3) +
  labs(y = "", x = "Present value / income",
       title = "Value of avoidable mortality",
       subtitle = "Changes from the baseline for different base VSLs",
       caption = "Parametes: 2019 data, 3% discount, income elasticity of 0.8 or 1.2, exponential VSL decay") +
  scale_color_manual(name = "Base VSL", labels = c("OECD"="OECD mean income x 100", "US"="USA mean income x 160"),
                     values = c("OECD"=elast.palette[3], "US"='black')) +
  scale_shape_manual(name = "Base VSL", labels = c("OECD"="OECD mean income x 100", "US"="USA mean income x 160"),
                     values = c("OECD"=18, "US"=19)) +
  #scale_x_continuous(limits = c(6,14), breaks=seq(6, 14, 2)) +
  scale_x_continuous(limits = c(0,33), breaks=seq(0, 30, 5)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))

if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_base_VSL.pdf"),
                   width = 6.5, height = 4)

basevsl.palette <- hcl.colors(5, "Heat")

g4 <- ggplot() +
  geom_errorbar(data=agg.values.region[inc.elasticity.case == "B" & disc.rate == 0.03 &  year == 2019, .(Vmin = min(b_log_1yr*100), Vmax = max(b_log_1yr*100)), by=wb.region],
                aes(y=wb.region, xmin = Vmin, xmax = Vmax), size = 0.3, width = 0, color = 'gray30') +
  geom_point(data=agg.values.region[inc.elasticity.case == "B" & disc.rate == 0.03 &  year == 2019],
             aes(y=wb.region, x=b_log_1yr*100, color=as.factor(base.income.case), shape=as.factor(base.income.case)), size=3) +
  labs(y = "", x = "Percent of annual income (%)",
       subtitle = "D: Base VSLs") +
  scale_color_manual(name = "Base VSL", labels = c("OECD"="OECD mean income x 100", "US"="USA mean income x 160"),
                     values = c("OECD"=elast.palette[3], "US"='black')) +
  scale_shape_manual(name = "Base VSL", labels = c("OECD"="OECD mean income x 100", "US"="USA mean income x 160"),
                     values = c("OECD"=18, "US"=19)) +
  scale_x_continuous(limits = c(0,60), breaks=seq(0, 60, 10)) +
  scale_y_discrete(limits = rev) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 7),
        axis.text.x = element_text (size = 10))

if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_bdot_base_VSL.pdf"),
                   width = 6.5, height = 4)

jpeg("fig_sensitivity.jpg", width = 10, height = 10, units = 'in', res = 300)
(g1 + g2) / (g3 + g4)
dev.off()

# 5.5 combined 

agg.values.region[base.income.case == "US", base.income.case.label:= "USA mean income x 160"]
agg.values.region[base.income.case == "OECD", base.income.case.label:= "OECD mean income x 100"]
agg.values.region$base.income.case.label <- factor(agg.values.region$base.income.case.label,
                                                   levels = c("USA mean income x 160", "OECD mean income x 100"))

agg.values.region[disc.rate == 0.00, disc.rate.x_coord:=0]
agg.values.region[disc.rate == 0.01, disc.rate.x_coord:=1]
agg.values.region[disc.rate == 0.03, disc.rate.x_coord:=2]
agg.values.region[disc.rate == 0.05, disc.rate.x_coord:=3]


elast.palette <- hcl.colors(5, "Heat")

# g <- ggplot(data=agg.values.region[year == 2019],  aes(x=as.factor(disc.rate*100))) +
g <- ggplot(data=agg.values.region[year == 2019]) +
  geom_point(aes(x=disc.rate.x_coord-0.25, y=b_constant_1yr*100, color=inc.elasticity.case, shape="None (fixed VSL)"), size=1.5, alpha = 0.8) +
  geom_point(aes(x=disc.rate.x_coord, y=b_log_1yr*100, color=inc.elasticity.case, shape="Logarithmic"), size=1.5, alpha = 0.8) +
  geom_point(aes(x=disc.rate.x_coord+0.25, y=b_crra_1yr*100, color=inc.elasticity.case, shape="Reciprocal"), size=1.5, alpha = 0.8) +
  labs(y = "Percent of annual income (%)", x = "Discount rate (%)") +
  #  scale_x_continuous(limits = c(0,60), breaks=seq(0, 60, 10)) +
  scale_y_continuous(limits = c(0, NA), breaks=seq(0, 100, 20)) +
  scale_color_manual(name = "Income elasticity:", labels = c("H"="1.5", "B"="0.8 (below USA/OECD) or 1.2 (above USA/OECD)", "L"="1.0"),
                     values = c("H"=elast.palette[3], "B"='black', "L"=elast.palette[1])) +
  scale_shape_manual(name = "Risk size adjustment:",
                     values = c("Reciprocal" = 15,
                                "Logarithmic" = 19,
                                "None (fixed VSL)" = 18)) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.margin = margin(b=0, unit = "cm"),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 7),
        axis.text.x = element_text (size = 8)) +
  facet_grid(base.income.case.label~wb.region, scales = "free_y")

g

if(SAVEFIG) ggsave(paste0("sensitivity analysis/SA_bdot_all.pdf"),
                   width = 6.5, height = 5.5)



## 6 VSMU decay 
mean.lambdas <- valuation.data[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == 2019,
                               .(
                                 VSLr_hat = weighted.mean(VSLr_hat, pop),
                                 S_a_hat = weighted.mean(S_a_hat, pop),
                                 L_a_hat = weighted.mean(L_a_hat, pop)
                               ),
                               by=.(wb.region)]

mean.lambdas[, lambda:=VSLr_hat/L_a_hat*S_a_hat^2]

USA_S_a_hat_19 <- mortality.data[country == "USA" & year == 2019, mean(S_a_hat)]
USA_L_a_hat_19 <- US_L_a_hat[year == 2019 & disc.rate == 0.03, US_L_a_hat]
USA_lambda_19 <- US_VSLr/USA_L_a_hat_19 * USA_S_a_hat_19^2   

vsmu.decay.data <- rbind(
  valuation.data[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == 2019 & 
                   wb.region %in% c("Sub-Saharan\nAfrica", "High-income") &
                   age %in% c(0, 20, 40, 60, 80),
                 .(
                   pop = sum(pop),
                   S_a = weighted.mean(S_a, pop),
                   L_a = weighted.mean(L_a, pop),
                   frontier_S_a = weighted.mean(frontier_S_a, pop)
                 ),
                 by=.(wb.region, age)],
  valuation.data[inc.elasticity.case == "B" & base.income.case == "US" & disc.rate == 0.03 & year == 2019 & 
                   country == "USA" &
                   age %in% c(0, 20, 40, 60, 80),
                 .(
                   wb.region = "USA",
                   pop = sum(pop),
                   S_a = weighted.mean(S_a, pop),
                   L_a = weighted.mean(L_a, pop),
                   frontier_S_a = weighted.mean(frontier_S_a, pop)
                 ),
                 by=.(age)]
)

vsmu.decay.data <- merge(vsmu.decay.data, mean.lambdas, by="wb.region", all.x = T)
vsmu.decay.data[wb.region == "USA", VSLr_hat:= US_VSLr]
vsmu.decay.data[wb.region == "USA", S_a_hat:= USA_S_a_hat_19]
vsmu.decay.data[wb.region == "USA", L_a_hat:= USA_L_a_hat_19]
vsmu.decay.data[wb.region == "USA", lambda:= USA_lambda_19]

# To segment the interval between S_a and frontier_S_a in each case, we make a cartesian product with a parameterized distance
vsmu.decay.data[,VAR:=1] # Auxiliary variable just for the product
vsmu.decay.data <- merge(vsmu.decay.data, data.table(dist=seq(0, 1, 0.1), VAR=1), by="VAR", allow.cartesian = T)
vsmu.decay.data[,VAR:=NULL]

vsmu.decay.data[,S_a_tilde:=S_a + dist*(frontier_S_a - S_a)]

# Calculate SMUs
vsmu.decay.data[, smu_change:=(S_a_tilde-S_a)*10^4]

# Calculate values for each case
vsmu.decay.data[, L_a_tilde:=S_a_tilde/S_a * L_a]

vsmu.decay.data[, b_constant_1yr:= lambda * (L_a_tilde - L_a)]
vsmu.decay.data[, b_log_1yr:= 1 - exp(-lambda * (S_a_tilde - S_a)/S_a_tilde * L_a/S_a)]
vsmu.decay.data[, b_crra_1yr:= 1 - 1/(1 + lambda * (S_a_tilde - S_a)/S_a_tilde * L_a/S_a)]                

# Ratio to average VSMU
vsmu.decay.data[b_constant_1yr==0, ratio_avg_log:=1]
vsmu.decay.data[b_constant_1yr==0, ratio_avg_crra:=1]
vsmu.decay.data[b_constant_1yr>0, ratio_avg_log:= b_log_1yr/b_constant_1yr]
vsmu.decay.data[b_constant_1yr>0, ratio_avg_crra:= b_crra_1yr/b_constant_1yr]

# Ratio to marginal VSMU
vsmu.decay.data[b_constant_1yr==0, ratio_mg_log:=1]
vsmu.decay.data[b_constant_1yr==0, ratio_mg_crra:=1]

vsmu.decay.data[b_constant_1yr>0, ratio_mg_log:=  (S_a/S_a_tilde)^2 * (1-b_log_1yr)]
vsmu.decay.data[b_constant_1yr>0, ratio_mg_crra:= (S_a/S_a_tilde)^2 * (1-b_crra_1yr)^2]

# Reverse regions for plot
vsmu.decay.data$wb.region <- factor(vsmu.decay.data$wb.region,
                                    levels=c("USA", "High-income", "Sub-Saharan\nAfrica"))

ggplot(vsmu.decay.data[age %in% c(0, 60, 80)], aes(x=smu_change)) +
  geom_line(aes(y=ratio_mg_log, colour=as.factor(age), linetype="Marginal-to-initial VSMU")) +
  geom_line(aes(y=ratio_avg_log, colour=as.factor(age), linetype="Average-to-initial VSMU")) +
  geom_line(aes(y=b_log_1yr, colour=as.factor(age), linetype="Value-to-income")) +
  labs(x="Survival probability change x 10,000 (SMUs)",
       y="Ratios") +
  scale_y_continuous(limits=c(0,1.01), breaks=seq(0, 1, 0.2)) +
  scale_linetype(name="Ratio") +
  scale_color_discrete(name="Age") +
  facet_wrap(~wb.region, scales="free_x") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.margin = margin(b=0, unit = "cm"),
        strip.text = element_text(size = 8),
        axis.text.x = element_text (size = 7)
  )

if(SAVEFIG) ggsave(paste0("sensitivity analysis/VSMU_decay_ratios.pdf"),
                   width = 6.5, height = 4.5)

