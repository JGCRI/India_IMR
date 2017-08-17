## Script to GCAM and India survey (NSS & NFHS) data for IM analysis
## STW, August 2015

# -----------------------------------------------------------------------------
## Notes
## Subset of scenarios/data, only Reference and Rice drought
## Use total reference consumption in India, scale to income class (from India NSS)
## Price changes between drought and reference
## Estimate income effect from increased expenditures, by income class
## Estimate consumption changes using income and price elasticities, by income class
## Estimate change in IMR, by income class, using India National Family Health Survey IM data
## and estimated income and consumption impacts.

# -----------------------------------------------------------------------------
## Load libraries and functions
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(extrafont)
library(ggthemes)
library(Hmisc)
library(grid)
library(psych)

setwd(srcdir)
source("functions.R")

# -----------------------------------------------------------------------------
## Read in and clean GCAM data
d.gcam <- inputData(d, datadir, "GCAM_drought_scen_consumption.csv", 0)
d.gcam.clean <- subset(d.gcam, scenario == "PE_Reference" | scenario == "PE_Rice_10pct")
    d.gcam.clean$X <- NULL
  d.gcam.clean <- subset(d.gcam.clean, query == "ag_commod_demand" | query == "demand_balance_primary_good" | query == "ag_commod_price"
                     | query == "meat_price" | query == "food_cons_type" | query == "population" | query == "GDPpcap_PPP" 
                     | query == "GDPpcap_MER" | query == "meat_demand" | query == "cons_pcap")
  d.gcam.clean <- subset(d.gcam.clean, region == "India" & year == 2015)
    d.gcam.clean$urban_rural <- "all"
  d.gcam <- NULL
setwd(datadir)
write.csv(d.gcam.clean, "GCAM_cons_price_India.csv")

# -----------------------------------------------------------------------------
## Read in and clean India NSS data
d.income <- inputData(d, datadir, "inc_decile_exp.csv", 0)
  d.income <- melt(d.income, id.vars = c("region", "urban_rural", "unit"))
    d.income$query <- "expenditure"
    d.income$sector <- "total_food_nonfood"
    d.income$unit <- "value_Rs_pcap_30days"
    d.income$year <- 2010
  d.income <- dcast(d.income, region + urban_rural + year + query + sector + unit ~ variable, value.var = "value")

d.cons.share <- inputData(d, datadir, "Report_560_Table4B_2012.csv", 3)
    d.cons.share$D10 <- (d.cons.share$P0_05 + d.cons.share$P05_10) / 2
    d.cons.share$P0_05 <- NULL
    d.cons.share$P05_10 <- NULL
    d.cons.share$D100 <- (d.cons.share$P90_95 + d.cons.share$P95_100) / 2
    d.cons.share$P90_95 <- NULL
    d.cons.share$P95_100 <- NULL
    d.cons.share <- colnameReplace(d.cons.share, "P10_20", "D20")
    d.cons.share <- colnameReplace(d.cons.share, "P20_30", "D30")
    d.cons.share <- colnameReplace(d.cons.share, "P30_40", "D40")
    d.cons.share <- colnameReplace(d.cons.share, "P40_50", "D50")
    d.cons.share <- colnameReplace(d.cons.share, "P50_60", "D60")
    d.cons.share <- colnameReplace(d.cons.share, "P60_70", "D70")
    d.cons.share <- colnameReplace(d.cons.share, "P70_80", "D80")
    d.cons.share <- colnameReplace(d.cons.share, "P80_90", "D90")

d.cons.level <- inputData(d, datadir, "Report_560_Table3B_2012.csv", 3)
    d.cons.level$D10 <- (d.cons.level$P0_05 + d.cons.level$P05_10) / 2
    d.cons.level$P0_05 <- NULL
    d.cons.level$P05_10 <- NULL
    d.cons.level$D100 <- (d.cons.level$P90_95 + d.cons.level$P95_100) / 2
    d.cons.level$P90_95 <- NULL
    d.cons.level$P95_100 <- NULL
    d.cons.level <- colnameReplace(d.cons.level, "P10_20", "D20")
    d.cons.level <- colnameReplace(d.cons.level, "P20_30", "D30")
    d.cons.level <- colnameReplace(d.cons.level, "P30_40", "D40")
    d.cons.level <- colnameReplace(d.cons.level, "P40_50", "D50")
    d.cons.level <- colnameReplace(d.cons.level, "P50_60", "D60")
    d.cons.level <- colnameReplace(d.cons.level, "P60_70", "D70")
    d.cons.level <- colnameReplace(d.cons.level, "P70_80", "D80")
    d.cons.level <- colnameReplace(d.cons.level, "P80_90", "D90")
    d.cons.level$query <- gsub("consumption", "cons_level", d.cons.level$query)

nssVars <- function(d, a, b, e)
    ## Add columns for variable (a), unit (b), location (e), and replace column names
  {
    d$query <- a
    d$unit <- b
    d$region <- "India"
    d$urban_rural <- e
    d$year <- 2010
    d$all.classes <- NULL
    d$no..of.hhs.reporting.consumption.per.1000.hhs <- NULL
    d$no..of.hhs.reporting.consumption.in.sample <- NULL
    d$no..of.hhs.reporting.consumption..14. <- NULL
    d <- colnameReplace(d, "DU10", "D10")
    d <- colnameReplace(d, "DU20", "D20")
    d <- colnameReplace(d, "DU30", "D30")
    d <- colnameReplace(d, "DU40", "D40")
    d <- colnameReplace(d, "DU50", "D50")
    d <- colnameReplace(d, "DU60", "D60")
    d <- colnameReplace(d, "DU70", "D70")
    d <- colnameReplace(d, "DU80", "D80")
    d <- colnameReplace(d, "DU90", "D90")
    d <- colnameReplace(d, "DU100", "D100")
    d <- colnameReplace(d, "DR10", "D10")
    d <- colnameReplace(d, "DR20", "D20")
    d <- colnameReplace(d, "DR30", "D30")
    d <- colnameReplace(d, "DR40", "D40")
    d <- colnameReplace(d, "DR50", "D50")
    d <- colnameReplace(d, "DR60", "D60")
    d <- colnameReplace(d, "DR70", "D70")
    d <- colnameReplace(d, "DR80", "D80")
    d <- colnameReplace(d, "DR90", "D90")
    d <- colnameReplace(d, "DR100", "D100")
    d <- colnameReplace(d, "item", "sector")
    printlog(colnames(d))
    return (d)
  }

d.cons.rural <- inputData(d, datadir, "538-4B-R-consum-2010.csv", 3)
  d.cons.rural <- nssVars(d.cons.rural, "consumption", "kg_pcap_30days", "rural")
  d.cons.rural <- subset(d.cons.rural, sector == "grain_rice" | sector == "grain_total cereals")
d.cons.urban <- inputData(d, datadir, "538-4B-U-consum-2010.csv", 3)
  d.cons.urban <- nssVars(d.cons.urban, "consumption", "kg_pcap_30days", "urban")
  d.cons.urban <- subset(d.cons.urban, sector == "grain_rice" | sector == "grain_total cereals")
d.exp.urban <- inputData(d, datadir, "538-6B-U-value-2010_withRice.csv", 3)
  d.exp.urban <- nssVars(d.exp.urban, "expenditure", "value_Rs_pcap_30days", "urban")
  d.exp.urban <- subset(d.exp.urban, sector == "cereal" | sector == "rice" | sector == "total : food")
d.exp.rural <- inputData(d, datadir, "538-6B-R-value-2010_withRice.csv", 3)
  d.exp.rural <- nssVars(d.exp.rural, "expenditure", "value_Rs_pcap_30days", "rural")
  d.exp.rural <- subset(d.exp.rural, sector == "cereal" | sector == "rice" | sector == "total : food")

d.nss <- rbind(d.income, d.cons.level, d.cons.share, d.cons.rural, d.cons.urban, d.exp.rural, d.exp.urban)
    d.nss$sector <- gsub(" : ", "_", d.nss$sector)
  d.income <- NULL
  d.cons.share <- NULL
  d.cons.rural <- NULL
  d.cons.urban <- NULL
  d.cons.level <- NULL
  d.exp.rural <- NULL
  d.exp.urban <- NULL

# -----------------------------------------------------------------------------
## Create IM dataframe (raw values from India_NFHS3_Chapter 07 - Infant and Child Mortality, Table 7.2)
## "Infant mortality (1q0): The probability of dying before the first birthday"
## Rates by urban and rural "wealth index" quintiles
## NFHS conducted in 2005-2006, rates are for births in the preceding 5-year period (2001-2005)
## NOTE: "Wealth index" != "Decile expenditures"
    # One wealth index and quintile distribution for all of India
    # IMR reported by wealth quintile and rural-urban status, assign IMR to expenditure classes by the of U/R population in each wealth index quintile
    # IMR(U10) = average(U-lowest, U-second) = 63.3
    # IMR(U20) = U-third = 49.8
    # IMR(U30-U50) = U-fourth = 46.2
    # IMR(U60-U100) = U-highest = 27.4
    # IMR(R10-R30) = R-lowest = 70.7
    # IMR(R40-R60) = R-second = 69.2
    # IMR(R70-R80) = R-third = 60.6
    # IMR(R90) = R-fourth = 42.3
    # IMR(R100) = R-fifth = 33.6

d.imr <- data.frame(region = c("India", "India"), urban_rural = c("urban", "rural"), year = c(2005, 2005), query = c("infant_mortality", "infant_mortality"),
                     sector = c("", ""), unit = c("per_1000_births", "per_1000_births"), D10 = c(63.3, 70.7), D20 = c(49.8, 70.7), 
                     D30 = c(46.2, 70.7), D40 = c(46.2, 69.2), D50 = c(46.2, 69.2), D60 = c(27.4, 69.2), D70 = c(27.4, 60.6), 
                     D80 = c(27.4, 60.6), D90 = c(27.4, 42.3), D100 = c(27.4, 33.6))

d.survey <- rbind(d.nss, d.imr)
setwd(datadir)
write.csv(d.survey, "India_surveydata_cons_exp_imr.csv")
