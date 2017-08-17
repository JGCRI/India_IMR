## Script to estimate clean data for IMR impacts and estimate income elasticities in India
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

setwd(srcdir)
source("functions.R")

# -----------------------------------------------------------------------------
## Read in and clean GCAM and NSS data
d.gcam <- inputData(d, datadir, "GCAM_cons_price_India.csv", 0)
    d.gcam$X <- NULL
d.survey <- inputData(d, datadir, "India_surveydata_cons_exp_imr.csv", 0)
    d.survey$X <- NULL
    d.survey$query <- ifelse(d.survey$sector == "staple", "calorie_share", d.survey$query)

# -----------------------------------------------------------------------------
## Data processing and analysis
  ## Protein consumption from animal origin
protein <- function(d)
  {
    temp <- subset(d.survey, sector == "protein" | sector == "protein_dairy" | sector == "protein_egg_fish_meat")
      temp <- melt(temp, id.vars = 1:6)
      temp <- dcast(temp, region + urban_rural + year + variable ~ sector, value.var = "value")
        temp$protein_animal <- (temp$protein * (temp$protein_dairy + temp$protein_egg_fish_meat)/100)
      temp <- subset(temp, select = c("region", "urban_rural", "year", "variable", "protein_animal"))
      temp <- dcast(temp, region + urban_rural + year ~ variable, value.var = "protein_animal")
        temp$year <- 2011
        temp$unit <- "g_pcap_pday"
        temp$query <- "cons_level"
        temp$sector <- "protein_animal"
    d <- rbind(d.survey, temp)
    return(d)
  }
d.analysis <- protein(d.analysis)
  d.analysis <- subset(d.analysis, sector == "total_food_nonfood" | sector == "staple" | sector == "" | sector == "protein_animal" 
                        | sector == "grain_rice" | sector == "rice" | sector == "total_food_nonfood" | sector == "total")

# -----------------------------------------------------------------------------
## Share rice consumption of grains
riceShare <- function(d, s1, s2, v1, v2)
  {
    temp <- subset(d.survey, sector == s1 | sector == s2 )
      temp <- melt(temp, id.vars = 1:6)
      temp <- dcast(temp, region + urban_rural + year + variable ~ sector, value.var = "value")
        temp[[v1]] <- (temp[[s1]] / temp[[s2]])
      temp <- subset(temp, select = c("region", "urban_rural", "year", "variable", v1))
      temp <- dcast(temp, region + urban_rural + year ~ variable, value.var = v1)
        temp$year <- 2010
        temp$unit <- "share"
        temp$query <- v2
        temp$sector <- v1
    d <- rbind(d.analysis, temp)
    return(d)
  }
  d.analysis <- riceShare(d.survey, "rice", "total_food_nonfood", "share_total_exp_rice", "expenditure") # share of total expenditure on rice (exp unit value in Rs/cap/30days)
  d.analysis <- riceShare(d.survey, "rice", "cereal", "share_cereal_exp_rice", "expenditure") # share of cereal expenditure on rice (exp unit value in Rs/cap/30days)
  d.analysis <- riceShare(d.survey, "grain_rice", "grain_total cereals", "share_rice_cons_grain", "consumption") # share rice consumption of all grains (cons unit kg/cap/30days)

# -----------------------------------------------------------------------------
## Weighted average expenditures (to convert GCAM average GDP/cap and consumption/cap to exp classes) for ln(GDP) for IMR estimation.
## WB (2010), 69% of Indian population lives in rural areas.  Need to weight rural/urban to get population averages
## Rural weight = 0.69/0.31 = 2.2, then need to average accounting for additional population in rural areas (2.2*10 + 10 = 32)
avgExp <- function(d)
  {
    rural_weight = 2.2
    temp <- subset(d, sector == "total_food_nonfood")
      temp <- melt(temp, id.vars = 1:6)
        temp$weight_total_exp <- ifelse(temp$urban_rural == "rural", (temp$value * rural_weight), temp$value)
    weight_avg_exp <- sum(temp$weight_total_exp) / 32
    gdp_pcap_ppp <- as.numeric(filter(d.gcam, query == "GDPpcap_PPP" & scenario == "PE_Reference") %>% select(value))  
        temp$gdp_pcap_ppp <- gdp_pcap_ppp * (temp$value / weight_avg_exp)
        temp$value <- NULL
        temp$weight_total_exp <- NULL
        temp <- colnameReplace(temp, "gdp_pcap_ppp", "value")
        temp$query <- "income"
        temp$sector <- "gdp_pcap_weight_exp"
        temp$unit <- "thous2005USD_pcap_pyear"
      temp <- dcast(temp, region + urban_rural + year + query + sector + unit ~ variable, value.var = "value")
    d <- rbind(temp, d)
    return(d)
  }
  d.analysis <- avgExp(d.analysis)
write.csv(d.analysis, "India_data_for_IM_est_20150917.csv")

# -----------------------------------------------------------------------------
## Implied income elasticities for rice and meat from NSS data
  ## elasticity_income = ((Q2 - Q1) / Q1) / ((Y2 - Y1) / Y1)
  ## Note: there are price differentials across income groups, too (exp/cons increases with income). Reasons for this could be (1) quality increases 
  ## and (2) rice subsidies (price controls) for lower income groups. So these income elasticities are also picking up some price effects.
deriveElast <- function(d, s1, x1)
  {
    temp <- subset(d, sector == s1 | sector == "gdp_pcap_weight_exp")
    percentChange <- function(d2, x1, x2, x3)
      {
        d2[[x3]] <- (d2[[x2]] - d2[[x1]]) / d2[[x1]]
        return(d2)
      }
        temp <- percentChange(temp, "D10", "D20", "pct20")
        temp <- percentChange(temp, "D20", "D30", "pct30")
        temp <- percentChange(temp, "D30", "D40", "pct40")
        temp <- percentChange(temp, "D40", "D50", "pct50")
        temp <- percentChange(temp, "D50", "D60", "pct60")
        temp <- percentChange(temp, "D60", "D70", "pct70")
        temp <- percentChange(temp, "D70", "D80", "pct80")
        temp <- percentChange(temp, "D80", "D90", "pct90")
        temp <- percentChange(temp, "D90", "D100", "pct100")
      temp <- subset(temp, select = c("region", "urban_rural", "year", "query", "sector", "pct20", "pct30", "pct40", "pct50", "pct60", "pct70", "pct80", "pct90", "pct100"))
        temp$year <- 2010
      temp <- melt(temp, id.vars = 1:5)
      temp <- dcast(temp, region + urban_rural + year + variable ~ sector, value.var = "value")
        temp[[x1]] <- (temp[[s1]]) / (temp$gdp_pcap_weight_exp)
      elasticities <- subset(temp, select = c("urban_rural", "variable", x1))
    return(elasticities)
  }

d.elast.rice <- deriveElast(d.analysis, "grain_rice", "inc_elast_rice")
    ## Unit is %change kg/cap/month per 1% change income
    d.elast.rice$unit <- "pct_change_kg_pcap_pmonth_p1pctUSD"    
    d.elast.rice <- colnameReplace(d.elast.rice, "inc_elast_rice", "inc_elast")
    d.elast.rice <- colnameReplace(d.elast.rice, "variable", "exp_decile")
    d.elast.rice$commod <- "rice"
d.elast.an.prot <- deriveElast(d.analysis, "protein_animal", "inc_elast_an_prot")
    d.elast.an.prot$unit <- "pct_change_g_pcap_pday_p1pctUSD"
    d.elast.an.prot <- colnameReplace(d.elast.an.prot, "inc_elast_an_prot", "inc_elast")
    d.elast.an.prot <- colnameReplace(d.elast.an.prot, "variable", "exp_decile")
    d.elast.an.prot$commod <- "prot_an"
d.elasticity <- rbind(d.elast.an.prot, d.elast.rice)

setwd(datadir)
write.csv(d.elasticity, "India_elasticity.csv")
  d.elast.an.prot <- NULL
  d.elast.rice <- NULL
  d.elasticity <- NULL

