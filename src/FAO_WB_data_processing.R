# Script to process FAO food security indicators
# STW, November 2014, updated August 2015 to estimate infant mortality

## NOTES ##
## FAO - Food Security Indicators, second release October 15, 2014
## Food-Security-Statistics@FAO.org
## World Bank Infant Mortality Data, update 7/25/15, World Development Indicators

# -----------------------------------------------------------------------------
# Define functions and load libraries
## Load libraries
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
# Read in data

## ISO country codes
printlog("Read iso codes")
d.iso <- inputData(d, datadir, "iso_GCAM_regID.csv", 3)
  d.iso <- subset(d.iso, select = c("iso", "country_name"))

## Function to clean individual indicator dataframes
dataClean <- function(d, x_unit, x_indicator)
{
  r_developing <- as.numeric(which(grepl(5852, d$FAOST_CODE)))
  r_developed <- as.numeric(which(grepl(5851, d$FAOST_CODE)))
  d$development <- ifelse(as.numeric(row.names(d)) >= r_developing & as.numeric(row.names (d)) < r_developed, "developing", "developed")
  d$development <- ifelse(d$Regions.Subregions.Countries == "World", "world_avg", d$development)
  r_developing <- NULL
  r_developed <- NULL
  d$FAOST_CODE <- NULL
  d$X <- NULL
  d$X.1 <- NULL
  d$X.2 <- NULL
  d <- melt(d, id.vars = c("Regions.Subregions.Countries", "development"))
  d <- colnameReplace(d, "variable", "year")
  d$unit <- x_unit
  d$indicator <- x_indicator
  return(d)
}
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_1_1", 2)
  d.temp <- dataClean(d.temp, "percent_3yr_avg", "energy_adequacy") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5)) + 1
  d.clean <- d.temp
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_1_3", 2)
  d.temp <- dataClean(d.temp, "percent_3yr_avg", "share_staple") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5)) + 1
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_1_4", 2)
  d.temp <- dataClean(d.temp, "g_cap_day_3yr_avg", "protein_total") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5)) + 1
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_1_5", 2)
  d.temp <- dataClean(d.temp, "g_cap_day_3yr_avg", "protein_animal") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5)) + 1
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_2_4", 2)
  d.temp <- dataClean(d.temp, "2011_intl$", "gdp_pcap_ppp") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5))
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_2_6", 2)
  d.temp <- dataClean(d.temp, "percent_3yr_avg", "undernourishment") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5)) + 1
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_2_9", 2)
  d.temp <- dataClean(d.temp, "percent_3yr_avg", "food_inadequacy") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5)) + 1
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_3_5", 2)
  d.temp <- dataClean(d.temp, "index", "food_price_volatility") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5))
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
d.temp <- inputData(d, datadir, "FAO_Food_Security_csv", "V_4_7", 2)
  d.temp <- dataClean(d.temp, "percent", "anemia_pregnancy") 
    d.temp$year <- as.numeric(substr(d.temp$year, 2,5))
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL
    d.clean <- colnameReplace(d.clean, "Regions.Subregions.Countries", "country_name")
    d.clean$value <- as.numeric(gsub("<", "", d.clean$value))
## Merge with iso codes
  d.clean <- merge(d.clean, d.iso, by = "country_name", all.x = TRUE)
    d.clean <- subset(d.clean, country_name != "" & country_name != "Africa" & country_name != "Asia" & country_name != "Caribbean" & country_name != "Caucasus and Central Asia" 
                       & country_name != "Developed countries" & country_name != "Developing countries" & country_name != "Eastern Asia" 
                       & country_name != "Eastern Asia (excluding China)" & country_name != "Holy See" & country_name != "Landlocked developing countries" 
                       & country_name != "Latin America" & country_name != "Latin America and the Caribbean" & country_name != "Least developed countries" 
                       & country_name != "Low income economies" & country_name != "Low-income food-deficit countries" & country_name != "Lower-middle-income economies"
                       & country_name != "Northern Africa" & country_name != "Oceania " & country_name != "Small island developing States" & country_name != "South-Eastern Asia"
                       & country_name != "Southern Asia" & country_name != "Southern Asia (excluding India)" & country_name != "Sub-Saharan Africa" 
                       & country_name != "Western Asia" & country_name != "World")
## Replace iso codes for countries with non-standard names
    d.clean$iso.y <- NULL
    d.clean <- colnameReplace(d.clean, "iso.x", "iso")
  d.clean <- isoReplace(d.clean, "iso", "country_name")

## Read and clean WB infant mortality data
d.temp <- inputData(d, datadir, "sp.dyn.imrt.in_Indicator_en_csv_v2.csv", 4)
    d.temp <- colnameReplace(d.temp, "Country.Name", "country_name")
    d.temp <- colnameReplace(d.temp, "Country.Code", "iso")
      d.temp$iso <- tolower(d.temp$iso)
    d.temp <- colnameReplace(d.temp, "Indicator.Name", "indicator")
      d.temp$indicator <- "infant_mortality"
    d.temp$unit <- "per_1000_live_births"
    d.temp$Indicator.Code <- NULL
    d.temp$X <- NULL
  d.temp <- melt(d.temp, id.vars = c("iso", "country_name", "indicator", "unit"), na.rm = TRUE)
    d.temp <- yr(d.temp)
  d.temp <- subset(d.temp, year > 1989)
    d.temp$development <- ifelse((d.temp$iso == "alb" | d.temp$iso == "and" | d.temp$iso == "aus" | d.temp$iso == "aut" | d.temp$iso == "blr" | d.temp$iso == "bel" | d.temp$iso == "bmu" 
                                  | d.temp$iso == "bih" | d.temp$iso == "bgr" | d.temp$iso == "can" | d.temp$iso == "hrv" | d.temp$iso == "cyp" | d.temp$iso == "cze" | d.temp$iso == "dnk" 
                                  | d.temp$iso == "est" | d.temp$iso == "fro" | d.temp$iso == "fin" | d.temp$iso == "fra" | d.temp$iso == "deu" | d.temp$iso == "gib" | d.temp$iso == "grc" 
                                  | d.temp$iso == "grl" | d.temp$iso == "hun" | d.temp$iso == "isl" | d.temp$iso == "irl" | d.temp$iso == "isr" | d.temp$iso == "ita" | d.temp$iso == "jpn" 
                                  | d.temp$iso == "lva" | d.temp$iso == "lie" | d.temp$iso == "ltu" | d.temp$iso == "lux" | d.temp$iso == "mlt" | d.temp$iso == "mco" | d.temp$iso == "mne" 
                                  | d.temp$iso == "nld" | d.temp$iso == "nzl" | d.temp$iso == "nor" | d.temp$iso == "pol" | d.temp$iso == "prt" | d.temp$iso == "mda" | d.temp$iso == "rom" 
                                  | d.temp$iso == "rus" | d.temp$iso == "spm" | d.temp$iso == "smr" | d.temp$iso == "srb" | d.temp$iso == "scg" | d.temp$iso == "svk" | d.temp$iso == "svn" 
                                  | d.temp$iso == "esp" | d.temp$iso == "swe" | d.temp$iso == "che" | d.temp$iso == "mkd" | d.temp$iso == "ukr" | d.temp$iso == "gbr" | d.temp$iso == "usa"),
                                  "developed", "developing")
## Merge with food security indicators
  d.clean <- rbind(d.clean, d.temp)
  d.temp <- NULL

## Dataframe for analaysis, cast by indicator variable
d.analysis <- dcast(d.clean, iso + country_name + development + year ~ indicator, value.var = "value")
    d.analysis$developed <- ifelse(d.analysis$development == "developed", 1, 0)
    d.analysis$development <- NULL
    d.analysis$infant_mortality <- d.analysis$infant_mortality / 1000
## Pairwise correlations
d.cor <- subset(d.analysis, select = c("year", "anemia_pregnancy", "energy_adequacy", "food_inadequacy", "food_price_volatility", 
                                        "gdp_pcap_ppp", "infant_mortality", "protein_animal", "protein_total", "share_staple", 
                                        "undernourishment", "developed"))
  cor(d.cor, use = "pairwise.complete.obs", method = "pearson") # type can be pearson or spearman
  rcorr(as.matrix(d.cor, "pearson"))
  d.cor <- NULL

## Test multicollinearity
summary(anemia.staple.ols <- lm(anemia_pregnancy ~ share_staple, d.analysis))
summary(anemia.protein.ols <- lm(anemia_pregnancy ~ protein_animal, d.analysis))
summary(anemia.foodinadeq.ols <- lm(anemia_pregnancy ~ food_inadequacy, d.analysis))
summary(staple.protein.ols <- lm(share_staple ~ protein_animal, d.analysis))
summary(staple.foodinadeq.ols <- lm(share_staple ~ food_inadequacy, d.analysis))

## Regressions
d.est <- na.omit(subset(d.analysis, select = c("iso", "infant_mortality", "year", "gdp_pcap_ppp", "anemia_pregnancy", "food_inadequacy", "share_staple", "protein_animal")))
  d.est$ln_gdp_pcap <- log(d.est$gdp_pcap_ppp)

describe(d.est) 
## Simple OLS regression
  ## 1: gdp/cap, anemia, food inadequacy, share of staples, animal protein
summary(inf.mort.ols <- lm(infant_mortality ~ year + gdp_pcap_ppp + anemia_pregnancy + food_inadequacy + share_staple + protein_animal, data = d.est))
    d.est$im_ols_fit <- predict.lm(inf.mort.ols)
## Use logistic regression because IM is a rate
  ## 2: gdp/cap, anemia, food inadequacy, share of staples, animal protein
summary(inf.mort.logit <- glm(infant_mortality ~ year + gdp_pcap_ppp + anemia_pregnancy + food_inadequacy + share_staple + protein_animal, d.est, family = quasibinomial()))
    d.est$im_logit_fit <- predict.glm(inf.mort.logit, type = "response")
  ## 3: gdp/cap, share of staples, animal protein
summary(im.stap.prot.logit <- glm(infant_mortality ~ year + gdp_pcap_ppp + share_staple + protein_animal, d.est, family = quasibinomial()))
    d.est$im_sp_logit_fit <- predict.glm(im.stap.prot.logit, type = "response")
## Logit with ln(gdp/cap)
  ## 4: ln(gdp/cap), anemia, food inadequacy, share of staples, animal protein
summary(inf.mort.lngdp.logit <- glm(infant_mortality ~ year + ln_gdp_pcap + anemia_pregnancy + food_inadequacy + share_staple + protein_animal, d.est, family = quasibinomial()))
    d.est$im_lngdp_logit_fit <- predict.glm(inf.mort.logit, type = "response")
  ## 5: ln(gdp/cap), share of staples, animal protein
summary(im.stap.lngdp.prot.logit <- glm(infant_mortality ~ year + ln_gdp_pcap + share_staple + protein_animal, d.est, family = quasibinomial()))
    d.est$im_sp_lngdp_logit_fit <- predict.glm(im.stap.lngdp.prot.logit, type = "response")
    ## 5_FE: ln(gdp/cap), share of staples, animal protein, country FE
summary(im.stap.lngdp.prot.logit.fe <- glm(infant_mortality ~ year + ln_gdp_pcap + share_staple + protein_animal + factor(iso), d.est, family = quasibinomial()))
    d.est$im_sp_lngdp_logit_fit_fe <- predict.glm(im.stap.lngdp.prot.logit.fe, type = "response")
  ## 6: tansform DV to log odds ratio:
    d.est$ln_odds_pr_im <- log(d.est$infant_mortality / (1 - d.est$infant_mortality))
summary(test <- lm(ln_odds_pr_im ~ year + ln_gdp_pcap + share_staple + protein_animal, d.est))
  d.est$test_pred_ln_odds_im <- predict.lm(test)
  d.est$test_pred_im <- exp(d.est$test_pred_ln_odds_im) / (1 + exp(d.est$test_pred_ln_odds_im))

## Plot to compare fits
d.fig <- subset(d.est, select = c("iso", "infant_mortality", "im_logit_fit", "im_ols_fit", "im_sp_logit_fit", "im_sp_lngdp_logit_fit", "im_lngdp_logit_fit", "test_pred_im"))
  d.fig <- melt(d.fig, id.vars = 1:2)
p <- ggplot(d.fig, aes(infant_mortality, value, color = variable)) + geom_point() + theme_basic #+ colScaleBrewer(Set1)
  p <- p + ggtitle("Model specification comparison") + labs(x = "Infant Mortality", y = "Predicted Infant Mortality")
  print(p)
setwd(figdir)
ggsave(plot = p, (paste("pred_inf_mort.pdf", sep = "")), width = 400, height = 297, units = "mm")

# Change to infant mortalities per 1000 live births (unit in WB data)
d.fig <- subset(d.est, select=c("iso", "year", "gdp_pcap_ppp", "anemia_pregnancy", "food_inadequacy", "share_staple", "protein_animal", "infant_mortality", "im_sp_lngdp_logit_fit_fe", "im_sp_lngdp_logit_fit"))
d.fig$infant_mortality <- d.fig$infant_mortality * 1000
d.fig$im_sp_lngdp_logit_fit_fe <- d.fig$im_sp_lngdp_logit_fit_fe * 1000
d.fig$im_sp_lngdp_logit_fit <- d.fig$im_sp_lngdp_logit_fit * 1000

p <- ggplot(d.fig, aes(infant_mortality, im_sp_lngdp_logit_fit, color = iso)) + geom_point(size = 2) + theme_basic + scale_color_hue(l = 50, c = 95)
  p <- p + labs(x = "Reported Infant Mortality Rate (deaths per 1,000 live births)", y = "Predicted Infant Mortality Rate")
  p <- p + geom_abline(slope = 1, intercept = 0, color = "black", size = .75) 
  p <- p + scale_x_continuous(limits = c(0, 200)) + scale_y_continuous(limits = c(0, 200))+ coord_equal(ratio = 1)
  p <- p + theme(legend.position = "none")
  print(p)
  setwd(figdir)
ggsave(plot = p, (paste("pred_inf_mort_finalmodel.pdf", sep = "")), width = 300, height = 300, units = "mm")

## Specification 5 fits the best, particularly at the high end, which is what we care most about.
  print(im.stap.lngdp.prot.logit$coefficient)

setwd(datadir)
write.csv(d.fig, "IMR_report_fitted.csv")
