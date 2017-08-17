## Script to estimate drought impacts on consumption, income, and infant mortality in India
## STW, 31 August 2015

# -----------------------------------------------------------------------------
## Notes:
## Reference data, by expenditure decile, rural and urban, from India NSS and NFHS:
  ## Total expenditure (scale to GDP/cap from GCAM)
  ## Share calories from staples
  ## g-protein from animal sources per day
  ## Infant mortality rates by wealth index, apply to expenditure deciles
  ## Income elasticity of demand (India_IM_by_income_20150911.R)
## "Drought" scenario = 10% rice production loss from India, China, and Southeast Asia
  ## Global production loss = 7%
  ## Increase in producer price ~ 80%
## Assume the only dietary changes are animal protein and rice

## To estimate changes in IM need (by income class):
## ln(GDP/cap) - use GCAM gdp/cap and weighted average expenditures from NSS to allocate to expenditure decile
## share consumption (calories) from staples: use weighted average share of grains (kg) from NSS with FAO share
  ## calories from grains & roots_tubers to estimate income
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

# -----------------------------------------------------------------------------
## Read in and clean data for analysis
d.analysis <- d
    d.analysis$year <- NULL # Assume all survey years ~ reflective of 2010, will apply to GCAM 2015

# -----------------------------------------------------------------------------
## Define constants
  ## Change in global producer price of rice in drought scenario (percent increase)
price_change_rice <- 0.8
  ## Income elasticities, rice = %change(kg/cap/30days/2005USD) / %change gdp/cap, meat = %change(g/cap/day) / %change gdp/cap
    ## For rice, three breaks capture the U shape elasticity curve pretty well (see MEDEA/Framework Paper/India_elasticity.xlsx)
ie_rice_0_25 <- 0.08 # R10-30, U10
ie_rice_25_85 <- -0.04 # R40-90, U20-70
ie_rice_85_100 <- -0.11 # R100, U80-U100
    ## For meat, exponentially decreasing income elasticity, five breaks fit a lot better
ie_prot_an_0_10 <- 2.8 # R10
ie_prot_an_10_40 <- 1.5 # R20-50, U10-20
ie_prot_an_40_70 <- 1.0 # > R60-80, U30-40
ie_prot_an_70_95 <- 0.5 # > R90-100, U50-80
ie_prot_an_95_100 <- 0.3 # > U90-U100
  ## Calories per gram/rice
calorie_rice <- 1.29

# -----------------------------------------------------------------------------
  ## Estimate change in rice expenditures and pct change total expenditures
expDrought <- function(d)
{
  temp <- subset(d, query == "expenditure")
    temp <- melt(temp, id.vars = 1:5)
    temp <- dcast(temp, region + urban_rural + query + variable ~ sector, value.var = "value")
      temp$D_rice_exp <- temp$rice - temp$rice * (1 + price_change_rice) # rice expenditure
      temp$exp_drought_share <- ((temp$total_food_nonfood + temp$D_rice_exp) / temp$total_food_nonfood)
    temp <- subset(temp, select = c("region", "urban_rural", "query", "variable", "exp_drought_share"))
      temp$query <- "income"
      temp$sector <- "drought_change"
      temp$unit <- "share"
    temp <- dcast(temp, region + urban_rural + query + sector + unit ~ variable, value.var = "exp_drought_share")
  d <- rbind(d, temp)
}
  d.analysis <- expDrought(d.analysis)

# -----------------------------------------------------------------------------
  ## Estimate change in GDP/cap using pct change in total expenditures
incomeDrought <- function(d)
{
  temp <- subset(d, query == "income")
    temp <- melt(temp, id.vars = 1:5)
    temp <- dcast(temp, region + urban_rural + query + variable ~ sector, value.var = "value")
      temp$gdp_pcap_drought <- temp$drought_change * temp$gdp_pcap_weight_exp
  temp <- subset(temp, select = c("region", "urban_rural", "query", "variable", "gdp_pcap_drought"))
      temp$query <- "income"
      temp$sector <- "gdp_pcap_drought"
      temp$unit <- "thous2005USD_pcap_pyear"
  temp <- dcast(temp, region + urban_rural + query + sector + unit ~ variable, value.var = "gdp_pcap_drought")
  d <- rbind(d, temp)
}
  d.analysis <- incomeDrought(d.analysis)

# -----------------------------------------------------------------------------
  ## Change_Rice = Rice_O*(elast_rice)*(pct_change_gdp/cap)
## units: grain_rice = kg/cap/30days, elasticity_rice (%change_grain_rice)/(%change_gdp/cap), gdp/cap = thous2005USD/cap/year
temp <- subset(d.analysis, sector == "grain_rice" | sector == "gdp_pcap_weight_exp" | sector == "gdp_pcap_drought")
  temp <- melt(temp, id.vars = 1:5)
  temp <- dcast(temp, region + urban_rural + variable ~ sector, value.var = "value")
    temp$id <- ifelse(temp$urban_rural == "rural", paste("R", temp$variable, sep = ""), paste("U", temp$variable, sep = ""))
      # Change in consumption = ref_consumption * income change * 100 * elasticity  = kg/cap/30days * % * elast (%/%)
      # Don't need to change share income to percent because we'd just have to divide by 100 again to get the share (rather than percent) for rice
    temp$D_rice_cons <- ifelse((temp$id == "RD10" | temp$id == "RD20" | temp$id == "RD30" | temp$id == "UD10"),
                         (temp$grain_rice * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_rice_0_25),
                         (temp$grain_rice * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_rice_25_85))
      temp$D_rice_cons <- ifelse((temp$id == "RD100" | temp$id == "UD80" | temp$id == "UD90" | temp$id == "UD100"),
                           (temp$grain_rice * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_rice_85_100), temp$D_rice_cons)
    temp$D_cal_drought_rice <- temp$D_rice_cons * calorie_rice * 1000 / 30 #Calories/g rice, but D rice is kg, divide by 30 to get cal/day
  temp <- subset(temp, select = c("region", "urban_rural", "variable", "D_cal_drought_rice"))
  temp <- dcast(temp, region + urban_rural ~ variable, value.var = "D_cal_drought_rice")
    temp$query <- "cons_level"
    temp$sector <- "D_rice_cons"
    temp$unit <- "cal_pcap_pday"
  d.analysis <- rbind(d.analysis, temp)
  temp <- NULL

# -----------------------------------------------------------------------------
  ## Change in calories from animal source
d.gcam <- inputData(d, datadir, "GCAM_cons_price_India.csv", 0)
## Calories from animal sources, average for India.  Assume calories scale with protein for all income classes
# Convert calories to grams (1 g-protein = 4 calories), but calories from animals are not all protein:
## Chicken ~ 45% calories from protein, Beef ~ 41%, Pork ~ 45%, Sheep ~ 33%, Fish ~ 32%, Dairy ~ 23%
d.gcam <- subset(d.gcam, query == "cons_pcap" & scenario == "PE_Reference")
  d.gcam <- dcast(d.gcam, scenario + region + query + unit + year ~ sector, value.var = "value")
  animal_cal_prot_ratio <- (d.gcam$Beef + d.gcam$OtherMeat_Fish + d.gcam$Pork + d.gcam$Poultry + d.gcam$SheepGoat + d.gcam$Dairy) /
  ((d.gcam$Beef * 0.4 + d.gcam$OtherMeat_Fish * 0.3 + d.gcam$Pork * 0.45 + d.gcam$Poultry * 0.45 + d.gcam$SheepGoat * 0.33 + d.gcam$Dairy * 0.2) / 4)
  d.gcam <- NULL
## units: protein_animal = g/day, elasticity_prot_an = g/day/2005USD g/cap/30days/2005USD/year, gdp/cap = thous2005USD/year
temp <- subset(d.analysis, sector == "protein_animal" |  sector == "gdp_pcap_weight_exp" | sector == "gdp_pcap_drought")
  temp <- melt(temp, id.vars = 1:5)
  temp <- dcast(temp, region + urban_rural + variable ~ sector, value.var = "value")
    temp$id <- ifelse(temp$urban_rural == "rural", paste("R", temp$variable, sep = ""), paste("U", temp$variable, sep = ""))
    ## Estimate change in animal protein consumption (g/person/day)
    temp$D_prot_an_cons <- ifelse((temp$id == "RD20" | temp$id == "RD30" | temp$id == "RD40" | temp$id == "RD50" | temp$id == "UD10" | temp$id == "UD20"),
                                (temp$protein_animal * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_prot_an_10_40),
                                (temp$protein_animal * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_prot_an_40_70))
      temp$D_prot_an_cons <- ifelse(temp$id == "RD10", (temp$protein_animal * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_prot_an_0_10), temp$D_prot_an_cons)
      temp$D_prot_an_cons <- ifelse(temp$id == "RD90" | temp$id == "RD100" | temp$id == "UD50" | temp$id == "UD60" | temp$id == "UD70" | temp$id == "UD80",
                                     (temp$protein_animal * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_prot_an_70_95), temp$D_prot_an_cons)
      temp$D_prot_an_cons <- ifelse(temp$id == "UD90" | temp$id == "UD100", (temp$protein_animal * ((temp$gdp_pcap_drought / temp$gdp_pcap_weight_exp) - 1) * ie_prot_an_95_100), temp$D_prot_an_cons)
    ## Animal protein consumption (g/person/day) in drought scenario
    temp$prot_an_drought <- temp$protein_animal + temp$D_prot_an_cons
    ## Estimate change in calories from animal protein
    temp$D_cal_an_drought <- temp$D_prot_an_cons * animal_cal_prot_ratio
  temp <- subset(temp, select = c("region", "urban_rural", "variable", "D_cal_an_drought", "prot_an_drought"))
    temp <- colnameReplace(temp, "variable", "var")
  temp <- melt(temp, id.vars = 1:3)
    temp$query <- "cons_level"
    temp$sector <- ifelse(temp$variable == "D_cal_an_drought", "D_prot_an_cons", "protein_animal_drought")
    temp$unit <- ifelse(temp$variable == "D_cal_an_drought", "cal_pcap_pday", "g_pcap_pday")
    temp$variable <- NULL
  temp <- dcast(temp, region + urban_rural + query + sector + unit ~ var, value.var = "value")
  d.analysis <- rbind(d.analysis, temp)
  temp <- NULL

# -----------------------------------------------------------------------------
  ## Change in total calories and share from staples
temp <- subset(d.analysis, unit == "cal_pcap_pday")
  temp <- melt(temp, id.vars = 1:5)
  temp <- dcast(temp, region + urban_rural + query + unit + variable ~ sector, value.var = "value")
    temp$total_cal_drought <- temp$total + temp$D_prot_an_cons + temp$D_rice_cons
  temp <- subset(temp, select = c("region", "urban_rural", "query", "unit", "variable", "total_cal_drought"))
  temp <- dcast(temp, region + urban_rural + query + unit ~ variable, value.var = "total_cal_drought")
    temp$sector <- "total_cal_drought"
  d.analysis <- rbind(d.analysis, temp)
  temp <- NULL

# -----------------------------------------------------------------------------
  ## Share calories from staples for drought scenario
  temp <- subset(d.analysis, sector == "staple" | sector == "total" | sector == "total_cal_drought" | sector == "D_rice_cons")
  temp <- melt(temp, id.vars = 1:5)
  temp <- dcast(temp, region + urban_rural + variable ~ sector, value.var = "value")
    temp$staple_cal_ref <- (temp$staple / 100) * temp$total
    temp$staple_cal_drought <- temp$staple_cal_ref + temp$D_rice_cons
    temp$staple_share_drought <- (temp$staple_cal_drought / temp$total_cal_drought) * 100
  temp <- subset(temp, select = c("region", "urban_rural", "variable", "staple_share_drought"))
  temp <- dcast(temp, region + urban_rural ~ variable, value.var = "staple_share_drought")
    temp$query <- "calorie_share"
    temp$sector <- "staple_drought"
    temp$unit <- "percent"
  d.analysis <- rbind(d.analysis, temp)
  temp <- NULL

# -----------------------------------------------------------------------------
  ## Infant mortality rate estimation
## From FAO and WB data, country-cross section -- Model specification from MEDEA/WB_IM_data/FAO_WB_data_processing.R:
## ln(p(infant_mortality) / (1-p(infant_mortality)) = year + ln_gdp_pcap + share_staple + protein_animal
## To estimate p(im): ## Estimate: ln(p(IM) / (1 - p(IM))) = (b0 + b1*year + b2*ln(gdp/cap) + b3*share_staple + b4*protein_animal)
## e(ln(p(IM) / (1 - p(IM)))) / (1 + e(ln(p(IM) / (1 - p(IM))))) = p(IM)

d.im <- subset(d.analysis, sector == "staple" | sector == "staple_drought" | sector == "" | sector == "gdp_pcap_weight_exp" |
                  sector == "gdp_pcap_drought" | sector == "protein_animal" | sector == "protein_animal_drought" |
                  sector == "total" | sector == "total_cal_drought")
    d.im$sector <- gsub("gdp_pcap_weight_exp", "gdp_pcap", d.im$sector)
    d.im$sector <- ifelse(d.im$query == "infant_mortality", "infant_mortality_observed", d.im$sector)
  d.im <- melt(d.im, id.vars = 1:5)
  d.im <- dcast(d.im, region + urban_rural + variable ~ sector, value.var = "value")
## Conversions:
  ## thous2005USD --> 2011USD (already using PPP)
gdp_2005_2011 <- 1000 * 1.12
  d.im$ln_gdp_pcap_ref <- log(d.im$gdp_pcap * gdp_2005_2011)
  d.im$ln_gdp_pcap_drought <- log(d.im$gdp_pcap_drought * gdp_2005_2011)

b0 <- 38.16
b1_year <- -0.01913
b2_ln_gdp_cap <- -0.3725
b3_share_stap <- 0.0073
b4_prot_an <- -0.0113

predIM <- function(d, x1, x2, x3, x4, y)
{
  d$ln_odds_im <- b0 + b1_year * x1 + b2_ln_gdp_cap * d[[x2]] + b3_share_stap * d[[x3]] + b4_prot_an * d[[x4]]
  d[[y]] <- (exp(d$ln_odds_im) / (1 + exp(d$ln_odds_im))) * 1000
  d$ln_odds_im <- NULL
  return(d)
}
## test for 2010, then predict ref for 2015
# d.im <- predIM(d.im, 2010, "ln_gdp_pcap_ref", "staple", "protein_animal", "pred_im_2010")
d.im <- predIM(d.im, 2015, "ln_gdp_pcap_ref", "staple", "protein_animal", "infant_mortality_pred_ref")
d.im <- predIM(d.im, 2015, "ln_gdp_pcap_drought", "staple_drought", "protein_animal_drought", "infant_mortality_pred_drought")
    ## To estimate IM_drought compared to observed levels, use ratio of predicted ref to predicted drought times observed
    d.im$infant_mortality_drought_level <- (d.im$infant_mortality_pred_drought / d.im$infant_mortality_pred_ref) * d.im$infant_mortality_observed
    d.im$infant_mortality_drought_abs_change <- ((d.im$infant_mortality_pred_drought / d.im$infant_mortality_pred_ref) * d.im$infant_mortality_observed
                                                  - d.im$infant_mortality_observed)
    d.im$infant_mortality_drought_pct_change <- (d.im$infant_mortality_drought_abs_change / d.im$infant_mortality_observed) * 100

# -----------------------------------------------------------------------------
## SENSITIVITY TESTS:
# -----------------------------------------------------------------------------

d.im <- predIM(d.im, 2015, "ln_gdp_pcap_drought", "staple", "protein_animal", "infant_mortality_pred_drought_income")
d.im <- predIM(d.im, 2015, "ln_gdp_pcap_ref", "staple_drought", "protein_animal", "infant_mortality_pred_drought_staple")
d.im <- predIM(d.im, 2015, "ln_gdp_pcap_ref", "staple", "protein_animal_drought", "infant_mortality_pred_drought_protein")
d.im <- predIM(d.im, 2015, "ln_gdp_pcap_ref", "staple_drought", "protein_animal_drought", "infant_mortality_pred_drought_staple_protein")

setwd(datadir)
write.csv(d.im, "IMR_pred_rice_drought.csv")

## Figure
d.fig <- subset(d.im, select = c("urban_rural", "variable", "infant_mortality_observed", "infant_mortality_pred_ref", "infant_mortality_pred_drought"))
    d.fig$income_class <- paste(d.fig$urban_rural, d.fig$variable)
    d.fig <- colnameReplace(d.fig, "variable", "decile")
  d.fig <- melt(d.fig, id.vars = c(1:2,6))
p <- ggplot(d.fig, aes(income_class, value, color = variable)) + geom_point() + theme_basic
print(p)
ggsave(plot = p, (paste("inf_mort_ref_drought.pdf", sep = "")), width = 400, height = 297, units = "mm")


## Comparison observed to predicted
d.fig <- subset(d.im, select = c("urban_rural", "gdp_pcap", "infant_mortality_observed", "infant_mortality_pred_ref"))
  d.fig$gdp_pcap <- d.fig$gdp_pcap * 1000
  d.fig <- colnameReplace(d.fig, "infant_mortality_observed", "Reported")
  d.fig <- colnameReplace(d.fig, "infant_mortality_pred_ref", "Predicted")
  d.fig <- melt(d.fig, id.vars = 1:2)
  ## Use average income per rural/urban wealth quintile (more consistent with reported data)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 70.70000), 1374, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 69.20000), 2048, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 60.60000), 2749, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 42.30000), 3538, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 33.60000), 6059, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 63.30000), 1442, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 49.80000), 2000, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 46.30000), 2436, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 46.20000), 3122, d.fig$gdp_pcap)
  d.fig$gdp_pcap <- ifelse((d.fig$variable == "Reported" & d.fig$value == 27.40000), 7129, d.fig$gdp_pcap)
  d.fig$ln_gdp_pcap <- log(d.fig$gdp_pcap)
  d.fig <- unique(d.fig)

  rep <- subset(d.fig, variable == "Reported")
  summary(reg.rep <- lm(value ~ ln_gdp_pcap, data = rep))
  rep$fit <- predict.lm(reg.rep)
  pred <- subset(d.fig, variable == "Predicted")
  summary(reg.pred <- lm(value ~ ln_gdp_pcap, data = pred))
  pred$fit <- predict.lm(reg.pred)
  d.fits <- rbind(rep, pred)
  d.fits <- subset(d.fits, select = c("urban_rural", "gdp_pcap", "variable", "fit"))
  d.fig <- merge(d.fig, d.fits, by = c("urban_rural", "gdp_pcap", "variable"))

p <- ggplot(d.fig, aes(gdp_pcap, value, color = variable, shape = urban_rural)) + geom_point(size = 6)
  p <- p + theme_basic + scale_color_manual(values = c("royalblue3", "red3"))
  p <- p + labs(x = "Income (2005 USD/cap)", y = "IMR (deaths in first year per 1,000)")
  p <- p + theme(legend.title = element_blank())
  # p <- p + geom_smooth(aes(gdp_pcap, fit, color = variable), size = 0.5, show.legend = FALSE) #+ scale_color_manual(values = c("royalblue3", "red3"))
  print(p)
  setwd(figdir)
  ggsave(plot = p, (paste("imr_rep_pred.pdf", sep = "")), width = 300, height = 235, units = "mm")
