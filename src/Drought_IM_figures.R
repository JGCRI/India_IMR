# Script to generate IMR figures
# STW, August 2016

# -----------------------------------------------------------------------------
# Input data
d <- inputData(d, datadir, "IMR_pred_rice_drought.csv", 0)

# -----------------------------------------------------------------------------
## Figures

setwd(figdir)
figure <- function(d, x1, x2, z1, z2, z3)
{
  d.fig <- subset(d, variable == x1 | variable == x2)
  p <- ggplot(d.fig, aes(IMR_quint, value, fill = factor(variable, labels = c("Reference", "Drought")))) 
  p <- p + geom_bar(position = "dodge", stat = "identity") 
  p <- p + theme_basic + scale_fill_manual(values = c("royalblue3", "red3"))
  p <- p + ggtitle(paste(z1, "by wealth index", sep = " ")) + labs(x = z2, y = z3)
  p <- p + theme (axis.title.x = element_text(vjust = -0.35), axis.title.y = element_text(vjust = 0.5), plot.title = element_text(vjust = 1.5))
  p <- p + theme(legend.title = element_blank()) 
  print(p)
  ggsave(plot = p, paste(x2, ".pdf", sep = ""), width = 400, height = 297, units = "mm")
}

figure(d.im.quintile, "gdp_pcap", "gdp_pcap_drought", "Reference and drought GDP/cap, ", "Rural/Urban Wealth Quintile", "GDP/cap (thousand 2011USD)")
figure(d.im.quintile, "infant_mortality", "infant_mortality_drought", "Reference and drought infant mortality rate, ", "Rural/Urban Wealth Quintile", 
        "Infant Mortality (deaths < 1 year per 1,000 births)")
figure(d.im.quintile, "protein_animal", "protein_animal_drought", "Reference and drought protein consumption, ", "Rural/Urban Wealth Quintile", 
        "Animal protein (g per capita per day)")
figure(d.im.quintile, "staple", "staple_drought", "Reference and drought calorie share from staples, ", "Rural/Urban Wealth Quintile", 
        "Consumption share of staple commodities (percent)")

figure2 <- function(d, x1, z1, z2, c1, c2)
{
  d.fig <- subset(d, variable == x1)
  p <- ggplot(d.fig, aes(IMR_quint, value, fill = factor(urban_rural, labels = c("Rural", "Urban")))) 
  p <- p + geom_bar(stat = "identity") 
  p <- p + theme_basic + scale_fill_manual(values = c(c1, c2))
  p <- p + ggtitle(paste(z1)) 
  p <- p + labs(x = "Rural/Urban Wealth Quintile", y = "Percent change")
  p <- p + theme (axis.title.x = element_text(vjust = -0.35), axis.title.y = element_text(vjust = 0.5), plot.title = element_text(vjust = 1.5))
  p <- p + theme(legend.title = element_blank()) 
  print(p)
  ggsave(plot = p, z2, width = 400, height = 297, units = "mm")
}

figure2(d.im.quintile, "gdp_pcap_pct_change", "Percent change GDP/cap, by wealth index", "Percent_change_drought_gdp_pcap.pdf", "green4", "darkorchid4")
figure2(d.im.quintile, "IMR_pct_change", "Percent change Infant Mortality Rate, by wealth index", "Percent_change_drought_imr.pdf", "green4", "darkorchid4")
figure2(d.im.quintile, "total", "Reference caloric consumption, by wealth index", "Ref_calories.pdf", "green4", "darkorchid4")

d.fig <- subset(d.im.quintile, variable == "staple_share_pct_change" | variable == "animal_protein_pct_change")
p <- ggplot(d.fig, aes(IMR_quint, value, fill = factor(variable, labels = c("Animal protein", "Share staples")))) 
  p <- p + geom_bar(position = "dodge", stat = "identity") #+ facet_wrap(~ variable)
  p <- p + theme_basic + scale_fill_manual(values = c("darkorange2", "gold"))
  p <- p + ggtitle(paste("Percent change in staple and protein consumption, by wealth index", sep = " ")) 
  p <- p + labs(x = "Rural/Urban Wealth Quintile", y = "Percent change")
  p <- p + theme (axis.title.x = element_text(vjust = -0.35), axis.title.y = element_text(vjust = 0.5), plot.title = element_text(vjust = 1.5))
  p <- p + theme(legend.title = element_blank()) 
  print(p)
ggsave(plot = p, paste("Percent_change_staple_protein.pdf", sep = ""), width = 400, height = 297, units = "mm")
  d.fig <- NULL



