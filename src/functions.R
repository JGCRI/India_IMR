# --------------------------------------------------------------------------------------
## printlog: time-stamped output ##
  # params: msg (message", " can be many items); ts (ad timestamp), cr (print CR)
  printlog <- function(msg, ..., ts=TRUE, cr=TRUE) {
    if(ts) cat(date(), " ")
    cat(msg, ..., sep=" ")
    if(cr) cat("\n")
  }

# --------------------------------------------------------------------------------------
## Function "inputData" to read in data and output number of rows and columns and variable names
  ## folderPath = location of folder that contains input file
  ## inputFile = name of .csv file to be read in

inputData <- function(d, folderPath, inputFile, skipNumber)
{
  printlog(paste("Reading data...", inputFile, sep = " "))
  setwd(folderPath)
  d <- read.csv(inputFile, skip = skipNumber, stringsAsFactors = FALSE, header = TRUE)
  printlog(nrow(d), "rows")
  printlog(ncol(d), "columns")
  printlog(colnames(d))
  return(d)
}

# --------------------------------------------------------------------------------------
## Shorter command to replace column names
colnameReplace <- function (d, x, y)
{ 
  colnames(d)[ colnames(d) == x ] <- y  
  return(d)
}

# --------------------------------------------------------------------------------------
## Create year variable in melted dataframe ##
  yr <- function(d)
  {
    d$variable <- as.numeric(substr(d$variable, 2, 5))
    d <- colnameReplace(d, "variable", "year")
    return(d)
  }

# --------------------------------------------------------------------------------------
## Function to count from the end of the string
substrRight <- function(x, n)
  {
  substr(x, nchar(x) - n + 1, nchar(x))
  }

# -----------------------------------------------------------------------------
## Merge columns from two data frames using two variables
mergeData <- function(d1, d2, x1, x2)
{  
  d1$id <- paste(d1[[x1]], d1[[x2]], sep = "_")
  d2$id <- paste(d2[[x1]], d2[[x2]], sep = "_")
  d1 <- merge(d1, d2, by = "id", all.x = TRUE)
  x1.x <- paste(x1, "x", sep = ".")
  x2.x <- paste(x2, "x", sep = ".")
  x1.y <- paste(x1, "y", sep = ".")
  x2.y <- paste(x2, "y", sep = ".")
  d1[[x1.y]] <- NULL
  d1[[x2.y]] <- NULL
  d1 <- colnameReplace(d1, x1.x, x1)
  d1 <- colnameReplace(d1, x2.x, x2)
  d1$id <- NULL
  return(d1)
}

# -----------------------------------------------------------------------------
## Define basic theme for figures
theme_basic <- theme_bw() + 
  theme(legend.text = element_text(size = 16, family = "Gill Sans MT", vjust = .5)) + 
  theme(legend.title = element_text(size = 16, family = "Gill Sans MT", vjust = 2)) +
  theme(axis.text = element_text(size = 16, family = "Gill Sans MT")) + 
  theme(axis.title = element_text(size = 20, family = "Gill Sans MT", face = "bold")) +
  theme(plot.title = element_text(size = 24, family = "Gill Sans MT", face = "bold", vjust = 1)) +
  theme(strip.text = element_text(size = 14, family = "Gill Sans MT"))

# --------------------------------------------------------------------------------------
## ISO replacements for countries with non-standard names (can ad to as they come up)
## d = dataframe name, iso_id = name of column with iso variable (usually "iso"), country_id = name of column with country variable (usually "country_name")
isoReplace <- function(d)
{
  iso <- function(x, y)
  {
    d$iso <- ifelse(d$country_name == x, y, d$iso)
    return(d)
  }
  d <- d %>% 
    iso("Brunei", "brn") %>% 
    iso("Burma (Myanmar)", "mmr") %>% 
    iso("China, mainland", "chn") %>% 
    iso("China, Hong Kong SAR", "hkg") %>% 
    iso("China, Taiwan Province of", "twn") %>% 
    iso("Congo (Brazzaville)", "cog") %>% 
    iso("Congo (Kinshasa)", "cod") %>% 
    iso("Cote dIvoire (IvoryCoast)", "civ") %>% 
    iso("Cote Divoire", "civ") %>% 
    iso("Cote d'Ivoire", "civ") %>% 
    iso("C\x99te d'Ivoire", "civ") %>% 
    iso("C\x92\x82te d'Ivoire", "civ") %>% 
    iso("C\xed\xc7te d'Ivoire", "civ") %>% 
    iso("C\xc8te d'Ivoire", "civ") %>% 
    iso("C̫te d'Ivoire", "civ") %>% 
    iso("C͉te d'Ivoire", "civ") %>% 
    iso("Ethiopia PDR", "eth") %>% 
    iso("Gambia, The", "gmb") %>% 
    iso("Iran", "irn") %>% 
    iso("Korea, North", "prk") %>% 
    iso("Democratic People's Republic of Korea", "prk") %>% 
    iso("Democratic Peoples Republic of Korea", "prk") %>% 
    iso("North Korea", "prk") %>% 
    iso("Korea, South", "kor") %>% 
    iso("Korea", "kor") %>% 
    iso("Republic of Korea", "kor") %>% 
    iso("South Korea", "kor") %>% 
    iso("Laos", "lao") %>% 
    iso("Libya", "lby") %>% 
    iso("Palestinian Territories", "pse") %>% 
    iso("Russia", "rus") %>% 
    iso("Sudan and South Sudan", "sdn") %>% 
    iso("Syria", "syr") %>% 
    iso("Tanzania", "tza") %>% 
    iso("Vietnam", "vnm") %>% 
    iso("Bel-lux", "bel") %>% 
    iso("Belgium-Luxembourg", "bel") %>% 
    iso("Bosnia Herzg", "bih") %>% 
    iso("Brunei Darsm", "brn") %>% 
    iso("Cent Afr Rep", "caf") %>% 
    iso("Czech Rep", "cze") %>% 
    iso("Czech Rep.", "cze") %>% 
    iso("Former Czechoslovakia", "cze") %>% 
    iso("Czechoslovakia", "cze") %>% 
    iso("Dominican Rp", "dom") %>% 
    iso("Eq Guinea", "gnq") %>% 
    iso("Fr Guiana", "guf") %>% 
    iso("Guineabissau", "gnb") %>% 
    iso("Iran", "irn") %>% 
    iso("Laos", "lao") %>% 
    iso("Libya", "lby") %>% 
    iso("Macedonia", "mkd") %>% 
    iso("Moldova Rep", "mda") %>% 
    iso("Papua N Guin", "png") %>% 
    iso("Russian Fed", "rus") %>% 
    iso("Syria", "syr") %>% 
    iso("Tanzania", "tza") %>% 
    iso("Trinidad Tob", "tto") %>% 
    iso("Uk", "gbr") %>% 
    iso("Great Britain", "gbr") %>% 
    iso("Untd Arab Em", "are") %>% 
    iso("United States", "usa") %>% 
    iso("Usa", "usa") %>% 
    iso("USA", "usa") %>% 
    iso("Yugoslav Fr", "yug") %>% 
    iso("Zaire", "cod") %>% 
    iso("Brunei", "brn") %>% 
    iso("Central African Rep.", "caf") %>% 
    iso("Congo DRC", "cod") %>% 
    iso("Moldova", "mda") %>% 
    iso("Russia", "rus") %>% 
    iso("Vietnam", "vnm") %>% 
    iso("Bosnia & Herzegovina", "bih") %>% 
    iso("Cayman Is.", "cym") %>% 
    iso("Cook Is.", "cok") %>% 
    iso("Falkland Is.", "flk") %>% 
    iso("Faroe Is.", "fro") %>% 
    iso("Marshall Is.", "mhl") %>% 
    iso("Micronesia", "fsm") %>% 
    iso("Occupied Palestinian Territory", "pse") %>% 
    iso("Sao Tome & Principe", "stp") %>% 
    iso("Solomon Is.", "slb") %>% 
    iso("St. Kitts & Nevis", "kna") %>% 
    iso("St. Kitts and Nevis", "kna") %>% 
    iso("St. Lucia", "lca") %>% 
    iso("St. Vincent & the Grenadines", "vct") %>% 
    iso("St.Vincent and Grenadines", "vct") %>% 
    iso("Saint Vincent/Grenadines", "vct") %>% 
    iso("Svalbard", "sjm") %>% 
    iso("The Bahamas", "bhs") %>% 
    iso("The Gambia", "gmb") %>% 
    iso("Timor-Leste", "tls") %>% 
    iso("Trinidad & Tobago", "tto") %>% 
    iso("Turks & Caicos Is.", "tca") %>% 
    iso("Virgin Is.", "vir") %>% 
    iso("Bahamas, The", "bhs") %>% 
    iso("Falkland Islands (Islas Malvinas)", "flk") %>% 
    iso("Former Yugoslavia", "yug") %>% 
    iso("Yugoslav SFR", "yug") %>% 
    iso("Timor-Leste (East Timor)", "tls") %>% 
    iso("Turks and Caicos Islands", "tca") %>% 
    iso("Virgin Islands,  U.S.", "vir") %>% 
    iso("Antigua and Barbuda", "atg") %>% 
    iso("Bolivia (Plurinational State of)", "bol") %>% 
    iso("British Virgin Islands", "vgb") %>% 
    iso("Cabo Verde", "cpv") %>% 
    iso("Democratic Republic of the Congo", "cod") %>% 
    iso("Iran (Islamic Republic of)", "irn") %>% 
    iso("Lao People's Democratic Republic", "lao") %>% 
    iso("Libya", "lby") %>% 
    iso("Micronesia (Federated States of)", "fsm") %>% 
    iso("R\x8eunion", "reu") %>% 
    iso("R̩union", "reu") %>% 
    iso("R\x92\xa9union", "reu") %>% 
    iso("R\xed\xa9union", "reu") %>% 
    iso("Rͩunion", "reu") %>% 
    iso("Republic of Moldova", "mda") %>% 
    iso("Saint Helena, Ascension and Tristan da Cunha", "shn") %>% 
    iso("South Sudan", "ssd") %>% 
    iso("Sudan (former)", "sdn") %>% 
    iso("The former Yugoslav Republic of Macedonia", "mkd") %>% 
    iso("Turks and Caicos Islands", "tca") %>% 
    iso("United Republic of Tanzania", "tza") %>% 
    iso("United States Virgin Islands", "vir") %>% 
    iso("USSR", "svu") %>% 
    iso("Venezuela (Bolivarian Republic of)", "ven") %>% 
    iso("Wallis and Futuna Islands", "wlf") %>% 
    iso("West Bank and Gaza Strip", "pse") 
  return(d)
}

# --------------------------------------------------------------------------------------


