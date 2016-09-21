

# STEP 0. Package Setup and Initialization ========================================================================
library(RMySQL)
library(dplyr)

# Check which user is running the script
user <- Sys.getenv("HOME")

# SET YOUR LOCAL WORKING DIRECTORY HERE 
# pwd <- ...

if (user == "/Users/yanjin.li"){
  # Set working directory
  setwd("/Users/yanjin1993/Google Drive/Columbia University /2016 Fall /Applied Data Science /001 Project")
  # Set local saving directory
  save.path <- "/export_dataframes"
} else {
  
  # Set working directory
  #setwd(pwd)
  # Set local saving directory
  # save.path <- "/export_dataframes"
}


# STEP 1. Data Manipulation and Processing ========================================================================
# 1.1 Data Loading 
datraw.pusa.ss14 <- read.csv("original_dataframes/csv_pus/ss14pusa.csv", header = TRUE)
datraw.pusb.ss14 <- read.csv("original_dataframes/csv_pus/ss14pusb.csv", header = TRUE)

# Variable Reference
# * __SENARIO__: Housing unit/GQ person serial number 
# * __PUMA__: Public use microdata area code 
# * __ST__: State code 
# * __JWMNP__: Travel time to work 
# * __JWTR__: Means of transportation to work 
# * __INDP__: Industry recode for 2013 and later based on 2012 IND codes 
# * __JWAP__: Time of arrival at work - hour and minute 
# * __PINCP__: Total person's income (signed)

datraw.pus <- rbind(datraw.pusa.ss14, datraw.pusb.ss14) %>% 
  select(SERIALNO, PUMA, ST, JWMNP, JWTR, WKHP, WKW, OCCP, INDP, JWAP, PINCP)

# 1.2 Copy original dataframe 
dat.pus <- datraw.pus %>% mutate(INDP = as.character(INDP),
                                 OCCP = as.character(OCCP),
                                 ST = as.numeric(ST))

# 1.3 PUS Data with Industry Info 
datraw.INDP2013 <- read.csv("original_dataframes/INDP_dict_2013.csv", colClasses = "character",  header = FALSE)
INDP.dict.2013 <- datraw.INDP2013 %>% 
  mutate(INDP = substr(V1, 1, 4), industry = substr(V1, 7, 9),
         industry.description = substr(V1, 7, 1000000L)) %>%
  select(-V1)

dat.pus <- as.data.frame(dat.pus) %>% left_join(INDP.dict.2013)

# 1.4 PUS Data with Occupation Info 
datraw.OCCP2013 <- read.csv("original_dataframes/OCCP_dict_2013.csv", colClasses = "character",  header = FALSE)
OCCP.dict.2013 <- datraw.OCCP2013 %>% 
  mutate(OCCP = substr(V1, 1, 4), occupation = substr(V1, 7, 9),
         occupation.description = substr(V1, 7, 1000000L)) %>%
  select(-V1, -V2)

dat.pus <- dat.pus %>% left_join(OCCP.dict.2013)

# 1.5 PUS Data with Location Code 
ST <- c(1, 2, 4:6, 8:13, 15:42, 44:51, 53:56, 72)
ST.code <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", 
             "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
             "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
             "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
             "WV", "WI", "WY", "PR")
dat.code <- data.frame(ST, ST.code)
dat.pus <- dat.pus %>% left_join(dat.code) 

# Remove rows with N/A
dat.pus <- dat.pus %>% na.omit()

# Save to local
save(dat.pus, file = "exported_dataframes/dat.pus")

