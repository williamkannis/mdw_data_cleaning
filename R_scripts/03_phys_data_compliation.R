################################################################################
#
#  Physical data compilation 
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/18/2026

# DESCRIPTION: Checks for differences in data structure of the physical data
# for SRS and WCA/TSL. Formats data sets into a consistent format and merges
# them into one data frame. 


### House keeping  #############################################################
rm(list=ls())

# Packages
library(dplyr)
library(janitor)

# Directories
raw_dir <- "raw_data"

# Data
phy_srs <- read.csv(file.path(raw_dir,"phys_1996_2024_p5_SRS.csv"))
phy_ts_wca <- read.csv(file.path(raw_dir,"phys_WCA.TSL.csv"))


### Unmatched colnames  ########################################################
colnames(phy_srs)[!colnames(phy_srs) %in% colnames(phy_ts_wca)]
colnames(phy_ts_wca)[!colnames(phy_ts_wca) %in% colnames(phy_srs)]
# ts_wca has a differnt name for rotcup, rotten. This can be renamed
# phy_ts has additional sample columns (e.e.g, phys_vegthk)..
phy_ts_wca %>% group_by(VEGTHK,PHYS_VEGTHK) %>% summarise(n=n())
phy_ts_wca %>% group_by(HELCOP,PHYS_HELCOP) %>% summarise(n=n())
phy_ts_wca %>% group_by(TRLDRY,PHYS_TRLDRY) %>% summarise(n=n())
phy_ts_wca %>% group_by(EMPCUP,PHYS_EMPCUP) %>% summarise(n=n())
phy_ts_wca %>% group_by(NOSAMP,PHYS_NOSAMP) %>% summarise(n=n())
phy_ts_wca %>% group_by(SITDEE,PHYS_SITDEE) %>% summarise(n=n())
# All of these are binary with 1 or NA  and mostly overlap witht eh normal  
# column. There is no case of the extra column having a 1 and the normal column 
# not. The maximum number of 1's is 50, but most have a few or none. This 
# column can be ignored.

### Check for 1998 issue
lapply(list(phy_srs,phy_ts_wca),function(x) x %>% 
         clean_names() %>% 
         group_by(year,period,site) %>% 
         summarise(
           n_mon = n_distinct(month),
           mon = paste(unique(month),collapse = ",")
           ) %>%
         filter(n_mon > 1))

lapply(list(phy_srs,phy_ts_wca),function(x) x %>% 
         clean_names() %>% 
         group_by(year,period,site) %>% 
         summarise(
           n_mon = n_distinct(month),
           mon = unique(month)
           ) %>%
         filter(n_mon > 1) %>% 
         arrange(year,period,site,mon) %>% 
         group_by(year,period,site) %>% 
         mutate(dif = lead(mon)-mon) %>% 
         filter(dif > 1))
# WCA had one period in which samples were taken more than 1 month apart
phy_ts_wca %>% 
  clean_names() %>% 
  group_by(year,period,site) %>% 
  summarise(
    n_mon = n_distinct(month),
    mon = paste(unique(month),
                collapse = ",")
    ) %>%
  filter(
    n_mon > 1,
    period==5,
    year ==2020)  
phy_ts_wca %>% 
  clean_names() %>% 
  filter(
    year == 2020,
    period == 5,
    site %in% sapply(5:8,function(x) paste0("0",x))
    )
# This is okay, because they are actually consequative dates 12-28 to 1-6
# No issues with wca only SRS 1998


### Format and merge data frames  ##############################################

# Format tsl/wca column names
ts_wca_format <- phy_ts_wca %>% 
  rename(ROTCUP = ROTTEN) %>% 
  mutate(
    year_date = YEAR,  # use for sample interval length calculation
    SITE = case_when(
      REGION == "TSL" & PLOT %in% c("D","E") ~ paste0(SITE,"sh") , # plots D and E are their own sites
      T ~ SITE
      )    
    )  
  
## CHANGE PLOTS D AND E TO SITES "sh"

# Format to match columns
srs_format <- phy_srs %>% 
  mutate(
     SITE= as.character(SITE),
         
     # fix 1998 period 5 issue.
     year_date = YEAR,
     YEAR = case_when(
       YEAR == 1998 & PERIOD == 5 & MONTH==1 ~ 1997,
       T ~ YEAR),
     CUM = case_when(
       YEAR == 1997 & CUM == 15 ~ 10,
       T ~ CUM
     )
   )

# Combine and clean columns
phy_df <- srs_format %>% 
  bind_rows(ts_wca_format) %>% 
  clean_names()
  

### Export data  ###############################################################

# final check
colnames(phy_df)
summary(phy_df)
nrow(phy_df) == sum(nrow(phy_ts_wca),nrow(phy_srs))

### MAKE ALL OF THE TESTING A FUNCTION USINF STOPIFNOT COMMANDS AND HAVE IT RETURN
# AN IDNETICAL DATA FRAME IF CORRECT> THIS IS THE ONE THAT EXPORTS> THIS PREVENT
# PROBLAMATIC DATA FROM CONTINUEING> EVERY ISSUE THAT ARISES IS A NEW CHECK,

# Export
saveRDS(phy_df,file.path(raw_dir,paste0("phys_raw_compiled_",Sys.Date(),".rds")))

# continue to compiled_data_exploration.R and phys_data_cleaning.R

