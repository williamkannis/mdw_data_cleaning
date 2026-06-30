################################################################################
#
#  Fish length data exploration
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/17/2026

# DESCRIPTION: Imports all four fish length data sets, explores data structure,
# and searches for issues with fish lengths. Data structure is examined in order
# to format data sets to facilitate data merging. Species names and comments are
# search for any issues with sampling. Each data cleaning category ends with a
# summary section with actionable items signified with the following symbols:

# !!! = address in data cleaning/compilation
# ??? = ask for clarification
# DONE = issue resolved or addressed in data cleaning scripts
# RSLVD = question answered

### House keeping  #############################################################
rm(list=ls())

# Packages
library(readxl)
library(dplyr)
library(janitor)
library(purrr)

# Directories
raw_dir <- "raw_data"

# Data
len_srs <- read.csv(file.path(raw_dir,"fslen_1996_2024_p5_SRS.csv"))
len_srs_12 <- read_excel(file.path(raw_dir,"FISH_SRS_2012_PER5.xlsx"))
len_srs_20 <- read_excel(file.path(raw_dir,"FISH_SRS_2020_PER3.xlsx"), sheet =2)
len_ts_wca <- read.csv(file.path(raw_dir,"fslen_WCA.TSL.csv"))
sp_code <- read.csv(file.path(raw_dir,"species_code.csv")) %>% clean_names()

data_list <- list(len_srs,len_srs_12,len_srs_20,len_ts_wca)
names(data_list) <- c("srs","srs_12","srs_20","ts_wca")


### Basic data organization    #################################################

### Format names consistently  ###
data_list <- lapply(data_list,clean_names)

### Do columns match among data sets? ###
common_col <- Reduce(intersect,lapply(data_list, colnames))
lapply(data_list, function(x) setdiff(colnames(x),common_col))
# DONE - The 2012 and 2020 SRS do not have a cum column, can be added with phy data
# DONE - The comment column is spelled different across data sets. 
# DONE - ts_wca have columns for drnfis and nofish, these need to be explored

### Standardize comment name  ###
data_list[[2]] <-rename(data_list[[2]],"comment"="comments")
data_list[[3]] <-rename(data_list[[3]],"comment"="comments")

### What is the drnfis column in wca_ts ###
unique(data_list[[4]]$drnfis)
# all values are NA, can be ignored

### What is the nofish column in wca_ts  ###
unique(data_list[[4]]$nofish)
# Binary (1 or NA)
data_list[[4]] %>% filter(!is.na(nofish)) %>% distinct(species)
# values of 1 are always NOfish
data_list[[4]] %>% filter(species == "NOFISH") %>% distinct(nofish)
# no fish column can exist without nofish=1
# This column can be ignored, it doesn't provide any additional information


### What regions are in data?  ###
lapply(data_list, function(x) unique(x[,"region"]))
# each data set has the appropriate regions


### Is CUM unique for each year and period? ###
sapply(data_list,function(x) n_distinct(x$cum) == n_distinct(x$year,x$period))
# the 2012 and 2020 data do not have cum columns


### Do multiple dates exist per period?  ###
sapply(data_list,function (x) x %>% 
         group_by(plot,period,year) %>% 
         summarise(n=n_distinct(paste(month,day,year))) %>% 
         filter(n > 1) %>% 
         nrow())
# multiple plots are sampled over multiple days and this is normal according to
# protocol


### Are site names distinct among regions  ###
bind_rows(lapply(data_list,function(x) x %>%
                   mutate(site= as.character(site)) %>% 
                   distinct(region,site))) %>% 
  distinct(region,site)
# SRS has sites as numeric (6,7,8,23,37,50)
# WCA has sites as character "01"-"11"
# TSL has site as chacter (CP,MD,TS)
# If you change sites to numeric, their will be overlap between
# wca and srs with sites 6 7 and 8. Need to create a unique numeric
# site id that incorporates region and site. 
# !!! THis can be done in Bayesian analysis script


### How many unique plots are there? ###
sapply(data_list, function (x) n_distinct(x$plot))
lapply(data_list, function (x) unique(x[,c("region","plot")]))
# In shark river sites a couple have plots D and E but these are inconsistent and
# from a side project and can be removed! WCA only has three 
# RSLVD - TLS has five plot types, ask about this 
#     DONE - JOEL SAID MAKE D AND E PLOTS


### Do all sites within a region have the same number of plots?
lapply(
  data_list, 
  function (x) x %>% 
    group_by(region,site) %>% 
    summarise(n=n_distinct(plot))
  )

# TS has 2/3 sites with 5 plots. Need to see if these are consistently sampled
lapply(data_list, function(x) x %>% 
         group_by(region,site,plot) %>% 
         summarise(
           n_year = n_distinct(year),
           n_cum=n_distinct(year,period)
           ) %>% 
         filter(plot %in% c("D","E")))
# SRS has 3 sites (6,23,50 - ie., Jeff Kline plots) with 5 plots, Joel says these
# can be ignored because they are inconsistent.
# TSL sites D and E plots were sampled all 30 years with some missing periods


### How many throws per plot?  ###
lapply(data_list, function (x) unique(x[,c("region","throw")]))
# TS and SRS have up to 7 throws, WCA only has a maximum of 6 throws
# RSLVD -what to do about WCA sample effort differences
#    !!! -JOEL: production will be standardized to area

lapply(data_list, function(x) x %>% 
         group_by(region,year,period,site,plot) %>% 
         summarise(n_throw = n_distinct(throw))%>%
                     filter(region == "WCA",n_throw ==6))
# only site 04 plot c during period 2 of 2015 had 6 throws. THis can probably 
# be removed


### Is length numeric?, if not, are there character entries  ##
lapply(data_list, function(x) unique(x$length[is.na(as.numeric(x$length))]))
# only srs_20 has a character for length, this is a ".", fine to change this
# to NA

### Period 5, 1998 issue  ###
lapply(data_list,function(x) x %>% 
         group_by(year,period,site) %>% 
         summarise(
           n_mon = n_distinct(month),
           mon = paste(unique(month),collapse = ",")
           ) %>%
         filter(n_mon > 1))

lapply(data_list,function(x) x %>% 
         group_by(year,period,site) %>% 
         summarise(n_mon = n_distinct(month),mon = unique(month)) %>%
         filter(n_mon > 1) %>% 
         arrange(year,period,site,mon) %>% 
         group_by(year,period,site) %>% 
         mutate(dif = lead(mon)-mon) %>% 
         filter(dif > 1))
# This problem only exits in srs data. all periods sampled over 2 months are
# consquective

# Double check
lapply(
  data_list, 
  function(x) x %>% 
    filter(year == 1998, period == 5) %>% 
    distinct(month) 
  )
# THis is correct


### Data organization to do  ###
# RESLVD - ask about extra plots in TS
#     DONE - JOEL: make D and E SITEsh
# RESLVD - ask about sampling effort differences between SRS and WCA 
#     !!! JOEL: make sampling effort by area
# DONE - clean up column names with janitor
# DONE - remove plots D and E from srs
# DONE - fix 1998 period 5 problem in srs length data
# DONE - fix 1998 period 5 problem in srs phy data
# DONE - fix comment column names srs 12-20
# DONE - add cum column to srs12 and 20
# DONE - remove "entered_by" "checked_by" "sorted_by" from srs12-20
# DONE - remove "drnfis" and "nofish" column from ts_wca
# !!! create unqie site/region id for bayesian analysis. THis can be done in
#     that script. As stan requires 1:n format so it will need to be specific to
#     that analysis


### Species names   ############################################################

### Species names  ###
lapply(data_list, function(x) unique(x$species[order(x$species)]))


### Species names not in species code ###
no_code_sp <-lapply(
  data_list,
  function(x) unique(
    x$species[!x$species %in% sp_code$species_abbreviation]
    )
  )
map2(
  data_list,
  no_code_sp,
  function(x,y) x %>% 
    filter(species %in% y,!is.na(length)) %>%
    select(species) %>% 
    distinct()
  )


### Blank name  ###
lapply(
  data_list, 
  function(x) x %>% 
    filter(species == "") %>% 
    distinct(length,comment)
  ) 
# Blank species names are associated with NA lengths, can be unsampled sites or
# dry sties

### hold name ###
lapply(
  data_list, 
  function(x) x %>% 
    filter(species == "HOLD") %>% 
    distinct(length,comment))
# only at sitedee and vegthk comments and has NA length not an issue


### SPECIES NAMES TO DO SUMMARY  ###
# !!! Explore blank species names more in compilation exploration script 
# DONE - In data cleaning names to remove: "DELETE", "ERYUMB" -dragonfly, 
# "." rotten fish
# DONE - in datacleaning change length of NOFISH to 0
# DONE - fix "ERYSUC" ~ "ERISUC"
# DONE - fix UNKSPP = UNIFIS



### Comments   #################################################################


### All comments  ###
lapply(data_list,function(x) unique(x$comment[order(x$comment)]))


### Comments associated with length-ed fish  ###
lapply(
  data_list,
  function(x) x %>% 
    filter(!is.na(length)) %>% 
    distinct(comment) %>%
    arrange(comment) %>%  
    pull()
  )
# DONE - comments to explore in more detailed:
#         "LOSFIS", "LOST", "LOST IN FIELD", "FIELD, L"
#         "RELEASED", "RELESD", "RELSED", "RELEASED IN FIELD", 
#         "FIELD, R", check this for fish under 80
#         "NOLENG","HEAD AND"


### Explore LOST comments  ###
los_com <- c("LOSFIS", "LOST", "LOST IN FIELD", "FIELD, L")
lapply(
  data_list, 
  function(x) x %>% 
    filter(comment %in% los_com) %>% 
    group_by(length) %>% 
    summarize(n=n())
  )
# lost fish mainly have NA lengths, but can rarely have a length, these should be removed
# DONE remove lengths from missing fish


### Explore RELEASED comments  ###
rel_com <- c("RELEASED", "RELESD", "RELSED", "RELEASED IN FIELD", "FIELD, R")
lapply(
  data_list, 
  function(x) x %>% 
    filter(comment %in% rel_com) %>% 
    arrange(length) %>%  
    distinct(length)
  )
# most released comments have length over 80 which is what the protocol say, but 
# some are missing or smaller, this is likely okay because they measured and id 
# the fish in the field

lapply(
  data_list, 
  function(x) x %>% 
    filter(comment %in% rel_com) %>% 
    arrange(species) %>%  
    distinct(species)
  )
# mainly large species.
# NO ACTION REQUIRED

### Explore other comments  ###
lapply(data_list, function(x) x %>% filter(comment == "NOLENG"))
# DONE - "NOLENG" are only associated with NA or length=1.0. These are likely 
# placeholders and length can be removed

# do lengths of 1 exist without this comment?
lapply(data_list, function(x) x %>% filter(length == 1))
# Length of 1 can exist without NOLENG comment. Joel said don't trust 
# measurements less then 3cm
# DONE - REMOVE fish lengths 3cm and smaller

lapply(data_list, function(x) x %>% filter(comment == "HEAD AND"))
# unknown of what this means, this is likely a part missing issue, but will ask

### are there lengths associated with unsampled/dry columns?###
unsam <- c(
  "VEGTHK",
  "SITDEE", "
  TOODEP",
  "NODATA",
  "EMPCUP",
  "TRLDRY",
  "NOSAMP",
  "HELCOP",
  "SITDRY",
  "ROTTEN")
lapply(data_list,function(x)x %>% filter(comment %in% unsam,!is.na(length)))
# no length data for comments indicating unsamplable conditions nor dry sites,
# or rotten fish
# all good here !


### Comment to do summary ###
# RESLVD - ask if "HEAD AND" refers to missing parts
#     DONE - JOEL: treat as PRTMIS
# DONE - fix sloppy comment names in data cleaning: PRTMIS = 
# c("PRTMIS (", "PRTMIS,", "PRTSMIS",""PRTMIS H","PRTMIS (head only).")
# DONE - fix LOSFIS = c("LOSFIS", "LOST", "LOST IN FIELD", "FIELD, L")
# DONE - comments to remove len: "PRTMIS","UNIFIS", "LOSFIS"
# DONE - remove fish lengths smaller than 3cm
#   ??? ask joel if these should have NA lengths or just remove in general


### End of exploration #########################################################

# continue to phys_data_compilation.R and fslen_data_compilation.R

