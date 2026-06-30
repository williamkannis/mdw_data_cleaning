################################################################################
#
#  Fish length data compilation
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/18/2026

# DESCRIPTION: Formats and merges all length data sets. Data set specific issues
# identified in data cleaning script all dealt with prior to merging. Issues
# common across data sets will be dealt with in the data cleaning script


### House keeping  #############################################################
rm(list=ls())

# Packages
library(readxl)
library(dplyr)
library(janitor)

# Directories
raw_dir <- "raw_data"

# Data
len_srs <- read.csv(file.path(raw_dir,"fslen_1996_2024_p5_SRS.csv"))
len_srs_12 <- read_excel(file.path(raw_dir,"FISH_SRS_2012_PER5.xlsx"))
len_srs_20 <- read_excel(file.path(raw_dir,"FISH_SRS_2020_PER3.xlsx"), sheet =2)
len_ts_wca <- read.csv(file.path(raw_dir,"fslen_WCA.TSL.csv"))
phy_srs <- read.csv(file.path(raw_dir,"phys_1996_2024_p5_SRS.csv"))


### SRS main data prep  ########################################################
srs_format <-len_srs %>% 
  clean_names() %>% 
  mutate(site = as.character(site)) %>% 
  
  # fix 1998 period 5 issue. Create column to retain sample year to aid in the 
  # estimation of sample interals, but fix year for correct grouping
  mutate(year_date = year,
         year = case_when(
            year == 1998 & period == 5 & month==1 ~ 1997,
            T ~ year),
         cum = case_when(
            year == 1997 & cum == 15 ~ 10,
            T ~ cum
        ))


### SRS 2012 and 2020 prep  ####################################################
srs_12_20_format <- len_srs_20 %>% 
  
  # Bind both data sets together (they're in same format minus length)
  mutate(Length = as.numeric(Length)) %>% 
  bind_rows(len_srs_12) %>% 
  
  # Format columns names
  clean_names() %>% 
  rename(comment = comments) %>% 
  mutate(site = as.character(site)) %>% 
  select(-entered_by,-checked_by,-sorted_by) %>% 
  
  # Add cum column from phy data
  left_join(phy_srs %>% clean_names() %>% distinct(year,period,cum),
            by = join_by(year,period))


### WCA and TSL data prep  #####################################################
ts_wca_format <- len_ts_wca %>% 
  
  # Format column names
  clean_names() %>% 
  
  # TSL plots D and E can be treated as their own sites
  mutate(
    site = case_when(
      region == "TSL" & plot %in% c("D","E") ~ paste0(site,"sh"),
      T ~ site
    ) 
  ) %>% 
  select(-nofish,-drnfis)


### Compilation  ###############################################################
len_df <- bind_rows(srs_format,srs_12_20_format,ts_wca_format) %>% 
  
  # add year_date to all data sets
  mutate(year_date = case_when(
    is.na(year_date) ~ year,
    T ~ year_date)) 

# Standardize important comment types and species
losfis <- c(
  "LOSFIS", 
  "LOST", 
  "LOST IN FIELD", 
  "FIELD, L")
prtmis <- c(
  "PRTMIS (", 
  "PRTMIS,", 
  "PRTSMIS",
  "PRTMIS H",
  "PRTMIS (head only).",
  "HEAD AND"
  )
relsed <- c(
  "RELEASED",
  "RELESD", 
  "RELSED", 
  "RELEASED IN FIELD", 
  "FIELD, R"
  )
sitdee <- c("TOODEP")
rotcup <- c("ROTTEN")

len_df <- len_df %>% 
  mutate(
    comment = case_when(
      comment %in% losfis ~ "LOSFIS",
      comment %in% prtmis ~ "PRTMIS",
      comment %in% relsed ~ "RELSED",
      comment %in% sitdee ~ "SITDEE",
      comment %in% rotcup ~ "ROTCUP",
      T ~ comment
    ),
    comment = casefold(comment),
    species = case_when(
      species == "UNKSPP" ~ "UNIFIS",
      species == "ERYSUC" ~ "ERISUC",
      T~species
  ))


### Export  ####################################################################

# final check
colnames(len_df)
summary(len_df)
nrow(len_df) == sum(sapply(list(len_srs,len_srs_12,len_srs_20,len_ts_wca),nrow))
len_df %>% 
  arrange(comment) %>% 
  distinct(comment) %>% 
  pull()
len_df %>% 
  arrange(species) %>% 
  distinct(species) %>% 
  pull()
len_df %>% 
  arrange(site) %>% 
  distinct(site) %>% 
  pull()
len_df %>% 
  group_by(site) %>% 
  summarise(n_plot = n_distinct(plot)) %>% 
  arrange(n_plot) %>%  
  print(n=30)

### MAKE ALL OF THE TESTING A FUNCTION USINF STOPIFNOT COMMANDS AND HAVE IT RETURN
# AN IDNETICAL DATA FRAME IF CORRECT> THIS IS THE ONE THAT EXPORTS> THIS PREVENT
# PROBLAMATIC DATA FROM CONTINUEING> EVERY ISSUE THAT ARISES IS A NEW CHECK,
### MAKE ALL OF THE TESTING A FUNCTION USINF STOPIFNOT COMMANDS AND HAVE IT RETURN
# AN IDNETICAL DATA FRAME IF CORRECT> THIS IS THE ONE THAT EXPORTS> THIS PREVENT
# PROBLAMATIC DATA FROM CONTINUEING> EVERY ISSUE THAT ARISES IS A NEW CHECK,


# Export
saveRDS(len_df,file.path(raw_dir,paste0("fslen_raw_compiled_",Sys.Date(),".rds")))

# continue to compiled_data_exploration.R and fslen_data_cleaning.R

