################################################################################
#
#  Physical data cleaning
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/20/2026

# DESCRIPTION: Prepares physical site data by correcting issues identified in
# compiled data exploration scripts. Returns a data set that is ready to pair
# with fish length data set to filter out "unsampleable" conditions and to be
# as explanatory variables in analysis. Each data cleaning step has justification
# given for that cleaning step. Script ends with a function to check that all 
# issues are addressed properly before exporting.


### House keeping  #############################################################
rm(list=ls())

# Packages
library(dplyr)
library(purrr)

# Directories
raw_dir <- "raw_data"
clean_dir <- "cleaned_data"

# Data (make sure is most up to date!)
phy_df <- readRDS(file.path(raw_dir,"phys_raw_compiled_2026-02-24.rds")) 
len_df <- readRDS(file.path(raw_dir,"fslen_raw_compiled_2026-02-24.rds"))


### Fix duplication issue  #####################################################

# Complied data exploration script discovered an issue in the physical data
# where throws were duplicated at several WCA sites during period 5 of 2020.
# Sites were initially visited in late December but conditions were 
# unsampleable, so sites were revisited in early January. This issue arises
# because the nosample entries were not removed when the sampled entries were
# entered, leading to sampled and unsampled entry for every throw at those sites.
# Additionally, the sampled throws during this time were mistakenly assigned
# cum 120 rather than the correct value of 125.

# extract duplicated throws
dup <- phy_df %>% 
  group_by(year,period,site,plot,throw) %>% 
  filter(
    n()>1,
    month ==12
  ) 

# remove duplicated sample
phy_df_nodup <- phy_df %>% 
  anti_join(dup) %>% 
  
  # fix incorrect cum value
  mutate(
    cum = case_when(
      year == 2020 & cum == 120 ~ 125,
      T~ cum
    )
  )

# did only duplicated rows get removed?
nrow(phy_df_nodup) == nrow(phy_df)-nrow(dup)


### Fix erroneous comments  ####################################################

# Complied data exploration script discovered an issue in the physical data
# where comments suggesting that fish data were not collected due to 
# "unsampleable" conditions where incorrectly assigned to throws where fish data
# where collected. This needs to be correct so fish data is not mistaken cleaned
# out of fish length data set in length cleaning scripts.

# combine site and length data
combine_df <- phy_df %>% 
  left_join(len_df)

# Extract throws marked site deep, but with length data
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## THIS TENANTIVE ON Q RESPONSE  ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
sitdee_issue <- combine_df %>% 
  filter(
    !is.na(sitdee),
    !is.na(length)) %>% 
  distinct(region,cum,site,plot,throw) %>% 
  mutate(sitdee_issue = T)

# Extract throws marked site dry, but with length data
sitdry_issue <- combine_df %>% 
  filter(
    !is.na(sitdry),
    !is.na(length)) %>% 
  distinct(region,cum,site,plot,throw) %>% 
  mutate(sitdry_issue = T)

# Extract throws marked veg thick, but with length data
vegthk_issue <- combine_df %>% 
  filter(
    !is.na(vegthk),
    !is.na(length) | comment == "prtmis"
    ) %>% 
  distinct(region,cum,site,plot,throw) %>% 
  mutate(vegthk_issue = T)

# Extract throws marked nondata, but with lengths
nodata_issue <- combine_df %>% 
  filter(
    !is.na(nodata),
    !is.na(length)
  ) %>% 
  distinct(region,cum,site,plot,throw) %>% 
  mutate(nodata_issue = T)

# Create one dataframe for data issues
issue_df <- reduce(list(sitdee_issue,sitdry_issue,vegthk_issue,nodata_issue),
                     full_join)


# Change erroneous comments
phy_df_fixed <- phy_df_nodup %>% 
  left_join(issue_df) %>% 
  mutate(
    sitdee = case_when(
      sitdee_issue == T ~ NA,
      T~sitdee
    ),
    sitdry = case_when(
      sitdry_issue == T ~ NA,
      T~sitdry
    ),
    vegthk = case_when(
      vegthk_issue ==T ~ NA,
      T~vegthk
    ),
    nodata = case_when(
      nodata_issue == T ~ NA,
      T~nodata
    )
  ) %>% 
  select(-sitdee_issue,-vegthk_issue,nodata_issue)

# Did the number of rows remain the same?
nrow(phy_df_fixed) == nrow(phy_df_nodup)


### Water year and period  #####################################################

# Data is kept using calender year. This does not reflect the water cycle in
# the everglades, water management follows a water year May-April. Here, a water 
# period and year will be assigned to each line of data. A water year begins on
# calender period 3 and ends on period 2 of the following year. A water year is 
# named after the year of that May So a sampled taken in feb of 2024, would
# be in water year 2023.

# The physical data set already has columns for wateryear and waterperiod but
# these were mainly based on months. So the number of periods per wateryear is
# inconsistent among years and sites. Waterperiod in this data also refer to month
# codes (1-12) as opposed to period codes (1-5). We will retain this columns 
# with adjusted names and create new columns for our water year/period values.

# Create new wateryear and period columns
phy_df_wy <- phy_df_fixed %>% 
  rename(
    wateryear_old =wateryear,
    waterperiod_old = waterperiod
  ) %>% 
  mutate(
    wateryear = case_when(
      period < 3 ~year -1,
      period >= 3 ~ year,
    ),
    waterperiod = case_when(
      period >= 3 ~ period -2,
      period < 3 ~ period +3
    )
  ) 

# Did new columns get created?
setdiff(colnames(phy_df_wy),colnames(phy_df_fixed))

# Check that waterperiod and year match the preexisting cum order
# MAYBE MOVE THIS TO FINAL CHECK
phy_df_wy %>% 
  distinct(cum,year,period,wateryear,waterperiod) %>% 
  arrange(cum) %>% 
  group_by(wateryear) %>%
  mutate(l = lead(waterperiod,order_by = cum)-waterperiod) %>% 
  filter(1 != 1) %>% 
  nrow()==0


### Misc. cleaning  ############################################################

# Remove plots D and E from SRS, these were sampled for side project and were not
# consistently sampled across time series.

# Extract data for SRS plots D and E
bad_plot <- phy_df_wy %>% 
  filter(
    region == "SRS",
    plot %in% c("D","E")
  )

# remove D and E data
phy_df_clean <- phy_df_wy %>% 
  anti_join(bad_plot) 

# Did the correct number of plots get removed?
nrow(phy_df_clean) == nrow(phy_df_wy)- nrow(bad_plot)


### Final Check  ###############################################################
summary(phy_df_wy)

# Did all duplicated throws get removed?
phy_df_clean %>% group_by(cum,year,period,site,plot,throw) %>% filter(n()>1) %>% nrow()==0
phy_df_clean %>% group_by(cum,site,plot,throw) %>% filter(n()>1) %>% nrow()==0
phy_df_clean %>% group_by(year,period,site,plot,throw) %>% filter(n()>1) %>% nrow()==0

# year/period/cum combos?
phy_df_clean %>% 
  arrange(cum) %>% 
  distinct(cum,year,period) %>% 
  group_by(cum) %>% 
  filter(n()>1) %>% 
  nrow() == 0

# Are all non-sampled comments no longer associated with length data?
phy_df_clean %>% 
  filter(
    !is.na(vegthk) |
      !is.na(sitdee) |
      !is.na(sitdry) |
      !is.na(notvis) |
      !is.na(nodata) |
      !is.na(helcop) |
      !is.na(trldry)
  ) %>% 
  left_join(len_df) %>% 
  filter(!is.na(length)) %>% 
  nrow() == 0

### MAKE ALL OF THE TESTING A FUNCTION USINF STOPIFNOT COMMANDS AND HAVE IT RETURN
# AN IDNETICAL DATA FRAME IF CORRECT> THIS IS THE ONE THAT EXPORTS> THIS PREVENT
# PROBLAMATIC DATA FROM CONTINUEING> EVERY ISSUE THAT ARISES IS A NEW CHECK,


### Export  ####################################################################
saveRDS(phy_df_clean,file.path(clean_dir,paste0("phys_cleaned_",Sys.Date(),".rds")))


# Continue to fslen_data_cleaning.R and/or analysis specific scripts.

