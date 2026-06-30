################################################################################
#
#  Fish length data cleaning
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/20/2026

# DESCRIPTION: Prepares fish length data by correcting issues identified in
# compiled data exploration scripts. Returns a data set that is ready to pair
# with the physical data set for analysis. Each data cleaning step has 
# justification  given for that cleaning step. Script ends with a function to 
# check that all issues are addressed properly before exporting.


### House keeping  #############################################################
rm(list=ls())

# Packages
library(dplyr)

# Directories
raw_dir <- "raw_data"
clean_dir <- "cleaned_data"
trait_dir <-"trait_data"

# Data (make sure is most up to date!)
len_df <- readRDS(file.path(raw_dir,"fslen_raw_compiled_2026-02-24.rds"))
phy_df <- readRDS(file.path(clean_dir,"phys_cleaned_2026-02-25.rds"))  
max_len_df <- readRDS(file.path(trait_dir,"fslen_max_length_2026-02-24.rds"))

### Remove Incomplete or issued samples  #######################################

# Remove plots D and E from SRS, these were sampled for side project and were not
# consistently sampled across time series. Additionally, an extra throw with a
# single fish was taken at a WCA site, remove this

# Extract data for SRS plots D and E
bad_sample <- len_df %>% 
  filter(
    region == "SRS" & plot %in% c("D","E") |
      region == "WCA" & throw == 6
  )

# remove problem data
len_df_rmplot <- len_df %>% 
  anti_join(bad_sample)

# Did the correct number of plots get removed?
nrow(len_df_rmplot) == nrow(len_df) - nrow(bad_sample)


### Filter using physical data comments  #######################################

# Physical data set contains comments then can be used to identify circumstances
# in which fish samples was not taken during a throw. It is important to 
# distinguish these instances from when no fish were observed (true zero). Below
# we remove throws where fish were not sampled

# Identify throws that were not sampled
nosamp_throws <- phy_df %>% 
  filter(
    !is.na(sitdee) |    # Site was deep and no sample was taken
      !is.na(vegthk) |  # Vegetation was too thick to sample
      !is.na(notvis) |  # Site was not visited
      !is.na(helcop) |  # These throws were skipped due to logistics related to helicopter
      !is.na(trldry) |  # Was not able to access site due to dry conditions
      !is.na(nodata) |  # No sample taken and no reason given
      !is.na(nosamp)    # No sample taken and no reason given
  )

# Are Non sampled throws free of length data?
nosamp_throws %>% 
  left_join(len_df_rmplot) %>% 
  filter(!is.na(length) | comment == "prtmis") %>% 
  nrow() == 0

# remove from data
len_df_sampled <- len_df_rmplot %>% 
  anti_join(nosamp_throws)

# were only intended columns removed?
nrow(len_df_sampled)  == 
  nrow(len_df_rmplot) - nrow(len_df_rmplot %>% inner_join(nosamp_throws))


### Filter using length comments  ##############################################

# The physical data comments eliminate much of the non-sampled throws, but there
# are instance where physical comments are absent but length comments indicate
# fish were not sampled during a throw. Below we will remove throws based on
# length data. When the comment "empcup" is on its own, it indicates that no  
# fish were collected, and does not indicate missing data. As such we will  
# retain these data.

# Comments that indicate sample was not taken
unsam <- c("vegthk",  # Vegetation was too thick to sample
           "sitdee",  # Site was deep and no sample was taken
           "notvis",  # Site was not visited
           "helcop",  # These throws were skipped due to logistics related to helicopter
           "trldry",  # Was not able to access site due to dry conditions
           "nodata",  # No sample taken and no reason given
           "nosamp")  # No sample taken and no reason given

# Remove above comments from data
len_df_filtered <- len_df_sampled %>% 
  filter(!comment %in% unsam)

# were only intended columns removed?
nrow(len_df_filtered)  == 
  nrow(len_df_sampled) - nrow(len_df_sampled %>% filter(comment %in% unsam))


### Extra throws from physical data  ###########################################

# In addition to identifying throws where fish were not sampled, the physical
# data set identifies instances where throw data are missing from the length 
# data set, but were not recorded because fish were not found due to dry 
# conditions (i.e., true zeros). We will want to retain this data. Below, 
# additional dry site data from physical data set are formatted and added to 
# length data.

# Extract extra throws from dry sampling dates
extra_throws <- phy_df %>% 
  anti_join(
    len_df_filtered, 
    by =join_by(region,cum,year,period,site, plot,throw,)
  ) %>% 
  filter(!is.na(sitdry)) %>%  
  select(
    region, cum, year, period, month, 
    day, site, plot, throw, year_date
  ) %>%
  mutate(
    species = "SITDRY", 
    sex = "",
    comment = "sitdry",
  )
 
# Add extra throws to data
len_df_all <- len_df_filtered %>% 
  bind_rows(extra_throws)

# Did we retain the same number of columns?
ncol(len_df_all) == ncol(len_df_filtered)

# are column names still the same?
unique(colnames(len_df_all) == colnames(len_df_filtered))

# Did we add the appropriate amount of rows
nrow(len_df_all) == nrow(len_df_filtered) + nrow(extra_throws)


### Standardize species names  #################################################

# The data has duplicate species name codes that functionally have
# the same meaning for our purpose (e.g., NOFISH = c(EMPCUP,"SITDRY)) and here
# we consolidate these to single names. Additionally some entries have BLANK
# species names that have meaning dependent on comment data. Here we use comments
# to inform these species names. Finally, there are some species names that indicate
# missing samples or taxa outside of our interest, and these can be removed.
# standardize these names.

# check what comments blank names have
len_df_all %>% 
  filter(species == "") %>% 
  distinct(comment) %>% 
  arrange(comment) %>% 
  pull()

len_df_standname <- len_df_all %>% 
 
  # Change blank names to comment specific names
   mutate(
    species = case_when(
      species == "" ~ casefold(comment,upper=T),
      T ~ species
    ),
    
    # Standardize species names
    species = case_when(
      species %in% c("EMPCUP","SITDRY") ~ "NOFISH",  # empcup and sitdry indicate true zeros
      species %in% c("PRTMIS","LOSFIS") ~ "UNIFIS",  # if fish are lossed or have parts missing, treat as unid fish
      species == "NOFISH" & !is.na(length) ~ "UNIFIS", # if nofish entries have lengths, this is likely a mistake
      species == "ROTCUP" ~ ".",  # create one name for rotten specimens
      T ~ species
    )
  ) %>% 
  
  # Remove problematic species
  filter(
    !species %in%  c(".",        # indicates that fish specimen was not preserved properly, thus no data were available 
                     "ERYUMB",   # This is a dragonfly, this belongs in invert data not fish. remove for now
                     "DELETE",   # Contact said this could be removed 
                     "")           # for now remove remaining blank sp names !!! DEPENDENT ON JOELS RESPONSE
  )  
## WHAT TO DO WITH BLANK NAME, BLANK COMMENT??

# Did the correct number of rows get removed?
nrow(len_df_standname) == 
  nrow(len_df_all) - nrow(
    len_df_all %>% 
      filter(
        species %in%  c(".","ERYUMB","DELETE") | 
          species == "" & comment %in% c("","rotcup")
        )
    )


### Duplicated no fish entries  ################################################

# There are instances where NOFISH species entries occur in throws with either
# other entries of NOFISH or with actual length data. In cases when NOFISH
# entries are duplicated, we can keep just one. In the case where NOFISH occurs
# in throws with fish data, we can assume this is an error and remove the NOFISH
# entry from these throws.

# Find throws with duplicate NOFISH lines, and retain single line for each
nofish_dup <- len_df_standname %>% 
  filter(species == "NOFISH") %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(n() > 1) %>% 
  slice(1)

# Find throws with NOFISH and fish lines, remove NOFISH lines
nofish_incor <-len_df_standname %>% 
  inner_join(
    len_df_standname %>% 
      filter(species == "NOFISH") %>% 
      distinct(cum,region,site,plot,throw)
    ) %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(
    n_distinct(species) > 1,
    !species %in% c("NOFISH")
    )

# Replace above throws in the data set with cleaned versions
len_df_nodup <- len_df_standname %>% 
  anti_join(
    nofish_dup,
    join_by(year,cum,period,region,site,plot,throw,species)
    ) %>% 
  bind_rows(nofish_dup) %>% 
  anti_join(
    nofish_incor, 
    join_by(year,cum,period,region,site,plot,throw)
    ) %>% 
  bind_rows(nofish_incor)

# Are the right amount of rows retained?
nrow(len_df_nodup) == 
  nrow(len_df_standname) - 
  nrow(len_df_standname %>% 
         filter(species == "NOFISH") %>% 
         group_by(year,cum,period,region,site,plot,throw) %>% 
         filter(n() > 1) %>% 
         slice(2:n())) - 
  nrow(len_df_standname %>% 
         inner_join(len_df_standname %>% 
                      filter(species == "NOFISH") %>% 
                      distinct(cum,region,site,plot,throw)) %>% 
         group_by(year,cum,period,region,site,plot,throw) %>% 
         filter(n_distinct(species) > 1 &
                species %in% c("NOFISH")) %>% 
         slice(1))

### Erroneous and missing lengths  #############################################

# Some species names, length values, or comments indicate that length values are 
# incorrect. Below we will change problematic lengths to NAs:

# 1. Fish were measured with calipers and reported to hundred place, its not
#    likely that the calipers have this precision, round to mm
# 2. Traps are designed to sample fish samller than 80mm so all larger fish
#    need to be removed
# 3. Fish with length < 3 are not likely to be true measurements, change to NA
# 4. When comments indicate the fish have a part missing or were lost, the 
#    lengths are not valid. Change to NA
# 5. When species names are UNFIS, this should have length of NA to help missing
#    data imputation
# 6. When no fish are present, change length to zero as placeholder
# 7. Fish larger than their recorded maximum length should be changed to NA

## !!! THINK IF YOU WANT TO ADD FIVE TO MAX LENGTH. THIS WILL DEPEND ON JOELS
## !!! RESPONSE. WHATERVER YOU DO,UPDATE TEST AFTER

# Are all species included in life history data?
len_df_nodup %>% 
  filter(!species %in% max_len_df$species,
         !species %in% c("UNIFIS","NOFISH")) %>% 
  nrow() == 0

# How many fish are larger than recorded maximum length
len_df_nodup %>% 
  left_join(max_len_df) %>% 
  filter(length > max_len)

# Remove fish larger than 80mm with the exception of small bodied fish with 
# erroneously large lengths, these will have lengths dealt with in the next
# section.
len_df_rmlen <- len_df_nodup %>% 
  left_join(max_len_df) %>% 
  filter(length <= 80 | 
           length > 80 & max_len <= 80 |
           is.na(length)) %>%  
  
  # Address erroneous lengths
  mutate(
    length = round(length), # lengths should be rounded to nearest mm
    length = case_when(
      length > max_len ~ NA_real_,  # remove lengths of fish much larger than recorded max lengths
      length < 3 ~ NA_real_,  # lengths under 3mm are not likely to be real measurements
      comment %in%c("prtmis","losfis")~ NA_real_,  # remove partial lengths or missing fish
      species == "UNIFIS" ~ NA_real_,  # remove lengths of unidentified fish to aid imputation
      species == "NOFISH" ~ 0,  # When no fish are present, change length to zero as placeholder
      T ~ length  
      )
    ) %>% 
  select(-max_len)

# Were the correct number of rows removed?
nrow(len_df_nodup) -
(nrow(len_df_nodup %>% 
       filter(length>80)) - 
  nrow(len_df_nodup %>% 
         left_join(max_len_df) %>% 
         filter(length>80,max_len <=80))) == nrow(len_df_rmlen)


### Water year conversion  #####################################################

# Data is kept using calender year. This does not reflect the water cycle in
# the everglades, water management follows a water year May-April. Here, a water 
# period and year will be assigned to each line of data. A water year begins on
# calender period 3 and ends on period 2 of the following year. A water year is 
# named after the year of that May So a sampled taken in feb of 2024, would
# be in water year 2023.

# Create water year and period columns
len_df_wy <- len_df_rmlen %>% 
  mutate(
    waterperiod = case_when(
      period >= 3 ~ period -2,
      period < 3 ~ period +3
    ),
    wateryear = case_when(
      period < 3 ~year -1,
      period >= 3 ~ year,
    )
  ) 
  

### Final formatting  ###########################################################

# Consolidate date columns into one date column and retain only useful columns
len_df_for <- len_df_wy %>% 
  mutate(date = as.Date(paste(year_date,month,day,sep = "-"))) %>% 
  select(cum,year,wateryear,period,waterperiod,date,
         region,site,plot,throw,species,length,sex,comment)

# Did dates get created correctly?
summary(len_df_for$date)

# What columns are not included?
setdiff(colnames(len_df_wy),colnames(len_df_for))


### Final check  ###############################################################

# Are  problematic throws and plots removed?
df %>% 
  filter(
    region == "SRS" & plot %in% c("D","E") |region == "WCA" & throw > 5 ) %>% 
  nrow() == 0
                       
# are there comments left that imply sites were not sampled?
df %>% filter(comment %in%  unsam) %>% nrow() == 0  

# are problematic and duplicate species codes removed?
df %>% 
  filter(species %in% c("",
                        "SITDRY",
                        "ROTOCUP",
                        "PRTMIS",
                        "LOSFIS",
                        "ERYUMB",
                        "DELETE")) %>% 
  nrow() == 0

# Are NOFISH entries duplicates removed?
df %>% 
  filter(species == "NOFISH") %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(n() > 1) %>% 
  nrow ==0

# Are nofish entires absent from throws with length data
df %>% 
  inner_join(df %>% 
               filter(species == "NOFISH") %>% 
               distinct(cum,region,site,plot,throw)) %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(n_distinct(species) > 1) %>% 
  nrow() == 0

# are all problematic lengths NAs
df %>% 
  filter(
    comment %in% c("prtmis","nolen","losfis") |
      species == "UNIFIS",
    !is.na(length)
  ) %>% nrow() == 0

# Do all nofish entries have length of zero?
df %>% filter(species == "NOFISH", length != 0) %>% nrow() == 0

# Are all lengths 3mm and under removed?
df %>% filter(length < 3, length != 0) %>% nrow() == 0

# Are all fish greater than 80cm removed
df %>% filter(length > 80) %>% nrow() ==0

# Are fish all smaller than recorded maximum length?
df %>% left_join(max_len_df) %>% filter(length > max_len+5) %>% nrow() == 0

# Check that waterperiod and year match the preexisting cum order
df %>% 
  distinct(cum,year,period,wateryear,waterperiod) %>% 
  arrange(cum) %>% 
  group_by(wateryear) %>%
  mutate(l = lead(waterperiod,order_by = cum)-waterperiod) %>% 
  filter(1 != 1) %>% 
  nrow()==0

len_check_fun <- function(df){
  
  # Are  problematic throws and plots removed?
  stopifnot("Problematic throw and/or plots not removed"=  
              df %>% 
                filter(
                  region == "SRS" & plot %in% c("D","E") |
                    region == "WCA" & throw > 5
                ) %>% 
                nrow() == 0)

  # are there comments left that imply sites were not sampled?
  stopifnot("Comments remain in data that imply sites were not sampled" =
              df %>% filter(comment %in%  unsam) %>% nrow() == 0)
   
  # are problematic and duplicate species codes removed?
  stopifnot("Problematic and/or duplicate species codes not removed" =
              df %>% 
              filter(species %in% c("",
                                    "SITDRY",
                                    "ROTOCUP",
                                    "PRTMIS",
                                    "LOSFIS",
                                    "ERYUMB",
                                    "DELETE")) %>% 
              nrow() == 0)

  # Are NOFISH entries duplicates removed?
  stopifnot("Duplicated entries of NOFISH exist" =
              df %>% 
              filter(species == "NOFISH") %>% 
              group_by(year,cum,period,region,site,plot,throw) %>% 
              filter(n() > 1) %>% 
              nrow ==0)

  # Are nofish entires absent from throws with length data
  stopifnot("NOFISH entries exist in throws with fish lengths" =
              df %>% 
              inner_join(df %>% 
                           filter(species == "NOFISH") %>% 
                           distinct(cum,region,site,plot,throw)) %>% 
              group_by(year,cum,period,region,site,plot,throw) %>% 
              filter(n_distinct(species) > 1) %>% 
              nrow() == 0)
  
  # are all problematic lengths NAs
  stopifnot("Problematic lengths still exsit in data" = 
              df %>% 
              filter(
                comment %in% c("prtmis","nolen","losfis") |
                  species == "UNIFIS",
                !is.na(length)
              ) %>% nrow() == 0)
  
  # Do all nofish entries have length of zero?
  stopifnot("NOFISH entries have lengths other than zero" =
              df %>% filter(species == "NOFISH", length != 0) %>% nrow() == 0)
  
  
  # Are all lengths 3mm and under removed?
  stopifnot("Lengths less than 3 mm still in data" =
              df %>% filter(length < 3, length != 0) %>% nrow() == 0)
  
  # Are all fish greater than 80cm removed
  stopifnot("Fish greater than 80mm in data" =
              df %>% filter(length > 80) %>% nrow() ==0)
  
  # Are fish all smaller than recorded maximum length?
  stopifnot("Fish are greater then expected length" = 
              df %>% left_join(max_len_df) %>% filter(length > max_len) %>% nrow() == 0)
  
  
  # Check that waterperiod and year match the preexisting cum order
  stopifnot("Wateryear and waterperiod order incorrect" =
              df %>% 
              distinct(cum,year,period,wateryear,waterperiod) %>% 
              arrange(cum) %>% 
              group_by(wateryear) %>%
              mutate(l = lead(waterperiod,order_by = cum)-waterperiod) %>% 
              filter(1 != 1) %>% 
              nrow()==0)
  
  # If all good, return data
  df
}

len_df_clean <-len_check_fun(len_df_for)

### Export  ####################################################################
saveRDS(len_df_clean,file.path(clean_dir,paste0("fslen_cleaned_",Sys.Date(),".rds")))

# continue to analysis specific data cleaning and imputation scripts.
# End of generic data cleaning work flow
                        
