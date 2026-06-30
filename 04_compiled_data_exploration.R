################################################################################
#
#  Compiled data exploration
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/17/2026

# DESCRIPTION: Checks the compiled length and physical data sets for duplication
# size structure, and match issues. Determines how much data is mismatched 
# between the two data sets and identifies additional throw data in physical
# data set. Each Each data cleaning category ends with a summary section with 
#actionable items signified with the following symbols:

# !!! = address in data cleaning/compilation
# ??? = ask for clarification
# DONE = issue resolved or addressed in data cleaning scripts
# EXPD = exported data
# RSLVD = question answered

### House keeping  #############################################################
rm(list=ls())

# Packages
library(dplyr)
library(tidyr)

# Directories
raw_dir <- "raw_data"
q_dir <- "question_data"
trait_dir <- "trait_data"

# Data (make sure is most up to date!)
len_df <- readRDS(file.path(raw_dir,"fslen_raw_compiled_2026-02-24.rds"))
phy_df <- readRDS(file.path(raw_dir,"phys_raw_compiled_2026-02-24.rds"))
max_len_df <- readRDS(file.path(trait_dir,"fslen_max_length_2026-02-24.rds"))


### Compiled length data  ######################################################

### Check for duplicated throws among grouping types
len_df %>% 
  distinct(cum,year,period,site,plot,throw) %>%
  group_by(cum,year,period,site,plot,throw) %>% 
  filter(n()>1)
len_df %>% 
  distinct(cum,year,period,site,plot,throw) %>%
  group_by(year,period,site,plot,throw) %>% 
  filter(n()>1)
len_df %>% 
  distinct(cum,year,period,site,plot,throw) %>%
  group_by(cum,site,plot,throw) %>% 
  filter(n()>1)
# all good here

### Check for duplicate cum values
len_df %>% 
  arrange(cum) %>% 
  distinct(cum,year,period) %>% 
  group_by(cum) %>% 
  filter(n()>1)
# all good here as well!

# are cum columns are calculated the same
len_df %>% 
  mutate(cum_check = period + 5*(year - min(year))) %>% 
  filter(cum != cum_check) %>% 
  nrow()
# all cum values are consistent. Can safely use cum instead of period and year

### To do summary  ###
# DONE no action required


### Compiled physical data  ####################################################

### Is any of the data duplicated ? ###
#among throws?
phy_df %>% group_by(cum,year,period,site,plot,throw) %>% filter(n()>1)
phy_df %>% group_by(cum,site,plot,throw) %>% filter(n()>1)
phy_df %>% group_by(year,period,site,plot,throw) %>% filter(n()>1)
# duplicates exist in WCA 2020 period 5 but only when cum is excluded from
# grouping. Suggests a duplication issue and a mismatch between cum and year/period
# The cum issue is due to the duplicated differing in cum 120 and 125
# explore this below

# Extract cum 120 data
phy_df %>% filter(year ==2020, cum ==120)
phy_df %>% filter(region=="WCA", cum == 120)

# check which cum values are duplicated
phy_df %>% 
  arrange(cum) %>% 
  distinct(cum,year,period) %>% 
  group_by(cum) %>% 
  filter(n()>1)
# cum 120 is duplicate between 2019-2020. should be only 2019. 2020 should
# be 125. check this out below

phy_df %>% 
  mutate(cum_check = period + 5*(year - min(year))) %>% 
  filter(cum != cum_check) %>% 
  nrow()
# duplication issue causes cum values to be inconsistent

phy_df %>% 
  arrange(cum) %>% 
  group_by(cum,year,period) %>% 
  summarise(n_site = n_distinct(site),
            n_plot = n_distinct(site,plot),
            n_throw = n_distinct(site,plot,throw)) %>% 
  filter(cum %in% c(120,125))

phy_df %>% 
  filter(region =="WCA",
         year == 2020,
         cum == 120)

# check length data to see if issue exist here
len_df %>% 
  filter(region =="WCA",
         year == 2020,
         period == 5) %>% 
  distinct(month,cum)
# In length data period 5 2020 is cum 125
# Duplicates occur in WCA period 5 2020. There were sites that weren't visited in
# th end of December due to deep water or novisit comments. These were then visited
# again in a couple days later in January. Data was collected here. Both of these
# exist in the data. This only occurs in phy data set. Was obscured because the
# January visit was mistakenly given the cum of 120.
#
# DONE - Remove the December visits from the data set
# DONE - change cum 120 year 2020 to cum 125

## Export duplicates for nate and oc
# dup_issue <- phy_df %>% 
#   group_by(year,period,site,plot,throw) %>% 
#   filter(n()>1) %>% 
#   arrange(site,plot,throw)
# write.csv(dup_issue,file.path(q_dir,"phy_wca_dup_issue.csv"))

### duplicate fix code ###

# Extract duplicated site to remove
dup_2020 <- phy_df %>% 
  group_by(year,period,site,plot,throw) %>% 
  filter(n()>1,
         month ==12) 
# remove duplicated sample
phy_df2 <- phy_df %>% 
  anti_join(dup_2020) %>% 
  
  # fix incorrect cum value
  mutate(cum = case_when(
    year == 2020 & cum == 120 ~ 125,
    T~ cum
  ))

# recheck for duplicates
phy_df2 %>% group_by(cum,year,period,site,plot,throw) %>% filter(n()>1)
phy_df2 %>% group_by(cum,site,plot,throw) %>% filter(n()>1)
phy_df2 %>% group_by(year,period,site,plot,throw) %>% filter(n()>1)

# recheck for duplicate cums
phy_df2 %>% 
  arrange(cum) %>% 
  distinct(cum,year,period) %>% 
  group_by(cum) %>% 
  filter(n()>1)
# problem resolved

# are cum columns are calculated the same
phy_df2 %>% 
  mutate(cum_check = period + 5*(year - min(year))) %>% 
  filter(cum != cum_check) %>% 
  nrow()
# This is all good now! can safely use cum instead of period and year


### Are all possible throws included in these data? ###
region_list <- unique(phy_df2$region)
t_list <- c(7,5,7)
Map(function(r,t) phy_df2 %>% 
      filter(region == r) %>% 
      group_by(cum,site,plot) %>% 
      summarise(n_throw = n()) %>% 
      filter(n_throw != t),
    region_list,t_list)
# all throws accounted for in each region


### Are all possible plots in included in these data
p_list <- c(3,3,3)
Map(
  function(r,p) phy_df2 %>% 
    filter(region == r) %>% 
    group_by(cum,site) %>% 
    summarise(n_plot = n_distinct(plot)) %>% 
    filter(n_plot != p) %>% 
    ungroup() %>% 
    distinct(site),
  region_list,p_list
  )
# only sites that do not have expected number of plots are those with known exceptions
# additional plots d and e at jeff kline sites (will remove these) and only 2 plots
# at the sh TSL sites
# DONE - remove srs plots D and E


### To do summary ###
# EXPD - export duplicate data for nate and co
# RSVLD inform nate et al., that there is a duplication issue masked by incorrect
#     cum value.
# EXPD - exported duplicates for nate et al.
# DONE - Remove the problem December visits from phy data set
# DONE - change cum 120 year 2020 to cum 125
# DONE - remove srs plots D and E


### Matched data  ##############################################################

### Are all throws from length data in physical?
len_df %>% anti_join(phy_df2,join_by(cum,site,plot,throw))
# Only the extra throw from WCA 2015, period 2 site 4 plot C is not in phys data

# RSLVD -  ask if throw id is mistaken? There is only one fish recorded here
#     DONE - JOEL: remove this throw
# EXPD -   export extra throw data

# write.csv(len_df %>% filter(region == "WCA",throw ==6),
#           file.path(q_dir,"wca_throw6.csv"),
#           row.names = F)


### Are all throws from physical data in lengthh data
phy_df2 %>% 
  anti_join(len_df,join_by(cum,site,plot,throw)) %>% 
  nrow()
phy_df2 %>% 
  anti_join(len_df,join_by(cum,year,period,site,plot,throw)) %>% 
  nrow()
phy_df2 %>% 
  anti_join(len_df,join_by(year,period,site,plot,throw)) %>% 
  nrow()
# There are over 1982 extra throws in phys data. COnsistent among year and cum groupings
# Find out what regions these are from
phy_df2 %>% 
  anti_join(len_df,join_by(cum,site,plot,throw)) %>% 
  group_by(region) %>% 
  summarise(n=n())
# This occurs in all regions.


### Extra phy data exploration ###
# DO these data have values in comment columns?
phy_df2 %>% 
  anti_join(len_df,join_by(cum,site,plot,throw)) %>% 
  select(region,empcup,vegthk,sitdee,sitdry,notvis,
         nodata,helcop,trldry,airtrl,nosamp,rotcup) %>% 
  group_by(region) %>% 
  summarise(across(where(is.numeric),~sum(.x,na.rm = T)))
# many have comments, but comment colimn empcup and rocup do not appear in any of 
# the extra data

# How much of the extra data has no comments
no_comment <- phy_df2 %>% 
  anti_join(len_df,join_by(cum,site,plot,throw)) %>% 
  select(region,year,cum,site,plot,throw,empcup,vegthk,sitdee,sitdry,notvis,
         nodata,helcop,trldry,airtrl,nosamp,rotcup) %>% 
  mutate(
    n_comments = rowSums(
      across(c(empcup,vegthk,sitdee,sitdry,notvis,nodata,
               helcop,trldry,airtrl,nosamp,rotcup)
             ),
             na.rm = T)
    ) %>% 
  select(year,cum,region,site,plot,throw,n_comments) %>% 
  filter(n_comments!=1) %>% 
  select(-n_comments)
# all data have one or less comment column selected
# 372 throws are in the physical data but length data and lack a comment
# EXPD - export this data and ask if length data exists for these throws
# write.csv(no_comment,file.path(q_dir,"extra_data_no_comment.csv"),row.names = F)


### To do summary  ###
# RSLVD - ask if throw id 6 is mistaken? There is only one fish recorded here
#      DONE - JOEL: remove this throw
# EXPD  - extra throw data exported to q_dir
# ??? ask about comment-less data in phy data that are missing from length data
#   !!! No response yet, but continue as if data are missing (done)
# EXPD  - no comment extra data is exported

### Match comment columns to length data #######################################
site_comment_list <- c("empcup","vegthk","sitdee","sitdry","notvis",
                       "nodata","helcop","trldry","nosamp","rotcup")

# Change phy comments into long format and merge with length data
combined_comments <- phy_df2 %>% 
  select(
    region,cum,site,plot,throw,empcup,vegthk,sitdee,sitdry,notvis,
    nodata,helcop,trldry,airtrl,nosamp,rotcup
    ) %>% 
  mutate(   # Create column indicating no comments
    n_comments = rowSums(across(c(empcup,vegthk,sitdee,sitdry,notvis,nodata,
                                  helcop,trldry,airtrl,nosamp,rotcup)),
                          na.rm = T),
    na_comment = case_when(
      n_comments == 0 ~ 1,
      T ~ NA
      )
    ) %>% 
  select(-n_comments) %>% 
  pivot_longer(  # Convert comment data to long format and only retain non-na comment values
    -c(region,site,cum,plot,throw),
    names_to = "site_comment",
    values_to = "n"
    ) %>% 
  filter(n==1) %>% 
  mutate(
    site_comment = case_when(
      site_comment == "na_comment" ~ NA,
      T~ site_comment
      )
    ) %>% 
  select(-n)%>% 
  inner_join(len_df) %>%  # attach to length data and format comments to match
  mutate(
    comment = case_when(
      comment %in% c("",".") ~ NA,
      T~comment
    ),
    comment_match = case_when(
      comment == site_comment ~T,
      is.na(comment) & is.na(site_comment) ~ T,
      T ~ F
    )
    )
  

# What combinations of comments exist, and are there length data associated with 
# these?
lapply(site_comment_list, function(x) combined_comments %>% 
         
         # Retain only unmatch comments of interset with fish lengths
         filter(!is.na(site_comment),
                site_comment == x) %>% 
         group_by(region,site_comment,comment) %>% 
         summarise(n_len = length(length[!is.na(length)])))

# Extract data for sites with probalamtic comments with length data
mis_match <- lapply(site_comment_list, function(x) combined_comments %>% 
         
         # Retain only unmatch comments of interset with fish lengths
         filter(!is.na(site_comment),
                site_comment == x,
                comment_match==F,
                !is.na(length)))

### EMPCUP ISSUE ###
# empcup site comment can refer to data in the case of WCA 05 B throw 4, cum 135.
# ??? ASK ABOUT THIS
# empcup <- mis_match[[1]] %>% 
#   left_join(phy_df2)

# EXPD  - export empcup  issue
# write.csv(empcup,file.path(q_dir,"empcup_issue.csv"),row.names=F)

### SITDEE ISSUE  ###
# EXPD  - export site deep issue
# write.csv(mis_match[[3]],file.path(q_dir,"sitdee_issue.csv"),row.names=F)
# RSLVD - DO we keep these fish data?
#       JOEL: if depth is less than a meter keep

# look at physical data for site deep
mis_match[[3]] %>% 
  left_join(phy_df) %>% 
  distinct(field_depth,eden_depth_corrected)
# No field dpeth, but eden depth is less than a meter, these data are fine to keep
# DONE - In phy cleaning, change the stideep comment to NA

# Samples with field depths greater than 1 m
# field_d_high <- phy_df2 %>% filter(field_depth > 100,is.na(sitdee)) 

# JOEL SAID THROWS WITH A DEPTH GREATER THAN 100 should not be trusted, but there
# are some throws with up to 3m field depths with fish data
# RSVD ask if these should be trusted? and what cut off should I use
#   DONE - trust the data
# *** export high field depth samples
# write.csv(field_d_high,file.path(q_dir,"high_field_depth.csv"),row.names = F)

### OTHER ISSUES ###
# DONE - VEGTHK can be associated with length data, change to NA if length data are present
# DONE - sitdry can refer to fish data. This is a mistake with site column and comment can be removed
# DONE - No data can refer to fish data and no fish data, change to na when fish lengths are present
# DONE - remove notvis, helcop,trldry, sitdee, vegthk and nosamp site column data 
#       after above issues are fixed, these were not sampled. Missing data


## Are there length comments, whn there are no site comments?
combined_comments %>% 
  
  # Retain only unmatched comments of interest with fish lengths
  filter(is.na(site_comment)) %>% 
  group_by(site_comment,comment) %>% 
  summarise(n_len = length(length[!is.na(length)])) %>% 
  print(n=100)
 

### TO DO SUMMARY  ###
# EXPD  - empcup issue data exported
# RSLVD ask about empcup issue
#   DONE treat as unifis
# EXPD - site deep issue data exported
# RSLVD - ask about site deep issue
#     DONE - remove sitdee comment from that throw if field depth is less than a meter
# EXPD - field depth great than 1m samples
# RSVD Joel said that samples taken in water depths over 1m can be problamtic, there
#     were some samples taken in deep water. Is there a threshold where I should
#     remove these samples?
#  DONE JOEL SAID THESE ARE FINE
# DONE - In phy data cleaning remove nodata, vegthk, sitdee, sitdry,comments from
#        throws with fish lengths.
# DONE - remove throws with notvis, helcop,trldry, nosamp, nodata, vegthk, sitdee, 
#        and sitdry site comments, these were not sampled
#        and do not have any fish lengths or problematic mismatches



### Blank species names  #######################################################

# Examine comments associated with blank species names, after removing throws that
# were not sampled

# unsampled comments
unsam <- c("vegthk",  # Vegetation was too thick to sample
           "sitdee",  # Site was deep and no sample was taken
           "notvis",  # Site was not visited
           "helcop",  # These throws were skipped due to logistics related to helicopter
           "trldry",  # Was not able to access site due to dry conditions
           "nodata",  # No sample taken and no reason given
           "nosamp")  # No sample taken and no reason given

# combination of phy and len comments
combined_comments %>% 
  filter(
    species == "",
    !comment %in% unsam,
    !site_comment %in% unsam
    ) %>% 
  mutate(
    na_length = case_when(
      is.na(length) ~ T,
      T~F
    )
  ) %>% 
  distinct(site_comment,comment,na_length)

# DONE - change blank species name to comment, then cahnge partmis,losfis to unifis,
# DONE - change rotcup to "."
# DONE - change empcup and sitdry to "NOFISH


# extract unique comments from blank species
blank_comments <- combined_comments %>% 
  filter(
    species == "",
    !comment %in% unsam,
    !site_comment %in% unsam
  ) %>% 
  mutate(
    na_length = case_when(
      is.na(length) ~ T,
      T~F
    )
  ) %>% 
  distinct(comment) %>% 
  pull()
names(blank_comments) <- blank_comments

# Examine if blank species of certain comments, are the only observation in their
# respective throw
lapply(blank_comments,function(x) {
  id <-combined_comments %>% 
    mutate(
      species = case_when(
        species == "" ~ comment,
        T ~ species
      )
    ) %>% 
    filter(
      species == x,
      !comment %in% unsam,
      !site_comment %in% unsam
    ) %>% 
    distinct(year,cum,period,region,site,plot,throw)
  
  combined_comments %>% 
    inner_join(id) %>% 
    group_by(year,cum,period,region,site,plot,throw) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)
}
  )

## blank species with EMPCUP,ROTCUP, SITDRY comments are the only entry
# in that throw. 
# blank species with losfis, and prtmis can have multiple fish. These are likely
# supposed to be unifis


# WHat about na comments?
na_id <-combined_comments %>% 
  mutate(
    species = case_when(
      species == "" ~ comment,
      T ~ species
    )
  ) %>% 
  filter(
    is.na(species),
    !comment %in% unsam,
    !site_comment %in% unsam
  ) %>% 
  distinct(year,cum,period,region,site,plot,throw)

combined_comments %>% 
  inner_join(id) %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)
# NA COMMENTS blank names appear amongs many throw with other entries

# Examine blank species, na comments full (phy+len data)
# blank_na <- combined_comments %>%
#   inner_join(na_id) %>%
#   group_by(year,cum,period,region,site,plot,throw) %>%
#   mutate(n = n()) %>%
#   filter(species == "") %>%
#   left_join(phy_df2)
# No obvious issues, water depths are sample-able, no site comments. Send to Joel
# EXPD Extract blank species with NA comments
# write.csv(blank_na,file.path(q_dir,"blank_species_no_comments.csv"))

# Extract entire throws with blank species no comments
# blank_na_full <- combined_comments %>%
#   inner_join(id) %>%
#   group_by(year,cum,period,region,site,plot,throw) %>%
#   left_join(phy_df2)
# write.csv(blank_na_full,file.path(q_dir,"blank_species_no_comments_full.csv"))

### To do summary  ###
# EXPD - exported blank species no comments
# ??? ask about blank species with no comments
#  !!! remove for now (done), but await Joel response
# DONE - change blank species with empcup and sitdry to NOFISH
# DONE - change blank species with losfis or prtmis comment to UNIFIS
# DONE - change blank species with rotcup to "." these will be removed


### Duplicate sitedry and nofish issue #########################################

# ARE NOFISH and SITDRY entries the only observations for respective throw?
lapply(c("NOFISH", "SITDRY"),function(x) {
  id <-combined_comments %>% 
    filter(
      species == x,
      !comment %in% unsam,
      !site_comment %in% unsam
    ) %>% 
    distinct(year,cum,period,region,site,plot,throw)
  
  combined_comments %>% 
    inner_join(id) %>% 
    group_by(year,cum,period,region,site,plot,throw) %>% 
    filter(n() > 1)
    #summarise(n = n()) %>% 
    #filter(n > 1)
}
)
# NOFISH can appear in throws with fish data, what does this mean.
# other cases, there are multiple case of nofish entry (usually 3). IS there
# meaning to this?

# SITDRY species can appear in throws with duplicates of sitdry, but do not
# occur wiht other fish. THESE should be removed

combined_comments %>% 
  filter(species == "NOFISH") %>% 
  distinct(year,cum,period,region,site,plot,throw) %>% 
  nrow()

# REMOVE DUPLICATED NOFISH AND SITDRY ENTIRES
no_fish_throws <- combined_comments %>% 
  filter(species %in% c("NOFISH","SITDRY")) %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(n() > 1) %>% 
  slice(1)

# replace in data set
a2 <- combined_comments %>% 
  anti_join(
    no_fish_throws,
    join_by(year,cum,period,region,site,plot,throw,species)
    ) %>% 
  bind_rows(no_fish_throws)

# DOes this have the right number of rows?
a2 %>% 
  filter(species %in% c("NOFISH","SITDRY")) %>% 
  nrow()

a %>% 
  distinct(year,cum,period,region,site,plot,throw) %>% 
  nrow()

# do duplcates still exist for these
a2 %>% 
  filter(species %in% c("NOFISH","SITDRY")) %>%  
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(n() > 1)

# remove nofish from throws with fish values
b <- a2 %>% 
  inner_join(combined_comments %>% 
               filter(species %in% c("NOFISH","SITDRY")) %>% 
               distinct(year,cum,period,region,site,plot,throw)) %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(n_distinct(species) > 1,
         !species %in% c("NOFISH"))

c <-a2 %>% 
  anti_join(
    b,
    join_by(year,cum,period,region,site,plot,throw) 
    )%>% 
  bind_rows(b)
      
# Is there the correct number of throws

# Are throws with NOFISH and sitedry free of other entires?
c %>% 
  filter(species %in% c("NOFISH","SITDRY")) %>% 
  group_by(year,cum,period,region,site,plot,throw) %>% 
  filter(n() > 1)
  

## DO THIS AFTER PROBLEM SPECIES ARE REMOVED

### To do summary  ###
# RSLVD - Ask about duplicated site dry and nofish species name
#       DONE JOEL: remove these duplicate, most likely a mistake
# RSLVD - Ask about NOFISH species names in throw with other fish data
#       DONE JOEL: Retain length data, remove NOFISH rows.


### Maximum fish length exploration  ###########################################

# Confusing species codes
morph_df %>% 
  filter(is.na(max_len))
# ATHSPP and CENSPP are not in metadata and it is uncertain which genera these
# refer to. CENSPP is likely Centropomus but need to be sure.
# How common are these species codes?
len_df %>% 
  filter(species %in% c("ATHSPP","CENSPP")) %>% 
  group_by(species) %>% 
  summarise(n_ind = n(),
            n_throw = n_distinct(cum,region,site,plot,throw),
            n_plot = n_distinct(cum,region,site,plot),
            n_site = n_distinct(cum,region,site),
            n_region = n_distinct(region),
            n_cum = n_distinct(region))
# ATH was only at one throw with 30 ind
# CENPP were all in two periods of 2009 at site MD and MDsh
# write.csv(len_df %>% filter(species %in% c("ATHSPP","CENSPP")),
#           file.path(q_dir,"athspp_censpp.csv"))


# What species have fish larger than 80mm?
len_df %>% 
  filter(length > 80) %>% 
  distinct(species) %>% 
  arrange(species) %>% 
  pull()
# The only focal species greater than 80mm are GAMHOL and HETFOR and these are
# longer than expected length so these will be dealt with in the max length
# section

# Are all species included in life history data?
len_df %>% 
  filter(!species %in% max_len_df$species,
         !species %in% c("UNIFIS","NOFISH")) %>% 
  distinct(species)
# only species name not included are non-standardized nosamp/nofish names, and
# the dragon fly entry

# How many fish are larger than recorded maximum length
len_df %>% 
  left_join(max_len_df) %>% 
  filter(length > max_len) %>% 
  mutate(len_dif = length-max_len) %>% 
  arrange(len_dif)
# a handful of fish are larger, but few are much larger than their lengths
# Some are large fish that would be removed before this step


# Extract sall bodied fish with larger than expected lengths to show Joel
# big_fish <- len_df %>% 
#   left_join(max_len_df) %>% 
#   filter(length > max_len,
#          max_len <=80)%>% 
#   mutate(len_dif = length-max_len) %>% 
#   arrange(len_dif)
# write.csv(big_fish,file.path(q_dir,"big_fish.csv"))


### Max length to do summary  ###
# EXPD - throws with CENSPP and ATHSPP
# RSLVD What genera does CENSPP and ATHSPP refer to?
#  DONE change CENSPP = Centropomus species and ATHSPP = Atherinopsidae species
#      on brigde table
# DONE - First remove fish larger than 80mm, but keep those that have max lengths 
#        <= 80mm as these fish were actually not likely larger than 80mm.
# EXPD - file with small bodied fish larger than expected body length
# ??? Some fish are only slightly larger than max size from fish morph. Ask Joel
#     if these fish are of a size expected in the evergaldes
# !!! Waiting on tech response. But there are so few of these it doesnt hurt to
#     just make these NAs (done)


### Water year and period  #####################################################

# Are there periods within the same year with different wateryears?
phy_df2 %>% 
  distinct(cum,year,period,wateryear) %>% 
  arrange(cum,wateryear) %>% 
  group_by(cum,year,period) %>% 
  filter(n()>1) %>% 
  print(n=30)
# many instances of this occurs, mainly in period 2 (end of wateryear) which 
# implies month may be used instead of period. Explore this more

# Does each month have the same water year within years?
phy_df2 %>% 
  distinct(year,month,wateryear) %>% 
  arrange(month,wateryear) %>% 
  group_by(month,year) %>% 
  filter(n()>1) %>% 
  print(n=30)
# Yes they do

# WHat months are in the current vs previous wateryear
phy_df2 %>% 
  distinct(year,month,wateryear) %>% 
  mutate(lag = wateryear-year) %>% 
  arrange(month,year) %>% 
  distinct(month,lag) 
# months 1-4 are in previous wateryear (lag =-1) and months 5-12 are in the
# current (no lag) wateryear. THe exveption is month one which can also have
# no lag. Explore this:
phy_df %>% 
  distinct(cum,year,period,wateryear,month) %>% 
  filter(month == 1) %>% 
  mutate(lag = wateryear-year)
# Wateryear in physical data set seems to be mainly based on month not period.
# but this logic is not applied consistently. Period 5 of 1997 was measured in
# month 1 but water year does not have the -1 lag but period 5 of 2020 does  have
# the lag.
# For our case, I think we should use period to assign wateryear to keep this 
# consistent 
# Should bring up to Nate and co that period 5 of some WCA sites in 2020 were marked
# wateryear 1999. They were recorded in January of the following year and have the
# 2020 year id but the fact that the year was adjusted inst carried into water
# year calculation.

# wateryear_issue <- phy_df2 %>% 
#   filter(year == 2020) %>% 
#   distinct(region,cum,year,period,site,month,wateryear) %>% 
#   arrange(cum)
# write.csv(wateryear_issue,file.path(q_dir,"wateryear_issue_2020.csv"))

# Wateryears begin on period 3 of the previous year and ends on period 2 of the current
len_df %>% 
  mutate(
    waterperiod = case_when(
      period >= 3 ~ period -2,
      period < 3 ~ period +3
    ),
    wateryear = case_when(
      period < 3 ~year -1,
      period >= 3 ~ year,
    )
  ) %>% 
  distinct(cum,year,period,wateryear,waterperiod) %>% 
  arrange(cum)
# This code keeps the cum order consistent with water period and water year
# how to test this with code:
len_df %>% 
  mutate(
    waterperiod = case_when(
      period >= 3 ~ period -2,
      period < 3 ~ period +3
    ),
    wateryear = case_when(
      period < 3 ~year -1,
      period >= 3 ~ year,
    )
  ) %>% 
  distinct(cum,year,period,wateryear,waterperiod) %>% 
  arrange(cum) %>% 
  group_by(wateryear) %>% 
  mutate(l = lead(waterperiod,order_by = cum)-waterperiod) %>% 
  filter(l != 1)


### Explore water periods ###
phy_df2 %>% 
  distinct(month,waterperiod) %>% 
  arrange(month)
# waterperiod in physdata looks like it is more like watermonth,
# each month is assigned a value between 1-12 with the formaula:
# month < 5 = month+8
# month >= 5 = month-4
# FOr are use, we need to make our own water period column



### Water year to do summary  ###
# ??? Address with the group the use of period rather than wateryear to classify
# water years
# EXPD - 2020 period 5 wateryear issue
# RSLVD - Let Nate and co know about the above issue
# DONE phys_df waterperiod and wateryear columns are not what we need, need to
#     rename existing columns and create new columns for our measures.


### End of exploration #########################################################

# continue to phys_data_cleaning.R and fslen_data_cleaning.R

