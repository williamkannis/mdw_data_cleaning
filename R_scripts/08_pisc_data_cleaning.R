################################################################################
#
#  Piscivore data cleaning
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/25/2026

# DESCRIPTION: 


### House keeping  #############################################################
rm(list=ls())

# Packages
library(dplyr)
library(janitor)

# Directories
raw_dir <- "raw_data"
clean_dir <- "cleaned_data"


# Data (make sure is most up to date!)
pis_df <- read.csv(file.path(raw_dir,"pisc_index.csv"))
len_df <- readRDS(file.path(raw_dir,"fslen_raw_compiled_2026-02-24.rds"))
phy_df <- readRDS(file.path(clean_dir,"phys_cleaned_2026-02-25.rds"))  


### Explore data for any issues  ###############################################

# Quick look for NAs and date range. Will not be ablt to ask for more data for
# missing sites and years because we have everything currently
summary(pis_df)

# What sites appear?
pis_df %>% 
  clean_names() %>% 
  distinct(region,site)

# what sites are missing in length data?
pis_df %>% 
  clean_names() %>% 
  anti_join(len_df,by=join_by(site))

# Need to add trailing zero to WCA sites
pis_df %>% 
  clean_names() %>% 
  mutate(
    site = case_when(
      region == "WCA" ~paste0("0",site),
      T~ site
    )
  ) %>% 
  anti_join(len_df,by=join_by(site))
# pandhadle data is only extra sites

# What sites are missing frm pisc data
len_df %>% 
  anti_join(pis_df %>% 
              clean_names() %>% 
              mutate(
                site = case_when(
                  region == "WCA" ~paste0("0",site),
                  T~ site
                )
              ),
            join_by(site)) %>% 
  distinct(site)
# expceted sites missing



# Are any site/years duplicated
pis_df %>% 
  clean_names() %>% 
  group_by(region,site,year) %>% 
  filter(n()>1)
# no duplication

# How many years are available per site
pis_df %>% 
  clean_names() %>% 
  group_by(region,site) %>% 
  summarise(n_years = n_distinct(year))


###  Clean up data  ############################################################

# CLean name and format site names to match rest of data
pis_df_clean <- pis_df %>% 
  clean_names() %>% 
  mutate(
    site = case_when(
      region == "WCA" ~paste0("0",site),
      T~ site
    )
  )


### Export data  ###############################################################

saveRDS(pis_df_clean,file.path(clean_dir,paste0("pisc_cleaned_",Sys.Date(),".rds")))
  
