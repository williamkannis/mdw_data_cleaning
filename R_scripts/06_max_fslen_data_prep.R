################################################################################
#
#  Maximum length data preparation
#
################################################################################

# AUTHOR: William K. Annis

# CREATED: 02/24/2026

# DESCRIPTION: Creates a data frame with maximum lengths of each species in the 
# data set to check if any erroneously large measurements are present in the data.
# Fish lengths are aquired from the Fish Morph data set and formatted to match our data structure


### House keeping  #############################################################
rm(list=ls())

# Packages
library(janitor)
library(dplyr)
library(stringr)
library(taxize)

# Directories
raw_dir <- "raw_data"
trait_dir <-"trait_data"

# Data (make sure is most up to date!)
len_df <- readRDS(file.path(raw_dir,"fslen_raw_compiled_2026-02-24.rds"))
sp_code_df <- read.csv(file.path(raw_dir,"species_code.csv"))
morph_df <- read.csv(file.path(trait_dir,"fishmorph_database.csv"))

### Create list of species  ####################################################

# format species code
sp_code_for <- sp_code_df %>% 
  clean_names() %>% 
  rename(species = species_abbreviation) %>% 
  select(-notes)

# What species are present in data set?
dataset_sp <-len_df %>% 
  distinct(species) %>% 
  arrange(species) %>% 
  pull()

# What species in data set are not in sp codes?
dataset_sp[!dataset_sp %in% sp_code_for$species]


# Create rows in species code for non included species and filter out species not in dataset
extra_sp_df <- data.frame(species = c("AMESPP", "ATHSPP", "CENSPP", "FUNSPP"))
sp_code_all <- sp_code_for %>% 
  full_join(extra_sp_df) %>% 
  filter(species != "UNIFIS",
         species %in% dataset_sp)  %>% 
  mutate(
    scientific_name = case_when(
      species == "AMESPP"~"Ameiurus spp",
      species == "ATHSPP" ~ "Ath spp",
      species == "CENSPP" ~ "Centropomus spp",
      species == "FUNSPP" ~ "Fundulus spp",
      T~scientific_name
      )
    )


### Format trait data to match species code data  ##############################

# Format trait data
morph_for <- morph_df %>% 
  clean_names() %>% 
  rename(
    scientific_name = genus_species,
    max_len = m_bl
    ) %>% 
  mutate(genus = word(scientific_name,1)) %>% 
  select(family,genus,scientific_name,max_len)
  
# which of out species are not in trait data
sp_code_all$scientific_name[!sp_code_all$scientific_name %in% morph_for$scientific_name]


### Harmonize scientific names  ################################################

# check if all species names are up to date and correct. 
# Has limit of 50 species at onece
sp_names_harm1 <- taxize::gna_verifier(sp_code_all$scientific_name[1:50], 
                                      data_source_ids = 3, 
                                      canonical =  TRUE)
sp_names_harm2 <- taxize::gna_verifier(sp_code_all$scientific_name[51:nrow(sp_code_all)], 
                                       data_source_ids = 3, 
                                       canonical =  TRUE)

# Create data frame with current and updated species names
sp_harm <-bind_rows(sp_names_harm1,sp_names_harm2) %>% 
  rename(scientific_name = submittedName,
         updated_name = currentCanonicalFull) %>% 
  select(scientific_name,updated_name)


# Merge in new names to 
sp_code_harm <- sp_code_all %>% 
  left_join(sp_harm) %>%
  
# Format genus level names and other mismatches
  mutate(
    updated_name = case_when(
      substr(species,4,6) == "SPP" ~word(scientific_name,1),
      species == "HEMLET" ~ "Hemichromis letourneuxi",
      T ~ updated_name
    )
  )

# check if updated names match traits
sp_code_harm$updated_name[!sp_code_harm$updated_name %in% morph_for$scientific_name]
# almost all species are included now


### Merge in length data to species codes  #####################################

# genus level id
morph_genus <- morph_for %>% 
  filter(genus %in% c("Esox","Lepomis","Ameiurus","Centropomus","Fundulus")) %>% 
  group_by(genus) %>% 
  summarise(max_len = max(max_len, na.rn=T)) %>% 
  rename(updated_name = genus)

# family level id
morph_fam <- morph_for %>% 
  filter(family == "Cichlidae",
         !is.na(max_len)) %>% 
  group_by(family) %>% 
  summarise(max_len = max(max_len, na.rn=T)) %>% 
  rename(updated_name = family)

#Combine all levels into one
morph_all <- morph_for %>% 
  rename(updated_name = scientific_name) %>% 
  select(updated_name,max_len) %>% 
  bind_rows(morph_genus,morph_fam)

# Add in lengths
sp_lengths_df <- sp_code_harm %>% 
  left_join(morph_all, join_by(updated_name)) %>% 
  mutate(
    max_len = case_when(
      species == "LABVAN" ~ max_len[species =="LABSIC"], # missing from trait data, use closely related spcies
      T ~ max_len
    ),
    max_len = max_len*10  # convert to mm
  ) %>% 
  select(species,max_len)

## WHAY IS ATHSPP??



### Final check  ###############################################################
summary(sp_lengths_df)

# Are all species in the final trait data?
unique(len_df$species)[!unique(len_df$species) %in% sp_lengths_df$species]

# any missing length values
sp_lengths_df %>% filter(is.na(max_len))


### Export  ####################################################################
saveRDS(sp_lengths_df, file.path(trait_dir,paste0("fslen_max_length_",Sys.Date(),".rds")))

