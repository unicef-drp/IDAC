# Project: dataforchildrenonthemove.org/DataGateway
# Script: Produce tables for datagateway. To use to create charts and that people can download.

rm(list=ls())

# PROFILE ----
# set working directories and all directories 
# this is the profile for the PROD-SDG_report_2023 project
# this profile should be loaded before running any other script

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/IDAC/dataforchildrenonthemove.org/Data Gateway/clean_data")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/IDAC/06_Data_Gateway")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/Migration and Displacement/Data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))

# Packages ----
library(dplyr) 
library(tidyr) 
library(rsdmx)

# Countries and regions ----
x <- getURL("https://raw.githubusercontent.com/unicef-drp/Country-and-Region-Metadata/refs/heads/main/output/all_regions_long_format.csv")
all_countries <- read.csv(text = x)

#unique Grouping combinations
regional_groupings <- unique(all_countries |> select(Regional_Grouping, Region, Region_Code))

# Migrants ----
migrants <- readSDMX(providerId = "UNICEF",
                         resource = "data",
                         flowRef = "MG",
                         version = "1.0",
                         key = ".MG_INTNL_MG_CNTRY_DEST.") |>
  as.data.frame() |> 
  mutate(OBS_VALUE_num = as.numeric(OBS_VALUE))

write.table(migrants,
            file=file.path(projectFolder, "migrants.csv"),
            sep=',',
            na='',
            row.names=FALSE)

# UNHCR Refugees asylum----
ref_UNHCR_asylum <- readSDMX(providerId = "UNICEF",
                     resource = "data",
                     flowRef = "MG",
                     version = "1.0",
                     key = ".MG_RFGS_CNTRY_ASYLM.") |>
  as.data.frame() |> 
  mutate(OBS_VALUE_num = as.numeric(OBS_VALUE))

write.table(ref_UNHCR_asylum,
            file=file.path(projectFolder, "refugees_UNHCR_asylum.csv"),
            sep=',',
            na='',
            row.names=FALSE)

# UNHCR Refugees origin ----
ref_UNHCR_origin <- readSDMX(providerId = "UNICEF",
                     resource = "data",
                     flowRef = "MG",
                     version = "1.0",
                     key = ".MG_RFGS_CNTRY_ORIGIN.") |>
  as.data.frame() |> 
  mutate(OBS_VALUE_num = as.numeric(OBS_VALUE))

write.table(ref_UNHCR_origin,
            file=file.path(projectFolder, "refugees_UNHCR_origin.csv"),
            sep=',',
            na='',
            row.names=FALSE)

# IDP Stock ----
IDP_stock <- readSDMX(providerId = "UNICEF",
                     resource = "data",
                     flowRef = "MG",
                     version = "1.0",
                     key = ".MG_INTERNAL_DISP_PERS.") |>
  as.data.frame() |> 
  mutate(OBS_VALUE_num = as.numeric(OBS_VALUE))



write.table(IDP_stock,
            file=file.path(projectFolder, "IDP_stock.csv"),
            sep=',',
            na='',
            row.names=FALSE)

# IDP New displacements ----
IDP_new <- readSDMX(providerId = "UNICEF",
                      resource = "data",
                      flowRef = "MG",
                      version = "1.0",
                      key = ".MG_NEW_INTERNAL_DISP.") |>
  as.data.frame() |> 
  mutate(OBS_VALUE_num = as.numeric(OBS_VALUE))

write.table(IDP_new,
            file=file.path(projectFolder, "IDP_new.csv"),
            sep=',',
            na='',
            row.names=FALSE)

# Refugees UNRWA  ----
ref_UNRWA <- readSDMX(providerId = "UNICEF",
                             resource = "data",
                             flowRef = "MG",
                             version = "1.0",
                             key = ".MG_UNRWA_RFGS_CNTRY_ASYLM.") |>
  as.data.frame() |> 
  mutate(OBS_VALUE_num = as.numeric(OBS_VALUE))

write.table(ref_UNRWA,
            file=file.path(projectFolder, "ref_UNRWA.csv"),
            sep=',',
            na='',
            row.names=FALSE)


# Counting number of observations ----
print(paste0("Migrants: ", nrow(migrants)))
print(paste0("Ref asylum: ", nrow(ref_UNHCR_asylum)))
print(paste0("Ref origin: ", nrow(ref_UNHCR_origin)))
print(paste0("IDP_stock: ", nrow(IDP_stock)))
print(paste0("IDP_new: ", nrow(IDP_new)))
print(paste0("Ref UNRWA: ", nrow(ref_UNRWA)))

print(paste0("Total number of obs: ", 
             nrow(migrants) + 
               nrow(ref_UNHCR_asylum) +
               nrow(ref_UNHCR_origin) +
               nrow(IDP_stock) + 
               nrow(IDP_new) +
               nrow(ref_UNRWA)))

