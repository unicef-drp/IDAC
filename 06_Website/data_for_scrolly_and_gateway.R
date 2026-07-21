# Project: dataforchildrenonthemove.org
# Script: Produce tables for Scrolly and datagateway. To use to create charts and that people can download.

rm(list=ls())

# PROFILE ----
# set working directories and all directories 
# this profile should be loaded before running any other script

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path("D:/OneDrive - UNICEF/Migration and Displacement/IDAC Working Documents/B. IDAC Website/data for scrolly and gateway"))
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "codeVS/IDAC/06_Website")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/Migration and Displacement/Data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))

# Packages ----
pacman::p_load("dplyr", "tidyr", "rsdmx", "readxl")

# Scrolly ----

## EXTRA_DATA 2SHEET ----
# total refugees (UNHCR + UNRWA) and AS
ref_and_AS <- 35640044 + 5964782 + 9000000
paste0("Refugees (UNHCR + UNRWA + AS): ", round(ref_and_AS / 1000000, 1))
ref_and_AS_0to17 <- 13990454.96 + 1654046 + 2800000
paste0("Refugees (UNHCR + UNRWA + AS) (0-17): ", round(ref_and_AS_0to17 / 1000000, 1))
paste0("Percentage: ", round(ref_and_AS_0to17 / ref_and_AS * 100))

# total forced from home within their countries IDP
32679609 / 82145130

## 2024 BY_CAUSE_PEOPLE SHEET ----
BY_CAUSE_PEOPLE <- read_excel("D:/OneDrive - UNICEF/Migration and Displacement/Data/IDMC/IDMC2026/source_data/IDMC_Internal_Displacement_Conflict-Violence_Disasters.xlsx",
  sheet = "3_IDPs_SADD_estimates") |>
  filter(Sex == "Both sexes") |>
  mutate(total = `0-4` + `5-11` + `12-17`,
    TIME_PERIOD = "_T") |>
  select(ISO3, TIME_PERIOD, Cause, total) |>
  pivot_wider(names_from = Cause, values_from = total) |>
  select(ISO3, TIME_PERIOD, Disaster, Conflict)

write.table(BY_CAUSE_PEOPLE, file.path(projectFolder, "BY_CAUSE_PEOPLE.csv"),
  na = "", row.names = FALSE, sep = ",")
  
load("D:/OneDrive - UNICEF/Migration and Displacement/Data/IDMC/IDMC2026/IDMC_2026.RData")
BY_CAUSE_EVENTS <- idmc.new |>
  filter(year == 2025) |>
  mutate(idp.new.0to17 = round(idp.new.0to17)) |>
  select(ISO3Code, cause, idp.new.0to17) |>
  pivot_wider(names_from = cause, values_from = idp.new.0to17)

write.table(BY_CAUSE_EVENTS, file.path(projectFolder, "BY_CAUSE_EVENTS.csv"),
  na = "", row.names = FALSE, sep = ",")

WEATHER_EVENTS <- idmc.new.disaster.events |> 
  filter(hazard_category_name == "Weather related") |>
  filter(year >= 2016) |>
  group_by(year) |>
  summarise(idp_dis_new_0to17 = sum(idp_dis_new_0to17, na.rm = TRUE)) |> 
  mutate(idp_dis_new_0to17 = round(idp_dis_new_0to17))

CONFLICT_EVENTS <- idmc.new |>
  filter(cause == "Conflict", year >= 2016) |>
  group_by(year) |>
  summarise(idp_dis_new_0to17 = sum(idp.new.0to17, na.rm = TRUE)) |> 
  mutate(idp_dis_new_0to17 = round(idp_dis_new_0to17))

WEATHER_VS_CONFLICT <- left_join(WEATHER_EVENTS, CONFLICT_EVENTS, by = "year", suffix = c("_weather", "_conflict"))

write.table(WEATHER_VS_CONFLICT, file.path(projectFolder, "WEATHER_VS_CONFLICT.csv"),
  na = "", row.names = FALSE, sep = ",")

# Data Gateway ----
## Migrants ----
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

## UNHCR Refugees asylum----
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

## UNHCR Refugees origin ----
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

## IDP Stock ----
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

## IDP New displacements ----
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

## Refugees UNRWA  ----
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

## Refugees per capita ----
ref.per.capita <- readSDMX(providerId = "UNICEF",
                     resource = "data",
                     flowRef = "MG",
                     version = "1.0",
                     key = ".MG_RFGS_CNTRY_ASYLM_PER1000.") |>
  as.data.frame()

write.table(ref.per.capita,
            file=file.path(projectFolder, "ref_per_capita.csv"),
            sep=',',
            na='',
            row.names=FALSE)


##  Counting number of observations ----
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