# Project: ESARO Regional snapshot Fact 8
# Author: Sebastian Palmas

rm(list=ls())

# Profile ----

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/ESARO")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/Dem-Analytics")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/Migration and Displacement/Data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))
stopifnot(dir.exists(rawdataFolder))

# Packages ----
library(circlize)
library(forcats)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tmap)
library(xlsx)


source(file.path(repoFolder, "01_key_numbers_migration/addUnits.r"))

# Load data
load(file.path(rawdataFolder,"IDMC/IDMC2024/idmc_2024.RData"))
load(file.path(rawdataFolder,"UNICEF/country_metadata/country_metadata.Rdata")) 

# countries
ehoa <- c("TZA", "UGA", "SDN", "SSD", "SOM",
          "RWA", "KEN", "ETH", "ERI", "DJI", 
          "BDI")
ea <- country_aggregations |> filter(ParentPrintName == "Eastern Africa") |> pull(iso3)

# FACT 8 ----

idmc.new.disaster.events.country <- idmc.new.disaster.events.2008.2023 |> 
  filter(iso3 %in% ehoa, hazard.cat ==  "Weather related") |> 
  filter(year > 2018) |> 
  group_by(iso3) |> 
  summarise(idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T), .groups = 'drop') |> 
  arrange(idp.dis.new.0to17) |> 
  mutate(perc = round(100*idp.dis.new.0to17/sum(idp.dis.new.0to17)))

idmc.new.droughtflood.events.country <- idmc.new.disaster.events.2008.2023 |> 
  filter(iso3 %in% ea, hazard.type %in%  c("Flood", "Drought")) |> 
  filter(year > 2018) |> 
  group_by(iso3) |> 
  summarise(idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T), .groups = 'drop') |> 
  arrange(-idp.dis.new.0to17) |> 
  mutate(perc = round(100*idp.dis.new.0to17/sum(idp.dis.new.0to17)))


idmc.new.disaster.events.summary <- idmc.new.disaster.events.2008.2023 |> 
  filter(iso3 %in% ehoa, hazard.cat ==  "Weather related") |> 
  filter(year > 2018) |> 
  group_by(year) |> 
  summarise(idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T))


idmc.new.disaster.events.ea <- idmc.new.disaster.events.2008.2023 |> 
  filter(iso3 %in% ea, hazard.cat ==  "Weather related") |> 
  filter(year > 2018) |> 
  group_by(iso3, hazard.type) |> 
  summarise(idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T), .groups = 'drop')

idmc.new.disaster.events.ea.drought <- idmc.new.disaster.events.2008.2023 |> 
  filter(iso3 %in% ea, hazard.type ==  "Drought") |> 
  filter(year > 2018) |> 
  summarise(idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T), .groups = 'drop')

idmc.new.disaster.events.ea.drought.country <- idmc.new.disaster.events.2008.2023 |> 
  filter(iso3 %in% ea, hazard.type ==  "Drought") |> 
  filter(year > 2018) |>
  group_by(iso3) |> 
  summarise(idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T), .groups = 'drop') |> 
  mutate(perc = round(100*idp.dis.new.0to17/sum(idp.dis.new.0to17)))


# Map ----
africa <- country_aggregations |> filter(ParentPrintName == "Africa") |> pull(iso3)

idmc.new.disaster.events.africa <- idmc.new.disaster.events.2008.2023 |> 
  filter(iso3 %in% africa, hazard.type %in%  c("Drought", 'Flood'),
         year > 2018,
         !is.na(loc.coordinates)) 

write.csv(idmc.new.disaster.events.africa, file = file.path(projectFolder, "Africa_flood_drought_coordinates.csv"))

afr_coordinates <- read.xlsx(file.path(rawdataFolder, "IDMC/IDMC2024/source_data/Africa_flood_drought_coordinates.xlsx"),
                             sheetName="Sheet2")
  

temp <- read.table(text = as.character(idmc.new.disaster.events.africa$loc.coordinates), 
                   sep = ",", header = FALSE, fill = TRUE)

temp2 <-  read.table(text = gsub("[()\",\t]", " ", txt))


write.csv(idmc.new.disaster.events.africa, file = file.path(projectFolder, "africa_events.csv"))
