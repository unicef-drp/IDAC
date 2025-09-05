# Project: IDAC 9 Facts for children on the move
# Author: Main file

rm(list=ls())

# PROFILE ----
source(file.path("08_IDAC_9facts/profile.R"))

# PACKAGES ----
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(waffle)
library(hrbrthemes)
library(kableExtra)
library(forcats)
library(scales)

# for maps
library(ggforce) #for geom_circle plot in fact 7
library(sp) # plot coordinates
library(plotrix) #draw.circle
library(caroline) #for pie charts over a scatterplot

source(file.path("00_helpers/addUnits.R"))
source(file.path(repoFolder, "9 Facts 2024/pies_overplot.R"))

# LOAD DATA ----
load(file.path(rawdataFolder, "UNPD/WPP2022/wpp_location.Rdata"))
load(file.path(rawdataFolder, "UNPD/WPP2022/UN_MigrantStock.Rdata"))
load(file.path(rawdataFolder, "UNPD/WPP2022/UN_MigrantStockByOriginAndDestination.Rdata"))
load(file.path(rawdataFolder, "UNPD/WPP2022/UN_MigrantStockAge0to17.Rdata")) #country data
load(file.path(rawdataFolder, "UNPD/WPP2022/UN_MigrantStockAge.Rdata")) #country by age groups
load(file.path(rawdataFolder, "UNPD/WPP2022/wpp_age_group.Rdata"))
load(file.path(rawdataFolder,'UNHCR/GlobalTrends2024/UNHCR_2024.RData'))
load(file.path(rawdataFolder,"IDMC/IDMC2025/idmc_2025.RData"))
load(file.path(rawdataFolder, 'UNRWA/unrwa_refugees.RData'))
load(file.path(rawdataFolder, 'Asylum seekers estimate/AS_estimate_2023.RData'))

country_regions <- readr::read_csv(file.path(rawdataFolder,"UNICEF/country_metadata/country_regions_master.csv")) 
load(file.path(path.basic, 'Data/WB/WB_classifications_2020.Rdata'))  #World bank regions

# CHARTS ----
source(file.path("08_IDAC_9facts/9facts_charts.R"))

# MAPS ----
source(file.path("08_IDAC_9facts/9facts_charts.R"))
