# Project: IDAC 10 Facts for children on the move
# Script: Main file

# PROFILE ----
source(file.path("01_9facts/profile.R"))

# PACKAGES ----
library(pacman)
p_load(circlize, dplyr, eurostat, forcats, ggplot2, kableExtra, rsdmx, scales, stringr, tidyr, openxlsx)

# for circle charts
p_load(igraph, ggraph)

# for maps
p_load(ggforce, sp, plotrix, caroline)

#ggforce #for geom_circle plot in fact 7
#sp # plot coordinates
#plotrix #draw.circle
#caroline for pie charts over a scatterplot

# HELPER FUNCTIONS ----
source(file.path("00_helpers/addUnits.R"))
source(file.path("00_helpers/pies_overplot.R"))

# LOAD MIGRATION DATA ----
mig2024 <- readSDMX(providerId = "UNICEF", resource = "data", flowRef = "MG", version = "1.0",
                  key = "WORLD.MG_INTNL_MG_CNTRY_DEST.._T") |>
  as.data.frame()
# https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/MG/WORLD.MG_INTNL_MG_CNTRY_DEST.._T/all/

#load(file.path(rawdataFolder, "UNPD/UNMigrantStock2020/UN_MigrantStockByOriginAndDestination.Rdata"))
load(file.path(rawdataFolder, "UNPD/UNMigrantStock2020/UN_MigrantStockAge0to17.Rdata")) #country data
load(file.path(rawdataFolder, "UNPD/UNMigrantStock2024/mig_stock_dest_orig.Rdata")) 
#load(file.path(rawdataFolder, "UNPD/UNMigrantStock2020/UN_MigrantStockAge.Rdata")) #country by age groups
load(file.path(rawdataFolder,"IDMC/IDMC2025/idmc_2025.RData"))
load(file.path(rawdataFolder,'UNHCR/GlobalTrends2024/UNHCR_2024.RData'))
load(file.path(rawdataFolder, 'Asylum seekers estimate/AS_estimate_2024.RData'))

## UNRWA data ----
unrwa <- readSDMX(providerId = "UNICEF", resource = "data", flowRef = "MG", version = "1.0",
                  key = "WORLD.MG_UNRWA_RFGS_CNTRY_ASYLM.") |>
  as.data.frame()


# GEOGRAPHIC AREAS ----
regions <- read.csv("https://raw.githubusercontent.com/unicef-drp/Country-and-Region-Metadata/refs/heads/main/output/all_regions_long_format.csv")
regions_unique <- regions |> select(Region_Code, UNSD_Code) |> unique()
geo_areas <- read.csv("https://raw.githubusercontent.com/unicef-drp/Country-and-Region-Metadata/refs/heads/main/raw_data/SDMX_meta_info/geographic_areas.csv")
countries_key <-paste0(regions$ISO3Code |> unique() |> sort(), collapse = "+")

## SDG regions
regions_sdg <- regions |> 
  filter(Region_Code %in% c("UNSDG_EASTERNASIASOUTHEASTERNASIA",
                            "UNSDG_CENTRALASIASOUTHERNASIA",
                            "UNSDG_LAC",
                            "UNSDG_WESTERNASIANORTHERNAFR",
                            "UNSDG_EUROPENORTHERNAMR",
                            "UNSDG_OCEANIA",
                            "UNSDG_SUBSAHARANAFRICA"))

#adding territories to certain SDGs for plotting
regions_sdg <- regions_sdg |>
  bind_rows(tibble(ISO3Code = "HKG",  #Hong Kong
                   Region_Code = "UNSDG_EASTERNASIASOUTHEASTERNASIA",
                   Region = "Eastern and South-Eastern Asia")) |> 
  bind_rows(tibble(ISO3Code = "TWN",  #Taiwan
                   Region_Code = "UNSDG_EASTERNASIASOUTHEASTERNASIA",
                   Region = "Eastern and South-Eastern Asia")) |> 
  bind_rows(tibble(ISO3Code = "MAC",  #Macau
                   Region_Code = "UNSDG_EASTERNASIASOUTHEASTERNASIA",
                   Region = "Eastern and South-Eastern Asia")) |> 
  bind_rows(tibble(ISO3Code = "XKX",  #Kosovo
                   Region_Code = "UNSDG_EUROPENORTHERNAMR",
                   Region = "Europe and Northern America")) |> 
  bind_rows(tibble(ISO3Code = "AB9",  #Abyei
                   Region_Code = "UNSDG_SUBSAHARANAFRICA",
                   Region = "Sub-Saharan Africa"))

# MAP SETTINGS ----
# colors for polygons with data
NoDataColor <- "darkgray" # color for polygons with no data
boundary.color <- "grey" # color for country boundaries
background.color <- "white" # color for oceans, seas and lakes
coastline.color <- "grey"

plot.coastlines <- TRUE

## Read in the UN cartography polygon shapefile (no antarctica) --------------
#The .rds files are created in presave_sp_rds_object.R
world.robin <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.world.robin.rds"))  #country polygons
bnd <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.bnd.rds"))  #Boundaries lines
cst <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.cst.rds"))  #Coastal polygons
lks <- readRDS(file.path(rawdataFolder, "unmap/sp/sp.lks.rds"))  #Lakes polygons
lks.df <- fortify(lks)  #converting to a dataframe
# ant <- readRDS(file.path(path.map, "sp.ant.rds"))  #Antarctica polygon
# ant.df <- fortify(ant)  #converting to a dataframe
# ant.df$color.code <- NA  #to not plot Antarctica


sdg.color.code <- tibble(color=c("#EDA877",
                                 "#8C789E",
                                 "#7FBA70",
                                 "#EAE8FF",
                                 "#62ABC3",
                                 "#C978CF",
                                 "#E6738E",
                                 "#FEE07F",
                                 "#FFFFFF"),
                         Region=c("Eastern Asia and South-eastern Asia",
                                  "Central Asia and Southern Asia",
                                  "Latin America and the Caribbean",
                                  "Western Asia and Northern Africa",
                                  "Northern America and Europe",
                                  "Oceania excluding Australia and New Zealand",
                                  "Sub-Saharan Africa",
                                  "Australia and New Zealand",
                                  "World"))

world.robin.table <- tibble(ISO3Code = world.robin$ISO3_CODE)
world.robin.table <- world.robin.table |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |> 
  left_join(sdg.color.code, by = "Region") 

#adding the color column to the polygon object
world.robin$color <- world.robin.table$color 
world.robin$color[is.na(world.robin$color) ] <- NoDataColor
world.robin$color[world.robin$TERR_NAME == "Taiwan province of China"] <- world.robin$color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$color[world.robin$TERR_NAME == "Aksai Chin"] <- "#8C789E" #Background stripes color (color of India)
world.robin$color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- "#8C789E" # #Pakistan and India are colored the same, so no need for stripes
world.robin$color[world.robin$TERR_NAME == "Western Sahara"] <- NoDataColor
world.robin$color[world.robin$TERR_NAME == "Ilemi triangle"] <- world.robin$color[world.robin$TERR_NAME == "South Sudan"] # Triangle South Sudan and Kenya 
world.robin$color[world.robin$TERR_NAME == "Abyei"] <- world.robin$color[world.robin$TERR_NAME == "Sudan"]
world.robin$color[world.robin$TERR_NAME %in% c("Jersey", "Falkland Islands (Malvinas)")] <- world.robin$color[world.robin$TERR_NAME == "U.K. of Great Britain and Northern Ireland"]
world.robin$color[world.robin$TERR_NAME == "French Guiana"] <- world.robin$color[world.robin$TERR_NAME == "France"]

# UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of India
# to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China
# it will be layered on top of the map in a separate step
ac <- world.robin[world.robin$TERR_NAME == "Aksai Chin",]
acf <- fortify(ac) # transform to data frame for more plotting options
acf$color <- world.robin$color[which(world.robin$TERR_NAME == "China")]  #foreground stripes color

# three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
# to do that, we create a separate dataframe with each containing boundaries of the same type
bnd.line <- bnd[bnd$CARTOGRAPH == "International boundary line", ]
bnd.dash <- bnd[bnd$CARTOGRAPH == "Dashed boundary line" | bnd$CARTOGRAPH == "Undetermined international dashed boundary line", ]
bnd.dot <- bnd[bnd$CARTOGRAPH == "Dotted boundary line" | bnd$CARTOGRAPH == "Dotted boundary line (Abyei)", ]
bnd.ssd <- bnd[bnd$BDY_CNT01 == "SDN" & bnd$BDY_CNT02 == "SSD", ] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 


# CHARTS ----
source(file.path("08_IDAC_9facts/10facts_charts.R"))