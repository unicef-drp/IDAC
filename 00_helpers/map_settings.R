# MAP SETTINGS ----
# Sourced once from main.R (source(file.path("00_helpers/map_settings.R"))) before
# any FACT section that plots a map. Builds shared, fact-independent map objects
# used downstream:
#   - world.robin, bnd, cst, lks, lks.df: base cartography polygons/boundaries/
#     coastlines
#   - NoDataColor, boundary.color, background.color, coastline.color,
#     plot.coastlines: shared styling
#   - world.robin$color: per-country SDG-region fill used by FACT 3 (mig stock
#     map) and any other map colored by SDG region
#   - ac / acf: the Aksai Chin China/India stripe overlay
#   - bnd.line / bnd.dash / bnd.dot / bnd.ssd: boundary-style subsets, reused by
#     make.world.map() in the FACT 7/8 (IDP stock/new) map section instead of
#     being recomputed there
#   - places: territory centroids/status, left_join'd into fact7/fact8 in the
#     FACT 7/8 section to get long/lat for the pie-chart map overlays
#
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
lks.df <- sf::st_as_sf(lks)  #converting to a dataframe


sdg.color.code <- tribble(
  ~color,     ~Region,
  "#EDA877",  "Eastern Asia and South-eastern Asia",
  "#8C789E",  "Central Asia and Southern Asia",
  "#7FBA70",  "Latin America and the Caribbean",
  "#EAE8FF",  "Western Asia and Northern Africa",
  "#62ABC3",  "Northern America and Europe",
  "#C978CF",  "Oceania excluding Australia and New Zealand",
  "#E6738E",  "Sub-Saharan Africa",
  "#FEE07F",  "Australia and New Zealand",
  "#FFFFFF",  "World"
)

world.robin.table <- tibble(ISO3Code = world.robin$ISO3_CODE)
world.robin.table <- world.robin.table |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |>
  left_join(sdg.color.code, by = "Region")

#adding the color column to the polygon object: each country gets the fill color of its SDG region
world.robin$color <- world.robin.table$color
world.robin$color[is.na(world.robin$color)] <- NoDataColor #countries/territories with no SDG region match (e.g. disputed areas)

# Some territories don't get their own SDG-region color (they aren't in regions_sdg, or are
# disputed areas that should visually match a neighbor) and instead just copy another territory's color.
territory_color_matches <- tribble(
  ~TERR_NAME,                     ~match_TERR_NAME,                              ~note,
  "Taiwan province of China",     "China",                                       NA,
  "Aksai Chin",                   "India",                                       "background/India half of the China-India stripe; see acf$color below for the China half",
  "Arunachal Pradesh",            "India",                                       NA,
  "Jammu and Kashmir",            "India",                                       "India and Pakistan share a color, so no stripes are needed here",
  "Ilemi triangle",               "South Sudan",                                 "triangle disputed between South Sudan and Kenya",
  "Abyei",                        "Sudan",                                       NA,
  "Jersey",                       "U.K. of Great Britain and Northern Ireland",  NA,
  "Falkland Islands (Malvinas)",  "U.K. of Great Britain and Northern Ireland",  NA,
  "French Guiana",                "France",                                     NA
)

for (i in seq_len(nrow(territory_color_matches))) {
  world.robin$color[world.robin$TERR_NAME == territory_color_matches$TERR_NAME[i]] <-
    world.robin$color[world.robin$TERR_NAME == territory_color_matches$match_TERR_NAME[i]]
}

world.robin$color[world.robin$TERR_NAME == "Western Sahara"] <- NoDataColor #explicitly greyed out rather than colored as a disputed territory

# UN Cartography requires that the Aksai Chin region be striped half in the color of China and half in the color of India.
# world.robin$color already holds the India (background) half from territory_color_matches above; here we build a
# separate one-polygon layer for Aksai Chin colored as China (foreground), to be drawn on top of the map afterwards.
ac <- world.robin[world.robin$TERR_NAME == "Aksai Chin", ]
acf <- sf::st_as_sf(ac) # transform to data frame for more plotting options
acf$color <- world.robin$color[world.robin$TERR_NAME == "China"] #foreground stripes color

# Three styles of boundaries are mapped: standard solid line, dashed line for undetermined boundaries,
# and dotted line for selected disputed boundaries. Each gets its own dataframe of matching boundary segments.
bnd.line <- bnd[bnd$CARTOGRAPH == "International boundary line", ]
bnd.dash <- bnd[bnd$CARTOGRAPH %in% c("Dashed boundary line", "Undetermined international dashed boundary line"), ]
bnd.dot  <- bnd[bnd$CARTOGRAPH %in% c("Dotted boundary line", "Dotted boundary line (Abyei)"), ]
bnd.ssd  <- bnd[bnd$BDY_CNT01 == "SDN" & bnd$BDY_CNT02 == "SSD", ] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script

## Territory centroids, for point-based overlays (e.g. pie chart maps) ----
places <- tibble(ISO3Code = as.character(world.robin$ISO3_CODE),
                 TERR_NAME = world.robin$TERR_NAME,
                 long = coordinates(world.robin)[, 1],
                 lat = coordinates(world.robin)[, 2],
                 STATUS = world.robin$STATUS) |>
  mutate(ISO3Code = ifelse(TERR_NAME == "Abyei", "AB9", ISO3Code))