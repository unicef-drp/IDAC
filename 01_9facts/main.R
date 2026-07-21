# Project: IDAC 10 Facts for children on the move
# Script: Main file

# PATHS ----
projectFolder <- file.path("D:/OneDrive - UNICEF/Migration and Displacement/IDAC Working Documents/J. IDAC Publications/9 Key Facts/Version_2026/output") #Output files
repoFolder    <- file.path(Sys.getenv("USERPROFILE"), "codeVS/IDAC/01_9facts") #repository files
rawdataFolder <- file.path("D:/OneDrive - UNICEF/Migration and Displacement/Data/") #raw data folder

stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))

# PACKAGES ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(
  dplyr, eurostat, forcats, ggplot2, scales, stringr, tidyr, openxlsx,
  sf,            #for st_as_sf function in map_settings
  igraph, ggraph,    #For fact 5 and 6 circle packing plots
  #ggforce,         # geo_circle plot in fact 7
  #sp,               # plot coordinates
  #plotrix,          # draw.circle
  caroline          # Fact 7 and 8pie charts over a scatterplot
)


# LOAD MIGRATION AND DISPLACEMENT DATA ----
load(file.path(rawdataFolder, "UNPD/UNMigrantStock2020/UN_MigrantStockAge0to17.Rdata")) #Used in Fact 2
load(file.path(rawdataFolder, "UNPD/UNMigrantStock2024/mig_stock_dest_orig.RData"))
load(file.path(rawdataFolder, "IDMC/IDMC2026/IDMC_2026.RData"))
load(file.path(rawdataFolder, "UNHCR/GlobalTrends2025/UNHCR_2025.RData"))
AS.estimate <- read.csv(file.path(rawdataFolder, "Asylum seekers estimate/AS_estimate.csv"))
unrwa <- read.csv(paste0("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/",
                         "data/GLOBAL_DATAFLOW/WPP_GLOBAL.MG_UNRWA_RFGS_CNTRY_ASYLM._T/",
                         "all/?format=csv"))

# GEOGRAPHIC AREAS ----
regions <- read.csv("https://raw.githubusercontent.com/unicef-drp/Country-and-Region-Metadata/refs/heads/main/output/all_regions_long_format.csv")
geo_areas <- read.csv("https://raw.githubusercontent.com/unicef-drp/Country-and-Region-Metadata/refs/heads/main/raw_data/SDMX_meta_info/geographic_areas.csv")
countries_key <- paste0(regions$ISO3Code |> unique() |> sort(), collapse = "+")

## SDG regions to use in analysis
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
  bind_rows(tribble(
    ~ISO3Code, ~Region_Code,                        ~Region,
    "HKG",     "UNSDG_EASTERNASIASOUTHEASTERNASIA",  "Eastern and South-Eastern Asia",  #Hong Kong
    "TWN",     "UNSDG_EASTERNASIASOUTHEASTERNASIA",  "Eastern and South-Eastern Asia",  #Taiwan
    "MAC",     "UNSDG_EASTERNASIASOUTHEASTERNASIA",  "Eastern and South-Eastern Asia",  #Macau
    "XKX",     "UNSDG_EUROPENORTHERNAMR",             "Europe and Northern America",     #Kosovo
    "AB9",     "UNSDG_SUBSAHARANAFRICA",              "Sub-Saharan Africa"               #Abyei
  ))
  
# HELPER FUNCTIONS ----
source(file.path("00_helpers/addUnits.R"))
source(file.path("00_helpers/circlepack_chart.R")) #For fact 5 and 6 circle packing plots
source(file.path("00_helpers/pies_overplot.R"))
source(file.path("00_helpers/map_settings.R"))

# FACT 1: Gaps ----
gap.refugees.table <- ref.asylum |>
  filter(year == 2025) |>
  mutate(coverage.sex.age.low = coverage.sex.age < 0.5)

gap.refugee.perc <- round(100 * sum(gap.refugees.table$coverage.sex.age.low) / nrow(gap.refugees.table))
print(paste0("% of countries lacking reliable age data on refugees: ", gap.refugee.perc, "%"))


# FACT 2: Mig Stock estimate----

approx.2024 <- mig.stock.0to17 |>
  filter(sex == "both") |>
  select(year, pop.mig.total, pop.mig.0to17.eu) |>
  group_by(year) |>
  summarise_all(sum, na.rm = TRUE) |>
  mutate(pop.mig.0to17.eu.perc = pop.mig.0to17.eu / pop.mig.total,
         approx.2024 =  round(pop.mig.0to17.eu.perc * 304021813 / 1000000))

## Fact 2 range pie ----
# Range indicator: share of migrants aged 0-17 is estimated at 12% (low) to 14% (high) of all migrants
fact2.range.pie <- tribble(
  ~category,    ~value,
  "0-12%",      12,
  "12-14%",     2,
  "remainder",  86
) |>
  mutate(category = factor(category, levels = category))

fact2.range.fig <- ggplot(fact2.range.pie, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1, linewidth = 0.8) +
  coord_polar(theta = "y", direction = -1) +
  scale_fill_manual(values = c("0-12%" = "#774C9E", "12-14%" = "#774C9E80", "remainder" = "grey92"),
                    guide = "none") +
  annotate("text", x = 1, y = 0, vjust = 2.5,
           label = "BETWEEN\n37 AND 42 MILLION\nMIGRANT CHILDREN",
           size = 9, fontface = "bold", color = "#774C9E", lineheight = 0.9) +
  theme_void()
print(fact2.range.fig)
ggsave(plot = fact2.range.fig, filename = file.path(projectFolder, "fact2_range.pdf"),
       device = "pdf", width = 15, height = 15, units = "cm")

## fact2.xls ----
fact2.xls <- fig2 |> 
  mutate(OBS_VALUE = value / 1000000) |>
  select(year, name, OBS_VALUE, label)

# FACT 3: Mig Origin-Dest  ----
sdg_regions <- tribble(
  ~sdg_region,                                     ~location_code,
  "Sub-Saharan Africa",                             1834,
  "Northern Africa and Western Asia",               1833,
  "Central and Southern Asia",                      1831,
  "Eastern and South-Eastern Asia",                 1832,
  "Latin America and the Caribbean",                1830,
  "Oceania (excluding Australia and New Zealand)",  1835,
  "Australia/New Zealand",                          1836,
  "Europe and Northern America",                    1829
)

mig_stock_dest_orig_sdg_regions <- mig.stock.dest.orig |>
  filter(year == 2024) |>
  filter(UNSD_Code.dest %in% sdg_regions$location_code) |>
  filter(UNSD_Code.orig %in% sdg_regions$location_code) |>
  mutate(pop.mig = pop.mig / 1000000) |>
  left_join(sdg_regions |> rename(sdg_region.orig = sdg_region),
            by = c("UNSD_Code.orig" = "location_code")) |>
  left_join(sdg_regions |> rename(sdg_region.dest = sdg_region),
            by = c("UNSD_Code.dest" = "location_code")) |>
  select(sdg_region.orig, sdg_region.dest, pop.mig)

#combining Oceania (exluding Australia and New Zealand) and Australia/New Zealand
mig_stock_dest_orig_sdg_regions <- mig_stock_dest_orig_sdg_regions |>
  mutate(sdg_region.orig = if_else(sdg_region.orig %in% c("Oceania (excluding Australia and New Zealand)", "Australia/New Zealand"),
                                   "Oceania", sdg_region.orig),
         sdg_region.dest = if_else(sdg_region.dest %in% c("Oceania (excluding Australia and New Zealand)", "Australia/New Zealand"),
                                   "Oceania", sdg_region.dest)) |>
  group_by(sdg_region.orig, sdg_region.dest) |>
  summarise(pop.mig = sum(pop.mig), .groups = "drop")

#Writing the data to a CSV file for use in app.flourish.studio to create the alluvial chart
write.csv(mig_stock_dest_orig_sdg_regions,
          file = file.path(projectFolder, "fact3.csv"),
          na = "", row.names = FALSE)

# FACT 4: Forcibly displaced over time----
fact4.idmc <- idmc.stock |>
  filter(cause == "Conflict" & sex == "Both sexes")

## Correcting Gaza 2023-2025 --------------------------------------------------------------------------------------------------------------
#IDMC estimates that in 2023-2025, 70% of IDPs in Gaza are also registered as UNRWA refugees
# To correct for this, we remove these registered refugees from IDMC numbers by conflict
# We only keep 30% of the numbers from PSE in 2023, 2024 and 2025.
# Because IDMC uses 3 groups for 0-17 data, we remove 70% from all groups

# See: https://www.unhcr.org/refugee-statistics/insights/explainers/forcibly-displaced-pocs.html (Acessed on 2025-10-13)
# Website saved as PDF in \Migration and Displacement/Forcibly displaced and stateless population categories _ UNHCR.pdf
gaza.correction.cols <- c("idp.stock.0to4", "idp.stock.0to17", "idp.stock.5to11",
                          "idp.stock.12to17", "idp.stock.18to59", "idp.stock.60plus")
gaza.correction.rows <- fact4.idmc$year %in% c(2023, 2024, 2025) & fact4.idmc$ISO3Code == "PSE"
fact4.idmc[gaza.correction.rows, gaza.correction.cols] <- fact4.idmc[gaza.correction.rows, gaza.correction.cols] * .3

# Shared shape for each fact4 source: yearly totals, tagged with a pop.type label
summarise_fact4_component <- function(df, pop, pop.0to17, label, na.rm = TRUE) {
  df |>
    group_by(year) |>
    summarise(pop = sum({{ pop }}, na.rm = na.rm),
              pop.0to17 = sum({{ pop.0to17 }}, na.rm = na.rm)) |>
    mutate(pop.type = label) |>
    select(year, pop.type, pop, pop.0to17)
}

fact4.idmc <- fact4.idmc |>
  summarise_fact4_component(idp.stock, idp.stock.0to17, "Conflict-related\ninternally displaced children")

fact4.unrwa <- unrwa |>
  mutate(year = as.numeric(TIME_PERIOD),
         pop = as.numeric(OBS_VALUE) * (AGE == "_T"),
         pop.0to17 = as.numeric(OBS_VALUE) * (AGE == "Y0T17")) |>
  summarise_fact4_component(pop, pop.0to17, "Palestine refugee children\n(registered with UNRWA)", na.rm = FALSE)
#fact4.unrwa$pop.0to17[fact4.unrwa$pop.0to17 == 0] <- NA  #Values are not zero, should be NA

fact4.unhcr <- ref.asylum |>
  summarise_fact4_component(ref, ref.0to17.estimate, "Refugee children")

fact4.AS <- AS.estimate |>
  summarise_fact4_component(AS, AS.0to17.estimated, "Asylum-seeking children")

fact4 <- bind_rows(fact4.idmc,
                   fact4.unhcr,
                   fact4.unrwa,
                   fact4.AS)

fact4.total <- fact4 |>
  filter(year %in% 2010:2025) |>
  group_by(year) |>
  summarise(pop.0to17 = sum(pop.0to17)) |>
  mutate(pop.type = "Total forcibly displaced children")

fact4.2018.plus <- fact4 |>
  filter(year >= 2018) |>
  select(-pop)

cols <- c("Total forcibly displaced children" = "#0092C4",
          "Conflict-related\ninternally displaced children" = "#774C9E",
          "Asylum-seeking children" = "#E60037",
          "Refugee children" = "#FDC200",
          "Palestine refugee children\n(registered with UNRWA)" = "#39A443")

fact4_new <- bind_rows(fact4.total, fact4.2018.plus) |>
  mutate(pop.0to17 = pop.0to17 / 1000000,
         pop.label = round(pop.0to17, digits = 1),
         show.bar = if_else(pop.type == "Total forcibly displaced children" & year >= 2018, FALSE, TRUE),
         pop.type = factor(pop.type, levels = names(cols)))  #order of labels, driven by cols so the two can't drift apart

# Plot
fact4.fig <- ggplot(fact4_new |> filter(show.bar), aes(x = factor(year), y = pop.0to17, fill = pop.type)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(data = fact4_new, 
            mapping = aes(label = pop.label, y = pop.0to17), 
            colour = "white", size = 3, fontface = "bold",
            position = position_stack(vjust = 0.5))+
  geom_text(data = fact4_new |> filter(pop.type == "Total forcibly displaced children"), 
            mapping = aes(label = pop.label, y = pop.0to17),
            colour = "#0092C4", size = 4, fontface = "bold", vjust = -.3)+
  scale_fill_manual(values = cols,
                    name = "") +
  scale_x_discrete(breaks = 2010:2025) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = NULL, y = NULL)+
  theme_classic() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  theme(axis.line = element_line(colour = "grey50", linewidth = 0.4),
        axis.text = element_text(size = 8, color = "grey50"), 
        axis.ticks = element_line(colour = "grey50", linewidth = 0.4),
        axis.title = element_text(size = 8, color = "grey50"),
        legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(color = "#0092C4"),
        plot.subtitle = element_text(color = "#0092C4"))
print(fact4.fig)
ggsave(filename = file.path(projectFolder, "fact4.pdf"),
       device = "pdf", width = 20, height = 20, units = "cm")


# FACT 5: Refugee Origin ----
fact5 <- ref.origin |>
  filter(year == 2025) |>
  filter(!(ISO3Code %in% c("UNK", "XXA", "TIB", "LUX", "PLW"))) |>
  select(ISO3Code, ref.0to17.estimate) |> 
  left_join(geo_areas , by = c("ISO3Code" = "id")) |>
  left_join(regions_sdg |> select(Region, Region_Code, ISO3Code), by = "ISO3Code")

# Top 10 countries of origin of refugee children ----
ref.origin.top10 <- ref.origin |>
  filter(year == 2025) |>
  filter(!(ISO3Code %in% c("UNK", "XXA", "TIB", "LUX", "PLW"))) |>
  left_join(regions_sdg |> select(Region, Region_Code, ISO3Code), by = "ISO3Code") |>
  arrange(desc(ref.0to17.estimate)) |>
  mutate(ref.0to17.estimate.perc = 100 * ref.0to17.estimate / sum(ref.0to17.estimate),
         ref.0to17.estimate.perc.cumul = round(cumsum(ref.0to17.estimate.perc)),
         ref.0to17.estimate.perc = round(ref.0to17.estimate.perc),
         ref.0to17.estimate = addUnits(ref.0to17.estimate, digits = 1)) |>
  slice_head(n = 10) |>
  select(ISO3Code, ref.0to17.estimate, ref.0to17.estimate.perc, ref.0to17.estimate.perc.cumul)

#Fixing country labels
fact5$country.label <- fact5$name_abbr
fact5$country.label[fact5$ref.0to17.estimate < 100000] <- NA
#fact5$country.label[fact5$country.label == "Democratic Republic of the Congo"] <- "DRC"
#fact5$country.label[fact5$country.label == "Central African Republic"] <- "CAR"
#fact5$country.label[fact5$country.label == "Syrian Arab Republic"] <- "Syria"

## Analysis by Region ----
fact5_region <- fact5 |>
  group_by(Region_Code) |>
  summarise(ref.0to17.region = sum(ref.0to17.estimate)) |>
  arrange(desc(ref.0to17.region)) |> 
  mutate(ref.0to17.region.mill = round(ref.0to17.region / 1000000, digits = 1),
         ref.0to17.region.perc = 100 * ref.0to17.region / sum(ref.0to17.region),
         ref.0to17.region.perc.cumul = round(cumsum(ref.0to17.region.perc)))
View(fact5_region)

fact5_toView <- fact5 |>
  arrange(desc(ref.0to17.estimate)) |>
  mutate(ref.0to17.estimate.perc = 100 * ref.0to17.estimate / sum(ref.0to17.estimate),
         ref.0to17.estimate.perc.cumul = round(cumsum(ref.0to17.estimate.perc)),
         ref.0to17.estimate.perc = round(ref.0to17.estimate.perc),
         ref.0to17.estimate = round(signif(ref.0to17.estimate, digits = 2)),
         ref.0to17.estimate.mill = round(ref.0to17.estimate / 1000000, digits = 1)) |> 
  left_join(fact5_region |> select(Region_Code, ref.0to17.region), by = "Region_Code") |> 
  mutate(perc.of.region = round(100 * (ref.0to17.estimate / ref.0to17.region))) |> 
  select(ISO3Code, name,
         ref.0to17.estimate, ref.0to17.estimate.mill, ref.0to17.estimate.perc, ref.0to17.estimate.perc.cumul,
         Region_Code, perc.of.region)
View(fact5_toView)

fact5_toView_ssa <- fact5 |>
  filter(Region_Code == "UNSDG_SUBSAHARANAFRICA") |>
  arrange(desc(ref.0to17.estimate)) |> 
  mutate(ref.0to17.estimate.perc = 100 * ref.0to17.estimate / sum(ref.0to17.estimate),
         cumul.perc = round(cumsum(ref.0to17.estimate.perc)),
         ref.0to17.estimate.perc = round(ref.0to17.estimate.perc),
         ref.0to17.estimate.thousands = round(signif(ref.0to17.estimate, digits = 2)),
         ref.0to17.estimate = round(ref.0to17.estimate / 1000000, digits = 1)) 
View(fact5_toView_ssa)

## Analysis by Continent ----
regions_continents <- regions |> filter(Regional_Grouping == "UNSDG_REGION_GLOBAL", Region %in% c("Africa", "Asia"))
fact5_continent <- fact5 |>
  left_join(regions_continents |> select(ISO3Code, Continent = Region), by = "ISO3Code") |> 
  group_by(Continent) |> 
  summarise(ref.0to17.continent = sum(ref.0to17.estimate), .groups = "drop") |> 
  arrange(desc(ref.0to17.continent)) |> 
  mutate(ref.0to17.continent.mill = round(ref.0to17.continent / 1000000, digits = 1),
         ref.0to17.continent.perc = 100 * ref.0to17.continent / sum(ref.0to17.continent),
         ref.0to17.continent.perc.cumul = round(cumsum(ref.0to17.continent.perc)))
View(fact5_continent)

## Chart ----
plot_circlepack(
  fact5,
  name_label_overrides = c(
    "Central African Republic" = "CAR",
    "Venezuela (Bolivarian Republic of)" = "Venezuela\n(Bol. Rep. of)",
    "Syrian Arab Republic" = "Syrian\nArab\nRep.",
    "Democratic Republic of the Congo" = "DRC",
    "South Sudan" = "South\nSudan"
  ),
  out_file = "fact5_origin.pdf"
)

## fact5.xls ----
fact5.xls <- fact5 |>
  mutate(OBS_VALUE = round(ref.0to17.estimate)) |>
  select(name, Region, OBS_VALUE)

# FACT 6: Refugee Asylum ----
fact6 <- ref.asylum |>
  filter(year == 2025) |>
  filter(!(ISO3Code %in% c("UNK", "XXA", "TIB", "LUX", "PLW"))) |>
  select(ISO3Code, ref.0to17.estimate) |>
  left_join(geo_areas , by = c("ISO3Code" = "id")) |>
  left_join(regions_sdg |> select(Region, Region_Code, ISO3Code), by = "ISO3Code")

#Fixing country labels
fact6$country.label <- fact6$name_abbr
fact6$country.label[fact6$ref.0to17.estimate < 100000] <- NA

## Analysis by Region ----
fact6_region <- fact6 |>
  group_by(Region_Code) |>
  summarise(ref.0to17.region = sum(ref.0to17.estimate)) |>
  arrange(desc(ref.0to17.region)) |>
  mutate(ref.0to17.region.mill = round(ref.0to17.region / 1000000, digits = 1),
         ref.0to17.region.perc = round(100 * ref.0to17.region / sum(ref.0to17.region)))
View(fact6_region)

## Analysis by Continent ----
regions_continents <- regions |> filter(Regional_Grouping == "UNSDG_REGION_GLOBAL", Region %in% c("Africa", "Asia"))
fact6_continent <- fact6 |>
  left_join(regions_continents |> select(ISO3Code, Continent = Region), by = "ISO3Code") |> 
  group_by(Continent) |> 
  summarise(ref.0to17.continent = sum(ref.0to17.estimate)) |> 
  arrange(desc(ref.0to17.continent)) |> 
  mutate(ref.0to17.continent.mill = round(ref.0to17.continent / 1000000, digits = 1),
         ref.0to17.continent.perc = 100 * ref.0to17.continent / sum(ref.0to17.continent))
View(fact6_continent)

## Analysis by country ----
fact6_toView <- fact6 |>
  arrange(desc(ref.0to17.estimate)) |> 
  mutate(ref.0to17.estimate.perc = 100 * ref.0to17.estimate / sum(ref.0to17.estimate),
         ref.0to17.estimate.perc.cumul = round(cumsum(ref.0to17.estimate.perc)),
         ref.0to17.estimate.perc = round(ref.0to17.estimate.perc),
         ref.0to17.estimate = round(signif(ref.0to17.estimate, digits = 2)),
         ref.0to17.estimate.mill = round(ref.0to17.estimate / 1000000, digits = 1),
         ref.0to17.estimate.thsd = signif(ref.0to17.estimate, digits = 2)) |>
  left_join(fact6_region |> select(Region_Code, ref.0to17.region), by = "Region_Code") |>
  mutate(perc.of.region = round(100 * (ref.0to17.estimate / ref.0to17.region))) |>
  select(ISO3Code, name,
         ref.0to17.estimate, ref.0to17.estimate.mill, ref.0to17.estimate.thsd,
         ref.0to17.estimate.perc, ref.0to17.estimate.perc.cumul, 
         Region_Code, perc.of.region)
View(fact6_toView)

fact6_toView_ssa <- fact6 |>
  filter(Region_Code == "UNSDG_SUBSAHARANAFRICA")|>
  arrange(desc(ref.0to17.estimate)) |> 
  mutate(ref.0to17.estimate.perc = 100 * ref.0to17.estimate / sum(ref.0to17.estimate),
         cumul.perc = round(cumsum(ref.0to17.estimate.perc)),
         ref.0to17.estimate.perc = round(ref.0to17.estimate.perc),
         ref.0to17.estimate.thousands = round(signif(ref.0to17.estimate, digits = 2)),
         ref.0to17.estimate = round(ref.0to17.estimate / 1000000, digits = 1)) 
View(fact6_toView_ssa)

## Fact 6 Chart ----
plot_circlepack(
  fact6,
  name_label_overrides = c(
    "Democratic Republic of the Congo" = "DRC",
    "Iran (Islamic Republic of)" = "Iran (Islamic\nRepublic of)",
    "United Kingdom" = "United\nKingdom",
    "South Sudan" = "South\nSudan"
  ),
  out_file = "fact6_asylum.pdf"
)

## fact6.xls ----
fact6.xls <- fact6 |>
  mutate(OBS_VALUE = round(ref.0to17.estimate)) |> 
  select(name, Region, OBS_VALUE)

## Fact 6: most refugees per capita ----
ref_per_cap <- read.csv(paste0("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/",
                         "data/GLOBAL_DATAFLOW/", countries_key,
                         ".MG_RFGS_CNTRY_ASYLM_PER1000._T/",
                         "/?startPeriod=2025&endPeriod=2025&format=csv")) |>
  filter(OBS_VALUE != "<1") |>
  mutate(OBS_VALUE_num = as.numeric(OBS_VALUE)) |>
  arrange(desc(OBS_VALUE_num)) |>
  slice_head(n = 10) |>
  select(REF_AREA, Geographic.area, OBS_VALUE)
View(ref_per_cap)

# FACT 7 and 8 : MAP IDP and IDP NEW----
## Fact 7 summary----
fact7.idmc.stock <- idmc.stock |>
  filter(year == 2025) |>
  filter(sex == "Both sexes") |> 
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code")

idmc.stock.summary <-  fact7.idmc.stock |>
  group_by(cause, year) |>
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = TRUE), .groups = "drop") |>
  mutate(perc = round(100 * pop.0to17 / sum(pop.0to17), digits = 1),
         pop.0to17.millions = round(pop.0to17 / 1000000, digits = 1))

idmc.stock.summary <- idmc.stock.summary |>
  bind_rows(
    summarise(idmc.stock.summary, cause = "Total", year = unique(year),
              pop.0to17 = sum(pop.0to17), perc = sum(perc),
              pop.0to17.millions = sum(pop.0to17.millions))
  )

idmc.stock.summary.region <-  fact7.idmc.stock |>
  filter(cause == "Conflict") |>
  group_by(Region) |>
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(pop.0to17)) |>
  mutate(perc = 100 * pop.0to17 / sum(pop.0to17),
         pop.0to17.millions = round(pop.0to17 / 1000000, digits = 1),
         perc.cumul = round(cumsum(perc)),
         perc = round(perc))

idmc.stock.2025 <-  idmc.stock |>
  filter(year == 2025, sex == "Both sexes") |>
  summarise(pop = sum(idp.stock, na.rm = TRUE),
            pop.0to17 = sum(idp.stock.0to17, na.rm = TRUE), .groups = "drop") |>
  mutate(perc = round(100 * pop.0to17 / pop, digits = 1))
idmc.stock.2025

## Fact 8 summary----
idmc.new.2025 <- idmc.new |>
  filter(year == 2025) |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code")

idmc.new.summary <- idmc.new.2025 |>
  group_by(cause) |>
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = TRUE), .groups = "drop") |>
  mutate(perc = round(100 * idp.new.0to17 / sum(idp.new.0to17), digits = 1),
         idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))

idmc.new.summary <- idmc.new.summary |>
  bind_rows(
    summarise(idmc.new.summary, cause = "Total",
              idp.new.0to17 = sum(idp.new.0to17),
              perc = sum(perc),
              idp.new.0to17.millions = sum(idp.new.0to17.millions))
  )

idmc.new.summary.region.conf <- idmc.new.2025 |>
  filter(cause == "Conflict") |>
  group_by(Region) |>
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = TRUE), .groups = "drop") |> 
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))

idmc.new.summary.region.dis <- idmc.new.2025 |>
  filter(cause == "Disaster") |>
  group_by(Region) |>
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(idp.new.0to17)) |>
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1),
         perc = round(100 * idp.new.0to17 / sum(idp.new.0to17), digits = 1),
         perc.cumul = round(cumsum(perc), digits = 1))

## Top 10 disaster events 2025 ----
idmc.top10.events.2025 <- idmc.new.disaster.events |>
  filter(year == 2025) |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |>
  arrange(desc(idp_dis_new)) |>
  slice_head(n = 10) |>
  select(event_name, country_name, Region, hazard_type_name, start_date, idp_dis_new, idp_dis_new_0to17)


## Prepare IDMC data for pie charts----
# `places` (territory centroids/status, keyed by ISO3Code) is defined in map_settings.R

fact7 <- fact7.idmc.stock |>
  select(ISO3Code, cause, idp.stock.0to17) |> 
  pivot_wider(names_from = cause, values_from = idp.stock.0to17) |> 
  mutate(Disaster.stock = ifelse(is.na(Disaster), 0, Disaster),
         Conflict.stock = ifelse(is.na(Conflict), 0, Conflict))  |> 
  left_join(places, by = "ISO3Code") |> 
  filter(STATUS != "PT Territory", !(TERR_NAME %in% c("Guernsey","Senkaku Islands","Gaza Strip","Kuril islands")) )|>
  mutate(fact7 = Conflict.stock + Disaster.stock) |> 
  mutate(fact7_color = fact7 > 0)

fact8 <- fact8.idmc.new |>
  select(ISO3Code, cause, idp.new.0to17) |>
  pivot_wider(names_from = cause, values_from = idp.new.0to17) |>
  mutate(Disaster.new = ifelse(is.na(Disaster), 0, Disaster),
         Conflict.new = ifelse(is.na(Conflict), 0, Conflict)) |>
  left_join(places, by = "ISO3Code") |>
  filter(STATUS != "PT Territory", !(TERR_NAME %in% c("Guernsey","Senkaku Islands","Gaza Strip","Kuril islands"))) |>
  mutate(fact8 = Conflict.new + Disaster.new) |>
  mutate(fact8_color = fact8 > 0)

# Territories that don't get their own presence flag directly, and instead copy another
# territory's flag (the same disputed/dependent-territory list used for map coloring in map_settings.R)
presence_flag_matches <- tribble(
  ~TERR_NAME,                   ~match_TERR_NAME,
  "Taiwan province of China",   "China",
  "Arunachal Pradesh",          "India"
)
# Aksai Chin (China/India) and Jammu and Kashmir (India/Pakistan) are disputed territories where
# both sides always have data for facts 7 & 8, so they're marked TRUE directly instead of striped.
presence_flag_direct_true <- c("Aksai Chin", "Jammu and Kashmir")

set_presence_flag_overrides <- function(flag) {
  for (i in seq_len(nrow(presence_flag_matches))) {
    flag[world.robin$TERR_NAME == presence_flag_matches$TERR_NAME[i]] <-
      flag[world.robin$TERR_NAME == presence_flag_matches$match_TERR_NAME[i]]
  }
  flag[world.robin$TERR_NAME %in% presence_flag_direct_true] <- TRUE
  flag
}

#add column to specify if polygons are present in the data, to color them differently
world.robin$fact7_color <- set_presence_flag_overrides(
  world.robin$ISO3_CODE %in% (fact7 |> filter(fact7_color) |> pull(ISO3Code)))
world.robin$fact8_color <- set_presence_flag_overrides(
  world.robin$ISO3_CODE %in% (fact8 |> filter(fact8_color) |> pull(ISO3Code)))

# fact7/fact8-specific settings for make.world.map(), keyed by the ind0 argument
indicator_specs <- list(
  fact7 = list(filter_column = "fact7_color", spec.nms = c("Conflict.stock", "Disaster.stock")),
  fact8 = list(filter_column = "fact8_color", spec.nms = c("Conflict.new", "Disaster.new"))
)

make.world.map <- function(ind0){
  filter_column <- indicator_specs[[ind0]]$filter_column
  spec.nms <- indicator_specs[[ind0]]$spec.nms
  radii_multiply <- 0.003

  #color countries only if they have data (bnd.line/bnd.dash/bnd.dot/bnd.ssd come from map_settings.R)
  world.robin$colorcode <- ifelse(world.robin@data[[filter_column]], "#d3f5ef", "grey97")
  fact7_8 <- get(ind0) |> filter(get(filter_column)) #filtering only data for chosen indicator

  #scale legend specs
  legend.scales <- tibble(scale = c(250000, 500000, 1000000, 2000000),
                          long = rep(x = -12000000, times = 4),
                          lat = seq(from = -4000000, to = -2000000, length.out = 4),
                          scale.label = c("250 K", "500 K", "1 M", "2 M"))

  #Pie specs Data
  color.table <- c(adjustcolor("#ED7D30", alpha.f = 0.7), adjustcolor("#774C9E", alpha.f = 0.7))
  names(color.table) <- spec.nms
  pie.list <- lapply(1:nrow(fact7_8),
                     function(i) as.table(caroline::nv(as.vector(as.matrix(fact7_8[i,spec.nms])),spec.nms))) #list of vectors with datas
  
  #Pie specs legend
  pie.list.legend <- lapply(1:nrow(legend.scales),
                            function(i) as.table(caroline::nv(as.vector(as.matrix(legend.scales[i,"scale"])),"scale"))) #list of vectors with datas
  color.table.legend <- c(adjustcolor("#808080", alpha.f = 0.7))
  names(color.table.legend) <- "scale"
  
  
  # write the map function
  unpd.map <- function(){
    plot(world.robin,border =NA, col=world.robin$colorcode, bg = background.color) # plot country/area polygons
    lines(bnd.line, col = boundary.color, lwd = 0.2, lty = 1) # plot solid boundaries
    lines(bnd.dash, col = boundary.color, lwd = 0.2, lty = 2) # plot dashed boundaries
    lines(bnd.dot, col = boundary.color, lwd = 0.2, lty = 3) # plot dotted boundaries
    lines(bnd.ssd, col = boundary.color, lwd = 0.2, lty = 2) # plot SSD-SDN boundary
    
    lks.grp <- unique(lks.df$group)
    for (gp in lks.grp) {
      lk <- lks.df[lks.df$group == gp,]
      polygon(lk$long,lk$lat, col = background.color, border = boundary.color, lty = 1, lwd = 0.2) # plot lakes as background color
    }
    
    if (plot.coastlines == TRUE) {
      lines(cst, col = coastline.color, lwd = 0.2, lty = 1) # plot coastlines as solid lines
    }
    
    #plotting pie charts in scatterplot
    pies_overplot(x = pie.list, x0 = fact7_8$long, y0 = fact7_8$lat, radii = sqrt(fact7_8 |> pull(ind0)) * radii_multiply,
                  color.table = color.table, lty = 0)
    
    #plotting legend pie charts in scatterplot
    pies_overplot(x = pie.list.legend, x0 = legend.scales$long, y0 = legend.scales$lat, radii = sqrt(legend.scales |> pull("scale")) * radii_multiply,
                  color.table = color.table.legend, lty = 0)
    
    #Legend labels
    text(x = legend.scales$long+1800000, y = legend.scales$lat, labels = legend.scales$scale.label, cex = 0.8, col = adjustcolor("#808080", alpha.f=0.7))
  }
  
  pdf(file = file.path(projectFolder, paste0(ind0, ".pdf")), width = 8, height = 4)
  par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
      las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
  unpd.map()
  dev.off() # close the pdf
}

#run function to build maps
make.world.map(ind0 = "fact7")
make.world.map(ind0 = "fact8")

## fact7 and fact8 xls ----
fact7.xls <- fact7 |>
  select(TERR_NAME, ISO3Code, Disaster, Conflict)

fact8.xls <- fact8 |>
  mutate(Disaster = round(Disaster),
         Conflict = round(Conflict)) |> 
  select(TERR_NAME, ISO3Code, Disaster, Conflict)


# FACT 9: Weather vs Conflict----
conflict.2016.2025 <- idmc.new |> 
  filter(year >= 2016, cause == "Conflict") |>
  group_by(year) |> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = TRUE)) |> 
  mutate(cause = "Conflict and violence")

weather.2016.2025 <- idmc.new.disaster.events |> 
  filter(year >= 2016, hazard_category_name == "Weather related") |>
  group_by(year) |> 
  summarise(idp.new.0to17 = sum(idp_dis_new_0to17, na.rm = TRUE)) |> 
  mutate(cause = "Weather")

new.disp.conf.weat <- bind_rows(conflict.2016.2025, weather.2016.2025) |> 
  mutate(idp.new.0to17 = idp.new.0to17 / 1000000)

View(new.disp.conf.weat |> group_by(cause) |> summarise(idp.new.0to17 = sum(idp.new.0to17)) |> mutate(idp.new.0to17.mill = round(idp.new.0to17)))

g <- ggplot(new.disp.conf.weat, aes(x = factor(year), y = idp.new.0to17, fill = cause, group = cause)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Year") + ylab("New internal displacements\n(in millions)") +
  theme_classic() +
  theme(axis.line = element_line(colour = "grey50", linewidth = 0.4),
        axis.text = element_text(size = 8, color = "grey50"), 
        axis.ticks = element_line(colour = "grey50", linewidth = 0.4), 
        axis.title = element_text(size = 8, color = "grey50"),
        legend.position="bottom", 
        legend.title=element_blank())
print(g)
ggsave(file.path(projectFolder, "fact9.pdf"), width = 12, height = 8, units = "cm")

# FACT 10: UASC NUMBERS ----
#Asylum applicants considered to be unaccompanied minors by citizenship, age and sex - annual data
migr_asyunaa <- get_eurostat("migr_asyunaa", time_format = "num", stringsAsFactors = TRUE)
#write.table(migr_asyunaa, file = file.path(rawdataFolder, "Eurostat/migr_asyunaa.csv"), row.names = F, sep = ",")

#in Europe in 2015, when around 103,000 unaccompanied minors applied for asylum in European countries  
migr_asyunaa_annual <- migr_asyunaa |> filter(age == "TOTAL", sex == "T", geo == "EU27_2020", citizen == "TOTAL") |> group_by(TIME_PERIOD) |> summarise(values = sum(values))
migr_asyunaa_annual_af <- migr_asyunaa |> filter(age == "TOTAL", sex == "T", geo == "EU27_2020", citizen == "AF") |> group_by(TIME_PERIOD) |> summarise(values = sum(values))

# Unaccompanied children
## USA UASC ----
sbo_22_25 <- read.csv(file.path(rawdataFolder, "US CBP/sbo-encounters-fy22-fy25.csv"))) |>
  mutate(Fiscal.Year = as.numeric(Fiscal.Year))

encounters <-  sum(sbo_22_25$Encounter.Count) 
encounters.UASC <-  sbo_22_25 |> filter(Demographic == "UAC" ) |> pull(Encounter.Count) |> sum()
print(paste0("Total encounters 2022 and 2025 Fiscal years: ", addUnits(encounters) ))
print(paste0("UASC encounters 2022 and 2025 Fiscal years: ", addUnits(encounters.UASC) ))
print(paste0("UASC encounters among total: ", round(100*encounters.UASC/encounters), "%. 1 in ", round(1/(encounters.UASC/encounters))))

## Mexico ----
# Source: https://portales.segob.gob.mx/es//PoliticaMigratoria/Boletines_Estadisticos
mex <- readxl::read_xlsx(path = file.path(rawdataFolder, "Mexico/Compilacion_Cuadro_3_1_5_eventos_de_ninos.xlsx"), sheet = "3.1.5 Children") |>
  filter(Year >= 2019)
mex.totalevents <- readxl::read_xlsx(path = file.path(rawdataFolder, "Mexico/Compilacion_Cuadro_3_1_5_eventos_de_ninos.xlsx"), sheet = "3.1 Total Events")|>
  filter(Year >= 2019)

mex.total <- mex.totalevents |> pull(events) |> sum()
mex.children <- mex |> pull(Children) |> sum()
mex.UASC <- mex |> filter(Status == "Unaccompanied") |>  pull(Children) |> sum()
print(paste0("Events of irregular situation in Mexico between 2019 and 2025: ", addUnits(mex.total) ))
print(paste0("Events of children in irregular situation in Mexico between 2019 and 2025: ", addUnits(mex.children) ))
print(paste0("UASC events of irregular situation in Mexico between 2019 and 2025: ", addUnits(mex.UASC) ))
print(paste0("UASC events among total: ", round(100 * mex.UASC/(mex.total)), "%. 1 in ", round(1 / (mex.UASC / mex.total))))
print(paste0("UASC events among children: ", round(100 * mex.UASC/(mex.children)), "%. 1 in ", round(1 / (mex.UASC / mex.children))))

## EU asylum applications by Unaccompanied children ----
EU27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
          "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
          "PL", "PT", "RO", "SE", "SI", "SK")
migr_asyappctza <- read.csv(file.path(rawdataFolder, "Eurostat/migr_asyappctza.csv"))

#Asylum applicants by type, citizenship, age and sex - annual aggregated data
migr_asyappctza <- get_eurostat_json(id = "migr_asyappctza",
  filters = list(
    geo = "EU27_2020",
    sex = "T",
    age = c("TOTAL", "Y_LT18"),
    applicant = "TOTAL",
    citizen = "TOTAL") 
)

#Asylum applicants considered to be unaccompanied minors by citizenship, age and sex - annual data
migr_asyunaa <- get_eurostat_json(id = "migr_asyunaa",
  filters = list(
    geo = "EU27_2020",
    sex = "T",
    age = "TOTAL",
    citizen = "TOTAL") 
)

#sum() does not have na.rm = TRUE because we want to avoid underestimating. There are some cases where  values for EU27_2020 are not reported yet
EU.as <- migr_asyappctza |> filter(time >=2015, age=="TOTAL") |> pull(values) |> sum() 
EU.as.children <- migr_asyappctza |> filter(time >=2015, age=="Y_LT18") |> pull(values) |> sum()
EU.as.UASC <- migr_asyunaa |> filter(time >=2015) |> pull(values) |> sum()

print(paste0("Total asylum applicants in the EU between 2015 and 2024: ", addUnits(EU.as) ))
print(paste0("Children asylum applicants in the EU between 2015 and 2024: ", addUnits(EU.as.children) ))
print(paste0("UASC asylum applicants in the EU between 2015 and 2024: ", addUnits(EU.as.UASC) ))
print(paste0("UASC asylum applicants among all applicants: ", round(100*EU.as.UASC/EU.as), "%. 1 in ", round(1/(EU.as.UASC/EU.as))))
print(paste0("UASC asylum applicants among children: ", round(100*EU.as.UASC/EU.as.children), "%. 1 in ", round(1/(EU.as.UASC/EU.as.children))))

## UK ----
# Source: https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum-applications-decisions-and-resettlement
#Notes: UK considers some people between 18-29 years old as UASC
uk <- readxl::read_xlsx(file.path(rawdataFolder, "UK/asylum-claims-datasets-mar-2026.xlsx"),
                range="Data_Asy_D01!A2:J84977") |> 
  filter(Year %in% seq(2015, 2025,1))

uk_applications <- uk |>  pull(Claims) |> sum()
uk_children_applications <- uk |> filter(Age=="Under 18") |> pull(Claims) |> sum()
uk_UASC_applications <- sum(uk |> filter(Age=="Under 18", UASC=="UASC") |> pull(Claims))

print("-----Children asylum applicants in the UK between 2014 and 2023----")
print(paste0("Total applicants: ", addUnits(uk_applications) ))
print(paste0("Children: ", addUnits(uk_children_applications) ))
print(paste0("UASC: ", addUnits(uk_UASC_applications)))
print(paste0("Percentage UASC among total: ", round(100*(uk_UASC_applications/uk_applications)), "%"))
print(paste0("Percentage UASC among children: ", round(100*(uk_UASC_applications/uk_children_applications)), "%"))


# FACT X: Weather ----
weather.2016.2025.region <- idmc.new.disaster.events |> 
  filter(year >= 2016, hazard_category_name == "Weather related") |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |> 
  group_by(Region) |> 
  summarise(idp.new.0to17 = sum(idp.dis.new.0to17, na.rm = TRUE)) 

drought.2016.2025.region <- idmc.new.disaster.events |> 
  filter(year >= 2016, hazard.type == "Drought") |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |> 
  group_by(Region) |> 
  summarise(idp.new.0to17 = sum(idp.dis.new.0to17, na.rm = TRUE), .groups = "drop") |> 
  mutate(perc = round(100 * idp.new.0to17 / sum(idp.new.0to17)))

storm.2016.2025.region <- idmc.new.disaster.events |>
  filter(year >= 2016, hazard.type == "Storm") |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |>
  group_by(Region) |>
  summarise(idp.new.0to17 = sum(idp.dis.new.0to17, na.rm = TRUE), .groups = "drop") |>
  mutate(perc = round(100 * idp.new.0to17 / sum(idp.new.0to17)))


# EXPORT EXCEL WITH TABLES ----
list.of.sheets = list("fact1" = fact1.xls,
                      "fact2" = fact2.xls,
                      "fact3" = fact3.xls,
                      "fact4" = fact4.xls,
                      "fact5" = fact5.xls,
                      "fact6" = fact6.xls,
                      "fact7" = fact7.xls,
                      "fact8" = fact8.xls)
write.xlsx(list.of.sheets, file = file.path(projectFolder, "9facts_data.xlsx"))