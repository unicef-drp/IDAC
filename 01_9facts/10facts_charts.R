# FACT 1: Gaps ----
gap.refugees.table <- ref.asylum |>
  filter(year == 2025) |>
  mutate(coverage.sex.age.low = coverage.sex.age < 0.5)

gap.refugee.perc <- round(100 * sum(gap.refugees.table$coverage.sex.age.low) / nrow(gap.refugees.table))


# FACT 1: Mig Stock ----

fig1 <- mig.stock.0to17 |>
  filter(sex == "both", year >= 2000) |>
  mutate(pop.mig.adult = pop.mig.total - pop.mig.0to17.eu) |> 
  select(year, pop.mig.adult, pop.mig.0to17.eu) |>
  group_by(year) |>
  summarise_all(sum, na.rm = T) |> 
  rename(`18+ years` = pop.mig.adult, `Under 18 years` = pop.mig.0to17.eu) |> 
  pivot_longer(cols = `18+ years`:`Under 18 years`) |> 
  mutate(value = value / 1000000,
         label = round(value, 0))
fig1$name <- factor(fig1$name, levels = c("18+ years", "Under 18 years"))

# TODO 2026 update: 304021813, 42.5 and the 2020 anchor values are hardcoded estimates from the 2025 update and need to be replaced with real 2025 figures
fig1.dashed <- tribble(~year, ~value, ~name,
                       2020, 245.08349 + 35.51462, NA,
                       2025, 304021813 / 1000000, NA)
fig1.dashed2 <- tribble(~year, ~value, ~name,
                       2020, 35.51462, NA,
                       2025, 42.5, NA)

fact1 <- ggplot(fig1, aes(x = factor(year), y = value, group = name, color = name)) +
  geom_line(stat = "identity", position = "stack", linewidth = 2) +
  geom_line(data = fig1.dashed, stat = "identity", linewidth = 2, show.legend = FALSE, color = "#0092C433") +
  geom_line(data = fig1.dashed2, stat = "identity", linewidth = 2, show.legend = FALSE, color = "#00B40033") +
  geom_text(aes(label = label), position = "stack", size = 3, fontface = "bold", vjust = -1, hjust = "center", show.legend = FALSE)+
  scale_color_manual(values = c("#0092C4", "#00B400"), 
                     breaks = c("18+ years", "Under 18 years"),
                     name = "") +
  scale_x_discrete(breaks = c(2000, 2005, 2010, 2015, 2020, 2025)) +
  xlab("Year") +
  theme_classic() +
  theme(axis.line.x = element_line(linewidth = 0.5, colour = "grey"),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 8))
print(fact1)
ggsave(plot = fact1,width = 6, height = 3,
       filename = file.path(projectFolder, "figure2.pdf"), device = "pdf")

approx.2025 <- mig.stock.0to17 |>
  filter(sex == "both") |>
  select(year, pop.mig.total, pop.mig.0to17.eu) |>
  group_by(year) |>
  summarise_all(sum, na.rm = T) |>
  mutate(pop.mig.0to17.eu.perc = pop.mig.0to17.eu / pop.mig.total,
         approx.2025 =  round(pop.mig.0to17.eu.perc * 304021813 / 1000000)) # TODO 2026 update: replace 304021813 with the real 2025 total migrant stock estimate

## fact1.xls ----
fact1.xls <- fig1 |> 
  mutate(OBS_VALUE = value/1000000) |> 
  select(year, name, OBS_VALUE, label)

# FACT 2: Mig Origin-Dest  ----
orig.dest.region <- mig.stock.dest.orig |>
  filter(Location.dest %in% c("AFRICA", "EUROPE", "LATIN AMERICA AND THE CARIBBEAN", "ASIA", "NORTHERN AMERICA", "OCEANIA")) |>
  filter(Location.orig %in% c("AFRICA", "EUROPE", "LATIN AMERICA AND THE CARIBBEAN", "ASIA", "NORTHERN AMERICA", "OCEANIA")) |>
  filter(year == 2025) |>
  mutate(pop.mig = pop.mig / 1000000) |>
  select(Location.orig, Location.dest, pop.mig)

my_colors <- c("AFRICA" = "#00B520", 
               "EUROPE" = "#0090C2", 
               "LATIN AMERICA AND THE CARIBBEAN" = "#FFC52F",
               "ASIA" = "#EB143C", 
               "NORTHERN AMERICA" = "#79499C",
               "OCEANIA" = "#F1803C")
#grid.col = c("#b21f8c","#29a9e0","#8cc540","#fcda00","#39a443","#e87621", "grey")

pdf(file = file.path(projectFolder, "fact2.pdf"))

circos.clear()
circos.par(start.degree = 90, canvas.ylim = c(-1.1,1.1), gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0.5, 4))

# Base plot
chordDiagram(x = orig.dest.region ,
             grid.col = my_colors,
             transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"),
             diffHeight  = -0.04,
             annotationTrack = "grid",
             annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow",
             link.sort = TRUE,
             link.largest.ontop = TRUE)

circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector.
    circos.text(x = mean(xlim),
                y = 4,
                labels = sector.index,
                facing = "downward",
                niceFacing = T,
                cex = 0.7)
    
    # Add graduation on axis
    circos.axis(h = "top",
                major.at = NULL,
                minor.ticks = 1,
                major.tick.percentage = 0.5,
                labels.niceFacing = TRUE,
                labels.cex = 0.5)
  }
)
dev.off()

region.internal <- function(region){
  internal <- orig.dest.region |> filter(Location.orig == region, Location.dest == region) |> pull(pop.mig) 
  world <- orig.dest.region |> filter(Location.orig == region) |> pull(pop.mig) |> sum()
  perc.internal <- round(100 * internal / world)
  
  print(paste0("---------", region, "---------"))
  print(paste0("Of the total ", round(world), " million migrants from ", region, ", ", perc.internal, "% stayed in the region"))
}

region.internal("ASIA")
region.internal("EUROPE")
region.internal("AFRICA")
region.internal("LATIN AMERICA AND THE CARIBBEAN")
region.internal("NORTHERN AMERICA")
region.internal("OCEANIA")

## fact2.xls ----
fact2.xls <- orig.dest.region

# FACT 4: Forcibly displaced ----
fact4.idmc <- idmc.stock |> 
  filter(cause == "Conflict" & sex == "Both sexes") 

## Correcting Gaza 2024-2025 --------------------------------------------------------------------------------------------------------------
# TODO 2026 update: verify against the new IDMC/UNRWA reports that the 70%/30% split still applies to 2024-2025 before running
#IDMC estimates that in 2024-2025, 70% of IDPs in Gaza are also registered as UNRWA refugees
# To correct for this, we remove these registered refugees from IDMC numbers by conflict
# We only keep 30% of the numbers from PSE in 2024 and 2025.
# Because IDMC uses 3 groups for 0-17 data, we remove 70% from all groups

# See: https://www.unhcr.org/refugee-statistics/insights/explainers/forcibly-displaced-pocs.html (Acessed on 2025-10-13)
# Website saved as PDF in \Migration and Displacement/Forcibly displaced and stateless population categories _ UNHCR.pdf
fact4.idmc[fact4.idmc$year %in% c(2024,2025) & fact4.idmc$ISO3Code == "PSE", 6:12] <- fact4.idmc[fact4.idmc$year %in% c(2024,2025) & fact4.idmc$ISO3Code == "PSE", 6:12] * .3

fact4.idmc <- fact4.idmc |> 
  group_by(year)|> 
  summarise(pop = sum(idp.stock, na.rm = TRUE),
            pop.0to17 = sum(idp.stock.0to17, na.rm = TRUE)) |>
  mutate(pop.type = "Conflict-related\ninternally displaced children") |>
  select(year, pop.type, pop, pop.0to17)

fact4.unrwa <- unrwa |> 
  mutate(year = as.numeric(TIME_PERIOD),
         pop = as.numeric(OBS_VALUE) * (AGE == "_T"),
         pop.0to17 = as.numeric(OBS_VALUE) * (AGE == "Y0T17")) |>
  group_by(year) |>
  summarise(pop = sum(pop),
            pop.0to17 = sum(pop.0to17)) |>
  mutate(pop.type = "Palestine refugee children\n(registered with UNRWA)") |>
  select(year, pop.type, pop, pop.0to17)
#fact4.unrwa$pop.0to17[fact4.unrwa$pop.0to17 == 0] <- NA  #Values are not zero, should be NA

fact4.unhcr <- ref.asylum |>  
  group_by(year) |>
  summarise(pop = sum(ref, na.rm = TRUE),
            pop.0to17 = sum(ref.0to17.estimate, na.rm = TRUE)) |>
  mutate(pop.type = "Refugee and other internationally\ndisplaced children (UNHCR)") |> 
  select(year, pop.type, pop, pop.0to17) 

fact4.AS <- AS.estimate |>
  group_by(year) |> 
  summarise(pop = sum(AS , na.rm = TRUE),
            pop.0to17 = sum(AS.0to17.estimated , na.rm = TRUE)) |> 
  mutate(pop.type = "Asylum-seeking children") |> 
  select(year, pop.type, pop, pop.0to17)

fact4 <- bind_rows(fact4.idmc,
                   fact4.unhcr,
                   fact4.unrwa,
                   fact4.AS)

fact4.yearly.summary <- fact4 |>
  group_by(year) |>
  summarise(pop = sum(pop),
            pop.0to17 = sum(pop.0to17), .groups = "drop") |>
  mutate(pop.0to17.prop = pop.0to17 / pop,
         pop = round(pop / 1000000, digits = 1),
         pop.0to17 = round(pop.0to17 / 1000000, digits = 1))

fact4.2025.summary <- fact4 |>
  filter(year == 2025) |>
  group_by(pop.type) |>
  summarise(pop.0to17 = sum(pop.0to17), .groups = "drop")

fact4.total <- fact4 |>
  filter(year %in% 2010:2025) |>
  group_by(year) |>
  summarise(pop.0to17 = sum(pop.0to17)) |>
  mutate(pop.type = "Total forcibly displaced children")

fact4.2018.plus <- fact4 |>
  filter(year >= 2018) |>
  select(-pop)

fact4_new <- bind_rows(fact4.total, fact4.2018.plus) |>
  mutate(pop.0to17 = pop.0to17 / 1000000,
         pop.label = round(pop.0to17, digits = 1),
         show.bar = if_else(pop.type == "Total forcibly displaced children" & year >= 2018, FALSE, TRUE))

fact4.2025.unhcrunrwa <- fact4 |>
  filter(pop.type %in% c("Refugee and other internationally\ndisplaced children (UNHCR)",
                         "Palestine refugee children\n(registered with UNRWA)",
                         "Asylum-seeking children")) |>
  filter(year == 2025) |>
  summarise(pop = sum(pop),
            pop.0to17 = sum(pop.0to17)) |>
  mutate(perc = round(100 * pop.0to17 / pop))


#order of labels
fact4_new$pop.type <- factor(fact4_new$pop.type, levels = c("Total forcibly displaced children", 
                                                            "Conflict-related\ninternally displaced children",
                                                            "Asylum-seeking children",
                                                            "Refugee and other internationally\ndisplaced children (UNHCR)",
                                                            "Palestine refugee children\n(registered with UNRWA)"))

cols <- c("Total forcibly displaced children" = "#0092C4", 
          "Conflict-related\ninternally displaced children" = "#774C9E",
          "Asylum-seeking children" = "#E60037",
          "Refugee and other internationally\ndisplaced children (UNHCR)" = "#FDC200",
          "Palestine refugee children\n(registered with UNRWA)" = "#39A443")

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
        legend.position="bottom", 
        legend.title=element_blank(), 
        plot.title = element_text(color = "#0092C4"),
        plot.subtitle = element_text(color = "#0092C4"))
print(fact4.fig)
ggsave(filename = file.path(projectFolder, "fact4.pdf"),
       device = "pdf", width = 20, height = 20, units = "cm")

## fact.xls ----
fact4.xls <- fact4_new

# FACT 5: Refugee Origin ----
fact5 <- ref.origin |>
  filter(year == 2025) |>
  filter(!(ISO3Code %in% c("UNK", "XXA", "TIB", "LUX", "PLW"))) |>
  select(ISO3Code, ref.0to17.estimate) |> 
  left_join(geo_areas , by = c("ISO3Code" = "id")) |>
  left_join(regions_sdg |> select(Region, Region_Code, ISO3Code), by = "ISO3Code")

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
region_colors <- tibble(color_region = c("#EDA877","#EDA87770",
                                       "#8C789E","#8C789E70",
                                       "#7FBA70","#7FBA7070",
                                       "#EAE8FF","#EAE8FF70",
                                       "#62ABC3","#62ABC370",
                                       "#FEE07F","#FEE07F70",
                                       "#E6738E","#E6738E70",
                                       "#FFFFFF"),
                        Region = c("Eastern and South-Eastern Asia",
                                    "XEastern and South-Eastern Asia",
                                    "Central and Southern Asia",
                                    "XCentral and Southern Asia",
                                    "Latin America and the Caribbean",
                                    "XLatin America and the Caribbean",
                                    "Northern Africa and Western Asia",
                                    "XNorthern Africa and Western Asia",
                                    "Europe and Northern America",
                                    "XEurope and Northern America",
                                    "Oceania",
                                    "XOceania",
                                    "Sub-Saharan Africa",
                                    "XSub-Saharan Africa",
                                    "World"))

my_colors2 <- region_colors$color_region
names(my_colors2) <- region_colors$Region

df <- tibble(group = fact5$Region,
                 subgroup = fact5$name_abbr)
#adding rows of regional levels
df <- bind_rows(df, tibble(group = rep("World", length(region_colors$Region[c(1, 3, 5, 7, 9,11, 13)])),
                           subgroup = region_colors$Region[c(1, 3, 5, 7, 9, 11, 13)]))

df <- as.data.frame(table(df))
df <- filter(df, Freq > 0)

#the size of circle should be stored in the vertices object
vertices <- df |>
  dplyr::distinct(subgroup, Freq) |>
  dplyr::add_row(subgroup = "World", Freq = 0)|> 
  left_join(fact5 |> select(name_abbr, ref.0to17.estimate, Region), by=c("subgroup" = "name_abbr"))

vertices$ref.0to17.estimate[is.na(vertices$ref.0to17.estimate)] <- 0  #fill NA values of regions

#region category names for special coloring
vertices$Region[vertices$subgroup=="Sub-Saharan Africa"] <- "XSub-Saharan Africa"
vertices$Region[vertices$subgroup=="Northern Africa and Western Asia"] <- "XNorthern Africa and Western Asia"
vertices$Region[vertices$subgroup=="Central and Southern Asia"] <- "XCentral and Southern Asia"
vertices$Region[vertices$subgroup=="Eastern and South-Eastern Asia"] <- "XEastern and South-Eastern Asia"
vertices$Region[vertices$subgroup=="Europe and Northern America"] <- "XEurope and Northern America"
vertices$Region[vertices$subgroup=="Latin America and the Caribbean"] <- "XLatin America and the Caribbean"
vertices$Region[vertices$subgroup=="Oceania"] <- "XOceania"
vertices$Region[vertices$subgroup=="World"] <- "World"

#selecting which countries to show label
min.pop.label <- 150000
vertices$name_label <- NA
vertices$name_label[!is.na(vertices$ref.0to17.estimate) & vertices$ref.0to17.estimate>min.pop.label] <- vertices$subgroup[!is.na(vertices$ref.0to17.estimate) & vertices$ref.0to17.estimate>min.pop.label]
vertices$name_label[vertices$name_label == "Central African Republic"] <- "CAR"
vertices$name_label[vertices$name_label == "Venezuela (Bolivarian Republic of)"] <- "Venezuela\n(Bol. Rep. of)"
vertices$name_label[vertices$name_label == "Syrian Arab Republic"] <- "Syrian\nArab\nRep."
vertices$name_label[vertices$name_label == "Democratic Republic of the Congo"] <- "DRC"
vertices$name_label[vertices$name_label == "South Sudan"] <- "South\nSudan"

#region labels
vertices$region_label <- NA
vertices$region_label[vertices$ref.0to17.estimate == 0] <- vertices$subgroup[vertices$ref.0to17.estimate == 0]
vertices$region_label[vertices$region_label == "World"] <- NA
vertices$ref.0to17.estimate[vertices$ref.0to17.estimate == 0] <- 10000 #regional numbers must not be 0. Strange that this is not a problem for asylum

graph <- graph_from_data_frame(df, vertices = vertices)

ggraph(graph, layout = "circlepack", weight = ref.0to17.estimate) +
  geom_node_circle(aes(fill = Region, colour = as.factor(depth), group = depth), 
                   linewidth = 0.01) + #width of circle line
  scale_fill_manual(values = my_colors2, 
                    breaks = region_colors$Region[c(1,3,5,7,9,11,13)]) + #adding breaks so the legend only shows the color of the country circles and not the lighter region circles
  coord_fixed() +
  geom_node_text(aes(label = name_label), color = "black", repel = FALSE, size = 1.5, show.legend = F) +
  #geom_node_label(aes(label = region_label), color = "black", repel = TRUE, size = 2, show.legend = F) +
  scale_color_manual(values = c("0" = "white", "1" = "darkgrey", "2" = "darkgrey" ) ) +
  theme_void()+
  guides(colour="none")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 3),
        legend.key.size = unit(0.3, "cm"))
ggsave(file.path(projectFolder, "fact5_origin.pdf"), width = 8, height = 5, units = "cm")

## fact5.xls ----
fact5.xls <- fact5 |>
  mutate(OBS_VALUE = round(ref.0to17.estimate)) |> 
  select(name, Region, OBS_VALUE)

# FACT 6: Refugee Asylum ----
fact6 <- ref.asylum |>
  filter(year == 2025) |>
  filter(!(ISO3Code %in% c("UNK", "XXA", "TIB", "LUX", "PLW")) ) |> 
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

## Chart ----
region_colors <- tibble(color_region = c("#EDA877","#EDA87770",
                                         "#8C789E","#8C789E70",
                                         "#7FBA70","#7FBA7070",
                                         "#EAE8FF","#EAE8FF70",
                                         "#62ABC3","#62ABC370",
                                         "#FEE07F","#FEE07F70",
                                         "#E6738E","#E6738E70",
                                         "#FFFFFF"),
                        Region = c("Eastern and South-Eastern Asia",
                                   "XEastern and South-Eastern Asia",
                                   "Central and Southern Asia",
                                   "XCentral and Southern Asia",
                                   "Latin America and the Caribbean",
                                   "XLatin America and the Caribbean",
                                   "Northern Africa and Western Asia",
                                   "XNorthern Africa and Western Asia",
                                   "Europe and Northern America",
                                   "XEurope and Northern America",
                                   "Oceania",
                                   "XOceania",
                                   "Sub-Saharan Africa",
                                   "XSub-Saharan Africa",
                                   "World"))

my_colors2 <- region_colors$color_region
names(my_colors2) <- region_colors$Region

df <- tibble(group = fact6$Region,
             subgroup = fact6$name_abbr)
#adding rows of regional levels
df <- bind_rows(df, tibble(group = rep("World", length(region_colors$Region[c(1, 3, 5, 7, 9,11, 13)])),
                           subgroup = region_colors$Region[c(1, 3, 5, 7, 9, 11, 13)]))

df <- as.data.frame(table(df))
df <- filter(df, Freq > 0)

#the size of circle should be stored in the vertices object
vertices <- df |>
  dplyr::distinct(subgroup, Freq) |>
  dplyr::add_row(subgroup = "World", Freq = 0)|> 
  left_join(fact6 |> select(name_abbr, ref.0to17.estimate, Region), by=c("subgroup" = "name_abbr"))

vertices$ref.0to17.estimate[is.na(vertices$ref.0to17.estimate)] <- 0  #fill NA values of regions

#region category names for special coloring
vertices$Region[vertices$subgroup=="Sub-Saharan Africa"] <- "XSub-Saharan Africa"
vertices$Region[vertices$subgroup=="Northern Africa and Western Asia"] <- "XNorthern Africa and Western Asia"
vertices$Region[vertices$subgroup=="Central and Southern Asia"] <- "XCentral and Southern Asia"
vertices$Region[vertices$subgroup=="Eastern and South-Eastern Asia"] <- "XEastern and South-Eastern Asia"
vertices$Region[vertices$subgroup=="Europe and Northern America"] <- "XEurope and Northern America"
vertices$Region[vertices$subgroup=="Latin America and the Caribbean"] <- "XLatin America and the Caribbean"
vertices$Region[vertices$subgroup=="Oceania"] <- "XOceania"
vertices$Region[vertices$subgroup=="World"] <- "World"

#selecting which countries to show label
min.pop.label <- 150000
vertices$name_label <- NA
vertices$name_label[!is.na(vertices$ref.0to17.estimate) & vertices$ref.0to17.estimate>min.pop.label] <- vertices$subgroup[!is.na(vertices$ref.0to17.estimate) & vertices$ref.0to17.estimate>min.pop.label]
vertices$name_label[vertices$name_label == "Democratic Republic of the Congo"] <- "DRC"
vertices$name_label[vertices$name_label == "Iran (Islamic Republic of)"] <- "Iran (Islamic\nRepublic of)"
vertices$name_label[vertices$name_label == "United Kingdom"] <- "United\nKingdom"
vertices$name_label[vertices$name_label == "South Sudan"] <- "South\nSudan"
# vertices$name_label[vertices$name_label == "Syrian Arab Republic"] <- "Syrian\nArab\nRep."

#region labels
vertices$region_label <- NA
vertices$region_label[vertices$ref.0to17.estimate == 0] <- vertices$subgroup[vertices$ref.0to17.estimate == 0]
vertices$region_label[vertices$region_label == "World"] <- NA
vertices$ref.0to17.estimate[vertices$ref.0to17.estimate == 0] <- 10000 #regional numbers must not be 0. Strange that this is not a problem for asylum

graph <- graph_from_data_frame(df, vertices = vertices)

ggraph(graph, layout = "circlepack", weight = ref.0to17.estimate) +
  geom_node_circle(aes(fill = Region, colour = as.factor(depth), group = depth), 
                   linewidth = 0.01) + #width of circle line
  scale_fill_manual(values = my_colors2, 
                    breaks = region_colors$Region[c(1,3,5,7,9,11,13)]) + #adding breaks so the legend only shows the color of the country circles and not the lighter region circles
  coord_fixed() +
  geom_node_text(aes(label = name_label), color = "black", repel = FALSE, size = 1.5, show.legend = F) +
  #geom_node_label(aes(label = region_label), color = "black", repel = TRUE, size = 2, show.legend = F) +
  scale_color_manual(values = c("0" = "white", "1" = "darkgrey", "2" = "darkgrey" ) ) +
  theme_void()+
  guides(colour="none")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 3),
        legend.key.size = unit(0.3, "cm"))
ggsave(file.path(projectFolder, "fact6_asylum.pdf"), width = 8, height = 5, units = "cm")

## fact6.xls ----
fact6.xls <- fact6 |>
  mutate(OBS_VALUE = round(ref.0to17.estimate)) |> 
  select(name, Region, OBS_VALUE)

# FACT 3: MAP IMS ----
data3 <- mig.stock.0to17 |> filter(year == 2020, sex == "both") #This results in one value per iso3

#preparing circles layer
#this is a little tricky... The ID of the polygons of world.robin are the territory names, not iso3
#This means that some values of iso3 are duplicated. For example: PRT (Portugal and Azores), PSE (West Bank, Gaza)
#This means that we need to choose which polygon to use for each iso3
#to avoid removing polygons that are in the mig.stock data, we first add the migstock values to the table and then check for duplicates, 
world.robin@data <- world.robin@data |> left_join(data3 |> select(iso3, pop.mig.0to17), by=c("ISO3_CODE"="iso3"))

data3$iso3[!data3$iso3 %in% world.robin$ISO3_CODE]

#mig.stock values for "CHI" (Channel islands) "BLM" (Saint Barthélemy) "MAF" (Saint Martin (French part)) are not joined because these iso3 codes are not in world.robin
# We will plot CHI in Jersey
#BLM and MAF do not have miigration population
world.robin$pop.mig.0to17[world.robin$TERR_NAME == "Jersey"] <- data3$pop.mig.0to17[data3$area == "Channel Islands"]

#removing rows without values
world.robin.with.pop.mig.data <- world.robin[!is.na(world.robin$pop.mig.0to17),]

#removing duplicates (some territories have the same iso3, so they have duplicated mig data)
#manually choosing which to eliminate because it is complicated to make some rules to eliminate
#
world.robin.with.pop.mig.data <- world.robin.with.pop.mig.data[world.robin.with.pop.mig.data$pop.mig.0to17 > 0, ] 
world.robin.with.pop.mig.data <- world.robin.with.pop.mig.data[!world.robin.with.pop.mig.data$TERR_NAME %in% 
                                                                 c("Guernsey",
                                                                   "Kuril islands",
                                                                   "Senkaku Islands",
                                                                   "Madeira Islands",
                                                                   "Azores Islands",
                                                                   "West Bank" #Althoug mig population is probably larger in WB, migstock for PSE is plotted in in Gaza because there are two WB polygons and it is easier to eliminate these.  
                                                                 ),] 
world.robin.with.pop.mig.data <- world.robin.with.pop.mig.data[!world.robin.with.pop.mig.data$CAPITAL %in% 
                                                                 c("Klien"),] 
#to check if there are duplicated numbers, which most likely means that there are duplicated mig.stock values
#any(duplicated(world.robin.with.pop.mig.data$pop.mig.0to17))

#preparing table with centroids and data
pop.mig.in.centroids <- coordinates(world.robin.with.pop.mig.data) 
pop.mig.in.centroids <- tibble(x=pop.mig.in.centroids[,1], y=pop.mig.in.centroids[,2], pop.mig.0to17 = world.robin.with.pop.mig.data$pop.mig.0to17)

# map function
unpd.map <- function(){
  plot(world.robin, border = NA,col = world.robin$color, bg = background.color) # plot country/area polygons
  #points(x=fact7_8$long, y=fact7_8$lat) #to double check that points are inside plotting area
  polygon(acf$long, acf$lat,col = acf$color[1], border = NA, density = 130, angle = 45, lwd = 0.4) # plot Aksai Chin as striped region per UN Cartography requirements
  
  lines(bnd.line, col = boundary.color, lwd = 0.2, lty = 1) # plot solid boundaries
  lines(bnd.dash, col = boundary.color, lwd = 0.2, lty = 2) # plot dashed boundaries
  lines(bnd.dot, col = boundary.color, lwd = 0.2, lty = 3) # plot dotted boundaries
  lines(bnd.ssd, col = boundary.color, lwd = 0.2, lty = 2) # plot SSD-SDN boundary
  
  lks.grp <- unique(lks.df$group)
  for (gp in lks.grp) {
    lk <- lks.df[lks.df$group == gp,]
    polygon(lk$long, lk$lat, col = background.color, border = NA, lty = 1, lwd = 0.2) # plot lakes as background color
  }
  
  #if (plot.coastlines==TRUE) {
  #lines(cst, col=coastline.color, lwd=0.2, lty=1) # plot coastlines as solid lines
  #}
  
  for (i in 1:nrow(pop.mig.in.centroids))
  {
    draw.circle(x = pop.mig.in.centroids$x[i], 
                y = pop.mig.in.centroids$y[i],
                radius = 0.3 * pop.mig.in.centroids$pop.mig.0to17[i],
                border = "#52525290", col = "#52525280")}
}

#unpd.map() #to plot to Rstudio

#png(file = "output/fact3.png", width = 8, height = 4, units = "in", res = 200)
pdf(file = "output/fact3.pdf", width = 8, height = 4)
par(mfrow = c(1,1), omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), mgp = c(2, 0.5, 0), 
    las = 0, mex = 1, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)
unpd.map()
dev.off() # close the pdf

## fact3.xls ----
fact3.xls <- data3 |>
  mutate(OBS_VALUE = round(pop.mig.0to17.eu)) |> 
  select(area, OBS_VALUE)

# FACT 7 and 8 : MAP IDP and IDP NEW----
## Fact 7 summary----
fact7.idmc.stock <- idmc.stock |>
  filter(year == 2025) |>
  filter(sex == "Both sexes") |> 
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code")

idmc.stock.summary <-  fact7.idmc.stock |> 
  group_by(cause, year)|> 
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = T), .groups = 'drop') |> 
  mutate(perc = round(100 * pop.0to17 / sum(pop.0to17), digits = 1),
         pop.0to17.millions = round(pop.0to17 / 1000000, digits = 1))
View(idmc.stock.summary)

idmc.stock.summary <-  fact7.idmc.stock |> 
  group_by(cause, year)|> 
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = T), .groups = 'drop')

idmc.stock.summary.region <-  fact7.idmc.stock |> 
  filter(cause == "Conflict") |> 
  group_by(Region)|> 
  summarise(pop.0to17 = sum(idp.stock.0to17, na.rm = T), .groups = 'drop') |> 
  mutate(perc = round(100 * pop.0to17 / sum(pop.0to17), digits = 1),
         pop.0to17.millions = round(pop.0to17 / 1000000, digits = 1))
View(idmc.stock.summary.region)

idmc.stock.2025 <-  idmc.stock |>
  filter(year == 2025, sex == "Both sexes") |>
  summarise(pop = sum(idp.stock, na.rm = T),
            pop.0to17 = sum(idp.stock.0to17, na.rm = T), .groups = 'drop') |>
  mutate(perc = round(100 * pop.0to17 / pop, digits = 1))
idmc.stock.2025

## Fact 8 summary----
fact8.idmc.new <- idmc.new |>
  filter(year == 2025) |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") 

idmc.new.summary <- fact8.idmc.new |> 
  group_by(cause) |> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = T), .groups = 'drop') |> 
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))
View(idmc.new.summary)

idmc.new.summary.region.conf <- fact8.idmc.new|> 
  filter(cause == "Conflict") |> 
  group_by(Region)|> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = T), .groups = 'drop') |> 
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))
View(idmc.new.summary.region.conf)

idmc.new.summary.region.dis <- fact8.idmc.new|> 
  filter(cause == "Disaster") |> 
  group_by(Region)|> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = T),
            .groups = 'drop') |> 
  mutate(idp.new.0to17.millions = round(idp.new.0to17 / 1000000, digits = 1))
View(idmc.new.summary.region.dis)

## Fact Weather vs Conflict----
conflict.2016.2025 <- idmc.new |> 
  filter(year >= 2016, cause == "Conflict") |>
  group_by(year) |> 
  summarise(idp.new.0to17 = sum(idp.new.0to17, na.rm = T)) |> 
  mutate(cause = "Conflict and violence")

weather.2016.2025 <- idmc.new.disaster.events |> 
  filter(year >= 2016, hazard.cat == "Weather related") |>
  group_by(year) |> 
  summarise(idp.new.0to17 = sum(idp.dis.new.0to17, na.rm = T)) |> 
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
ggsave(file.path(projectFolder, "figure9.pdf"), width = 10, height = 7, units = "cm")

## Fact Weather ----
weather.2016.2025.region <- idmc.new.disaster.events |> 
  filter(year >= 2016, hazard.cat == "Weather related") |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |> 
  group_by(Region) |> 
  summarise(idp.new.0to17 = sum(idp.dis.new.0to17, na.rm = T)) 

drought.2016.2025.region <- idmc.new.disaster.events |> 
  filter(year >= 2016, hazard.type == "Drought") |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |> 
  group_by(Region) |> 
  summarise(idp.new.0to17 = sum(idp.dis.new.0to17, na.rm = T), .groups = "drop") |> 
  mutate(perc = round(100 * idp.new.0to17 / sum(idp.new.0to17)))

storm.2016.2025.region <- idmc.new.disaster.events |> 
  filter(year >= 2016, hazard.type == "Storm") |>
  left_join(regions_sdg |> select(ISO3Code, Region), by = "ISO3Code") |> 
  group_by(Region) |> 
  summarise(idp.new.0to17 = sum(idp.dis.new.0to17, na.rm = T), .groups = "drop") |> 
  mutate(perc = round(100 * idp.new.0to17 / sum(idp.new.0to17)))


## Prepare IDMC data for pie charts----
places <- data.frame(matrix(ncol = 5, nrow = length(world.robin)))
colnames(places) <- c("ISO3Code", "TERR_NAME", "long", "lat", "STATUS")

places$ISO3Code <- as.character(world.robin$ISO3_CODE)
places$TERR_NAME <- world.robin$TERR_NAME
places$long <- coordinates(world.robin)[, 1]
places$lat <- coordinates(world.robin)[, 2]
places$STATUS <- world.robin$STATUS
places$ISO3Code[places$TERR_NAME == 'Abyei'] <- 'AB9'

fact7 <- fact7.idmc.stock |>  
  select(ISO3Code, cause, idp.stock.0to17) |> 
  pivot_wider(names_from = cause, values_from = idp.stock.0to17) |> 
  mutate(Disaster.stock = ifelse(is.na(Disaster), 0, Disaster),
         Conflict.stock = ifelse(is.na(Conflict), 0, Conflict))  |> 
  left_join(places, by = "ISO3Code") |> 
  filter(STATUS != 'PT Territory', !(TERR_NAME %in% c('Guernsey','Senkaku Islands','Gaza Strip','Kuril islands')) )|> 
  mutate(fact7 = Conflict.stock + Disaster.stock) |> 
  mutate(fact8_color = fact7 > 0)

fact8 <- fact8.idmc.new |>  
  select(ISO3Code, cause, idp.new.0to17) |> 
  pivot_wider(names_from = cause, values_from = idp.new.0to17) |> 
  mutate(Disaster.new = ifelse(is.na(Disaster), 0, Disaster),
         Conflict.new = ifelse(is.na(Conflict), 0, Conflict))  |> 
  left_join(places, by = "ISO3Code") |> 
  filter(STATUS != 'PT Territory', !(TERR_NAME %in% c('Guernsey','Senkaku Islands','Gaza Strip','Kuril islands')) )|> 
  mutate(fact8 = Conflict.new + Disaster.new) |> 
  mutate(fact8_color = fact8 > 0)


#add column to specify if polygons are present in the data, to color them differently
world.robin$fact7_color <- world.robin$ISO3_CODE %in% (fact7 |> filter(fact7_color) |> pull(ISO3Code))
world.robin$fact7_color[world.robin$TERR_NAME == "Taiwan province of China"] <- world.robin$fact7_color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$fact7_color[world.robin$TERR_NAME == "Aksai Chin"] <- TRUE #China and India are colored, so no need for stripes
world.robin$fact7_color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$fact7_color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$fact7_color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- TRUE # #Pakistan and India are colored, so no need for stripes

world.robin$fact8_color <- world.robin$ISO3_CODE %in% (fact8 |> filter(fact8_color) |> pull(ISO3Code))
world.robin$fact8_color[world.robin$TERR_NAME == "Taiwan province of China"] <- world.robin$fact8_color[world.robin$TERR_NAME == "China"]   #color Taiwan the same as China
world.robin$fact8_color[world.robin$TERR_NAME == "Aksai Chin"] <- TRUE #China and India are colored, so no need for stripes
world.robin$fact8_color[world.robin$TERR_NAME == "Arunachal Pradesh"] <- world.robin$fact8_color[world.robin$TERR_NAME == "India"] #Same color as India
world.robin$fact8_color[world.robin$TERR_NAME == "Jammu and Kashmir"] <- TRUE # #Pakistan and India are colored, so no need for stripes

make.world.map <- function(ind0){
  library(mapplots) # No used functions found
  library(reshape2) # No used functions found
  
  #color countries only if they have data
  world.robin$colorcode <- NA
  # UN Cartography requires that the Askai Chin region be striped half in the color of China and half in the color of
  # Jammu-Kashmir (no data for most of UNPD purposes)
  # to do that, we create a separate polygon file that contains only the single region Aksai Chin and assign it the color of China for now
  # it will be layered on top of the map in a separate step
  #ac <- world.robin[world.robin$TERR_NAME=="Aksai Chin",]
  #acf <- fortify(ac) # transform to data frame for more plotting options
  #acf$colorcode <- world.robin$colorcode[which(world.robin$TERR_NAME=="China")]
  
  # three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
  # to do that, we create a separate dataframe with each containing boundaries of the same type
  bnd.line <- bnd[bnd$CARTOGRAPH == "International boundary line",]
  bnd.dash <- bnd[bnd$CARTOGRAPH == "Dashed boundary line" | bnd$CARTOGRAPH == "Undetermined international dashed boundary line",]
  bnd.dot <- bnd[bnd$CARTOGRAPH == "Dotted boundary line" | bnd$CARTOGRAPH == "Dotted boundary line (Abyei)",]
  bnd.ssd <- bnd[bnd$BDY_CNT01 == "SDN" & bnd$BDY_CNT02 == "SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 
  
  #data of the indicator
  #ind0 <- 'fact7'
  if(ind0 == "fact7"){
    filter_column <- "fact7_color"
    spec.nms <- c("Conflict.stock", "Disaster.stock")
    
    radii_multiply <- 0.003
    world.robin$colorcode[world.robin$fact7_color] <- "#d3f5ef"
    world.robin$colorcode[!world.robin$fact7_color] <- "grey97"
    fact7_8 <-  fact6 |> filter(get(filter_column)) #filtering only data for chosen indicator
    
    #scale legend specs
    legend.scales <- tibble(scale = c(250000, 500000, 1000000, 2000000),
                            long = rep(x = -12000000, times = 4),
                            lat = seq(from = -4000000, to = -2000000, length.out = 4),
                            scale.label = c("250 K", "500 K", "1 M", "2 M"))
  } else {
    filter_column <- "fact8_color"
    spec.nms <- c("Conflict.new", "Disaster.new")
    radii_multiply <- 0.003
    world.robin$colorcode[world.robin$fact8_color] <- "#d3f5ef"
    world.robin$colorcode[!world.robin$fact8_color] <- "grey97"
    
    fact7_8 <-  fact8 |> filter(get(filter_column)) #filtering only data for chosen indicator
    
    #scale legend specs
    legend.scales <- tibble(scale = c(250000, 500000, 1000000, 2000000),
                            long = rep(x = -12000000, times=4),
                            lat = seq(from = -4000000, to = -2000000, length.out = 4),
                            scale.label=c("250 K", "500 K", "1 M", "2 M"))
  }
  
  #Pie specs Data
  color.table <- c(adjustcolor("#ED7D30", alpha.f = 0.7), adjustcolor("#774C9E", alpha.f = 0.7))
  names(color.table) <- spec.nms
  pie.list <- lapply(1:nrow(fact7_8),
                     function(i) as.table(nv(as.vector(as.matrix(fact7_8[i,spec.nms])),spec.nms))) #list of vectors with datas
  
  #Pie specs legend
  pie.list.legend <- lapply(1:nrow(legend.scales),
                            function(i) as.table(nv(as.vector(as.matrix(legend.scales[i,"scale"])),"scale"))) #list of vectors with datas
  color.table.legend <- c(adjustcolor("#808080", alpha.f = 0.7))
  names(color.table.legend) <- "scale"
  
  
  # write the map function
  unpd.map <- function(){
    plot(world.robin,border =NA, col=world.robin$colorcode, bg = background.color) # plot country/area polygons
    #points(x=fact7_8$long, y=fact7_8$lat) #to double check that points are inside plotting area
    #polygon(acf$long,acf$lat,col=acf$colorcode[1],border=NA, density=130,angle=45,lwd=0.4) # plot Aksai Chin as striped region per UN Cartography requirements
    
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
    pies_overplot(x = pie.list.legend, x0 = legend.scales$long, y0 = legend.scales$lat, radii = sqrt(legend.scales |> pull('scale')) * radii_multiply,
                  color.table = color.table.legend, lty = 0)
    
    #Legend labels
    text(x = legend.scales$long+1800000, y = legend.scales$lat, labels = legend.scales$scale.label, cex = 0.8, col = adjustcolor("#808080", alpha.f=0.7))
  }
  
  #png(file = paste0("output/", ind0, ".png"), width = 8, height = 4, units = "in", res = 200)
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

# FACT 9: UASC NUMBERS ----
#Asylum applicants considered to be unaccompanied minors by citizenship, age and sex - annual data
migr_asyunaa <- get_eurostat("migr_asyunaa", time_format = "num", stringsAsFactors = TRUE)
#write.table(migr_asyunaa, file = file.path(rawdataFolder, "Eurostat/migr_asyunaa.csv"), row.names = F, sep = ",")

#in Europe in 2015, when around 103,000 unaccompanied minors applied for asylum in European countries  
migr_asyunaa_annual <- migr_asyunaa |> filter(age == "TOTAL", sex == "T", geo == "EU27_2020", citizen == "TOTAL") |> group_by(TIME_PERIOD) |> summarise(values = sum(values))
migr_asyunaa_annual_af <- migr_asyunaa |> filter(age == "TOTAL", sex == "T", geo == "EU27_2020", citizen == "AF") |> group_by(TIME_PERIOD) |> summarise(values = sum(values))


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

list.of.sheets = list("fact1" = fact4.xls,
                      "fact2" = fact5.xls,
                      "fact3" = fact6.xls,
                      "fact4" = fact7.xls,
                      "fact5" = fact8.xls,
                      "fact6" = new.disp.conf.weat)
write.xlsx(list.of.sheets, file = file.path(projectFolder, "key_facts_data.xlsx"))