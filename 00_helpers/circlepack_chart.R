# CIRCLE PACKING CHART ----
# Shared by fact 5 (refugee origin) and fact 6 (refugee asylum)

# regions prefixed with "X" get a translucent variant of their region's color, used to shade the lighter inner sub-circles
circlepack_region_colors <- tribble(
  ~Region,                              ~color_region,
  "Eastern and South-Eastern Asia",    "#EDA877",
  "XEastern and South-Eastern Asia",   "#EDA87770",
  "Central and Southern Asia",         "#8C789E",
  "XCentral and Southern Asia",        "#8C789E70",
  "Latin America and the Caribbean",   "#7FBA70",
  "XLatin America and the Caribbean",  "#7FBA7070",
  "Northern Africa and Western Asia",  "#EAE8FF",
  "XNorthern Africa and Western Asia", "#EAE8FF70",
  "Europe and Northern America",       "#62ABC3",
  "XEurope and Northern America",      "#62ABC370",
  "Oceania",                           "#FEE07F",
  "XOceania",                          "#FEE07F70",
  "Sub-Saharan Africa",                "#E6738E",
  "XSub-Saharan Africa",               "#E6738E70",
  "World",                             "#FFFFFF"
)
circlepack_colors <- setNames(circlepack_region_colors$color_region, circlepack_region_colors$Region)
#the 7 country-level region colors, excluding their translucent "X" variants and "World"
circlepack_world_rows <- circlepack_region_colors$Region[c(1, 3, 5, 7, 9, 11, 13)]
circlepack_x_regions <- c("Sub-Saharan Africa", "Northern Africa and Western Asia", "Central and Southern Asia",
                          "Eastern and South-Eastern Asia", "Europe and Northern America",
                          "Latin America and the Caribbean", "Oceania")

# fact_data needs columns: Region, name_abbr, ref.0to17.estimate
# name_label_overrides: named vector, e.g. c("Central African Republic" = "CAR"), used to shorten/wrap country labels
plot_circlepack <- function(fact_data, name_label_overrides = character(0), out_file, min_pop_label = 150000) {
  df <- tibble(group = fact_data$Region, subgroup = fact_data$name_abbr) |>
    bind_rows(tibble(group = "World", subgroup = circlepack_world_rows)) |>
    table() |> as.data.frame() |>
    filter(Freq > 0)

  #the size of circle should be stored in the vertices object
  vertices <- df |>
    dplyr::distinct(subgroup, Freq) |>
    dplyr::add_row(subgroup = "World", Freq = 0) |>
    left_join(fact_data |> select(name_abbr, ref.0to17.estimate, Region), by = c("subgroup" = "name_abbr"))
  vertices$ref.0to17.estimate[is.na(vertices$ref.0to17.estimate)] <- 0  #fill NA values of regions

  #region category names for special coloring
  vertices$Region[vertices$subgroup %in% circlepack_x_regions] <- paste0("X", vertices$subgroup[vertices$subgroup %in% circlepack_x_regions])
  vertices$Region[vertices$subgroup == "World"] <- "World"

  #selecting which countries to show label
  vertices$name_label <- NA
  is_labeled <- !is.na(vertices$ref.0to17.estimate) & vertices$ref.0to17.estimate > min_pop_label
  vertices$name_label[is_labeled] <- vertices$subgroup[is_labeled]
  override_idx <- match(vertices$name_label, names(name_label_overrides))
  vertices$name_label[!is.na(override_idx)] <- name_label_overrides[override_idx[!is.na(override_idx)]]

  vertices$ref.0to17.estimate[vertices$ref.0to17.estimate == 0] <- 10000 #regional totals must not be 0 for circlepack sizing

  graph <- graph_from_data_frame(df, vertices = vertices)

  p <- ggraph(graph, layout = "circlepack", weight = ref.0to17.estimate) +
    geom_node_circle(aes(fill = Region, colour = as.factor(depth), group = depth),
                     linewidth = 0.01) + #width of circle line
    scale_fill_manual(values = circlepack_colors,
                      breaks = circlepack_world_rows) + #adding breaks so the legend only shows the color of the country circles and not the lighter region circles
    coord_fixed() +
    geom_node_text(aes(label = name_label), color = "black", repel = FALSE, size = 1.5, show.legend = FALSE) +
    scale_color_manual(values = c("0" = "white", "1" = "darkgrey", "2" = "darkgrey")) +
    theme_void() +
    guides(colour = "none") +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 3),
          legend.key.size = unit(0.3, "cm"))

  ggsave(file.path(projectFolder, out_file), plot = p, width = 8, height = 5, units = "cm")
}
