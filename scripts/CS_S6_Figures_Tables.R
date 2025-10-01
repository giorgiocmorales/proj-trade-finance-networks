#CUNTO SANTANA (20XX)

# 6 FIGURES AND TABLES


# X. Datasets tables and graphs --------


# Countries by source ---------------

# 2.11.2 Graph of available countries by year
countries_by_year %>%
  ggplot(aes(x = year, y = n_countries, color = source)) +
  geom_line(linewidth = 1) +
  # facet_wrap(~source, ncol = 4, scales = "fixed") +
  theme_minimal()

# 2.12.1 Graph of available countries by year
common_countries_by_year %>%
  ggplot(aes(x = year, y = n_common)) +
  geom_line(linewidth = 1) +
  theme_minimal()

# Summaries by source ---------------

# Summary by source all datasets (value nominal)
summary_by_source_nom <- dataset_base %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj, na.rm = TRUE),
    Max = max(value_adj, na.rm = TRUE),
    Mean = mean(value_adj, na.rm = TRUE),
    Median = median(value_adj, na.rm = TRUE),
    `Std Dev` = sd(value_adj, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_nom)

# Summary by source all datasets (value cons)
summary_by_source_cons <- dataset_base %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj_cons, na.rm = TRUE),
    Max = max(value_adj_cons, na.rm = TRUE),
    Mean = mean(value_adj_cons, na.rm = TRUE),
    Median = median(value_adj_cons, na.rm = TRUE),
    `Std Dev` = sd(value_adj_cons, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_cons)

# Summary by source all datsets (value nominal)
summary_by_source_nom_fil <- dataset_base_filtered  %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj, na.rm = TRUE),
    Max = max(value_adj, na.rm = TRUE),
    Mean = mean(value_adj, na.rm = TRUE),
    Median = median(value_adj, na.rm = TRUE),
    `Std Dev` = sd(value_adj, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_nom_fil)

# Summary by source (value cons)
summary_by_source_cons_fil <- dataset_base_filtered  %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj_cons, na.rm = TRUE),
    Max = max(value_adj_cons, na.rm = TRUE),
    Mean = mean(value_adj_cons, na.rm = TRUE),
    Median = median(value_adj_cons, na.rm = TRUE),
    `Std Dev` = sd(value_adj_cons, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_cons_fil)


# X Datasets comparison

# Function to align direction based on direction map
align_direction <- function(data, value_column, direction_map) {
  data %>% 
    mutate(weight = .data[[value_column]]) %>%
    mutate(
      invert = direction_map[source],
      from = ifelse(invert, counterpart_iso3, jurisdiction_iso3),
      to   = ifelse(invert, jurisdiction_iso3, counterpart_iso3)
    ) %>%
    select(year, from, to, source, weight)
}

# Function to compare two aligned datasets
compare_sources <- function(data_A, data_B, name_A, name_B) {
  joined <- full_join(data_A, data_B, by = c("year", "from", "to"), suffix = c("_A", "_B"))
  
  summary_stats <- joined %>%
    mutate(match_flag = !is.na(weight_A) & !is.na(weight_B)) %>%
    summarise(
      total_A = sum(!is.na(weight_A)),
      total_B = sum(!is.na(weight_B)),
      matched = sum(match_flag),
      jaccard = matched / n_distinct(c(paste(from[!is.na(weight_A)], to[!is.na(weight_A)], year[!is.na(weight_A)]),
                                       paste(from[!is.na(weight_B)], to[!is.na(weight_B)], year[!is.na(weight_B)]))),
      cor_log = cor(log1p(weight_A[match_flag]), log1p(weight_B[match_flag]), use = "complete.obs"),
      mean_diff_pct = mean(abs(weight_A[match_flag] - weight_B[match_flag]) / pmax(weight_A[match_flag], weight_B[match_flag]) * 100, na.rm = TRUE),
      A_only = sum(!is.na(weight_A) & is.na(weight_B)),
      B_only = sum(!is.na(weight_B) & is.na(weight_A))
    ) %>%
    mutate(source_A = name_A, source_B = name_B) %>%
    select(source_A, source_B, everything())
  
  return(list(summary = summary_stats, raw = joined))
}

# 


# X.  Visualizations and results---------

# Define color palettes for regions and WEO groups
region_color <- c(
  "East Asia & Pacific" = "#D70036",
  "Europe & Central Asia" = "#003A5D",
  "Latin America & Caribbean" = "#157549",
  "Middle East & North Africa" = "#F1BA0D",
  "North America" = "#00ACC8",
  "South Asia" = "#291E58",
  "Sub-Saharan Africa" = "#F26122",
  "Antartica" = "grey"
)

weo_group_color <- c(
  "Advanced Economies" = "#003A5D",
  "Emerging and Developing Asia" = "#D70036",
  "Emerging and Developing Europe" = "#00ACC8",
  "Latin America and the Caribbean" = "#157549",
  "Middle East and Central Asia" = "#F1BA0D",
  "Sub-Saharan Africa" = "#F26122",
  "WEO Not-Monitored" = "grey"
)

source_color <- c(
  "CDIS Outward" = "#d9d9d9",  # Light grey
  "CPIS Assets" = "#bdbdbd",  # Medium light grey
  "LBS Claims" = "#969696"    # Darker light grey
)

# Function to create a custom legend plot
create_custom_legend <- function(colors, title = "Region") {
  legend_data <- data.frame(
    region = names(colors),
    color = colors
  )
  
  p <- ggplot(legend_data, aes(x = region, y = 1, fill = region)) +
    geom_point(shape = 21, size = 5, show.legend = TRUE) +  # Larger dots
    scale_fill_manual(values = colors) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(family = "Georgia", size = 12),
      legend.title = element_blank(),  # Remove the legend title
      legend.key = element_blank(),
      legend.box = "horizontal",
    ) +
    guides(fill = guide_legend(
      title = title, 
      nrow = 2, 
      byrow = TRUE, 
      keyheight = unit(1.5, "lines"),
      keywidth = unit(1.5, "lines"),
      override.aes = list(size = 5)
    ))
  return(p)
}

# Function to create a custom legend plot
create_custom_legend_lines <- function(colors, title = "Region") {
  legend_data <- data.frame(
    category = names(colors),
    color = colors
  )
  
  p <- ggplot(legend_data, aes(x = category, y = 1, color = category)) +
    geom_line(size = 1.2) +  # Larger lines for better visibility
    scale_color_manual(values = colors) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(family = "Georgia", size = 12),
      legend.title = element_blank(),  # Remove the legend title
      legend.key = element_blank(),
      legend.box = "horizontal",
    ) +
    guides(color = guide_legend(
      title = title, 
      byrow = TRUE, 
      keyheight = unit(1.5, "lines"),
      keywidth = unit(1.5, "lines"),
      override.aes = list(size = 5)
    ))
  return(p)
}

legend_region_plot <- create_custom_legend(region_color, title = "Region")
legend_weo_plot <- create_custom_legend(weo_group_color, title = "WEO-Group")
legend_source_plot <- create_custom_legend_lines(source_color, title = "Source")

# X.1 Network Diagram ------
# Function to create network graphs with selectable region variable and dynamic titles/captions
create_network_graph <- function(graph_list, database_name, year, jurisdiction_scores, threshold = 0.9, label_filter = 50, region_variable = "region", seed = 1234, node_positions = NULL) {
  set.seed(seed)
  
  # Construct the graph key
  graph_key <- paste(database_name, year, sep = "_")
  graph <- graph_list[[graph_key]]
  
  # Ensure the graph is an igraph object
  if (!inherits(graph, "igraph")) {
    stop("The retrieved object is not an igraph object")
  }
  
  # Filter connections based on weight
  graph <- delete_edges(graph, which(percent_rank(E(graph)$weight) < threshold))
  
  # Add additional attributes
  V(graph)$rank <- jurisdiction_scores$index_rank_pca[jurisdiction_scores$year == year]
  V(graph)$index <- jurisdiction_scores$index_final_pca[jurisdiction_scores$year == year]
  
  # Add region information based on the selected region variable
  nodes <- country_set %>% 
    rename(vertices = iso3c) %>% 
    select(vertices, country.name.en, region, weo_group_region)
  
  if (region_variable == "region") {
    V(graph)$group <- as.character(nodes$region[match(V(graph)$name, nodes$vertices)])
    node_colors <- region_color
  } else if (region_variable == "weo_group") {
    V(graph)$group <- as.character(nodes$weo_group_region[match(V(graph)$name, nodes$vertices)])
    node_colors <- weo_group_color
  } else {
    stop("Invalid region variable. Choose either 'region' or 'weo_group'.")
  }
  
  V(graph)$color <- V(graph)$group
  
  # Apply the node color palette
  V(graph)$color <- node_colors[V(graph)$color]
  
  # Remove isolated vertices
  isolated <- which(igraph::degree(graph) == 0)
  graph <- delete_vertices(graph, isolated)
  
  # Define node size
  V(graph)$size <- BBmisc::normalize(V(graph)$index, method = "range", range = c(2, 15))^2
  
  # Generate layout if not provided
  if (is.null(node_positions)) {
    layout <- create_layout(graph, layout = "drl")  # Use default layout
    node_positions <- data.frame(name = V(graph)$name, x = layout$x, y = layout$y)
  } else {
    layout <- node_positions
    layout <- layout[match(V(graph)$name, layout$name), ]
  }
  
  # Ensure all nodes have positions
  if (any(is.na(layout$x)) || any(is.na(layout$y))) {
    warning("Some nodes are missing positions; generating layout again.")
    layout <- create_layout(graph, layout = "drl")  # Use default layout
    node_positions <- data.frame(name = V(graph)$name, x = layout$x, y = layout$y)
  }
  
  # Merge layout with graph vertices
  layout <- layout %>%
    mutate(name = as.character(name)) %>%
    left_join(data.frame(name = V(graph)$name, size = V(graph)$size), by = "name")
  
  # Ensure edges have the source attribute
  if (!"source" %in% edge_attr_names(graph)) {
    stop("Edges must have a 'source' attribute.")
  }
  
  # Match and apply edge color based on source
  E(graph)$color <- source_color[E(graph)$source]
  E(graph)$color <- ifelse(is.na(E(graph)$color), "grey", E(graph)$color) # Default color for any unmatched sources
  
  # Generate the graph
  p <- ggraph(graph, layout = as.matrix(layout[, c("x", "y")])) +
    geom_edge_link0(aes(color = factor(source)), edge_width = 0.5) +  # Use varying shades of grey for edges based on source
    scale_edge_color_manual(values = source_color) +
    geom_node_point(aes(x = x, y = y, fill = group, size = size), shape = 21, alpha = 0.8) +  # Slight transparency for nodes
    geom_text(aes(x = x, y = y, label = ifelse(size > label_filter, name, "")), 
              family = "Georgia",
              size = 3, 
              color = "white") +  # Labels for large nodes inside the node
    geom_text_repel(aes(x = x, y = y, label = ifelse(size <= label_filter, name, "")), 
                    family = "Georgia",
                    size = 3, 
                    color = "black",  # Labels for small nodes in black
                    box.padding = 0.35,
                    point.padding = 0.5,
                    segment.color = NA) +  # Position labels for small nodes around the node
    scale_size_continuous(range = c(2, 15), guide = 'none') +  # Exclude size legend
    scale_fill_manual(values = node_colors) +
    theme_graph() +
    theme(legend.position = "none")  # Hide the legend in the main plot
  
  return(list(plot = p, colors = node_colors, edge_colors = source_color))
}

#2009
network_2009 <- create_network_graph(
  graph_list = combined_assets_results$graphs,               
  database_name = "combined_assets",                         
  year = 2009,                                           
  jurisdiction_scores = combined_assets_pca$final_data,             
  threshold = 0.90,                                       
  label_filter = 50,                                      
  region_variable = "region",
  seed = 101
)

combined_plot_2009 <- plot_grid(network_2009$plot, get_legend(legend_region_plot), get_legend(legend_source_plot), ncol = 1, rel_heights = c(4, 0.5, 0.5))
print(combined_plot_2009)

#2022
network_2022 <- create_network_graph(
  graph_list = combined_assets_results$graphs,               
  database_name = "combined_assets",                         
  year = 2022,                                           
  jurisdiction_scores = combined_assets_pca$final_data,             
  threshold = 0.90,                                       
  label_filter = 50,                                      
  region_variable = "region",
  seed = 101
)

combined_plot_2022 <- plot_grid(network_2022$plot, get_legend(legend_region_plot), get_legend(legend_source_plot), ncol = 1, rel_heights = c(4, 0.5, 0.5))
print(combined_plot_2022)


# X.2 Chord Diagrams----
# Chord diagram function
create_chord_graph <- function(graph_list, database_name, year, jurisdiction_scores, top_n = 50, region_variable = "region", ISO = NULL, seed = 101) {
  
  set.seed(seed)
  
  # Construct the graph key
  graph_key <- paste(database_name, year, sep = "_")
  graph <- graph_list[[graph_key]]
  
  # Ensure the graph is an igraph object
  if (!inherits(graph, "igraph")) {
    stop("The retrieved object is not an igraph object")
  }
  
  # Add ranking to the graph
  V(graph)$rank <- jurisdiction_scores$index_rank_pca[jurisdiction_scores$year == year]
  
  # Create data.frames
  top_nodes <- igraph::as_data_frame(graph, what = "vertices") %>% filter(rank <= top_n)
  top_edges <- igraph::as_data_frame(graph, what = "edges") %>% 
    filter(from %in% top_nodes$name, to %in% top_nodes$name) %>% 
    select(from, to, weight) %>%
    mutate(weight = weight / 1e+09)
  
  # Add region information based on the selected region variable
  nodes <- country_set %>% 
    rename(vertices = iso3c) %>% 
    select(vertices, country.name.en, region, weo_group_region)
  
  if (region_variable == "region") {
    group <- structure(top_nodes$region, names = top_nodes$name)
    group_color <- region_color
  } else if (region_variable == "weo_group") {
    group <- structure(top_nodes$weo_group_region, names = top_nodes$name)
    group_color <- weo_group_color
  } else {
    stop("Invalid region variable. Choose either 'region' or 'weo_group'.")
  }
  
  # Group color
  grid_color <- ifelse(group %in% names(group_color), group_color[group], "grey")
  names(grid_color) <- top_nodes$name
  
  if (!is.null(ISO)) {
    if (ISO == "no_color") {
      grid_color[] <- "#00000000"
    } else {
      grid_color[!names(grid_color) %in% ISO] <- "#00000000"
    }
  }
  
  # Chord Diagram Parameters
  circos.par(start.degree = 90)
  
  # Diagram
  chordDiagram(top_edges,
               preAllocateTracks = list(track.height = 0.025, track.margin = c(0.05, 0)),
               group = group,
               order = names(group),
               grid.col = grid_color,
               link.sort = TRUE,
               link.zindex = rank(top_edges$weight),
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               link.arr.type = "big.arrow",
               diffHeight = -uh(0.5, "mm"),
               annotationTrack = "grid")
  
  # Names in sectors
  circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 
                mean(ylim), 
                sector.index, 
                col = "white", 
                cex = 0.4, 
                facing = "clockwise", 
                niceFacing = TRUE)
  }, bg.border = NA)
  
  # Axes
  for (i in get.all.sector.index()) {
    breaks <- seq(0, 100000, by = 5000)
    circos.axis(major.at = breaks,
                labels.cex = 0.4, 
                sector.index = i,
                track.index = 2)
  }
  
  # Track of groups
  for (i in unique(group)) {
    jurisdiction = names(group[group == i])
    highlight.sector(sector.index = jurisdiction, 
                     track.index = 1, 
                     col = group_color[i])
  }
  
  circos.clear()
}

#Network graph structure
# Example call to the chord diagram function
create_chord_graph(graph_list = combined_assets_results$graphs, 
                   database_name = "combined_assets", 
                   year = 2009, 
                   jurisdiction_scores = combined_assets_pca$final_data, 
                   top_n = 50, 
                   region_variable = "region",
                   seed = 101)

create_chord_graph(graph_list = combined_assets_results$graphs, 
                   database_name = "combined_assets", 
                   year = 2022, 
                   jurisdiction_scores = combined_assets_pca$final_data, 
                   top_n = 50, 
                   region_variable = "region",
                   seed = 101)


# Print the combined chord plot
print(as.ggplot(get_legend(legend_region_plot)))
print(as.ggplot(get_legend(legend_weo_plot )))