#CUNTO SANTANA (20XX)

# 3 NETWORK ANALYSIS---

## 3.1 Network and Jurisdiction Measures-------

# Function to process datasets with an option for inflation adjustment
process_network <- function(data, 
                            country_set, 
                            dataset_name, 
                            value_column = "value_adj_cons", 
                            source_filter = NULL,
                            direction_map,
                            handle_obstatus = TRUE) {
  
  epsilon <- 1e-10
  
  # Optional filtering by source
  if (!is.null(source_filter)) {
    data <- data %>% filter(source %in% source_filter)
  }
  
  # Output containers
  graph_list <- list()
  network_scores <- tibble()
  jurisdiction_scores <- tibble()
  
  for (i in unique(data$year)) {
    
    # Subset and assign weights
    year_data <- data %>%
      filter(year == i) %>%
      mutate(weight = .data[[value_column]])
    
    # Handle confidential observations
    if (handle_obstatus) {
      year_data <- year_data %>%
        mutate(weight = ifelse(obs_status == "C" & is.na(weight), epsilon, weight))
    } else {
      year_data <- year_data %>% filter(!is.nan(weight))
    }
    
    # Final NA cleanup
    year_data <- year_data %>% filter(!is.na(weight) & !is.nan(weight))
    
    # Assign direction per source
    year_data <- year_data %>%
      mutate(
        invert = as.logical(direction_map[source]),
        from = ifelse(invert, counterpart_iso3, jurisdiction_iso3),
        to   = ifelse(invert, jurisdiction_iso3, counterpart_iso3)
      )
    
    # Construct edges
    edges <- year_data %>% select(from, to, weight, source)
    
    # Construct nodes
    nodes <- country_set %>%
      rename(vertices = iso3c) %>%
      select(vertices, country.name.en, region, weo_group_region)
    
    # Create graph
    graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    graph_key <- paste(dataset_name, as.character(i), sep = "_")
    graph_list[[graph_key]] <- graph
    
    # Graph attributes
    graph <- graph %>%
      set_graph_attr("year", i) %>%
      set_graph_attr("dataset", dataset_name) %>%
      set_edge_attr("n_weight", value = E(graph)$weight / max(E(graph)$weight, na.rm = TRUE)) %>%
      set_edge_attr("dist_weight", value = 1 / abs(E(graph)$weight)) %>%
      set_vertex_attr("n_region", value = as.integer(factor(V(graph)$region, levels = unique(nodes$region)))) %>%
      set_vertex_attr("n_weo_group_region", value = as.integer(factor(V(graph)$weo_group_region, levels = unique(nodes$weo_group_region)))) %>%
      delete_vertices(names(which(igraph::degree(graph) == 0)))
    
    E(graph)$weight[is.na(E(graph)$weight)] <- epsilon
    E(graph)$dist_weight[is.na(E(graph)$dist_weight)] <- epsilon
    
    # Network metrics
    features <- tibble(
      Year = i,
      Nodes = gorder(graph),
      Edges = gsize(graph),
      Density = edge_density(graph),
      Mean_distance_uw = mean_distance(graph, directed = TRUE, weights = NA),
      Mean_distance_w  = mean_distance(graph, directed = TRUE, weights = abs(E(graph)$weight)),
      Transitivity_uw  = transitivity(graph, weights = NA),
      Transitivity_w   = transitivity(graph, weights = E(graph)$dist_weight),
      Reciprocity = reciprocity(graph),
      Diameter_uw = diameter(graph, directed = TRUE, weights = NA),
      Diameter_w  = diameter(graph, directed = TRUE, weights = abs(E(graph)$dist_weight)),
      Assortativity_r = assortativity_nominal(graph, V(graph)$n_region),
      Assortativity_weo = assortativity_nominal(graph, V(graph)$n_weo_group_region),
      Acyclic = is_acyclic(graph)
    )
    network_scores <- bind_rows(network_scores, features)
    
    # Jurisdiction-level metrics
    V(graph)$strength_out <- strength(graph, mode = "out")
    V(graph)$strength_in  <- strength(graph, mode = "in")
    V(graph)$strength_total <- strength(graph, mode = "total")
    
    V(graph)$degree_out <- igraph::degree(graph, mode = "out")
    V(graph)$degree_in  <- igraph::degree(graph, mode = "in")
    
    V(graph)$closeness_uw <- closeness(graph, mode = "all", weights = NA)
    V(graph)$closeness_w  <- closeness(graph, mode = "all", weights = abs(E(graph)$dist_weight))
    
    V(graph)$betweenness_uw <- betweenness(graph, weights = NA)
    V(graph)$betweenness_w  <- betweenness(graph, weights = abs(E(graph)$dist_weight) + epsilon)
    
    V(graph)$eigen_vector_uw <- eigen_centrality(graph, directed = TRUE, weights = NA)$vector
    V(graph)$eigen_vector_w  <- eigen_centrality(graph, directed = TRUE, weights = abs(E(graph)$weight))$vector
    
    V(graph)$alpha_uw <- alpha_centrality(graph, alpha = (1 / eigen_centrality(graph, directed = TRUE, weights = NA)$value) * 0.5, weights = NA)
    V(graph)$alpha_w  <- alpha_centrality(graph, alpha = (1 / eigen_centrality(graph, directed = TRUE, weights = abs(E(graph)$weight))$value) * 0.5)
    
    V(graph)$page_rank_uw <- page_rank(graph, directed = TRUE, weights = NA)$vector
    V(graph)$page_rank_w  <- page_rank(graph, directed = TRUE, weights = abs(E(graph)$weight))$vector
    
    # Store node metrics
    results <- as_data_frame(graph, what = "vertices") %>%
      mutate(year = i) %>%
      select(year, everything())
    
    jurisdiction_scores <- bind_rows(jurisdiction_scores, results)
  }
  
  return(list(
    graphs = graph_list,
    scores = network_scores,
    jurisdiction = jurisdiction_scores
  ))
}

# Mapping of directions for inversion ------

direction_map <- c(
  # Trade (goods)
  "IMTS Exports" = FALSE,   # exporter -> importer
  "IMTS Imports" = TRUE,    # recorded at importer; invert to exporter -> importer
  
  # Portfolio
  "PIP Assets"      = FALSE, # holder (reporter) -> issuer (counterpart)
  "PIP Liabilities" = TRUE,  # recorded at debtor; invert to holder -> issuer
  
  # Direct investment
  "DIP Outward" = FALSE,  # investor (reporter) -> target (counterpart)
  "DIP Inward"  = TRUE,   # recorded at host; invert to investor -> target
  
  # Cross‑border banking
  "LBS Claims"      = FALSE, # reporting banks’ claims -> counterparty
  "LBS Liabilities" = TRUE   # recorded at debtor; invert to creditor -> debtor
)


# Test ----
finance_network <- process_network(data = dataset_base_filtered,
                                   country_set = country_set,
                                   dataset_name = "finance_network",
                                   value_column = "value_adj_cons",
                                   source_filter = c("PIP Assets","DIP Outward","LBS Claims"),
                                   direction_map = direction_map,
                                   handle_obstatus = FALSE)

trade_network <- process_network(data = dataset_base_filtered,
                                   country_set = country_set,
                                   dataset_name = "finance_network",
                                   value_column = "value_adj_cons",
                                   source_filter = c("IMTS Exports"),
                                   direction_map = direction_map,
                                   handle_obstatus = FALSE)

#Test results
trade_network_scores <- trade_network$scores
print(trade_network_scores)

finance_network_scores <- finance_network$scores
print(finance_network_scores)


trade_country_scores <- trade_network$jurisdiction
print(trade_country_scores)

finance_country_scores <- finance_network$jurisdiction
print(finance_country_scores)

##2.2 Normalize -------

# Normalize measures

normalize_measures <- function(jurisdiction_scores) {
  
  # ---- Annual normalization ----
  jurisdiction_scores_normalized <- jurisdiction_scores %>%
    group_by(year) %>%
    mutate(
      # Strength measures
      strength_out_norm    = normalize(strength_out, method = "range", range = c(0, 1)),
      strength_in_norm     = normalize(strength_in, method = "range", range = c(0, 1)),
      strength_total_norm  = normalize(strength_total, method = "range", range = c(0, 1)),
      
      # Degree measures
      degree_out_norm      = normalize(degree_out, method = "range", range = c(0, 1)),
      degree_in_norm       = normalize(degree_in, method = "range", range = c(0, 1)),
      
      # Closeness measures
      closeness_uw_norm    = normalize(closeness_uw, method = "range", range = c(0, 1)),
      closeness_w_norm     = normalize(closeness_w, method = "range", range = c(0, 1)),
      
      # Betweenness measures
      betweenness_uw_norm  = normalize(betweenness_uw, method = "range", range = c(0, 1)),
      betweenness_w_norm   = normalize(betweenness_w, method = "range", range = c(0, 1)),
      
      # Eigenvector measures
      eigen_vector_uw_norm = normalize(eigen_vector_uw, method = "range", range = c(0, 1)),
      eigen_vector_w_norm  = normalize(eigen_vector_w, method = "range", range = c(0, 1)),
      
      # Alpha measures
      alpha_uw_norm        = normalize(alpha_uw, method = "range", range = c(0, 1)),
      alpha_w_norm         = normalize(alpha_w, method = "range", range = c(0, 1)),
      
      # PageRank measures
      page_rank_uw_norm    = normalize(page_rank_uw, method = "range", range = c(0, 1)),
      page_rank_w_norm     = normalize(page_rank_w, method = "range", range = c(0, 1))
    ) %>%
    ungroup()
  
  # ---- Panel-wide percentiles ----
  jurisdiction_scores_percentile <- jurisdiction_scores %>%
    mutate(
      strength_out_per    = percent_rank(strength_out),
      strength_in_per     = percent_rank(strength_in),
      strength_total_per  = percent_rank(strength_total),
      
      degree_out_per      = percent_rank(degree_out),
      degree_in_per       = percent_rank(degree_in),
      
      closeness_uw_per    = percent_rank(closeness_uw),
      closeness_w_per     = percent_rank(closeness_w),
      
      betweenness_uw_per  = percent_rank(betweenness_uw),
      betweenness_w_per   = percent_rank(betweenness_w),
      
      eigen_vector_uw_per = percent_rank(eigen_vector_uw),
      eigen_vector_w_per  = percent_rank(eigen_vector_w),
      
      alpha_uw_per        = percent_rank(alpha_uw),
      alpha_w_per         = percent_rank(alpha_w),
      
      page_rank_uw_per    = percent_rank(page_rank_uw),
      page_rank_w_per     = percent_rank(page_rank_w)
    )
  
  return(list(
    normalized = jurisdiction_scores_normalized,
    percentile = jurisdiction_scores_percentile
  ))
}


#Test -----------

trade_norm <- normalize_measures(trade_country_scores)$normalized 
finance_norm <- normalize_measures(finance_country_scores)$normalized

trade_per <- normalize_measures(trade_country_scores)$percentile
finance_per <- normalize_measures(finance_country_scores)$percentile

combined_norm <- trade_norm %>%
  rename_with(~ paste0("trade_",.),
              -c(year, name, country.name.en, region, weo_group_region)) %>%
  inner_join(
    finance_norm %>% 
      rename_with(~ paste0("fin_",.),
                  -c(year, name, country.name.en, region, weo_group_region)),
    by = c("year", "name", "country.name.en", "region", "weo_group_region")
  )

combined_per<- trade_per %>%
  rename_with(~ paste0("trade_",.),
              -c(year, name, country.name.en, region, weo_group_region)) %>%
  inner_join(
    finance_per %>% 
      rename_with(~ paste0("fin_",.),
                  -c(year, name, country.name.en, region, weo_group_region)),
    by = c("year", "name", "country.name.en", "region", "weo_group_region")
  )

#2.3 Principal Component Analysis (PCA)-----

# Select columns fr PCA
pca_columns <- c("strength_out_per", "strength_in_per", "strength_total_per",
                 "degree_out_per", "degree_in_per", 
                 "closeness_uw_per", "closeness_w_per",
                 "betweenness_uw_per", "betweenness_w_per",
                 "eigen_vector_uw_per", "eigen_vector_w_per",
                 "alpha_uw_per", "alpha_w_per",
                 "page_rank_uw_per", "page_rank_w_per")

# Function for PCA and index calculation 
process_pca <- function(data, pca_columns, variance_threshold = 0.85) {
  tryCatch({
    # Validate input data
    if (is.null(data) || nrow(data) == 0) {
      stop("Input data is NULL or empty.")
    }
    
    # Ensure all PCA columns are present in the data
    if (!all(pca_columns %in% names(data))) {
      stop("Not all specified PCA columns are present in the data.")
    }
    
    # Prepare data for PCA, removing NAs in the specified columns
    pca_data <- data %>% 
      select(all_of(pca_columns)) %>% 
      na.omit()
    
    if (nrow(pca_data) < 2) {
      stop("Not enough data points after NA removal for PCA.")
    }
    
    # Perform PCA
    pca_result <- prcomp(pca_data, scale. = FALSE, center = FALSE)
    
    # Determine the number of components to retain based on variance threshold
    explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
    cumulative_var <- cumsum(explained_var)
    if (!any(cumulative_var >= variance_threshold)) {
      stop("Variance threshold not met by any principal components.")
    }
    num_components <- which(cumulative_var >= variance_threshold)[1]
    
    # Handle loadings safely for single component as a matrix
    loadings <- abs(pca_result$rotation[, 1:num_components, drop = FALSE])
    weights_per_variable_per_pc <- sweep(loadings, 2, explained_var[1:num_components], FUN="*")
    final_weights <- rowSums(weights_per_variable_per_pc) / sum(rowSums(weights_per_variable_per_pc))
    
    # Calculate a weighted index for each observation in the original data
    index_final_pca <- c(as.matrix(data %>% select(all_of(pca_columns))) %*% matrix(final_weights, nrow = length(final_weights), ncol = 1))
    data$index_final_pca <- index_final_pca*100
    
    # Add ranking within each year based on the composite index
    data <- data %>%
      group_by(year) %>%  # Ensure data is grouped by year
      mutate(index_rank_pca = rank(-index_final_pca, ties.method = "first")) %>%
      ungroup()  # Remove grouping
    
    # Return the modified dataset along with PCA results
    return(list(final_data = data, pca_result = pca_result))
  }, error = function(e) {
    message("An error occurred: ", e$message)
    return(NULL)  # Return NULL in case of error
  })
}

##2.4 Indices-----

###2.4.1 CDIS Outward -----
cdis_outward_filtered_results <- process_financial_network(data = cdis_outward_filtered, 
                                                           country_set = country_set,
                                                           dataset_name = "cdis_outward",
                                                           handle_obstatus = FALSE, 
                                                           use_inflation_adjusted = FALSE, 
                                                           invert_direction = FALSE)


#Indices
cdis_outward_filtered_indices <- calculate_indices(cdis_outward_filtered_results$jurisdiction)

#PCA
cdis_outward_filtered_pca <- process_pca(data = cdis_outward_filtered_indices$percentile,
                                         pca_columns = pca_columns,
                                         variance_threshold = 0.90)


###2.4.2 CPIS Assets----- 
cpis_assets_filtered_results <- process_financial_network(data = cpis_assets_filtered, 
                                                          country_set = country_set,
                                                          dataset_name = "cpis_assets",
                                                          handle_obstatus = FALSE, 
                                                          use_inflation_adjusted = FALSE, 
                                                          invert_direction = FALSE)

#Indices
cpis_assets_filtered_indices <- calculate_indices(cpis_assets_filtered_results$jurisdiction)

#PCA
cpis_assets_filtered_pca <- process_pca(data = cpis_assets_filtered_indices$percentile,
                                        pca_columns = pca_columns,
                                        variance_threshold = 0.90)

###2.4.5 LBS Claims-----
lbs_claims_filtered_results <- process_financial_network(data = lbs_claims_filtered, 
                                                         country_set = country_set,
                                                         dataset_name = "lbs_claims",
                                                         handle_obstatus = FALSE, 
                                                         use_inflation_adjusted = FALSE, 
                                                         invert_direction = FALSE)

#Indices
lbs_claims_filtered_indices <- calculate_indices(lbs_claims_filtered_results$jurisdiction)

#PCA
lbs_claims_filtered_pca <- process_pca(data = lbs_claims_filtered_indices$percentile,
                                       pca_columns = pca_columns,
                                       variance_threshold = 0.90)
###2.4.5 Combined Assets-----
combined_assets_results <- process_financial_network(data = combined_assets_base, 
                                                     country_set = country_set,
                                                     dataset_name = "combined_assets",
                                                     handle_obstatus = FALSE, 
                                                     use_inflation_adjusted = FALSE, 
                                                     invert_direction = FALSE)

#Indices
combined_assets_indices <- calculate_indices(combined_assets_results$jurisdiction)

#PCA
combined_assets_pca <- process_pca(data = combined_assets_indices$percentile,
                                   pca_columns = pca_columns,
                                   variance_threshold = 0.90)

#2.5 Network Results----
view(cdis_outward_filtered_results$scores)

view(cpis_assets_filtered_results$scores)

view(lbs_claims_filtered_results$scores)

view(combined_assets_results$scores)

#2.6 Indices Results----
view(cdis_outward_filtered_pca$final_data)  

view(cpis_assets_filtered_pca$final_data)  

view(lbs_claims_filtered_pca$final_data) 

view(combined_assets_pca$final_data)  

#2.7 PCA Results-----
cdis_outward_filtered_pca$pca_result
summary(cdis_outward_filtered_pca$pca_result)

cpis_assets_filtered_pca$pca_result
summary(cpis_assets_filtered_pca$pca_result)

lbs_claims_filtered_pca$pca_result
summary(lbs_claims_filtered_pca$pca_result)

combined_assets_pca$pca_result
summary(combined_assets_pca$pca_result)