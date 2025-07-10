#CUNTO SANTANA (20XX)

#2 NETWORK ANALYSIS---

##2.1 Network and Jurisdiction Measures-------

# Function to process datasets with an option for inflation adjustment
process_financial_network <- function(data, country_set, dataset_name, handle_obstatus = FALSE, use_inflation_adjusted = FALSE, invert_direction = FALSE) {
  graph_list <- list()
  network_scores <- tibble()
  jurisdiction_scores <- tibble()
  
  # Selecting the appropriate weight column based on user choice
  weight_col <- if(use_inflation_adjusted) "position_adj_cons" else "position_adj"
  epsilon <- 1e-10  # Define a small constant for epsilon
  
  for(i in unique(data$year)) {
    # Subset the data for the current year early to avoid repeating filtering
    year_data <- data %>%
      filter(year == i) %>%
      mutate(weight = .data[[weight_col]])  # Ensure weight is initialized here from the correct column
    
    if(handle_obstatus) {
      # Update weight based on obstatus conditionally
      year_data <- year_data %>%
        mutate(weight = ifelse(obs_status == "C" & is.na(weight), epsilon, weight))
    } else {
      # Drop rows with NaN values in the weight column if not handling obstatus
      year_data <- year_data %>%
        filter(!is.nan(weight))
    }
    
    # Ensure no NaN values in weights
    year_data <- year_data %>%
      filter(!is.nan(weight))
    
    # Graph construction
    if (invert_direction) {
      edges <- year_data %>%
        rename(from = counterpart_iso3, to = jurisdiction_iso3) %>%
        select(from, to, weight, source)
    } else {
      edges <- year_data %>%
        rename(from = jurisdiction_iso3, to = counterpart_iso3) %>%
        select(from, to, weight, source)
    }
    
    nodes <- country_set %>%
      rename(vertices = iso3c) %>%
      select(vertices, country.name.en, region, weo_group_region)
    
    graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    
    # Store the graph with the dataset name and year
    graph_key <- paste(dataset_name, as.character(i), sep = "_")
    graph_list[[graph_key]] <- graph
    
    # Network Attributes
    graph <- graph %>%
      set_graph_attr("year", value = i) %>%
      set_edge_attr("n_weight", value = E(graph)$weight/max(E(graph)$weight)) %>% # Normalized weights
      set_edge_attr("dist_weight", value = 1/abs(E(graph)$weight)) %>% # Inverse weight for when algorithms penalize larger values (larger distance)
      set_vertex_attr("n_region", value = as.integer(factor(nodes$region, levels = unique(nodes$region)))) %>%
      set_vertex_attr("n_weo_group_region", value = as.integer(factor(nodes$weo_group_region, levels = unique(nodes$weo_group_region)))) %>%
      delete_vertices(c(names(which(igraph::degree(graph) == 0))))
    
    # Ensure no NaN values in edge attributes
    E(graph)$weight[is.na(E(graph)$weight)] <- epsilon
    E(graph)$dist_weight[is.na(E(graph)$dist_weight)] <- epsilon
    
    # Network measures
    features <- tibble(Year = i, # Year
                       Nodes = gorder(graph), # Number of nodes
                       Edges = gsize(graph), # Number of links
                       Density = edge_density(graph), # Density
                       Mean_distance_uw = mean_distance(graph, directed = TRUE, weights = NA), #Mean distance unweighted
                       Mean_distance_w = mean_distance(graph, directed = TRUE, weights = abs(E(graph)$weight)), # Mean Distance (weights must be positive)
                       Transitivity_uw = transitivity(graph, weights = NA), # Transitivity (Unweighted)
                       Transitivity_w = transitivity(graph, weights = E(graph)$dist_weight), # Transitivity (Weighted)
                       Reciprocity = reciprocity(graph), # Reciprocity
                       Diameter_uw = diameter(graph, directed = TRUE, weights = NA), # Diameter (Unweighted)
                       Diameter_w = diameter(graph, directed = TRUE, weights = abs(E(graph)$dist_weight)), # Diameter (Weighted) (weights must be positive)
                       Assortativity_r = assortativity_nominal(graph, V(graph)$n_region),
                       Assortativity_weo = assortativity_nominal(graph, V(graph)$n_weo_group_region), # Assortativity
                       Acyclic = is_acyclic(graph)) #Check for cycles on the graph
    
    # Create network results
    network_scores <- bind_rows(network_scores, features)
    
    # Jurisdiction measures
    
    ## Strength
    # Out Strength 
    V(graph)$strength_out <- strength(graph, mode = "out")
    # In Strength 
    V(graph)$strength_in <- strength(graph, mode = "in")
    # Total Strength 
    V(graph)$strength_total <- strength(graph, mode = "total")
    
    ## Centralities
    
    # Out-degree
    V(graph)$degree_out <- igraph::degree(graph, mode = "out")
    # In-degree
    V(graph)$degree_in <- igraph::degree(graph, mode = "in")
    
    # Closeness Un-Weighted
    V(graph)$closeness_uw <- closeness(graph, mode = "all", weights = NA)
    # Closeness Weighted
    V(graph)$closeness_w <- closeness(graph, mode = "all", weights = abs(E(graph)$dist_weight)) # Weights must be positive
    
    # Betweenness Un-Weighted
    V(graph)$betweenness_uw <- betweenness(graph, weights = NA)
    # Betweenness Weighted
    V(graph)$betweenness_w <- betweenness(graph, weights = abs(E(graph)$dist_weight) + epsilon) # Weights as distance (must be positive) Add epsilon to solve for 
    
    # Eigenvector Un-Weighted
    V(graph)$eigen_vector_uw <- eigen_centrality(graph, directed = TRUE, weights = NA)$vector
    # Eigenvector Weighted
    V(graph)$eigen_vector_w <- eigen_centrality(graph, directed = TRUE, weights = abs(E(graph)$weight))$vector
    
    # Alpha Un-Weighted
    V(graph)$alpha_uw <- alpha_centrality(graph, alpha = (1/eigen_centrality(graph, directed = TRUE, weights = NA)$value)*0.5, weights = NA)
    # Alpha Weighted
    V(graph)$alpha_w <- alpha_centrality(graph, alpha = (1/eigen_centrality(graph, directed = TRUE, weights = abs(E(graph)$weight))$value)*0.5)
    
    # Page Rank Un-Weighted
    V(graph)$page_rank_uw <- page_rank(graph, directed = TRUE, weights = NA)$vector
    # Page Rank Weighted
    V(graph)$page_rank_w <- page_rank(graph, directed = TRUE, weights = abs(E(graph)$weight))$vector
    
    # Add results
    results <- igraph::as_data_frame(graph, what = "vertices") %>%
      mutate(year = i) %>%
      select(year, everything())
    
    jurisdiction_scores <- bind_rows(jurisdiction_scores, results)
    
    # Delete nodes and edges
    rm(edges)
    rm(nodes)
    
    ## Graph list
    graph_list[[graph_key]] <- graph
  }
  
  return(list(graphs = graph_list, scores = network_scores, jurisdiction = jurisdiction_scores))
}

##2.2 Interconnectedness Index -------

# Function to normalize and calculate indices
calculate_indices <- function(jurisdiction_scores) {
  # Initialize an empty tibble for normalized annual scores
  jurisdiction_scores_normalized <- tibble()
  
  # Annual normalization
  for(i in unique(jurisdiction_scores$year)){
    temporary_dataframe <- jurisdiction_scores %>% 
      filter(year == i) %>%
      mutate(
        # Normalizing strength measures
        strength_out_norm = BBmisc::normalize(strength_out, method = "range", range = c(0, 1)),
        strength_in_norm = BBmisc::normalize(strength_in, method = "range", range = c(0, 1)),
        strength_total_norm = BBmisc::normalize(strength_total, method = "range", range = c(0, 1)),
        
        # Normalizing degree measures
        degree_out_norm = BBmisc::normalize(degree_out, method = "range", range = c(0, 1)),
        degree_in_norm = BBmisc::normalize(degree_in, method = "range", range = c(0, 1)),
        
        # Normalizing closeness measures
        closeness_uw_norm = BBmisc::normalize(closeness_uw, method = "range", range = c(0, 1)),
        closeness_w_norm = BBmisc::normalize(closeness_w, method = "range", range = c(0, 1)),
        
        # Normalizing betweenness measures
        betweenness_uw_norm = BBmisc::normalize(betweenness_uw, method = "range", range = c(0, 1)),
        betweenness_w_norm = BBmisc::normalize(betweenness_w, method = "range", range = c(0, 1)),
        
        # Normalizing eigenvector measures
        eigen_vector_uw_norm = BBmisc::normalize(eigen_vector_uw, method = "range", range = c(0, 1)),
        eigen_vector_w_norm = BBmisc::normalize(eigen_vector_w, method = "range", range = c(0, 1)),
        
        # Normalizing alpha measures
        alpha_uw_norm = BBmisc::normalize(alpha_uw, method = "range", range = c(0, 1)),
        alpha_w_norm = BBmisc::normalize(alpha_w, method = "range", range = c(0, 1)),
        
        # Normalizing page rank measures
        page_rank_uw_norm = BBmisc::normalize(page_rank_uw, method = "range", range = c(0, 1)),
        page_rank_w_norm = BBmisc::normalize(page_rank_w, method = "range", range = c(0, 1))) %>%
      
      # Calculating indices
      mutate(index_str = rowMeans(select(., c(strength_out_norm, strength_in_norm, strength_total_norm))),
             index_uw = rowMeans(select(., c(degree_out_norm, degree_in_norm, closeness_uw_norm, betweenness_uw_norm, eigen_vector_uw_norm, alpha_uw_norm, page_rank_uw_norm))),
             index_w = rowMeans(select(., c(closeness_w_norm, betweenness_w_norm, eigen_vector_w_norm, alpha_w_norm, page_rank_w_norm)))) %>%
      mutate(index_final_norm = rowMeans(select(., c(index_str, index_uw, index_w))) * 100,
             index_rank_norm = rank(desc(index_final_norm))) %>%
      select(year, name, country.name.en, region, weo_group_region, 
             strength_out_norm, strength_in_norm, strength_total_norm, 
             degree_out_norm, degree_in_norm, closeness_uw_norm, closeness_w_norm,
             betweenness_uw_norm, betweenness_w_norm, eigen_vector_uw_norm, eigen_vector_w_norm, 
             alpha_uw_norm, alpha_w_norm, page_rank_uw_norm, page_rank_w_norm,
             index_str, index_uw, index_w, index_final_norm, index_rank_norm)
    
    jurisdiction_scores_normalized <- bind_rows(jurisdiction_scores_normalized, temporary_dataframe)
  }
  
  # Panel normalization (percentile) 
  jurisdiction_scores_percentile <- jurisdiction_scores %>%
    mutate(
      #Strength measures
      strength_out_per = percent_rank(strength_out),
      strength_in_per = percent_rank(strength_in),
      strength_total_per = percent_rank(strength_total),
      
      #Degree
      degree_out_per = percent_rank(degree_out), 
      degree_in_per = percent_rank(degree_in), 
      
      #Closeness
      closeness_uw_per = percent_rank(closeness_uw),
      closeness_w_per = percent_rank(closeness_w),
      
      #Betwenness
      betweenness_uw_per = percent_rank(betweenness_uw),
      betweenness_w_per = percent_rank(betweenness_w),
      
      #Eigen vector
      eigen_vector_uw_per = percent_rank(eigen_vector_uw),
      eigen_vector_w_per = percent_rank(eigen_vector_w),
      
      #Alpha
      alpha_uw_per = percent_rank(alpha_uw),
      alpha_w_per = percent_rank(alpha_w),
      
      #Page Rank
      page_rank_uw_per = percent_rank(page_rank_uw),
      page_rank_w_per = percent_rank(page_rank_w)) %>%
    
    #Index computation
    mutate(index_str_per = rowMeans(select(., c(strength_out_per, strength_in_per, strength_total_per))), #Select strength measures
           index_uw_per = rowMeans(select(., c(degree_out_per, degree_in_per, closeness_uw_per, betweenness_uw_per, eigen_vector_uw_per, alpha_uw_per, page_rank_uw_per))), #Sleect unweighted centrality measures
           index_w_per = rowMeans(select(., c(closeness_w_per, betweenness_w_per, eigen_vector_w_per, alpha_w_per, page_rank_w_per)))) %>% #Sleect weighted centrality measures
    mutate(index_final_per = rowMeans(select(., c(index_str_per, index_uw_per, index_w_per))) * 100) %>%
    select(year, name, country.name.en, region, weo_group_region, 
           strength_out_per, strength_in_per, strength_total_per, 
           degree_out_per, degree_in_per, 
           closeness_uw_per, closeness_w_per,
           betweenness_uw_per, betweenness_w_per,
           eigen_vector_uw_per, eigen_vector_w_per, 
           alpha_uw_per, alpha_w_per, 
           page_rank_uw_per, page_rank_w_per,
           index_str_per, index_uw_per, index_w_per, index_final_per)
  
  # Add rank within each year
  jurisdiction_scores_percentile_with_rank <- tibble()
  
  for(i in unique(jurisdiction_scores$year)){
    temporary_dataframe <- jurisdiction_scores_percentile %>% 
      filter(year == i) %>%
      mutate(index_rank_per = rank(desc(index_final_per)))
    
    jurisdiction_scores_percentile_with_rank <- bind_rows(jurisdiction_scores_percentile_with_rank, temporary_dataframe)
  }
  
  return(list(normalized = jurisdiction_scores_normalized, percentile = jurisdiction_scores_percentile_with_rank))
}

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