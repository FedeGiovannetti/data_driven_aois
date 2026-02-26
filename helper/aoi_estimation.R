

### HDBSCAN estimation ####


estimate_dbscan_params <-function(data, plot = FALSE){
  
  data_subset <- data %>%
    select(x, y)
  
  k_val <- nrow(data_subset) * 0.10   # To discuss properly
  
  knn_vector = kNNdist(data_subset, k = k_val) %>%
    sort(decreasing = T)
  
  knee = inflection::uik(x = 1:length(knn_vector), y = knn_vector)
  
  if(plot == TRUE){
    plot(1:length(knn_vector), knn_vector)
    abline(v=knee, col = "red")
  }
  
  parameters = data.frame(eps = knee, minPts = k_val)
  
  return(parameters)
  
}


peekbank_hdbscan <- function(data,
                             estimate_parameters = T,
                             eps = ...,
                             minPts = ...){
  
  data_subset <- data %>%
    select(x, y)
  
  if (estimate_parameters == TRUE) {
    
    parameters = estimate_dbscan_params(data_subset)
    
    clustering = hdbscan(data_subset,
                         # cluster_selection_epsilon = parameters$eps,
                         minPts = parameters$minPts
    ) 
    
  }else{
    
    clustering = hdbscan(data_subset,
                         # cluster_selection_epsilon = eps,
                         minPts = minPts)
    
    
  }
  
  labels = clustering$cluster
  
  return(labels)
}


### Centroids calcualtion ####

aoi_centroids <- function(data){
  
  centroids_dict = data %>% 
    filter(aoi != "other") %>% 
    group_by(administration_id, aoi, trial_id) %>% 
    summarise(centroid_x = mean(x),
              centroid_y = mean(y))  %>% 
    # ungroup()%>%
    select(administration_id, aoi, centroid_x, centroid_y, trial_id) %>% 
    # group_by(aoi, administration_id) %>%
    #   # summarise(centroid_x = mean(centroid_x),
    #   #           centroid_y = mean(centroid_y)) %>% 
    pivot_wider(id_cols = c("administration_id", "trial_id"),
                names_from = "aoi",
                values_from = c("centroid_x", "centroid_y"))
  
  return(centroids_dict)
  
}


fixed_centroids <- function(aoi_region_set){
  
  distractor_centroids = region_sets_to_df(aoi_region_set) %>%
    pivot_longer(
      cols = c(x_min, x_max, y_min, y_max),
      names_to = c(".value", "type"),
      names_sep = "_"
    ) %>% 
    group_by(side) %>% 
    summarise(x = mean(x),
              y = mean(y)) %>% 
    mutate(target_side = case_when(
      side == "l" ~ "right",
      side == "r" ~ "left"
    )) %>% 
    select(-side) %>% 
    rename(centroid_x_distractor = "x") %>% 
    rename(centroid_y_distractor = "y") 
  
  target_centroids = region_sets_to_df(aoi_region_set) %>%
    pivot_longer(
      cols = c(x_min, x_max, y_min, y_max),
      names_to = c(".value", "type"),
      names_sep = "_"
    ) %>% 
    group_by(side) %>% 
    summarise(x = mean(x),
              y = mean(y)) %>% 
    mutate(target_side = case_when(
      side == "r" ~ "right",
      side == "l" ~ "left"
    )) %>% 
    select(-side) %>% 
    rename(centroid_x_target = "x") %>% 
    rename(centroid_y_target = "y") 
  
  
  trial_types_with_centroids = trial_types %>% 
    left_join(target_centroids) %>% 
    left_join(distractor_centroids)
  
  return(trial_types_with_centroids)
  
}

#### labelling clusters ####


clusters_into_gazes <- function(clustered_data){
  
  trial_types_with_centroids = fixed_centroids(aoi_region_sets)
  
  
  cluster_dict =  clustered_data %>% 
    group_by(administration_id, clusters) %>% 
    mutate(cluster_size = n()) %>% 
    group_by(administration_id) %>% 
    arrange(desc(cluster_size), .by_group = TRUE) %>% 
    mutate(cluster_rank = dense_rank(desc(cluster_size))) %>%
    # group_by(administration_id, clusters, cluster_rank, aoi, trial_type_id) %>% 
    # group_by(administration_id, cls) %>% 
    group_by(administration_id,clusters, cluster_rank) %>%
    mutate(cluster_x = mean(x, na.rm = T),
           cluster_y = mean(y, na.rm = T)) %>%
    left_join(trial_types_with_centroids) %>%
    mutate(distance_target = sqrt((cluster_x - centroid_x_target )^2 + (cluster_y - centroid_y_target)^2)) %>%
    mutate(distance_distractor = sqrt((cluster_x - centroid_x_distractor)^2 + (cluster_y - centroid_y_distractor)^2)) %>%
    # select(clusters, distance_target, distance_distractor, cluster_rank, aoi) %>%
    # select(administration_id, clusters, distance_target, distance_distractor, cluster_rank, aoi, trial_type_id) %>%
    mutate(new_aoi = case_when(
      clusters == 0 ~ "other",
      # clusters > 0 & cluster_rank < 3 & aoi == "target" ~ "target",
      # clusters > 0 & cluster_rank < 3 & aoi == "distractor" ~ "distractor",
      clusters > 0 &
        # cluster_rank < 3 & 
        distance_target < distance_distractor ~ "target",
      clusters > 0 &
        # cluster_rank < 3 & 
        distance_target > distance_distractor ~ "distractor",
      TRUE ~ "no se"
    )) %>%
    # select(administration_id, clusters, new_aoi, cluster_rank, aoi, trial_type_id, 
    #        distance_target, distance_distractor, cluster_x, cluster_y,
    #        centroid_x_target, centroid_x_distractor, centroid_y_target, centroid_y_distractor) %>%
    ungroup()
  
  
  cluster_dict
  
  # gaze_data = clustered_data %>%
  #   left_join(cluster_dict) %>%
  #   left_join(trial_types)
  # 
  return(cluster_dict)
}





corrected_rectangles <- function(data){
  
  centroids_dict <- aoi_centroids(data)
  
  cluster_centroids =  data %>% 
    group_by(administration_id, clusters) %>% 
    mutate(cluster_size = n()) %>% 
    group_by(administration_id) %>% 
    
    ## Rank clusters based on size (the biggest should be target and distractor gazes)
    arrange(desc(cluster_size), .by_group = TRUE) %>% 
    mutate(cluster_rank = dense_rank(desc(cluster_size))) %>%
    group_by(administration_id, clusters, cluster_rank, aoi) %>% 
    
    
    # Calculate cluster centroids
    summarise(cluster_x = mean(x),
              cluster_y = mean(y)) %>% 
    
    # Bring original aois centroids
    left_join(centroids_dict) %>% 
    
    mutate(dif_centroids = )
    
    select(cluster_x, cluster_y)
  
  
  aoi_region_sets %>% 
    mutate(across(contains("_x_") + cluster_x))
  
  
  
  
  
  return(corrected_rect)
  
}


#### Functions for centering original aois (not working right now) ####

correct_boxes <- function(clustered_data){
  
  cluster_y_center <<- aoi_centroids(clustered_data)%>% 
    mutate(y_centroid = (centroid_y_target + centroid_y_distractor)/2 ) %>% 
    summarise(y_centroid = mean(y_centroid)) %>% 
    pull(y_centroid)
  
  
  boxes_y_center <<- (aoi_region_sets$r_y_min + aoi_region_sets$r_y_max)/2
  
  y_center_dif <<- cluster_y_center - boxes_y_center
  
  corrected_aoi_region_sets = aoi_region_sets %>% 
    mutate(across(contains("_y_"), ~.x + y_center_dif))
  
  return(corrected_aoi_region_sets)
  
}

correct_aois <- function(clustered_data){
  
  centroids_dict <<- clustered_data %>% 
    filter(new_aoi != "other") %>% 
    group_by(administration_id, trial_id, new_aoi) %>% 
    mutate(centroid_x = mean(x),
           centroid_y = mean(y)) %>% 
    ungroup()%>% 
    
    group_by(new_aoi, administration_id) %>% 
    summarise(centroid_x = mean(centroid_x),
              centroid_y = mean(centroid_y)) %>% 
    pivot_wider(id_cols = c("administration_id"), 
                names_from = "new_aoi",
                values_from = c("centroid_x", "centroid_y"))
  
  corrected_aoi_region_sets = clustered_data  %>% 
    
    filter(!is.na(x), !is.na(y)) %>% 
    filter(t_norm >= -500) %>% 
    filter(t_norm <= 2500) %>% 
    correct_boxes()

  corrected_aois =  clustered_data %>% 
    group_by(administration_id, new_aoi) %>% 
    mutate(cluster_size = n()) %>% 
    group_by(administration_id) %>% 
    
    ## Rank clusters based on size (the biggest should be target and distractor gazes)
    arrange(desc(cluster_size), .by_group = TRUE) %>% 
    mutate(cluster_rank = dense_rank(desc(cluster_size))) %>%

 
    

    
    # Calculate corrected rectangles (original rectangles but centered to the clusters)
    mutate(corrected_boxes = case_when(
      
      between(x, corrected_aoi_region_sets$l_x_min, corrected_aoi_region_sets$l_x_max) & 
        between(y, corrected_aoi_region_sets$l_y_min, aoi_region_sets$l_y_max) ~ "left_aoi",
      
      between(x, aoi_region_sets$r_x_min, aoi_region_sets$r_x_max) & 
        between(y, aoi_region_sets$r_y_min, aoi_region_sets$r_y_max) ~ "right_aoi",
      
      TRUE ~ "other"
    )) %>% 
    group_by(administration_id, corrected_boxes, new_aoi) %>%
    
    

    # Calculate corrected boxes centroids
    summarise(box_x = mean(x),
              box_y = mean(y)) %>% 
    
    # Bring original aois centroids
    left_join(centroids_dict) %>%
    
    mutate(distance_target = sqrt((box_x - centroid_x_target )^2 + (box_y - centroid_y_target)^2)) %>%
    mutate(distance_distractor = sqrt((box_x - centroid_x_distractor)^2 + (box_y - centroid_y_distractor)^2)) %>% 
    
    select(administration_id, new_aoi, distance_target, distance_distractor) %>% 
    mutate(aoi_corrected = case_when(
      distance_target < distance_distractor ~ "target",
      distance_target > distance_distractor ~ "distractor",
      TRUE ~ "other"
    )) %>% 
    

    select(administration_id, aoi_corrected, new_aoi) %>% 
    ungroup()
  
  corrected_data = clustered_data %>%  
    left_join(corrected_aois)
  
    return(corrected_data)
  
  
  
}

