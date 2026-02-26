
monitor_x <- max(c(administrations$monitor_size_x), na.rm = TRUE)
monitor_y <- max(c(administrations$monitor_size_y), na.rm = TRUE)
x_limit <- if (is.finite(monitor_x) && monitor_x > 0) monitor_x else NA
y_limit <- if (is.finite(monitor_y) && monitor_y > 0) monitor_y else NA

aoi_x_max <- max(c(aoi_region_sets$l_x_max, aoi_region_sets$r_x_max), na.rm = TRUE)
aoi_y_max <- max(c(aoi_region_sets$l_y_max, aoi_region_sets$r_y_max), na.rm = TRUE)
x_limit <- if (!is.na(x_limit)) max(x_limit, aoi_x_max) else NA
y_limit <- if (!is.na(y_limit)) max(y_limit, aoi_y_max) else NA


region_sets_to_df <- function(aoi_region_set){
  
  aoi_boxes <- aoi_region_set %>%
    tidyr::pivot_longer(cols = everything() & !aoi_region_set_id,
                        names_to = c("side", "axis", "bound"),
                        names_pattern = "(l|r)_(x|y)_(min|max)") %>%
    tidyr::pivot_wider(names_from = c(axis, bound), values_from = value) %>%
    mutate(side_label = ifelse(side == "l", "left", "right"))
  
  return(aoi_boxes)
  
  
}
