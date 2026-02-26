
con <- connect_to_peekbank(db_version = "2026.1")


administrations <- collect(get_administrations(dataset_name = "frank_tablet_2016", connection = con))
trials <- collect(get_trials(dataset_name = "frank_tablet_2016", connection = con))
trial_types <- collect(get_trial_types(dataset_name = "frank_tablet_2016", connection = con))
data <- collect(get_aoi_region_sets(connection = con))
# aoi_region_set <- collect(dataset_name = "frank_tablet_2016", get_aoi_region_sets(connection = con))
datasets <- collect(get_datasets(connection = con))
xy_timepoints <- collect(get_xy_timepoints(dataset_name = "frank_tablet_2016", connection = con))
aoi_timepoints <- collect(get_aoi_timepoints(dataset_name = "frank_tablet_2016", connection = con))

xy_aoi_timepoints <- xy_timepoints %>% 
  left_join(aoi_timepoints)

# I can't quite get the right coordinates 

# get_aoi_region_sets(connection = con)
# 
# aoi_regions_df <- aoi_region_set %>%
#     filter(aoi_region_set_id == ???) %>% 
#     pivot_longer(
#       cols = -aoi_region_set_id,
#       names_to = c("side", ".value"),
#       names_pattern = "(.)_(.*)"
#     )
#   
# 
# So I paste them from import.R -> https://github.com/peekbank/peekbank-data-import/blob/828908690da6feae701b116e5104ba985935c91a/data/frank_tablet_2016/import.R

aoi_region_sets <- tibble(
  aoi_region_set_id = 0,
  l_x_min = 0,
  l_x_max = 533,
  l_y_min = 400,
  l_y_max = 900,
  r_x_min = 1067,
  r_x_max = 1600,
  r_y_min = 400,
  r_y_max = 900
) 

