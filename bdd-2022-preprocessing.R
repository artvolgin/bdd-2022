

library(dplyr)
library(ggplot2)
library(sf)
library(units)
library(lme4)
library(gganimate)


USERNAME <- Sys.getenv("USERNAME")
setwd(paste0('C:/Users/', USERNAME, '/YandexDisk/COMPETITIONS/bdd_2022/data'))

# ------------------------------------------------------------------------------
# --- Loading and Preprocessing
# ------------------------------------------------------------------------------

# --- df_main
df_main <- rio::import('nyra_2019_complete.csv')
colnames(df_main) <- c('track_id','race_date','race_number','program_number',
                       'trakus_index','latitude','longitude','distance_id',
                       'course_type','track_condition','run_up_distance','race_type',
                       'purse','post_time','weight_carried','jockey','odds','position_at_finish')
df_main$race_date <- as.Date(df_main$race_date)
df_main$program_number <- trimws(df_main$program_number)

# unique id race
df_main$race_id <- paste(as.character(df_main$track_id),
                         as.character(df_main$race_date),
                         as.character(df_main$race_number),
                         sep = '_')
# unique id horse-race
df_main$horse_race_id <- paste(as.character(df_main$track_id),
                               as.character(df_main$race_date),
                               as.character(df_main$race_number),
                               as.character(df_main$program_number),
                               sep = '_')

# --- df_horses
df_horses <- read.csv('horse_ids.csv')
df_horses_names <- read.csv('horse_names.csv') %>% select(-c(X))
df_horses <- df_horses %>%
  left_join(df_horses_names) %>%
  select(-c(X)) %>%
  rename(race_number=race)
df_horses$race_date <- as.Date(df_horses$race_date)

# --- merge
df_main <- df_main %>%
  left_join(df_horses, by=c('track_id', 'race_date', 'race_number', 'program_number'))
# Drop useless columns
df_main <- df_main %>% select(-c(finishing_place)) # TODO: Remove 'finishing_place' or 'position_at_finish'?



# ------------------------------------------------------------------------------
# --- Plots and animation
# ------------------------------------------------------------------------------

# --- Plot one race
selected_race <- df_main_sf %>% filter(race_id=="BEL_2019-06-09_6")
plot(selected_race['trakus_index'])

# --- Plot one frame
selected_frame <- selected_race %>% filter(trakus_index==55)
plot(selected_frame['horse_id'])

# --- Animate
selected_race$X <- st_coordinates(selected_race$geometry)[,1]
selected_race$Y <- st_coordinates(selected_race$geometry)[,2]
selected_race <- selected_race %>% data.frame() %>% select(-c('geometry'))
nFrames <- max(selected_race$trakus_index)

anim <- ggplot() +
  #adding players
  geom_point(data = selected_race, aes(x = X, y = Y),
             alpha = 0.7) +
  #setting animation parameters
  transition_time(trakus_index)  +
  ease_aes('linear') +
  view_follow() +
  shadow_trail(distance = 0.05, max_frames = 10, color='grey')

animate(anim, width = 720, height = 440, fps = 10, renderer = av_renderer(),
        nframe = nFrames)


# ------------------------------------------------------------------------------
# --- Variables calculation
# ------------------------------------------------------------------------------

hspeed <- function(p1){st_distance(lag(p1), p1, by_element=TRUE) / 0.25}



# --- SPEED CALCULATION
df_main_sf <- st_as_sf(x = df_main, coords = c('longitude', 'latitude')) %>%
  st_set_crs("EPSG:4326")


calculateSpeedEgoAlters <- function(df_main_sf_race, horse_i_id){

  # 1. Select one horse as ego
  df_main_sf_race$pov_type <- ifelse(
    df_main_sf_race$horse_id %in% horse_i_id, 'ego', 'alters')

  # 2. Calculate speed for POV horse and for other
  df_result <- df_main_sf_race %>% data.frame() %>%
    group_by(pov_type, trakus_index) %>%
    summarise(speed=mean(speed, na.rm=T), .groups = 'drop') %>%
    ungroup()

  # Add indication of horse and race
  df_result$horse_id <- horse_i_id
  df_result$race_id <- df_main_sf_race$race_id[1]

  # To wider
  df_result <- df_result %>%
    tidyr::pivot_wider(id_cols=c('trakus_index', 'horse_id', 'race_id'),
                       names_from='pov_type', values_from = 'speed') %>%
    rename(ego_speed=ego, alters_speed=alters)

  # Add lags
  df_result$ego_speed_lag <- lag(df_result$ego_speed)
  df_result$alters_speed_lag <- lag(df_result$alters_speed)

  return(df_result)

}


# Calculate For all races
vec_race_ids <- unique(df_main_sf$race_id)[1:50]
i <- 1
list_results <- list()
for (rc_id in vec_race_ids){

  # 1. Select one race and calculate speed for all the horses there
  df_main_sf_race <- df_main_sf %>%
    filter(race_id==rc_id) %>%
    group_by(horse_id) %>%
    arrange(trakus_index) %>%
    mutate(speed=hspeed(geometry)) %>%
    na.omit()
  df_main_sf_race$speed <- as.numeric(df_main_sf_race$speed)

  vec_horse_ids <- unique(df_main_sf_race$horse_id)
  for (hrs_id in vec_horse_ids){

    list_results[[i]] <- calculateSpeedEgoAlters(df_main_sf_race, horse_i_id = hrs_id)
    print(i)
    i <- i + 1

  }
}

df_results <- data.table::rbindlist(list_results)



# ------------------------------------------------------------------------------
# --- Modelling
# ------------------------------------------------------------------------------

# Transform
df_results$horse_id <- as.factor(df_results$horse_id)

# Remove NAs
df_results <- df_results[!is.na(df_results$ego_speed_lag),]


# Normalise
df_results$ego_speed <- as.numeric(scale(df_results$ego_speed))
df_results$alters_speed <- as.numeric(scale(df_results$alters_speed))
df_results$ego_speed_lag <- as.numeric(scale(df_results$ego_speed_lag))
df_results$alters_speed_lag <- as.numeric(scale(df_results$alters_speed_lag))
df_results$trakus_index <-  as.numeric(scale(df_results$trakus_index))


# LMER
lmer_model <- lmer(ego_speed ~ ego_speed_lag + alters_speed + alters_speed_lag +
                   trakus_index + I(trakus_index^2) +
                   (1 + alters_speed_lag | horse_id),
                   df_results)

summary(lmer_model)










