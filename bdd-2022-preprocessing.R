

library(ggplot2)
library(sf)
library(units)
library(lme4)
library(gganimate)
library(magrittr)
library(dplyr)
library(move)


USERNAME <- Sys.getenv("USERNAME")
setwd(paste0('C:/Users/', USERNAME, '/YandexDisk/COMPETITIONS/bdd_2022/data'))


getTheta <- function(dir) {

  # theta = (dir - 90) %% 360
  # theta = if_else(theta > 180, 360 - theta, theta) * pi/180
  theta = dir * pi / 180
  return(theta)
}


getRotationMatrix <- function(theta){
  R = matrix(c(sin(theta), cos(theta), -cos(theta), sin(theta)), nrow = 2)
  R = R %*% matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
  return(R)
}

getSpeed <- function(p1){st_distance(lag(p1), p1, by_element=TRUE) / 0.25}


addRotationAndSpeedRace <- function(sf_race){


  # --- Calculate movement angle and speed
  list_horses_race <- list()
  for (i in 1:length(unique(sf_race$horse_id))){

    sf_horse_race <- sf_race %>% filter(horse_id==unique(sf_race$horse_id)[i])

    move_horse_race <-  move(
      x = sf_horse_race$x,
      y = sf_horse_race$y,
      time = as.POSIXct(sf_horse_race$trakus_index, origin="1970-01-01"),
      proj = CRS("+proj=longlat +ellps=WGS84")
    )

    sf_horse_race$speed <- getSpeed(sf_horse_race)
    sf_horse_race$angle <- c(NA, angle(move_horse_race))
    list_horses_race[[i]] <- sf_horse_race
  }
  # sf_race <- data.table::rbindlist(list_horses_race)
  sf_race <- do.call(rbind, list_horses_race)

  # --- Transform angle to theta
  sf_race$theta <- getTheta(sf_race$angle)
  sf_race <- sf_race[!is.na(sf_race$theta),] # Remove NAs


  # --- Rotate coordinates in the (average) direction of movement
  list_race.rotated <- list()
  for (frame_id in unique(sf_race$trakus_index)){

    sf_frame <- sf_race %>% filter(trakus_index == frame_id)

    # Take average theta
    theta <- mean((sf_race %>% filter(trakus_index == frame_id))$theta)
    # TODO: Try individual theta for each horse-frame

    # Rotation
    sfc_frame <- st_geometry(sf_frame)

    # Rotate axes
    sfc_frame.rotated <- sfc_frame * getRotationMatrix(theta)
    sf_frame$x_rotated <- data.frame(st_coordinates(data.frame(sfc_frame.rotated)$geometry))[,1]
    sf_frame$y_rotated <- data.frame(st_coordinates(data.frame(sfc_frame.rotated)$geometry))[,2]

    list_race.rotated[[frame_id]] <- sf_frame

  }

  sf_race <- do.call(rbind, list_race.rotated)

  return(sf_race)

}



calculateVariablesEgoAlters <- function(df_race, ego_horse){

  # Select one horse as ego
  df_result <- df_race
  df_result$pov_type <- ifelse(df_result$horse_race_id %in% ego_horse, 'ego', 'alters')

  # Groupby
  df_result <- df_result %>% group_by(pov_type, trakus_index) %>%
    summarise(speed=mean(speed), y_rotated=mean(y_rotated), .groups = 'drop')

  df_result <-  df_result %>%
    tidyr::pivot_wider(id_cols=c('trakus_index'),
                       names_from='pov_type', values_from = c('speed', 'y_rotated'))

  df_result$y_rotated_diff <- df_result$y_rotated_ego - df_result$y_rotated_alters
  df_result$speed_diff <- df_result$speed_ego - df_result$speed_alters

  # Add ego and race ids
  df_result$horse_race_id <- ego_horse
  df_result$race_id <- df_race$race_id[1]

  # Add lags
  df_result$speed_ego_lag <- lag(df_result$speed_ego)
  df_result$speed_alters_lag <- lag(df_result$speed_alters)

  return(df_result)

}


# ------------------------------------------------------------------------------
# --- Loading and Preprocessing
# ------------------------------------------------------------------------------

# --- df_main
df_main <- rio::import('nyra_2019_complete.csv')
colnames(df_main) <- c('track_id','race_date','race_number','program_number',
                       'trakus_index','latitude','longitude','distance_id',
                       'course_type','track_condition','run_up_distance','race_type',
                       'purse','post_time','weight_carried','jockey','odds','position_at_finish')
df_main <- df_main %>%
  mutate(race_date = as.Date(race_date),
         program_number = trimws(program_number),
         race_id = paste(as.character(track_id),
                          as.character(race_date),
                          as.character(race_number),
                          sep = '_'),
         horse_race_id = paste(as.character(track_id),
                               as.character(race_date),
                               as.character(race_number),
                               as.character(program_number),
                               sep = '_')
         )


# --- df_horses
df_horses <- read.csv('horse_ids.csv')
df_horses_names <- read.csv('horse_names.csv') %>% dplyr::select(-c(X))
df_horses <- df_horses %>%
  left_join(df_horses_names) %>%
  dplyr::select(-c(X)) %>%
  rename(race_number=race) %>%
  mutate(race_date = as.Date(race_date))

# --- merge
df_main <- df_main %>%
  left_join(df_horses, by=c('track_id', 'race_date', 'race_number', 'program_number'))
# Drop useless columns
df_main <- df_main %>% dplyr::select(-c(finishing_place)) # TODO: Remove 'finishing_place' or 'position_at_finish'?

# transform dataset to "sf"
df_main$x <- df_main$longitude
df_main$y <- df_main$latitude
sf_main <- st_as_sf(x = df_main, coords = c('longitude', 'latitude')) %>%
  st_set_crs("EPSG:4326")

# sort by trakus_index
sf_main <- arrange(sf_main, trakus_index)

# ------------------------------------------------------------------------------
# --- Normalise path by average vector of directions and Add speed
# ------------------------------------------------------------------------------

# set.seed(1)
# vec_race_ids <- sample(sf_main$race_id, 100)
vec_race_ids <- unique(sf_main$race_id)

list_main.add <- list()
for (i in 1:length(vec_race_ids)){

  sf_race <- sf_main %>% filter(race_id==vec_race_ids[i])
  sf_race <- addRotationAndSpeedRace(sf_race)
  list_main.add[[i]] <- sf_race
  print(i)

}
df_main.add <- data.table::rbindlist(list_main.add)

# Save to RDS
saveRDS(df_main.add, 'df_main.add.rds')


# ------------------------------------------------------------------------------
# --- Calculating variables for ego and alters
# ------------------------------------------------------------------------------

list_ego_alters <- list()
i <-  1
vec_race_ids <- unique(df_main.add$race_id)

for (j in 1:length(vec_race_ids)){

  df_race <- df_main.add %>% filter(race_id == vec_race_ids[j])

  vec_horse_race_ids <- unique(df_race$horse_race_id)
  for (ego_horse in vec_horse_race_ids){

    df_result <- calculateVariablesEgoAlters(df_race, ego_horse)
    list_ego_alters[[i]] <- df_result
    i <- i + 1

  }

  print(j)

}

df_ego_alters <- data.table::rbindlist(list_ego_alters)

# Save to RDS
saveRDS(df_ego_alters, 'df_ego_alters.rds')








# ------------------------------------------------------------------------------
# --- Modelling
# ------------------------------------------------------------------------------

# Model
df_ego_alters.model <- df_ego_alters %>%
  mutate(speed_ego=as.numeric(scale(speed_ego)),
         speed_alters=as.numeric(scale(speed_alters)),
         speed_ego_lag=as.numeric(scale(speed_ego_lag)),
         speed_alters_lag=as.numeric(scale(speed_alters_lag)),
         y_rotated_diff=as.numeric(scale(y_rotated_diff)),
         trakus_index=as.numeric(scale(trakus_index)))


model.1 <- lmer(y_rotated_diff ~ speed_ego + speed_alters + speed_ego_lag +
                speed_alters_lag + trakus_index + (1 + speed_ego | horse_race_id),
                df_ego_alters.model)
summary(model.1)


df_coeffs <- as.data.frame(ranef(model.1, condVar=F))

test <- df_ego_alters %>% filter(horse_race_id=="BEL_2019-09-06_6_3")
plot(test$trakus_index, test$y_rotated_diff)
plot(test$trakus_index, test$speed_ego)
plot(test$trakus_index, test$speed_alters)
plot(test$trakus_index, test$speed_diff)


test <- df_ego_alters %>% filter(horse_race_id=="BEL_2019-09-20_1_7")
plot(test$trakus_index, test$y_rotated_diff)
plot(test$trakus_index, test$speed_ego)
plot(test$trakus_index, test$speed_alters)
plot(test$trakus_index, test$speed_diff)


# TODO: Smoothing


# ------------------------------------------------------------------------------
# --- Distance from the center
# ------------------------------------------------------------------------------

# TODO:


# ------------------------------------------------------------------------------
# --- Angle of movement
# ------------------------------------------------------------------------------

# TODO:


# ------------------------------------------------------------------------------
# --- Plots and animation
# ------------------------------------------------------------------------------

# --- Plot one race
sf_race <- sf_main %>% filter(race_id=="BEL_2019-06-09_6")
plot(sf_race['trakus_index'])

# --- Plot one race for one horse
sf_horse_race <- sf_main %>% filter(horse_race_id=="BEL_2019-05-25_10_9")
plot(sf_horse_race['trakus_index'])

# --- Plot one frame for all horses
sf_frame <- sf_race %>% filter(trakus_index==55)
plot(sf_frame['horse_id'])


# ------- PLOT
df_frame <- df_race %>% filter(trakus_index==160)
ggplot(df_frame, aes(x, y)) +
  geom_point(aes(colour = horse_name), size = 10)


df_frame.rotated <- df_race.rotated %>% filter(trakus_index==160)
ggplot(df_frame.rotated, aes(x, y)) +
  geom_point(aes(colour = horse_name), size = 10)


# ------- ANIMATE
nFrames <- max(df_selected_race$trakus_index)

anim <- ggplot(df_selected_race, aes(x, y)) +
  #adding players
  geom_point(alpha = 0.7, aes(colour = horse_name), size=10) +
  #setting animation parameters
  transition_time(trakus_index)  +
  ease_aes('linear') +
  labs(title = 'Frame : {frame}') +
  view_follow()
# shadow_trail(distance = 0.05, max_frames = 10, color='grey')

gganimate::animate(anim, width = 720, height = 440, fps = 10, renderer = av_renderer(),
                   nframe = nFrames)





























