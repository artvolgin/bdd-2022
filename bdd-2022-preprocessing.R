

library(ggplot2)
library(sf)
library(units)
library(lme4)
library(gganimate)
library(magrittr)
library(dplyr)
library(move)
library(data.table)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

USERNAME <- Sys.getenv("USERNAME")
setwd(paste0('C:/Users/', USERNAME, '/YandexDisk/COMPETITIONS/bdd_2022/data'))


# getTheta <- function(dir) {
#
#   # theta = (dir - 90) %% 360
#   # theta = if_else(theta > 180, 360 - theta, theta) * pi/180
#   theta = dir * pi / 180
#   return(theta)
# }


# getRotationMatrix <- function(theta){
#   R = matrix(c(sin(theta), cos(theta), -cos(theta), sin(theta)), nrow = 2)
#   R = R %*% matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
#   return(R)
# }


getRotationMatrix <- function(theta){
  R = matrix(c(cos(theta), sin(theta),
               -sin(theta), cos(theta)), nrow = 2)
  # R = R %*% matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
  R = R %*% matrix(c(0, 1, 1, 0), nrow = 2, ncol = 2)
  return(R)
}


# getSpeed <- function(p1){
#   st_distance(lag(p1), p1, by_element=TRUE) / 0.25
# }


# ------------------------------------------------------------------------------
# --------------- 1. Loading and Preprocessing
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
         ) %>%
  # Remove 'Hurdle' races
  # see https://www.kaggle.com/competitions/big-data-derby-2022/discussion/344315
  filter(course_type != 'M')


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
df_main <- df_main %>%
  dplyr::select(-c(finishing_place)) # TODO: Remove 'finishing_place' or 'position_at_finish'?
saveRDS(df_main, 'df_main.rds')


# --- to sf
df_main$x <- df_main$longitude
df_main$y <- df_main$latitude
sf_main <- st_as_sf(x = df_main, coords = c('longitude', 'latitude')) %>%
  st_set_crs("EPSG:4326")
sf_main <- arrange(sf_main, trakus_index)
saveRDS(sf_main, 'sf_main.rds')

# --- sample of races
sf_main <- readRDS('sf_main.rds')
# set.seed(24)
# sf_main <- sf_main %>% filter(race_id %in% sample(sf_main$race_id, 100))
dt_main <- sf_main %>% as.data.table()


start_time <- Sys.time()

# ------------------------------------------------------------------------------
# --------------- 2. Correct frames
# ------------------------------------------------------------------------------

# 1. Remove frames before the error and after the finish line

removeFramesWithErrorsDt <- function(sf_horse_race){


  # Estimate distance
  sf_horse_race$distance <- sf::st_distance(sf_horse_race$geometry,
                                            lag(sf_horse_race$geometry),
                                            by_element = TRUE)

  sf_horse_race$distance <- as.numeric(sf_horse_race$distance)

  # Cumulative distance
  distance_cum <- cumsum(tidyr::replace_na(sf_horse_race$distance, 0))
  distance_cum <- distance_cum[2:length(distance_cum)]
  sf_horse_race$distance_cum <- c(0, distance_cum)

  # Error: No movement or Surge in distance
  # sf_horse_race$error <- (sf_horse_race$distance == set_units(0, 'm') |
  #                         sf_horse_race$distance > set_units(10, 'm'))
  sf_horse_race$error <- (sf_horse_race$distance == 0 |
                          sf_horse_race$distance > 10)

  if (any(sf_horse_race$error, na.rm=T)){

    trakus_after_error <- max(sf_horse_race[sf_horse_race$error]$trakus_index, na.rm=T)
    sf_horse_race <- sf_horse_race[trakus_index > trakus_after_error]

  }

  return(sf_horse_race)

}

dt_main <- (dt_main[, removeFramesWithErrorsDt(.SD),  by = .(horse_race_id)])


# removeFramesWithErrors <- function(sf_horse_race){
#
#   # Estimate distance
#   sf_horse_race$distance <- sf::st_distance(sf_horse_race$geometry,
#                                             lag(sf_horse_race$geometry),
#                                             by_element = TRUE)
#
#   # Cumulative distance
#   sf_horse_race$distance_cum <- c(0,
#                                   cumsum(as.numeric(sf_horse_race$distance)[2:nrow(sf_horse_race)]))
#
#   # Error: No movement or Surge in distance
#   sf_horse_race$error <- (sf_horse_race$distance == set_units(0, 'm') |
#                             sf_horse_race$distance > set_units(10, 'm'))
#
#   # Select trakus indices after error
#   sf_horse_race <- sf_horse_race %>%
#     filter(trakus_index > max(sf_horse_race[sf_horse_race$error,]$trakus_index, na.rm=T))
#
#   # Recalculate distance
#   sf_horse_race$distance_updated <- sf::st_distance(sf_horse_race$geometry,
#                                                     lag(sf_horse_race$geometry),
#                                                     by_element = TRUE)
#
#   return(sf_horse_race)
#
# }
#
#
# df_main <- sf_main %>%
#   group_by(horse_race_id) %>%
#   group_modify(~ removeFramesWithErrors(.x)) %>%
#   data.frame()

print(' ---------------------- 2. Correct frames')

# ------------------------------------------------------------------------------
# --------------- 3. Filter out observations after the finish line
# ------------------------------------------------------------------------------

# Use 'distance_id' to calculate percentage of completed track
dt_main$distance_track <- dt_main$distance_id*2.01168
dt_main$race_distance_pct <- (dt_main$distance_cum / dt_main$distance_track)

#  Cut after the finish line
dt_main <- dt_main[race_distance_pct <= 1]

print(' ---------------------- 3. Filter out observations after the finish line')

# ------------------------------------------------------------------------------
# --------------- 4. Calculate movement angle (theta)
# ------------------------------------------------------------------------------


getDistance <- function(coord, next_coord) {
  speed = (next_coord - coord)
  return(speed)
}


getThetaDt <- function(df){

  df$dist_x <- getDistance(df$x, lead(df$x))
  df$dist_y <- getDistance(df$y, lead(df$y))
  df$theta <- as.numeric(circular::coord2rad(cbind(df$dist_x, df$dist_y)))

  return(df)

}

# getThetaDt <- function(df){
#
#   df$theta <- c(NA, lwgeom::st_geod_azimuth(df$geometry))
#
#   return(sf_horse_race)
#
# }

dt_main <- (dt_main[, getThetaDt(.SD),  by = .(horse_race_id)])
dt_main <- dt_main[!is.na(dt_main$theta),]

print(' ---------------------- 4. Calculate movement angle (theta)')

# ------------------------------------------------------------------------------
# --------------- 5. Get new rotated coordinates
# ------------------------------------------------------------------------------

getRotationMatrix <- function(theta){

  R = matrix(c(cos(theta), sin(theta),
               -sin(theta), cos(theta)), nrow = 2)
  R = R %*% matrix(c(0, 1, 1, 0), nrow = 2, ncol = 2)

  return(R)

}


# dt_race <- dt_main[race_id == 'BEL_2019-10-05_9']

# dt_race$theta <- NULL



# # Rotated matrix of coordinates
# matrix_original <- as.matrix(data.frame(x=dt_frame$x, y=dt_frame$y))
#
# rotation_matrix <- getRotationMatrix(mean_theta)
#
# matrix_rotated <- matrix_original %*% rotation_matrix
#
# # Add rotated coordinates
# dt_frame$x_rotated <- matrix_rotated[,1]
# dt_frame$y_rotated <- matrix_rotated[,2]
#
# dt_frame$y_rotated_min_all <-  min(dt_frame$y_rotated, na.rm=T)
# dt_frame$y_rotated_max_all <- max(dt_frame$y_rotated, na.rm=T)
#
#
# dt_frame$relative_pos <- ((dt_frame$y_rotated-dt_frame$y_rotated_min_all) /
#                           (dt_frame$y_rotated_max_all-dt_frame$y_rotated_min_all))
#
# # dt_frame$relative_pos <- ((dt_frame$y_rotated-dt_frame$y_rotated_max_all) /
# #                           (dt_frame$y_rotated_min_all-dt_frame$y_rotated_max_all))
#
# dt_frame.select <- dt_frame %>% data.frame() %>%
#   dplyr::select(program_number, x, y, x_rotated, y_rotated, relative_pos)
#
#
# (dt_frame.select %>% arrange(-relative_pos))$program_number


rotateCoordinatesFrameDt <- function(dt_frame){

  # Average theta
  mean_theta <- mean(dt_frame$theta)

  # Rotated matrix of coordinates
  matrix_original <- as.matrix(data.frame(x=dt_frame$x, y=dt_frame$y))

  rotation_matrix <- getRotationMatrix(mean_theta)

  matrix_rotated <- matrix_original %*% rotation_matrix

  # Add rotated coordinates
  dt_frame$x_rotated <- matrix_rotated[,1]
  dt_frame$y_rotated <- matrix_rotated[,2]

  return(dt_frame)

}


rotateCoordinatesRaceDt <- function(dt_race){

  dt_race <- dt_race[, rotateCoordinatesFrameDt(c(.BY, .SD)), by = trakus_index]

  return(dt_race)

}

# dt_main <- as.data.table(df_main)

vec_race_ids <- unique(dt_main$race_id)
list_main <- list()
for (i in 1:length(vec_race_ids)){

  dt_race <- dt_main %>% filter(race_id==vec_race_ids[i])
  dt_race <- rotateCoordinatesRaceDt(dt_race)
  list_main[[i]] <- dt_race
  print(i)

}

dt_main <- data.table::rbindlist(list_main)

print(' ---------------------- 5. Get new rotated coordinates')

# ------------------------------------------------------------------------------
# --------------- 6. Calculate variables for individuals and environment
# ------------------------------------------------------------------------------


calculateVariablesInd <- function(df_horse_race){

  # --- Calculate speed
  # df_horse_race$speed <- df_horse_race$distance_updated*4
  df_horse_race$speed <- df_horse_race$distance*4
  df_horse_race$speed <- as_units(as.numeric(df_horse_race$speed), 'm/s')
  df_horse_race$speed_lag <- lag(df_horse_race$speed)

  # Calculate acceleration
  df_horse_race$accel <- c(NA, diff(df_horse_race$speed)) / 0.25
  df_horse_race$accel <- as_units(as.numeric(df_horse_race$accel), 'm/s^2')
  df_horse_race$accel_lag <- lag(df_horse_race$accel)

  # Calculate change of theta
  df_horse_race$theta_change <- c(NA, abs(diff(df_horse_race$theta)))

  return(df_horse_race)

}


# --- Calculate individual variables

dt_main <- dt_main[, calculateVariablesInd(c(.BY, .SD)), by = horse_race_id]

# df_main <- df_main %>%
#   group_by(horse_race_id) %>%
#   group_modify(~ calculateVariablesInd(.x))

# --- Calculate sum and min, max of all jockeys
dt_env <- dt_main[, .(jockey_n = .N,
                      speed_sum_all = sum(speed, na.rm=T),
                      speed_lag_sum_all = sum(speed_lag, na.rm=T),
                      accel_sum_all = sum(accel, na.rm=T),
                      accel_lag_sum_all = sum(accel_lag, na.rm=T),
                      y_rotated_sum=sum(y_rotated, na.rm=T),
                      y_rotated_min_all=min(y_rotated, na.rm=T),
                      y_rotated_max_all=max(y_rotated, na.rm=T),
                      x_rotated_sd_all=sd(x_rotated, na.rm=T),
                      y_rotated_sd_all=sd(y_rotated, na.rm=T)),
                      by = c('race_id', 'trakus_index')]

dt_main <- dt_env[dt_main, on = .(race_id, trakus_index)]


# Calculate mean speed and acceleration of the environment
dt_main$speed_env <- (dt_main$speed_sum_all - dt_main$speed) / (dt_main$jockey_n - 1)
dt_main$speed_lag_env <- (dt_main$speed_lag_sum_all - dt_main$speed_lag) / (dt_main$jockey_n - 1)
dt_main$accel_env <- (dt_main$accel_sum_all - dt_main$accel) / (dt_main$jockey_n - 1)
dt_main$accel_lag_env <- (dt_main$accel_lag_sum_all - dt_main$accel_lag) / (dt_main$jockey_n - 1)

# Relative position
dt_main$relative_pos <- ((dt_main$y_rotated-dt_main$y_rotated_min_all) /
                         (dt_main$y_rotated_max_all-dt_main$y_rotated_min_all))

# dt_main$relative_pos <- ((dt_main$y_rotated - dt_main$y_rotated_max_all) /
#                          (dt_main$y_rotated_min_all - dt_main$y_rotated_max_all))
#
# # ABS
# dt_main$relative_pos <- ((abs(dt_main$y_rotated) - abs(dt_main$y_rotated_max_all)) /
#                          (abs(dt_main$y_rotated_min_all) - abs(dt_main$y_rotated_max_all)))

# Distance from average
dt_main$y_rotated_mean <- (dt_main$y_rotated_sum - dt_main$y_rotated) / (dt_main$jockey_n - 1)
dt_main$y_rotated_diff <- dt_main$y_rotated - dt_main$y_rotated_mean



# ------------------------------------------------------------------------------
# --------------- 7. RACE PHASE
# ------------------------------------------------------------------------------


indicateStraightLine <- function(df){

  q95_theta_change <- quantile(df$theta_change, 0.95, na.rm=T)

  # df[theta_change > q95_theta_change, theta_change := q95_theta_change]

  df[theta_change > q95_theta_change,]$theta_change <- q95_theta_change

  df$straight_line <- (
    df$trakus_index %in% head(df$trakus_index, 20) |
      df$trakus_index %in% tail(df$trakus_index, 20) |
      df$theta_change < 0.015)

  return(df)

}

dt_main <- (dt_main[, indicateStraightLine(.SD), by = .(horse_race_id)])

# Save to RDS
df_main <- dt_main %>% data.frame()
df_main <- df_main %>% dplyr::select(-c('i.trakus_index', 'horse_race_id.1'))
saveRDS(df_main, 'df_main.rds')


end_time <- Sys.time()
end_time - start_time


# ------------------------------------------------------------------------------
# --------------- 8. Remove a horse with injury from individual-calculations
# ------------------------------------------------------------------------------


# TODO: --- Remove a horse with injury from individual-calculations

# TODO: Based on the max 'race_distance_pct', if lower 0.8

race_program_trakus <- df_main %>%
  group_by(race_id, program_number) %>%
  summarise(max_trakus_index=max(trakus_index))

race_program_trakus <- race_program_trakus %>%
  tidyr::pivot_wider(names_from=program_number, values_from=max_trakus_index) %>%
  ungroup() %>% data.frame()

race_program_trakus$max_frame <- race_program_trakus %>%
  dplyr::select(-race_id) %>% apply(1, max, na.rm=TRUE)
race_program_trakus$max_frame <- race_program_trakus$max_frame / 2

race_program_trakus$injury <- rowSums(
  race_program_trakus < race_program_trakus$max_frame, na.rm=T) > 1
sum(race_program_trakus$injury)



# ------------------------------------------------------------------------------
# --------------- 9. Aggregate statistics for jockey/horses in 2019
# ------------------------------------------------------------------------------

# TODO






# ------------------------------------------------------------------------------
# ---------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --------------- OLD CODE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ---------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------



rotateCoordinatesFrame <- function(df_frame){

  # Average theta
  mean_theta <- mean(df_frame$theta)

  # Rotated matrix of coordinates
  matrix_rotated <- as.matrix(df_frame[c('x', 'y')]) %*% getRotationMatrix(mean_theta)

  # Add rotated coordinates
  df_frame$x_rotated <- matrix_rotated[,1]
  df_frame$y_rotated <- matrix_rotated[,2]

  return(df_frame)

}


rotateCoordinatesRace <- function(df_race){

  df_race <- df_race %>%
    group_by(trakus_index) %>%
    group_modify(~ rotateCoordinatesFrame(.x))

  return(df_race)

}


df_main <- df_main %>%
  group_by(race_id) %>%
  group_modify(~ rotateCoordinatesRace(.x))




calculateVariablesIndEnv <- function(df_race, ind_horse_race_id){

  # Select one horse as ego
  df_result <- df_race
  df_result$pov_type <- ifelse(df_result$horse_race_id %in% ind_horse_race_id, 'ind', 'env')

  # Groupby
  df_result <- df_result %>% group_by(pov_type, trakus_index) %>%
    summarise(speed=mean(speed, na.rm=T),
              accel=mean(accel, na.rm=T),
              theta_change=mean(theta_change, na.rm=T),
              y_rotated=mean(y_rotated, na.rm=T),
              .groups = 'drop')

  df_result <-  df_result %>%
    tidyr::pivot_wider(id_cols=c('trakus_index'),
                       names_from='pov_type',
                       values_from = c('speed', 'accel', 'theta_change', 'y_rotated'))

  # df_result$speed_diff <- df_result$speed_ind - df_result$speed_env
  # df_result$accel_diff <- df_result$accel_ind - df_result$accel_env
  # df_result$y_rotated_diff <- df_result$y_rotated_ind - df_result$y_rotated_env

  # Add ego and race ids
  df_result$horse_race_id <- ind_horse_race_id
  df_result$race_id <- df_race$race_id[1]

  # Add lags, speed
  df_result$speed_ind_lag <- lag(df_result$speed_ind)
  df_result$speed_env_lag <- lag(df_result$speed_env)

  # Add lags, acceleration
  df_result$accel_ind_lag <- lag(df_result$accel_ind)
  df_result$accel_env_lag <- lag(df_result$accel_env)

  return(df_result)

}

# df_race <- df_main %>% filter(race_id=='AQU_2019-01-05_2')

list_ind_env <- list()
i <-  1
vec_race_ids <- unique(df_main$race_id)[1:10]

for (j in 1:length(vec_race_ids)){

  df_race <- df_main %>% filter(race_id == vec_race_ids[j])

  vec_horse_race_ids <- unique(df_race$horse_race_id)
  for (ind_horse_race_id in vec_horse_race_ids){

    df_result <- calculateVariablesIndEnv(df_race, ind_horse_race_id)
    list_ind_env[[i]] <- df_result
    i <- i + 1

  }

  print(j)

}
df_ind_env <- data.table::rbindlist(list_ind_env)






calculateVariablesEnv <- function(df_race, ind_horse_race_id){

  # Select only environment
  df_result <- df_race %>% filter(horse_race_id != ind_horse_race_id)

  # Calculate mean variable for the environemnt
  df_result <- df_result %>% group_by(trakus_index) %>%
    summarise(speed_mean_env=mean(speed, na.rm=T),
              accel_mean_env=mean(accel, na.rm=T),
              y_rotated_mean_env=mean(y_rotated, na.rm=T),
              # y_rotated_min_env=min(y_rotated, na.rm=T),
              # y_rotated_max_env=max(y_rotated, na.rm=T),
              .groups = 'drop')

  # df_result$speed_diff <- df_result$speed_ind - df_result$speed_env
  # df_result$accel_diff <- df_result$accel_ind - df_result$accel_env
  # df_result$y_rotated_diff <- df_result$y_rotated_ind - df_result$y_rotated_env

  # Add ego and race ids
  df_result$horse_race_id <- ind_horse_race_id
  df_result$race_id <- df_race$race_id[1]

  # Add lags, speed
  df_result$speed_mean_env_lag <- lag(df_result$speed_mean_env)

  # Add lags, acceleration
  df_result$accel_mean_env_lag <- lag(df_result$accel_mean_env)

  return(df_result)

}


list_env <- list()
i <-  1
vec_race_ids <- unique(df_main$race_id)[1:10]

for (j in 1:length(vec_race_ids)){

  df_race <- df_main %>% filter(race_id == vec_race_ids[j])

  vec_horse_race_ids <- unique(df_race$horse_race_id)
  for (ind_horse_race_id in vec_horse_race_ids){

    df_result <- calculateVariablesEnv(df_race, ind_horse_race_id)
    list_env[[i]] <- df_result
    i <- i + 1

  }

  print(j)

}
df_env <- data.table::rbindlist(list_env)



start_time <- Sys.time()

t1 <- df_main %>% group_by(race_id, trakus_index) %>%
  summarise(speed_mean_env=mean(speed, na.rm=T),
            accel_mean_env=mean(accel, na.rm=T),
            y_rotated_mean_env=mean(y_rotated, na.rm=T),
            y_rotated_min_env=min(y_rotated, na.rm=T),
            y_rotated_max_env=max(y_rotated, na.rm=T),
            .groups = 'drop')

end_time <- Sys.time()
end_time - start_time




# ---------------------------------------------------



sf_frame$index <- rownames(sf_frame)

ggplot(sf_frame) +
  geom_sf() +
  geom_sf_label(aes(label = index))

distance_to_points <- st_distance(sf_frame[1,], sf_frame)

distance_to_points <- as.numeric(distance_to_points)

distance_to_points <- distance_to_points[distance_to_points > 0] # Remove EGO

inv_distance_to_points <- 1 / (distance_to_points)^2 # Can raise to different powers

weights_from_inv_distance <- inv_distance_to_points / sum(inv_distance_to_points)

# Try with odds, replace to speed and accleration
input_alters <- sf_frame$odds[2:length(sf_frame$odds)]

mean(input_alters)
weighted.mean(input_alters, weights_from_inv_distance)








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


# 1. Individual and Surrounding (mean, weighted mean)
# 2. Acceleration
# 3. Relative position









# ------------------------------------------------------------------------------
# ---------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --------------- OLD CODE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ---------------~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------------------------------------------------



# TODO: weighted mean


# --- Plot one frame for all horses
# sf_frame <- sf_race %>% filter(trakus_index==55)


sf_main <- df_main %>% st_as_sf()
sf_main$speed <- as.numeric(sf_main$speed)

selected_race_id <- "AQU_2019-01-05_2"
sf_race <- sf_main %>% filter(race_id==selected_race_id)



vec_frames <- unique(sf_race$trakus_index)
list_variables <- list()
ind <- 1
for (i in 1:length(vec_frames)){

  selected_frame <- vec_frames[i]
  sf_frame <- sf_race %>% filter(trakus_index==selected_frame)

  vec_horse_race_id <- sf_frame$horse_race_id

  for (j in 1:length(vec_horse_race_id)){

    selected_horse_race_id <- vec_horse_race_id[j]
    sf_frame.ind <- sf_frame[sf_frame$horse_race_id == selected_horse_race_id,]
    sf_frame.env <- sf_frame[sf_frame$horse_race_id != selected_horse_race_id,]

    # Individual speed
    speed_ind <- sf_frame.ind$speed

    # Mean speed of the environment
    speed_env_mean <- mean(sf_frame.env$speed)

    # Weighted (by the inverse distance to the individual) mean speed of the environment
    dist_env <- as.numeric(st_distance(sf_frame.ind, sf_frame.env))
    inv_dist_env <- 1 / (dist_env)^3 # Can raise to different powers
    weights_inv_dist <- inv_dist_env / sum(inv_dist_env)
    speed_env_weighted_mean <- weighted.mean(sf_frame.env$speed, weights_inv_dist, na.rm=T)

    # Add calculated variables
    list_variables[[ind]] <- c('horse_race_id'=selected_horse_race_id,
                               'frame'=selected_frame,
                               'speed_ind'=speed_ind,
                               'speed_env_mean'=speed_env_mean,
                               'speed_env_weighted_mean'=speed_env_weighted_mean)
    ind <- ind + 1

  }

  print(i)

}


t <- rbind.data.frame(list_variables) %>% t() %>% data.frame()
t$speed_ind <- as.numeric(t$speed_ind)
t$speed_env_mean <- as.numeric(t$speed_env_mean)
t$speed_env_weighted_mean <- as.numeric(t$speed_env_weighted_mean)

t$diff_mean <- t$speed_ind - t$speed_env_mean
t$diff_weighted_mean <- t$speed_ind - t$speed_env_weighted_mean

plot(t$diff_mean, t$diff_weighted_mean)




hist(as.numeric(t$speed_ind) - as.numeric(t$speed_env_mean))

hist(as.numeric(t$speed_ind) - as.numeric(t$speed_env_weighted_mean))





# ------------------------------------------------------------------------------
# --------------- EDA on Horses, Jockeys, Races, and Tracks
# ------------------------------------------------------------------------------

# ~~~ track_id
# AQU - Aqueduct
# BEL - Belmont
# SAR - Saratoga

# ~~~ race_type
# STK - Stakes,
# WCL - Waiver Claiming,
# WMC - Waiver Maiden Claiming,
# SST - Starter Stakes,
# SHP - Starter Handicap,
# CLM - Claiming,
# STR - Starter Allowance,
# AOC - Allowance Optionl Claimer,
# SOC - Starter Optional Claimer,
# MCL - Maiden Claiming,
# ALW - Allowance,
# MSW - Maiden Special Weight

# ~~~ course_type
# M - Hurdle,
# D - Dirt,
# O - Outer turf,
# I - Inner turf,
# T - Turf

# ~~~ track_condition
# YL - Yielding,
# FM - Firm,
# SY - Sloppy,
# GD - Good,
# FT - Fast,
# MY - Muddy,
# SF - Soft


# --- races

df_races <- df_main %>% distinct(race_id, .keep_all=T)

eda.1 <- df_races %>% group_by(race_type) %>% summarise(mean_purse=mean(purse), n=n())
eda.2 <- df_races %>% group_by(track_id) %>% summarise(mean_purse=mean(purse), n=n())
eda.3 <- df_races %>% group_by(course_type) %>% summarise(mean_purse=mean(purse), n=n())
eda.4 <- df_races %>% group_by(course_type) %>% summarise(mean_distance_id=mean(distance_id),
                                                   min_distance_id=min(distance_id),
                                                   max_distance_id=max(distance_id),
                                                   n=n())
eda.5 <- df_races %>% group_by(race_type) %>% summarise(mean_distance_id=mean(distance_id),
                                                 min_distance_id=min(distance_id),
                                                 max_distance_id=max(distance_id),
                                                 n=n())

# --- jockeys

df_jockeys <- df_main %>% distinct(race_id, jockey, .keep_all=T)

cross.1 <- crosstab(df_jockeys,
                    row.vars='jockey',
                    col.vars='track_id',
                    type='f')

cross.2 <- crosstab(df_jockeys,
                    row.vars='jockey',
                    col.vars='race_type',
                    type='f')

cross.3 <- crosstab(df_jockeys,
                    row.vars='jockey',
                    col.vars='course_type',
                    type='f')


# ------------------------------------------------------------------------------
# --------------- Normalise path by average vector of directions and Add speed
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
# --------------- Outliers, Injuries, and Smoothing
# ------------------------------------------------------------------------------

# --- Outliers

df_main.add <- readRDS('df_main.add.rds')

df_main.add$speed <- as.numeric(df_main.add$speed)


# TODO: Fill only NAs with 1-2 missings

t <- df_main.add %>% filter(df_main.add$speed > 30)
unique(t$race_id)


# TODO: Limit speed to 30 m/s. Set speed higher than that to NA
sum(df_main.add$speed > 30)


t <- df_main.add %>% filter(speed > 50)

# Speed is equal to 0 for a dozen of frames and then sudden surge. Drop these cases
t2 <- df_main.add %>% filter(horse_race_id == 'BEL_2019-07-04_3_2')
t3 <- df_main.add %>% filter(horse_race_id == 'AQU_2019-03-15_9_7')
t5 <- df_main.add %>% filter(horse_race_id == 'BEL_2019-10-02_5_2')
t7 <- df_main.add %>% filter(horse_race_id == 'AQU_2019-12-07_1_8')
t8 <- df_main.add %>% filter(horse_race_id == 'BEL_2019-06-14_3_2')
t9 <- df_main.add %>% filter(horse_race_id == 'SAR_2019-08-22_5_10')
t10 <- df_main.add %>% filter(horse_race_id == 'AQU_2019-12-01_4_9')

# Sudden surge in the last frame
t4 <- df_main.add %>% filter(horse_race_id == 'AQU_2019-12-07_3_5')
t6 <- df_main.add %>% filter(horse_race_id == 'SAR_2019-08-11_10_3')


t11 <- df_main.add %>% filter(horse_race_id == 'BEL_2019-06-19_9_2')


hist((df_main.add %>% filter(speed < 30))$speed, breaks=100)

quantile(df_main.add$speed, 0.999999)

hist(df_main.add$speed, breaks=100)


# --- Injuries

# TODO: Remove a horse with injury from ego-calculations

race_program_trakus <- df_main.add %>%
  group_by(race_id, program_number) %>%
  summarise(max_trakus_index=max(trakus_index))
race_program_trakus <- race_program_trakus %>%
  tidyr::pivot_wider(names_from=program_number, values_from=max_trakus_index) %>%
  ungroup() %>% data.frame()

race_program_trakus$max_frame <- race_program_trakus %>%
  dplyr::select(-race_id) %>% apply(1, max, na.rm=TRUE)

race_program_trakus$injury <- rowSums(race_program_trakus!=race_program_trakus$max_frame, na.rm=T) > 1

sum(race_program_trakus$injury)

# race_injuries <- data.frame(race_id=race_program_trakus$race_id,
#                             injury=rowSums(race_program_trakus!=race_program_trakus$max_frame, na.rm=T) > 1)


# --- Add smoothing

# TODO:

# Plot one race
t <- df_main.add %>% filter(horse_race_id=='AQU_2019-01-01_9_10')
t <- df_main.add %>% filter(horse_race_id=='AQU_2019-01-01_9_11')
plot(t$trakus_index, t$speed)


# ------------------------------------------------------------------------------
# --------------- Calculate acceleration
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
# --------------- Cut by finish line
# ------------------------------------------------------------------------------

# TODO: Use 'distance_id' to calculate percentage of completed track

meters_distance <- df_main.add %>%
  group_by(horse_race_id) %>%
  summarise(meters=sum(speed)/4,
            distance=unique(distance_id)*2.01168)

plot(meters_distance$meters, meters_distance$distance)


# ------------------------------------------------------------------------------
# --------------- Weighted Mean
# ------------------------------------------------------------------------------

# TODO: weighted mean

# --- Plot one race for one horse
sf_race <- sf_main %>% filter(race_id=="BEL_2019-05-25_10")
plot(sf_race['trakus_index'])

# --- Plot one frame for all horses
sf_frame <- sf_race %>% filter(trakus_index==55)

sf_frame$index <- rownames(sf_frame)
plot(sf_frame['index'], pch = 18)

ggplot(sf_frame) +
  geom_sf() +
  geom_sf_label(aes(label = index))

distance_to_points <- st_distance(sf_frame[1,], sf_frame)

distance_to_points <- as.numeric(distance_to_points)

distance_to_points <- distance_to_points[distance_to_points > 0] # Remove EGO

inv_distance_to_points <- 1 / (distance_to_points)^2 # Can raise to different powers

weights_from_inv_distance <- inv_distance_to_points / sum(inv_distance_to_points)

# Try with odds, replace to speed and accleration
input_alters <- sf_frame$odds[2:length(sf_frame$odds)]

mean(input_alters)
weighted.mean(input_alters, weights_from_inv_distance)


# ------------------------------------------------------------------------------
# --------------- Distinguish between straight segments and turns
# ------------------------------------------------------------------------------

# TODO: Based on the changes in 'angle'

t <- df_main.add %>% filter(horse_race_id=='AQU_2019-01-01_9_10')
plot(t$trakus_index, t$angle)


plot(t$trakus_index[2:nrow(t)], abs(diff(t$angle)))


# ------------------------------------------------------------------------------
# --------------- Calculating variables for ego and alters
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

df_ego_alters <- readRDS('df_ego_alters.rds')

df_ego_alters.sample <- df_ego_alters %>% sample_n(100000)

df_ego_alters.sample$output <- exp(df_ego_alters.sample$y_rotated_diff*1000)

hist(df_ego_alters.sample$y_rotated_diff, breaks=100)
plot(df_ego_alters.sample$y_rotated_diff,
     df_ego_alters.sample$output)


df_ego_alters.sample$output <- (df_ego_alters.sample$y_rotated_ego*100 /
                                  df_ego_alters.sample$y_rotated_alter*100)

plot(df_ego_alters.sample$y_rotated_diff,
     df_ego_alters.sample$output)

plot(df_ego_alters.sample$speed_ego, df_ego_alters.sample$speed_alters)


test_output <- rnorm(10000)

hist(test_output)


hist(exp(test_output), breaks=100)

max(test_output)
exp(max(test_output))
exp(min(test_output))

hist(exp(test_output), breaks=100)


# ------------------------------------------------------------------------------
# --------------- Plots and animation
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





























