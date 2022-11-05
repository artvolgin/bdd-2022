
library(ggplot2)
library(sf)
library(units)
library(lme4)
library(gganimate)
library(magrittr)
library(move)
library(data.table)
library(ggmap)
library(stringr)
# library(dplyr)
# install.packages('tidyverse')
library(tidyverse)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


USERNAME <- Sys.getenv("USERNAME")
setwd(paste0('C:/Users/', USERNAME, '/YandexDisk/COMPETITIONS/bdd_2022/data'))


# ------------------------------------------------------------------------------
# ------ 0. Loading and Preprocessing
# ------------------------------------------------------------------------------

# --- df_main
df_main <- readRDS('df_main.rds')
df_fixed_coefs <- readRDS('df_fixed_coefs_full.rds')
df_random_slopes <- readRDS('df_random_slopes.rds')


# ------------------------------------------------------------------------------
# ------ 1. Structural model
# ------------------------------------------------------------------------------

# TODO:


# ------------------------------------------------------------------------------
# ------ 2. Structural model with coefficients + Random effects
# ------------------------------------------------------------------------------

# TODO:



# ------------------------------------------------------------------------------
# ------ 3. Errorplots. Random effects
# ------------------------------------------------------------------------------

df_random_slopes <- readRDS('df_random_slopes.rds')
df_random_slopes.full <- df_random_slopes %>% filter(model=='multilevel_full')

# Select only top-10 and bottom-10
df_random_slopes.full <- rbind(head(df_random_slopes.full, 10), tail(df_random_slopes.full, 10))


# plotting random effects
ggplot(data = df_random_slopes.full,
       aes(x = Estimate,
           y = reorder(jockey, Estimate),
           color = model)) +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper,
                     height = 0.3),
                 size = 0.75,
                 color = 'gray25' #,
                 #linetype=errorbar_linetype
  ) +

  geom_point(pch=21, size=1,
             position = position_dodge(width = 0.5)) +

  theme_minimal() +
  theme(axis.text.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(size = 16, face = "bold"))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.4) +
  labs(y = "", x = '')  +
  xlim(0.05, .17) +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        legend.position = "bottom") +
  ggtitle('Random Effects by Jockeys')



# ------------------------------------------------------------------------------
# ------ 4. Animation with two layouts. Speed time-series and movement of players
# ------------------------------------------------------------------------------

# TODO: Remove last ~5 frames

# TOP-3 Jockeys:
# BOTTOM-3 Jockeys:

# Jomar Torres




df_jockeys_wins <- df_main %>% distinct(jockey, race_id, .keep_all = T) %>% group_by(jockey) %>%
  summarise(mean_position_at_finish=mean(position_at_finish),
            first_places=sum(position_at_finish==1),
            top3=sum(position_at_finish<=3),
            games_n=n())
df_random_slopes <- df_random_slopes %>% left_join(df_jockeys_wins)
df_random_slopes$mean_position_at_finish
df_random_slopes$first_places_prop <- df_random_slopes$first_places / df_random_slopes$games_n
df_random_slopes$top3_prop <- df_random_slopes$top3 / df_random_slopes$games_n

df_jockeys_relative_pos <- df_main %>%
  group_by(jockey) %>%
  summarise(mean_relative_pos=mean(relative_pos, na.rm=T),
            mean_odds=mean(odds, na.rm=T))
df_random_slopes <- df_random_slopes %>% left_join(df_jockeys_relative_pos)


selected_names <- c('Javier Castellano',
                    'Heman K. Harkie',
                    'Declan Carroll',
                    'John R. Velazquez')
df_random_slopes$jockey <- ifelse(
  df_random_slopes$jockey %in% selected_names, df_random_slopes$jockey, NA)
point_size <- log(df_random_slopes$games_n)

ggplot(df_random_slopes, aes(x = Estimate, y = top3_prop, label=jockey)) +
  geom_point(size=point_size, alpha=0.3) +
  geom_text(hjust = 0.5, nudge_y = 0.015) +
  geom_hline(yintercept = mean(df_random_slopes$top3_prop)) +
  geom_vline(xintercept = mean(df_random_slopes$Estimate)) +
  theme_classic()



# TODO: Add movement vectors
# TODO: Add label horse/jockey to ego. Others as grey dots. Color of dots = speed/accel
# TODO: Add changes of speed/accel, speed/accel_env, relative_position


# ---------------- 2.1 Animate speed trajectories (difference)

# TURN: 'Manuel Franco', 'Luis R. Reyes'


selected_jockey <- 'Joey R. Martinez'
race_number <- 5

df_jockeys <- df_main %>% filter(jockey==selected_jockey)

df_race <- df_main %>% filter(race_id == unique(df_jockeys$race_id)[5])


# normalize <- function(df) {
#
#   race_distance_pct <- df$race_distance_pct
#   df$relative_pos_2 <- (race_distance_pct- min(race_distance_pct)) /
#     (max(race_distance_pct)-min(race_distance_pct))
#
#   return(df)
# }
#
# df_race <- as.data.table(df_race)
#
# df_race <- (df_race[, normalize(.SD),  by = .(trakus_index)])

# df_race <- df_race %>% group_by(trakus_index) %>%
#   group_modify(~ normalize(.x))



df_horse_race <- df_race %>% filter(jockey == selected_jockey)

df_horse_race$speed <- as.numeric(df_horse_race$speed)
df_horse_race$speed_env <- as.numeric(df_horse_race$speed_env)
df_horse_race$speed_diff <- df_horse_race$speed - df_horse_race$speed_env

# TODO: Add smoothing to difference
# TODO: Add area coloring beneath the line

# df_horse_race_long <- df_horse_race_long %>% filter(trakus_index < 30)

nFrames <- max(df_horse_race$trakus_index)
anim.1 <- ggplot(df_horse_race, aes(x=trakus_index, y=speed_diff)) +

  geom_line() +
  geom_hline(yintercept = 0, color='red', size=1) +
  transition_reveal(trakus_index) +
  # view_follow() +
  theme_classic() +
  labs(title = 'Frame : {frame}')

# gganimate::animate(anim.1, width = 720, height = 440, fps = 10, renderer = av_renderer(),
#                    nframe = nFrames)

gif.1 <- gganimate::animate(anim.1, width = 720, height = 440, fps = 10, nframe = nFrames)




# ---------------- 2.2 Animate spatial movement

# df_race <- df_race_jockeys %>% filter(race_id == selected_race_id)

df_race$selected_horse <- (df_race$jockey == selected_jockey)

df_race$speed <- as.numeric(df_race$speed)

df_race$x_lead <- lead(df_race$x)
df_race$y_lead <- lead(df_race$y)
df_race$x_lead <- rowMeans(df_race[,c('x', 'x_lead')])
df_race$y_lead <- rowMeans(df_race[,c('y', 'y_lead')])

# df_race$x_lead <- lead(df_race$x_rotated)
# df_race$y_lead <- lead(df_race$y_rotated)
# df_race$x_lead <- rowMeans(df_race[,c('x_rotated', 'x_lead')])
# df_race$y_lead <- rowMeans(df_race[,c('y_rotated', 'y_lead')])


library(stringr)

# df_race[!df_race$selected_horse,]$relative_pos <- NA

nFrames <- max(df_race$trakus_index)
anim.2 <- ggplot(df_race, aes(x, y)) +
  #adding players
  # geom_point(alpha = 0.7, aes(colour = horse_name), size=10) +
  geom_point(alpha = 1,
             aes(color = selected_horse), size =18) +

  geom_text(aes(label=relative_pos)) +
  # geom_text(aes(label=program_number)) +

  scale_color_manual(values = c("lightgrey","red")) +

  # geom_text(hjust=0.5, vjust=-2.7, fontface='bold') +

  # geom_segment(aes(yend = y_lead+0.000005, xend = x_lead+0.000005),
  #              size = 1, color='grey', arrow = arrow(length = unit(0.03, "npc"))) +

  geom_segment(aes(yend = y_lead, xend = x_lead),
               size = 1, color='grey', arrow = arrow(length = unit(0.03, "npc"))) +

  #setting animation parameters
  transition_time(trakus_index)  +
  ease_aes('linear') +
  labs(title = paste0(str_replace_all(df_race$race_id[1], '_', ', '), '. Trakus Index: {frame}')) +
  # theme_void() +
  view_follow() +
  theme_minimal() +
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size=14))


gif.2 <- gganimate::animate(anim.2, width = 720, height = 440, fps = 10, nframe = nFrames)

# gif.2 <- gganimate::animate(anim.2, width = 720, height = 440, fps = 10,
#                             renderer = av_renderer(),  nframe = nFrames)


library(animation)

# a <- gganimate::animate(anim.2, width = 720, height = 440, fps = 10,
#                         renderer = av_renderer(),  nframe = nFrames)
# anim_save("test_1.mp4", a)


# --- Combine in one animation
library(magick)

mgif.1 <- image_read(gif.1)
mgif.2 <- image_read(gif.2)

mgif.combined <- image_append(c(mgif.1[1], mgif.2[1]))
for(i in 2:length(mgif.1)){
  combined <- image_append(c(mgif.1[i], mgif.2[i]))
  mgif.combined <- c(mgif.combined, combined)
}

# mgif.combined
# image_write(mgif.combined, path = "combined.gif", format = "gif")
image_write(mgif.combined,
            path = paste0(selected_jockey, ' ', as.character(race_number),
                          '.gif'), format = "gif")


# ------------------------------------------------------------------------------
# ------ 4. Scatterplots. Random effects by jockey/horse variables
# ------------------------------------------------------------------------------

# TODO:


# ------------------------------------------------------------------------------
# ------ 5. Heatmaps. Random effects by jockey/horse variables. Split by phase of the race
# ------------------------------------------------------------------------------

df_random_slopes.long <- df_random_slopes %>%
  dplyr::select(jockey, Estimate, model) %>%
  tidyr::pivot_wider(names_from = 'model', values_from = 'Estimate') %>%
  data.frame()

rownames(df_random_slopes.long) <- df_random_slopes.long$jockey
df_random_slopes.long$jockey <- NULL

df_random_slopes.long <-  df_random_slopes.long %>% arrange(-multilevel_full)

# install.packages("pheatmap")
library(pheatmap)

library(devtools)
# install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)

df_random_slopes.long <- rbind(head(df_random_slopes.long, 10),
                               c(NA, NA, NA),
                               tail(df_random_slopes.long, 10))
rownames(df_random_slopes.long)[11] <- ''

mat <- as.matrix(df_random_slopes.long)

ComplexHeatmap::pheatmap(mat, cellwidth=60, cellheight =15,
         color = colorRampPalette(c("white", "firebrick3"))(50),
         cluster_rows=F, cluster_cols=F, display_numbers = F,
         legend=F,
         column_names_side = c("top"),
         cell_fun = function(j, i, x, y, width, height, fill) {
           if(!is.na(mat[i, j]))
             grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 10))
         })




# ------------------------------------------------------------------------------
# ------ 6. Plot with one frame, all jockeys, their movement vectors, and scale
# ------------------------------------------------------------------------------


df_race <- df_main %>% filter(race_id == 'AQU_2019-01-01_9')

selected_horse_race_id <- "AQU_2019-01-01_9_4"

df_race$selected_horse <- (df_race$horse_race_id == selected_horse_race_id)
df_race$speed <- as.numeric(df_race$speed)
df_race$x_lead <- lead(df_race$x)
df_race$y_lead <- lead(df_race$y)
df_race$x_lead <- rowMeans(df_race[,c('x', 'x_lead')])
df_race$y_lead <- rowMeans(df_race[,c('y', 'y_lead')])

# df_race$x_lead <- mean(lead(df_race$x), df_race$x, na.rm=T)
# df_race$y_lead <- mean(lead(df_race$y), df_race$y, na.rm=T)

movement_vec.start.x <- -73.82985
movement_vec.start.y <- 40.67532
movement_vec.end.x <- -73.83007
movement_vec.end.y <- 40.67515

df_frame <- df_race %>% filter(trakus_index == 175) # 125

df_frame$mean_theta <- mean(df_frame$theta)

selected_trakus <- df_frame$trakus_index[1]

line_to_movement_vec.1.start.x <- df_frame[df_frame$jockey == 'Luis R. Reyes',]$x
line_to_movement_vec.1.start.y <- df_frame[df_frame$jockey == 'Luis R. Reyes',]$y
line_to_movement_vec.1.end.x <- line_to_movement_vec.1.start.x - 0.000034*cos(2.27)
line_to_movement_vec.1.end.y <- line_to_movement_vec.1.start.y - 0.000034*sin(2.27)

line_to_movement_vec.2.start.x <- df_frame[df_frame$jockey == 'Joel Sone',]$x
line_to_movement_vec.2.start.y <- df_frame[df_frame$jockey == 'Joel Sone',]$y
line_to_movement_vec.2.end.x <- line_to_movement_vec.2.start.x - 0.000036*cos(2.27)
line_to_movement_vec.2.end.y <- line_to_movement_vec.2.start.y - 0.000036*sin(2.27)

line_to_movement_vec.3.start.x <- df_frame[df_frame$jockey == 'Andre Shivnarine Worrie',]$x
line_to_movement_vec.3.start.y <- df_frame[df_frame$jockey == 'Andre Shivnarine Worrie',]$y
line_to_movement_vec.3.end.x <- line_to_movement_vec.3.start.x - 0.000027*cos(2.27)
line_to_movement_vec.3.end.y <- line_to_movement_vec.3.start.y - 0.000027*sin(2.27)


(df_frame %>% filter(jockey == 'Joel Sone'))$relative_pos

vec_show_jockeys <- c('Luis R. Reyes', 'Joel Sone', 'Andre Shivnarine Worrie')
df_frame$jockey <- ifelse(df_frame$jockey %in% vec_show_jockeys, df_frame$jockey, NA)



ggplot(df_frame, aes(x, y, label=jockey)) +

  #adding players
  geom_point(alpha = 1,
             aes(color = selected_horse), size =18) +

  scale_color_manual(values = c("lightgrey","red")) +

  geom_text(hjust=0.5, vjust=-2.7, fontface='bold') +

  geom_segment(aes(yend = y_lead+0.000005, xend = x_lead+0.000005),
               size = 1, color='grey', arrow = arrow(length = unit(0.03, "npc"))) +

  # geom_segment(aes(yend = y + 0.00002*cos(theta), xend = x + 0.00002*sin(theta)),
  #              size = 1.5, color='grey', arrow = arrow(length = unit(0.02, "npc"))) +


  labs(title = paste0(str_replace_all(
    df_race$race_id[1], '_', ', '),
    '. Trakus Index: ',
    selected_trakus)) + # TODO

  theme_minimal() +

  # Movement vector
  geom_segment(aes(x = movement_vec.start.x,
                   y = movement_vec.start.y,
                   xend = movement_vec.end.x,
                   yend = movement_vec.end.y),
               linetype = 'dashed',
               size = 0.7) +

  # 1. Line from player to movement vector
  geom_segment(aes(x = line_to_movement_vec.1.start.x,
                   y = line_to_movement_vec.1.start.y,
                   xend = line_to_movement_vec.1.end.x,
                   yend = line_to_movement_vec.1.end.y),
               linetype = 'dashed',
               color='lightgrey',
               size = 1) +
  annotate("text",
           x=line_to_movement_vec.1.end.x + 0.00001,
           y=line_to_movement_vec.1.end.y,
           label= "100%", size = 6, angle = 40) +

  # 2. Line from player to movement vector
  geom_segment(aes(x = line_to_movement_vec.2.start.x,
                   y = line_to_movement_vec.2.start.y,
                   xend = line_to_movement_vec.2.end.x,
                   yend = line_to_movement_vec.2.end.y),
               linetype = 'dashed',
               color='lightgrey',
               size = 1) +
  annotate("text",
           x=line_to_movement_vec.2.end.x + 0.00001,
           y=line_to_movement_vec.2.end.y,
           label= "42%", size = 6, angle = 40, color='red') +

  # 3. Line from player to movement vector
  geom_segment(aes(x = line_to_movement_vec.3.start.x,
                   y = line_to_movement_vec.3.start.y,
                   xend = line_to_movement_vec.3.end.x,
                   yend = line_to_movement_vec.3.end.y),
               linetype = 'dashed',
               color='lightgrey',
               size = 1) +
  annotate("text",
           x=line_to_movement_vec.3.end.x + 0.00001,
           y=line_to_movement_vec.3.end.y,
           label= "0%", size = 6, angle = 40) +

  # Average movement vector
  annotate("text",
           x=-73.8300,
           y= 40.675195,
           label= "Relative postition", size = 6,
           angle = 40, fontface='bold') +


  theme(plot.title = element_text(hjust = 0.5, size=18),
        legend.position="none") +

  xlim(-73.83010, -73.82980) +
  ylim(40.67515, 40.67533)


# geom_segment(aes(yend = y + 0.00001*cos(theta), xend = x + 0.00001*sin(theta)),
#              size = 1.5, color='grey', arrow = arrow(length = unit(0.05, "npc"))) +






# ---------------- 2.1 Animate speed trajectories

df_race_jockeys <- df_main %>% filter(jockey == "Edward Escobar")

selected_horse_race_id <- unique(df_race_jockeys$horse_race_id)[1]


df_horse_race <- df_race_jockeys %>% filter(horse_race_id == selected_horse_race_id)
selected_race_id <- df_horse_race$race_id[1]

df_horse_race$speed <- as.numeric(df_horse_race$speed)
df_horse_race$speed_env <- as.numeric(df_horse_race$speed_env)

df_horse_race <- df_horse_race %>% dplyr::select(trakus_index, speed, speed_env)
df_horse_race_long <- tidyr::pivot_longer(df_horse_race, cols = c('speed', 'speed_env'), )


# df_horse_race_long <- df_horse_race_long %>% filter(trakus_index < 30)

nFrames <- max(df_horse_race_long$trakus_index)
anim.1 <- ggplot(df_horse_race_long, aes(x=trakus_index, y=value, color=name)) +

  geom_line() +
  transition_reveal(trakus_index) +
  view_follow() +
  theme_classic() +
  labs(title = 'Frame : {frame}')

# gganimate::animate(anim.1, width = 720, height = 440, fps = 10, renderer = av_renderer(),
#                    nframe = nFrames)

gif.1 <- gganimate::animate(anim.1, width = 720, height = 440, fps = 10, nframe = nFrames)

# TODO: Speed difference and Relative position (two separate Y-axis ticks)?



