
library(ggplot2)
library(dplyr)
library(sf)
library(units)
library(lme4)
library(magrittr)
library(OpenMx)

# install.packages('dplyr')

# wd
USERNAME = Sys.getenv("USERNAME")
setwd(paste0('C:/Users/', USERNAME, '/YandexDisk/COMPETITIONS/bdd_2022/data'))

# data
df_main = readRDS('df_main.rds')

# cor(df_main$trakus_index, df_main$race_distance_pct)
# cor(df_main$trakus_index, df_main$theta_change, use = 'complete.obs')
# cor(df_main$race_distance_pct, df_main$theta, use = 'complete.obs')
# cor(df_main$speed, df_main$speed_env, use = 'complete.obs')
# cor(df_main$accel, df_main$accel_env, use = 'complete.obs')

# filtering active jockeys
#df_main$jockey_horse_id = paste0(df_main$jockey, '_', df_main$horse_id)

#   clusters = as.data.frame(table(df_main$jockey))
#   nrow(clusters)
#   clusters =  clusters  %>%
#     filter(Freq > 2000) %>%
#     dplyr::select(jockey = Var1)
#   #clusters$jockey = as.character(clusters$jockey)
#   clusters$id = 1:nrow(clusters)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CHANGE DEP. VARIABLES
# df_main$relative_pos <- df_main$y_rotated_diff *1000
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CHANGE DEP. VARIABLES

# selecting variables for the modelling
df = df_main %>%
  dplyr::select(jockey,
                speed_env,
                speed_lag_env,
                speed,
                speed_lag,
                relative_pos = relative_pos,
                race_distance_pct,
                trakus_index,
                relative_pos_orig = relative_pos,
                y_rotated_diff,
                race_id,
                horse_race_id,
                purse,
                odds,
                position_at_finish,
                theta_change,
                straight_line) %>% na.omit %>%
  dplyr::mutate(speed_diff = speed - speed_env) %>%
  # left_join(clusters, by = 'jockey') %>%
  na.omit

df_jockeys <- df %>% distinct(jockey, race_id, .keep_all = T) %>%
  group_by(jockey) %>%
  summarise(games_n=n())

keep_jockeys <- (df_jockeys %>% filter(games_n>=5))$jockey # >2 is fine
df <- df %>% filter(jockey %in% keep_jockeys)


# ~~~~ Remove jockeys with NA's in random slopes std.error for TURN model
# remove_jockeys <- c("Keivan Serrano",
#                     "Christopher P. DeCarlo",
#                     "Mike E. Smith",
#                     "Kirk Johnson", "Mychel J. Sanchez", "Daniel Centeno",
#                     "Edwin Rivera", "Mario Gutierrez", "Kevin Carmona",
#                     "Cristina Bonilla", "Tyler Conner", "Adam Beschizza",
#                     "Trevor McCarthy", "Jose Baez", "Anthony Salgado", "Katie Davis",
#                     "Wilmer A. Garcia", "Emmanuel Esquivel")
# df <- df %>% filter(!(jockey %in% remove_jockeys))
# ~~~~ Remove jockeys with NA's in random slopes std.error for TURN model



df$id <- as.numeric(as.factor(df$jockey))
# df$id <- as.numeric(as.factor(df$horse_race_id))

df = as.data.frame(df)

# table(df$id)
# length(table(df$id))

# how many races for each jockey
df_sum = df %>% group_by(jockey, race_id) %>%
  summarise(n_by_races = n()) %>%
  group_by(jockey) %>% summarise(n = n())
df %<>% left_join(df_sum)

# scaling
df[,2:7] = sapply(df[,2:7], as.numeric)
# df[,2:5] = sapply(df[,2:5], scale)
# summary(df)


# Modelling
manifestVars = names(df)[2:7]

numManifest = length(manifestVars)
numSubjects = length(unique(df$jockey))

df = as.data.frame(df)
df_multilevel = df %>% dplyr::select(id, all_of(manifestVars))

df_multilevel.straight = df %>% filter(straight_line == T) %>% dplyr::select(id, all_of(manifestVars))
df_multilevel.turn = df %>% filter(straight_line == F) %>% dplyr::select(id, all_of(manifestVars))

# table(df_multilevel$id)


# syntax function
MultilevelOpenmx = function(dat = df_multilevel,name = NULL){


  # --- model syntax

  # N - number of observations
  # k - number of manifest vars
  multilevel_syntax = mxModel(paste0("multilevel", '_', name),
                           # matrix of random effects for each subject, N x k
                           mxMatrix("Full",
                                    nrow = numSubjects,
                                    ncol = 2,
                                    values = c(.5, .5),
                                    free = T,
                                    name = "Rand",
                                    byrow = T
                           ),
                           # matrix of regressions, k x k
                           mxMatrix("Full",
                                    numManifest,
                                    numManifest,
                                    labels = c(NA,NA,NA,NA,NA,'z3',
                                               'b1',NA,'b4',NA,NA,'z1',
                                               NA,NA,NA,NA,NA,'z4',
                                               'b3',NA,'b2',NA,NA,'z2',
                                               NA,'b5',NA,"randrow[1,1]",NA, NA,
                                               NA,NA,NA,NA,NA,NA),
                                    free = c(F,F,F,F,F,T,
                                             T,F,T,F,F,T,
                                             F,F,F,F,F,T,
                                             T,F,T,F,F,T,
                                             F,T,F,F,F,F,
                                             F,F,F,F,F,F),
                                    name = "A",
                                    byrow = T
                           ),
                           # matrix of variance-covariance where covars are 0, k x k
                           mxMatrix("Symm",
                                    numManifest,
                                    numManifest,
                                    values = c(1,
                                               0, 1,
                                               .5, 0, 1,
                                               0, .5, 0, 1,
                                               0, 0, 0, 0, 1,
                                               0, 0, 0, 0, 0, 1),
                                    free = c(T,
                                             F, T,
                                             T, F, T,
                                             F, T, F, T,
                                             F, F, F, F, T,
                                             F, F, F, F, F, T),
                                    labels = c('var_input1_t1',
                                               NA, 'var_input1_t2',
                                               'cov_input12_t1', NA, 'var_input2_t1',
                                               NA, 'cov_input12_t2', NA, 'var_input2_t2',
                                               NA, NA, NA, NA, 'var_output',
                                               NA, NA, NA, NA, NA, 'var_dist'),
                                    name = "S",
                                    byrow = T
                           ),
                           # filter matrix = identity since there are no latvars, k x k
                           mxMatrix("Full",
                                    numManifest,
                                    numManifest,
                                    values = c(1,0,0,0,0,0,
                                               0,1,0,0,0,0,
                                               0,0,1,0,0,0,
                                               0,0,0,1,0,0,
                                               0,0,0,0,1,0,
                                               0,0,0,0,0,1),
                                    free = F,
                                    byrow = T,
                                    name = "F"
                           ),
                           mxMatrix("Iden",
                                    numManifest,
                                    name = "I"),
                           # solving for covariance matrix
                           mxAlgebra(F %*% solve(I-A) %*% S %*% t(solve(I-A)) %*% t(F),
                                     name = "R",
                                     dimnames = list(manifestVars, manifestVars)
                           ),
                           # matrix of random intercepts - fixed to 0, k x 1
                           mxMatrix("Full",
                                    nrow = 1,
                                    ncol = length(manifestVars),
                                    values = 0,
                                    free = c(F,T,F,T,F,F),
                                    labels = c(NA,'mean1',NA,'mean2',"randrow[1,2]", NA),
                                    dimnames = list(NULL, manifestVars),
                                    name = "M"
                           ),
                           # algebra for extracting random effects
                           mxAlgebra(Rand[data.id,], name = "randrow"),
                           mxFitFunctionML(),
                           mxExpectationNormal(covariance = "R", means = "M"),
                           mxData(dat, type = 'raw')
                           )

  return(multilevel_syntax)
}

# fitting models for random effects
gc()
multilevel_full_syntax = MultilevelOpenmx(dat = df_multilevel, name = 'full')
multilevel_full_fit = mxRun(multilevel_full_syntax)
summary(multilevel_full_fit)
summary_list_full = summary(multilevel_full_fit, verbose = T)

# straight
multilevel_1_syntax = MultilevelOpenmx(dat = df_multilevel.straight, name = 'straight')
multilevel_1_fit = mxRun(multilevel_1_syntax)
summary(multilevel_1_fit)
summary_list_1 = summary(multilevel_1_fit, verbose = T)

# turn
multilevel_2_syntax = MultilevelOpenmx(dat = df_multilevel.turn, name = 'turn')
multilevel_2_fit = mxRun(multilevel_2_syntax)
summary(multilevel_2_fit)
summary_list_2 = summary(multilevel_2_fit, verbose = T)

# multilevel_1_syntax = MultilevelOpenmx(name = '1', probability = c(0, .33))
# multilevel_1_fit = mxRun(multilevel_1_syntax)
# summary(multilevel_1_fit)
# summary_list_1 = summary(multilevel_1_fit, verbose = T)
#
# multilevel_2_syntax = MultilevelOpenmx(name = '2', probability = c(.4, .6))
# multilevel_2_fit = mxRun(multilevel_2_syntax)
# summary(multilevel_2_fit)
# summary_list_2 = summary(multilevel_2_fit, verbose = T)
#
# multilevel_3_syntax = MultilevelOpenmx(name = '3', probability = c(.66, 1))
# multilevel_3_fit = mxRun(multilevel_3_syntax)
# summary(multilevel_3_fit)
# summary_list_3 = summary(multilevel_3_fit, verbose = T)

# fixed effects
fixed_coefs = summary_list_full[["parameters"]]
saveRDS(fixed_coefs, 'df_fixed_coefs_full.rds')
fixed_coefs = fixed_coefs[((2*numSubjects)+1):nrow(fixed_coefs),c(1,5,6)]
fixed_coefs$lower = fixed_coefs$Estimate - 1.96*fixed_coefs$Std.Error
fixed_coefs$upper = fixed_coefs$Estimate + 1.96*fixed_coefs$Std.Error

# random effects
rand_extract = function(level_id = 'jockey',
                        name = NULL,
                        summary_list = NULL){

  rand_coefs = summary_list[["parameters"]][1:(2*numSubjects),c(1,5,6)]
  rand_coefs$lower = rand_coefs$Estimate - 1.96*rand_coefs$Std.Error
  rand_coefs$upper = rand_coefs$Estimate + 1.96*rand_coefs$Std.Error
  rand_coefs$rand_intercept = grepl("2]", rand_coefs$name)
  rand_coefs$id = as.numeric(gsub(paste0(name,'.Rand\\[|\\]|,1|,2'), '', rand_coefs$name))
  rand_coefs$name = NULL
  rand_coefs %<>% left_join(unique(df[,c(level_id, 'id')]), by = 'id')
  rand_coefs %<>% arrange(desc(Estimate))
  rand_coefs$model = name

  return(rand_coefs)
}

rand_coefs_full = rand_extract(name = 'multilevel_full', summary_list = summary_list_full)
rand_coefs_1 = rand_extract(name = 'multilevel_straight', summary_list = summary_list_1)
rand_coefs_2 = rand_extract(name = 'multilevel_turn', summary_list = summary_list_2)
#rand_coefs_3 = rand_extract(name = 'multilevel_3', summary_list = summary_list_3)
#
rand_all = rbind.data.frame(rand_coefs_full,
                            rand_coefs_1,
                            rand_coefs_2)
#rand_all %<>% na.omit

# Random slopes
df_slopes = rand_all[rand_all$rand_intercept == FALSE,]

# df_slopes <- df_slopes %>% filter(model != 'multilevel_turn')
# Save to RDS
saveRDS(df_slopes, 'df_random_slopes.rds')


