
library(ggplot2)
library(dplyr)
library(sf)
library(units)
library(lme4)
library(magrittr)
library(OpenMx)
#library(umx)

# wd
USERNAME = Sys.getenv("USERNAME")
setwd(paste0('C:/Users/', USERNAME, '/YandexDisk/COMPETITIONS/bdd_2022/data'))

# data
df_ego_alters = readRDS('df_ego_alters.rds')
df_main.add = readRDS('df_main.add.rds')
ids = df_main.add %>%
  select(jockey, horse_id, horse_race_id)
ids = ids[!duplicated(ids$horse_race_id),]
length(table(ids$jockey))
length(table(ids$horse_id))
length(unique(ids$horse_race_id))

# adding ids
df_ego_alters %<>% left_join(ids)
clusters = as.data.frame(table(df_ego_alters$horse_race_id))
nrow(clusters)
summary(clusters$Freq)
clusters$id = 1:nrow(clusters)
clusters =  clusters  %>%
  filter(Freq < 300)
sample = clusters[1:100, c('Var1', 'id')] %>%
  select(horse_race_id = Var1, id)


# selecting variables for the modelling
df = df_ego_alters %>%
  dplyr::select(jockey,
                speed_alters,
                speed_alters_lag,
                speed_ego,
                speed_ego_lag,
                output = y_rotated_diff,
                trakus_index,
                output_orig = y_rotated_diff,
                race_id,
                speed_diff) %>% na.omit

df = as.data.frame(df)
#df$id = as.factor(df$id)
table(df$id)
length(table(df$id))

# how many races for each jockey
df_sum = df %>% group_by(jockey, race_id) %>%
  summarise(n_by_races = n()) %>%
  group_by(jockey) %>% summarise(n = n())
df %<>% left_join(df_sum)

# filtering negative output
df[,2:6] = sapply(df[,2:6], as.numeric)
df$filt = ifelse(df$output < 0, 0, 1)
# table(df$filt)
df = df[df$filt > 0,]

# scaling
df[,2:6] = sapply(df[,2:6], scale)

# not less than 3 races
df %<>% filter(n>3)
length(table(df$jockey))

# total n of rows by jockeys and races
df %<>% group_by(jockey, race_id) %>%
  mutate(n_rows = n())
table(df$n_rows)
hist(df$n_rows)
df %<>% filter(n_rows > 30) %>% ungroup()

# Modelling
manifestVars = names(df)[2:7]
numManifest = length(manifestVars)
numSubjects = length(unique(df$jockey))

keys = cbind.data.frame(jockey = levels(as.factor(df$jockey)), id = 1:numSubjects)
df %<>% left_join(keys)

df$trakus_index = scale(df$trakus_index)

df = as.data.frame(df)
df_multilevel = df %>% select(id, all_of(manifestVars))
table(df_multilevel$id)

# N - number of observations
# k - number of manifest vars

MultilevelRandEgo = mxModel("MultilevelRandEgo",
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
                                               NA, NA, NA, NA, NA, 'var_trakus'),
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
                           mxData(df_multilevel, type = 'raw')
)

MultilevelRandAlters = mxModel("MultilevelRandAlters",
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
                                                NA,'randrow[1,1]',NA,"b6",NA, NA,
                                                NA,NA,NA,NA,NA,NA),
                                     free = c(F,F,F,F,F,T,
                                              T,F,T,F,F,T,
                                              F,F,F,F,F,T,
                                              T,F,T,F,F,T,
                                              F,F,F,T,F,F,
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
                                                NA, NA, NA, NA, NA, 'var_trakus'),
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
                            mxData(df_multilevel, type = 'raw')
)

# fitting models for random effects
gc()
MultilevelRandEgoFit = mxRun(MultilevelRandEgo)
summary(MultilevelRandEgoFit)
summaty_list_ego = summary(MultilevelRandEgoFit, verbose = T)

MultilevelRandAltersFit = mxRun(MultilevelRandAlters)
summary(MultilevelRandAltersFit)
summaty_list_alters = summary(MultilevelRandAltersFit, verbose = T)

# random effects

# ego
rand_coefs_ego = summaty_list_ego[["parameters"]][1:(2*numSubjects),c(1,5,6)]
rand_coefs_ego$lower = rand_coefs_ego$Estimate - 1.96*rand_coefs_ego$Std.Error
rand_coefs_ego$upper = rand_coefs_ego$Estimate + 1.96*rand_coefs_ego$Std.Error
rand_coefs_ego$rand_intercept = grepl("2]", rand_coefs_ego$name)
rand_coefs_ego$id = as.numeric(gsub('MultilevelRandEgo.Rand\\[|\\]|,1|,2', '', rand_coefs_ego$name))
rand_coefs_ego$name = NULL
rand_coefs_ego %<>% left_join(unique(df[,c('jockey', 'id')]))
rand_coefs_ego$model = 'ego'
rand_coefs_ego %<>% arrange(desc(Estimate))

# alters
rand_coefs_alters = summaty_list_alters[["parameters"]][1:(2*numSubjects),c(1,5,6)]
rand_coefs_alters$lower = rand_coefs_alters$Estimate - 1.96*rand_coefs_alters$Std.Error
rand_coefs_alters$upper = rand_coefs_alters$Estimate + 1.96*rand_coefs_alters$Std.Error
rand_coefs_alters$rand_intercept = grepl("2]", rand_coefs_alters$name)
rand_coefs_alters$id = as.numeric(gsub('MultilevelRandAlters.Rand\\[|\\]|,1|,2', '', rand_coefs_alters$name))
rand_coefs_alters$name = NULL
rand_coefs_alters %<>% left_join(unique(df[,c('jockey', 'id')]))
rand_coefs_alters$model = 'alters'

# fixed effects

# ego
fixed_coefs_ego = summaty_list_ego[["parameters"]]
fixed_coefs_ego = fixed_coefs_ego[((2*numSubjects)+1):nrow(fixed_coefs_ego),c(1,5,6)]
fixed_coefs_ego$lower = fixed_coefs_ego$Estimate - 1.96*fixed_coefs_ego$Std.Error
fixed_coefs_ego$upper = fixed_coefs_ego$Estimate + 1.96*fixed_coefs_ego$Std.Error

# alters
fixed_coefs_alters = summaty_list_alters[["parameters"]]
fixed_coefs_alters = fixed_coefs_alters[((2*numSubjects)+1):nrow(fixed_coefs_alters),c(1,5,6)]
fixed_coefs_alters$lower = fixed_coefs_alters$Estimate - 1.96*fixed_coefs_alters$Std.Error
fixed_coefs_alters$upper = fixed_coefs_alters$Estimate + 1.96*fixed_coefs_alters$Std.Error

# plotting random effects

slopes = rbind(rand_coefs_ego[rand_coefs_ego$rand_intercept == FALSE,],
               rand_coefs_alters[rand_coefs_alters$rand_intercept == FALSE,])

ggplot(data = slopes,
                aes(x = Estimate,
                    y = reorder(jockey, Estimate),
                    color = model)) +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper,
                     height = 0.3),
                 size=0.75,
                 color='gray25'#,
                 #linetype=errorbar_linetype
  ) +

  geom_point(pch=21, size=1,
             position = position_dodge(width = 0.5)) +

  theme_minimal() +
  theme(axis.text.y = element_text(color = "black", size = 6),
        axis.text.x = element_text(size = 16, face = "bold"))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.4) +
  labs(y = "", x = '')  +
  xlim(-3, 2) +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        legend.position="bottom") +
  ggtitle('Random Effects by Jockeys')

slopes_cor_df = rand_coefs_ego[rand_coefs_ego$rand_intercept == FALSE,
                               c('jockey', 'Estimate')] %>%
  select(jockey, Estimate_ego = Estimate) %>%
  left_join(rand_coefs_alters[rand_coefs_alters$rand_intercept == FALSE,
                           c('jockey', 'Estimate')])
cor(slopes_cor_df$Estimate_ego, slopes_cor_df$Estimate)

# inspecting

test = df %>% filter(jockey == 'Darren Nagle')
test %<>% filter(race_id == names(table(test$race_id)[3]))
plot(test$trakus_index, test$output_orig)
plot(test$trakus_index, test$speed_diff)
plot(test$trakus_index, test$speed_ego)
plot(test$trakus_index, test$speed_alters)

#-----------------------------------------------------
#---------------------- LMER -------------------------
#-----------------------------------------------------
hist(df_sum$n, breaks = 100)

model_lmer = lmer(output ~ speed_ego + speed_alters + trakus_index + (1 + speed_ego | id), df)
gc()
summary(model_lmer)
performance::icc(model_lmer)

rand_lmer = as.data.frame(ranef(model_lmer, condVar = T))
rand_lmer$grp = as.numeric(as.character(rand_lmer$grp))
rand_lmer$rand_intercept = ifelse(rand_lmer$term == '(Intercept)', 'TRUE_lmer',
                                  'FALSE_lmer')
fixed = as.numeric(fixef(model_lmer)[2])
rand_lmer %<>%
  select(id = grp,
         Estimate = condval,
         Std.Error = condsd,
         rand_intercept) %>%
  mutate(upper = fixed + Estimate + 1.96*Std.Error,
         lower = fixed + Estimate - 1.96*Std.Error,
         Estimate = fixed + Estimate) %>%
  left_join(unique(df[, c('id', 'jockey')]))

rand_both_models = plyr::rbind.fill(rand_coefs, rand_lmer)

slopes_both = rand_both_models[rand_both_models$rand_intercept %in% c('FALSE',
                                                                      'FALSE_lmer') &
                                 rand_both_models$Estimate<2,]

ggplot(data = slopes_both,
  #data = rand_lmer[rand_lmer$rand_intercept == 'FALSE_lmer',],
       aes(x = Estimate,
           #y = reorder(jockey, Estimate),
           y = reorder(slopes_both[rand_intercept == 'FALSE', 'jockey'],
                       slopes_both[rand_intercept == 'FALSE', 'Estimate']),
           color = rand_intercept)) +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper,
                     height = 0.3),
                 size=0.75#,
                 #color='gray25'#,
                 #linetype=errorbar_linetype
  ) +

  geom_point(pch=21, size=1,
             position = position_dodge(width = 0.5)) +

  theme_minimal() +
  theme(axis.text.y = element_text(color = "black", size = 6),
        axis.text.x = element_text(size = 16, face = "bold"))  +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.4) +
  labs(y = "", x = '')  +
  xlim(-1.5, 2) +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        legend.position="bottom") +
  ggtitle('Random Effects by Jockeys')

# correlation between random effects and the number of races for each jockey

rand_lmer %<>% left_join(unique(df[, c('jockey', 'n')]))
cor(rand_lmer[rand_lmer$rand_intercept == 'FALSE_lmer', 'n'],
    rand_lmer[rand_lmer$rand_intercept == 'FALSE_lmer', 'Estimate'],
    method = 'spearman')

# correlation between random effects and average output for each jockey
temp = df %>% group_by(jockey) %>%
  summarise(mean_output_orig = mean(output_orig))

rand_lmer %<>% left_join(temp)

cor(rand_lmer[rand_lmer$rand_intercept == 'FALSE_lmer', 'mean_output_orig'],
    rand_lmer[rand_lmer$rand_intercept == 'FALSE_lmer', 'Estimate'],
    method = 'spearman')

# explore extremes

rank = 5
picktop = rand_lmer %>% select(jockey, rand_intercept, Estimate) %>%
  filter(rand_intercept == 'FALSE_lmer') %>%
  slice_max(Estimate, n = rank)  %>% pull(jockey)
picktop = picktop[length(picktop)]

test = df %>% filter(jockey == picktop)
test %<>% filter(race_id == names(table(test$race_id)[1]))
plot(test$trakus_index, test$output_orig)
plot(test$trakus_index, test$speed_diff)
plot(test$trakus_index, test$speed_ego)
plot(test$trakus_index, test$speed_alters)

rank = 95
pickbottom = rand_lmer %>% select(jockey, rand_intercept, Estimate) %>%
  filter(rand_intercept == 'FALSE_lmer') %>%
  slice_max(Estimate, n = rank)  %>% pull(jockey)
pickbottom = pickbottom[length(pickbottom)]

test = df %>% filter(jockey == pickbottom)
test %<>% filter(race_id == names(table(test$race_id)[1]))
plot(test$trakus_index, test$output_orig)
plot(test$trakus_index, test$speed_diff)
plot(test$trakus_index, test$speed_ego)
plot(test$trakus_index, test$speed_alters)


cor(rand_both_models$Estimate[rand_both_models$rand_intercept == 'FALSE'],
    rand_both_models$Estimate[rand_both_models$rand_intercept == 'FALSE_lmer'])
#-----------------------------------------------------------
#-------------------------- DAGs ---------------------------
#-----------------------------------------------------------

# plotting relationships with DAGs
library(ggplot2)
library(ggdag)
coord_dag <- list(
  x = c(speed_alters_lag = -5,
        speed_ego_lag = -5,
        speed_alters = 0,
        speed_ego = 0,
        output = 5),
  y = c(speed_alters_lag = 2,
        speed_ego_lag = -2,
        speed_alters = 2,
        speed_ego = -2,
        output = 0)
)
dag <- ggdag::dagify(speed_alters ~ speed_alters_lag,
                     speed_ego ~ speed_ego_lag,
                     speed_alters ~ speed_ego_lag,
                     speed_ego ~ speed_alters_lag,
                     output ~ speed_alters,
                     output ~ speed_ego,
                     coords = coord_dag,
                     labels = c('speed_alters' = 'Alters Speed',
                                'speed_alters_lag' = 'Lag of Alters Speed',
                                'speed_ego' = 'Egos Speed',
                                'speed_ego_lag' = 'Lag of Egos Speed',
                                'output' = 'Output'

                     )) %>%
  tidy_dagitty()

ggdag::ggdag(dag,
             text = F,
             use_labels = "label",
             node_size = 20,
             text_size = 4) +
  ggplot2::theme_void()

