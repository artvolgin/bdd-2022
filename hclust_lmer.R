
library(dplyr)
library(ggplot2)
library(lme4)
library(magrittr)

setwd("C:/Users/ru21406/YandexDisk/COMPETITIONS/bdd_2022/data")
df_main.agg = readRDS('df_main.agg.rds')
colnames(df_main.agg)
df = df_main.agg

# hclust
hc = hclust(dist(df[,2:41]), method ='ward.D2')
hc_memb = cutree(hc, k = 4)
#plot(hc)
table(hc_memb)

df = cbind.data.frame(df, hc_memb)

# filter jockeys > 5
df %<>% group_by(jockey) %>% dplyr::mutate(n = n()) %>%
  filter(n > 5)
table(df$n)
table(df$position_at_finish_top3)

# normalise dependent var
normalize = function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
df %<>% group_by(jockey) %>%
  dplyr::mutate(position_at_finish_minmax = normalize(position_at_finish))
hist(df$position_at_finish_minmax)

# as factor
df$hc_memb = relevel(as.factor(df$hc_memb), ref = '4')


#---------------------------------------------------------------
# random effects
mod0 = lmer(position_at_finish_minmax ~ hc_memb +
               scale(jockey_n) + scale(purse) + course_type + track_condition +
               (1 + hc_memb|jockey) + (1|horse_id), data = df,
            )
summary(mod0)
ranef_mod0 = as.data.frame(ranef(mod0, condVar = T))


# fixed
fixed = cbind.data.frame(fixed = c(as.numeric(fixef(mod0)[2]),
                         as.numeric(fixef(mod0)[3]),
                         as.numeric(fixef(mod0)[4])),
                         term = c('hc_memb1', 'hc_memb2', 'hc_memb3')
                         )
# table with effects
ranef_mod0 %<>%
  select(id = grp,
         Estimate = condval,
         Std.Error = condsd,
         term)  %>%
  filter(!term == '(Intercept)') %>%
  left_join(fixed, by = 'term') %>%
  mutate(upper = fixed + Estimate + 1.64*Std.Error,
         lower = fixed + Estimate - 1.64*Std.Error,
         Estimate = fixed + Estimate)

# plotting - SHOULD BE A HEATMAP
ggplot(data = ranef_mod0,
       aes(x = Estimate,
           y = reorder(id, Estimate),
       colour = term
       )
       ) +
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
 # xlim(0, 2) +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5),
        legend.position="bottom") +
  ggtitle('Random Effects by Jockeys')


# interactions

df$hc_memb = relevel(as.factor(df$hc_memb), ref = '4')
df$track_condition_bin = ifelse(df$track_condition %in% c('FM', 'FT ','GD'), 'dry', 'wet')

# int1
mod0_int1 = lmer(position_at_finish_minmax ~ hc_memb*track_condition_bin +
              scale(jockey_n) + scale(purse) + course_type +
              (1 |jockey) + (1|horse_id), data = df,
)
summary(mod0_int1)

# int2
mod0_int2 = lmer(position_at_finish_minmax ~ hc_memb*course_type +
                  scale(jockey_n) + scale(purse) + track_condition_bin +
                  (1 |jockey) + (1|horse_id), data = df,
)
summary(mod0_int2)

# int3
mod0_int3 = lmer(position_at_finish_minmax ~ hc_memb*scale(purse) +
                   course_type + scale(jockey_n) + track_condition_bin +
                   (1 |jockey) + (1|horse_id), data = df,
)
summary(mod0_int3)






