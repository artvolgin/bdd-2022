
# testing various models

library(dplyr)
library(ggplot2)
library(sf)
library(units)
library(lme4)
library(gganimate)
library(magrittr)


#USERNAME <- Sys.getenv("USERNAME")
#setwd(paste0('C:/Users/', USERNAME, '/YandexDisk/COMPETITIONS/bdd_2022/data'))
#
#df_results = readRDS('df_results.rds')
#df_results <- df_results[!is.na(df_results$ego_speed_lag),]
#require(OpenMx)
#library(umx)
#library(RAMpath)

df = df_results %>%
  group_by(jockey) %>%
  dplyr::mutate(num = n()) %>%
  filter(num < 500) %>%
  #dplyr::slice(n = min(n(), 20000)) %>%
  ungroup()

# generate output with cor = 0.4
len = as.data.frame(table(df$jockey))

fin = c()
old = c()
for (i in 1:nrow(len)){
  n = len[i,'Freq']
  rho   = 0.4
  theta = acos(rho)
  x1    = df %>%
    filter(jockey == as.character(len[i,'Var1'])) %>%
    dplyr::select(ego_speed_lag)
  x2    = rnorm(n, 2, 0.5)
  X     = cbind(x1, x2)
  Xctr  = scale(X, center=TRUE, scale=FALSE)

  Id   = diag(n)
  Q    = qr.Q(qr(Xctr[ , 1, drop=FALSE]))
  P    = tcrossprod(Q)
  x2o  = (Id-P) %*% Xctr[ , 2]
  Xc2  = cbind(Xctr[ , 1], x2o)
  Y    = Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))

  x = Y[ , 2] + (1 / tan(theta)) * Y[ , 1]
  cor(x1, x)

  fin = c(fin, x)
  old = c(old, as.numeric(x1$ego_speed_lag))
  print(i)
}
gc()
cor(fin, old)

jockey = as.character(rep(len[,1], as.numeric(as.character(len[,'Freq']))))
output = cbind.data.frame(jockey, fin)
#output = output[!duplicated(output$jockey),]
output %<>% arrange(jockey) %>% dplyr::select(-jockey)
df %<>% arrange(jockey)
df = cbind.data.frame(df, output)

#df %<>% left_join(output)
df$output = df$fin*20 + 20

cor.test(df$ego_speed_lag[df$jockey == len[28,1]],
         df$output[df$jockey == len[28,1]])
summary(df)

# Modelling
numSubjects = length(unique(df$jockey))
manifestVars = c('alters_speed', 'alters_speed_lag', 'ego_speed', 'ego_speed_lag', 'output')
numManifest = length(manifestVars)
df = df %>% dplyr::select(c(jockey, all_of(manifestVars)))

keys = cbind.data.frame(jockey = levels(as.factor(df$jockey)), id = 1:numSubjects)
df %<>% left_join(keys)
df = df[!is.na(df$ego_speed),]

# N - number of observations
# k - number of manifest vars

#mxOption(NULL,"Default optimizer","SLSQP")
multilevelModel <- mxModel("Multilevel",
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
                                     labels = c(NA,NA,NA,NA,NA,
                                                'b1',NA,'b4',NA,NA,
                                                NA,NA,NA,NA,NA,
                                                'b3',NA,'b2',NA,NA,
                                                NA,'b5',NA,"randrow[1,1]",NA),
                                     free = c(F,F,F,F,F,
                                              T,F,T,F,F,
                                              F,F,F,F,F,
                                              T,F,T,F,F,
                                              F,T,F,F,F),
                                     #values = c(F,F,F,F,F,
                                     #           .5,F,.5,F,F,
                                     #           F,F,F,F,F,
                                     #           .5,F,.5,F,F,
                                     #           F,.5,F,F,F),
                                     name = "A",
                                     byrow = T
                            ),
                            # matrix of var-covar where covars are 0, k x k
                            mxMatrix("Symm",
                                     numManifest,
                                     numManifest,
                                     values = c(1,
                                                 0, 1,
                                                 .5, 0, 1,
                                                 0, 0, 0, 1,
                                                 0, 0, 0, 0, 1),
                                     free = c(T,
                                              F, T,
                                              T, F, T,
                                              F, F, F, T,
                                              F, F, F, F, T),
                                     labels = c('var_input1_t1',
                                                NA, 'var_input1_t2',
                                                'cov_input12_t1', NA, 'var_input2_t1',
                                                NA, NA, NA, 'var_input2_t2',
                                                NA, NA, NA, NA, 'var_output'),
                                     name = "S",
                                     byrow = T
                            ),
                            # filter matrix = identity since there are no latvars, k x k
                            mxMatrix("Full",
                                     numManifest,
                                     numManifest,
                                     values = c(1,0,0,0,0,
                                                0,1,0,0,0,
                                                0,0,1,0,0,
                                                0,0,0,1,0,
                                                0,0,0,0,1),
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
                                     free = c(F,T,F,T,F),
                                     labels = c(NA,'mean1',NA,'mean2',"randrow[1,2]"),
                                     dimnames = list(NULL, manifestVars),
                                     name = "M"
                            ),
                            # algebra for extracting rand effects
                            mxAlgebra(Rand[data.id,], name = "randrow"),
                            mxFitFunctionML(),
                            mxExpectationNormal(covariance = "R", means = "M"),
                            mxData(df, type = 'raw'),
                            mxCI(c('Multilevel.Rand[1,1]',
                                   'Multilevel.Rand[2,1]',
                                   'Multilevel.Rand[3,1]'),
                                 interval = .9)
)

# ----------------------------------
# fitting the model for random effects

multilevelModelFit = mxRun(multilevelModel, intervals = TRUE)
summary(multilevelModelFit)

multilevelModelFit$output$estimate[1:numSubjects]
print(summary(multilevelModelFit)$CI)

umxConfint(multilevelModelFit, parm = 'all', run = TRUE)


#summary(multilevelModelFit,
#        refModels=mxRefModels(multilevelModelFit, run = TRUE))
#
#require(semTools)
#myModelFit <- mxRun(myModel)
#fitMeasuresMx(multilevelModelFit)
#
#multilevelModelSat <- omxSaturatedModel(multilevelModelFit, run=TRUE)
#summary(multilevelModelFit, multilevelModelSat)


# ----------------------------------
# An OpenMx equivalent to the mixed model

df = df %>% dplyr::select(id, all_of(manifestVars))
colnames(df) = c('ID', 'a', 'aa', 'b', 'bb', 'out')

df$a = as.numeric(scale(df$a))
df$aa = as.numeric(scale(df$aa))
df$b = as.numeric(scale(df$b))
df$bb = as.numeric(scale(df$bb))
df$out = as.numeric(scale(df$out))

df = df %>% group_by(ID) %>% slice_sample(n = 100)
df = as.data.frame(df)

#df %<>% filter(b < 5)
#hist(df$b, breaks = 200)
#as.data.frame(table(df$ID))

withinM = mxModel(
  "withinM", type = "RAM",
  latentVars = 'bEffect',
  manifestVars = c("a",
                   "aa",
                   "bb",
                   "out"),
  mxData(df, 'raw'),
  mxPath(c("a",
           "aa",
           #"bEffect",
           "bb",
           "out"),
         arrows = 2,
         values = 1),
  mxPath('bEffect', free = F, values = 0),
  mxPath('bb', 'aa',
         arrows = 2,
         values = .5),
  mxPath('aa', 'a', values = .5),
  mxPath('aa', 'bEffect', values = .5),
  mxPath('bb', 'a', values = .5),
  mxPath('bb', 'bEffect', values = .5),
  mxPath('bEffect', 'out', values = .5),
  mxPath('a', 'out', values = .5),
  mxPath('one', c('out',
                  'a'),
         values = .5),
  mxPath('one', c('bb',
                  'aa'),
         #labels = c('data.bb',
        #            'data.aa'),
         values = .5,
         free = T),
  mxPath("one", "bEffect", free=FALSE, labels="data.b")
  )

#plot(withinM, means = F)
#withinMFit = mxRun(withinM)
#summary(withinMFit)
#mxCheckIdentification(withinMFit)
#omxRMSEA(withinMFit, .05, .95, refModels=mxRefModels(withinMFit, run=TRUE))

betweenM = mxModel(
  "betweenM", type = "RAM", latentVars = c('int', 'slope'),
  mxData(data.frame(ID = df[!duplicated(df$ID), 'ID']), "raw",
         primaryKey = "ID"),
  mxPath(c('int', 'slope'), c('int', 'slope'), 'unique.pairs',
         arrows = 2, values = c(1, .5, 1))
)

within_between = mxModel(withinM, betweenM)

fin = mxModel(within_between,
              mxPath('betweenM.int', 'out',
                     values = .5,
                     free = F,
                     joinKey = 'ID'),
              mxPath('betweenM.slope', 'out',
                     labels = 'data.b',
                     free = F,
                     joinKey = 'ID'))

withinFit = mxRun(fin)
summary(withinFit)
gc()


# hybrid mode

mxOption(NULL,"Default optimizer","CSOLNP")
mxOption(NULL,"Number of Threads",(omxDetectCores() - 1))


m1 <- mxModel(model="withinM", type = "RAM",
              #latentVars = 'bEffect',
              manifestVars = c("a",
                               "aa",
                               "bb",
                               "out",
                               'b'),
              mxData(df, type = 'raw', sort = F),
              mxPath(c("a",
                       "aa",
                       "bb",
                       "out",
                       "b"),
                     arrows = 2,
                     values = 1),
              mxPath('bb', 'aa',
                     arrows = 2,
                     values = .5),
              mxPath('aa', 'a'),
              mxPath('aa', 'b'),
              mxPath('bb', 'a'),
              mxPath('bb', 'b'),
              mxPath('b', 'out'),
              mxPath('a', 'out'),
              mxPath('one', c('out',
                              'a',
                              'b'
              ),
              values = 1),
              mxPath('one', c('bb',
                              'aa'),
                     labels = c('data.bb',
                                'data.aa'),
                     free = F),
              #mxPath("one", "bEffect", free=FALSE, labels="data.b"),

              # this is the between level mapping
              mxMatrix(name="Z", nrow=5, ncol=2,
                       labels = c(NA,NA,NA,'data.b',NA,
                                  NA,NA,NA,NA,NA),
                       values = c(0,0,0,0,0,
                                  0,0,0,1,0),
                       dimnames = list(c("a",
                                         "aa",
                                         "bb",
                                         "out",
                                         "b"),
                                       c("int", "slope")),
                       joinKey = "ID", joinModel = "betweenM"),
              mxFitFunctionML(fellner=T))

m1$expectation$between <- "Z"
#plot(m1, means = F)

m2 <- mxModel(m1, mxModel(
  "betweenM", type = "RAM", latentVars = c('int', 'slope'),
  mxData(data.frame(ID = df[!duplicated(df$ID), 'ID']), "raw",
         primaryKey = "ID"),
  mxPath(c('int', 'slope'), c('int', 'slope'), 'unique.pairs',
         arrows = 2, values = c(1, .25, 1)
  ))
)

#m2$betweenM.S[1,1]$ubound <- 100
m2$betweenM$fitfunction <- NULL

m2$fitfunction$fellner <- TRUE
m2_fit <- mxRun(m2)
#m2_fit <- mxTryHard(m2)
gc()
summary(m2_fit)

