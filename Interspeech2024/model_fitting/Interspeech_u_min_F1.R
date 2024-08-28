# script for univariate F1 "minimal" model

library(brms)
library(lme4)

# Possible values for corpus: 
# "England-South"
# "North-America"
# "Scotland"
# "US-South"

corpus <- "US-South" # change the corpus name for each dialect 

dataset <- readRDS(paste("../data/dialects/", corpus, "_data.rds", sep=""))

F1 = bf(zF1 ~ 
          context*stressed_vowel +
          (1|p|word) + (1+context*stressed_vowel|q|speaker))

model <- brm(F1,
             data=dataset, 
             file=paste("../models/", corpus, "_u_min_F1", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)