# script for univariate F2 "minimal" model

library(brms)
library(lme4)

# Possible values for corpus: 
# "England-South"
# "North-America"
# "Scotland"
# "US-South"

corpus <- "US-South" # change the corpus name for each dialect 

dataset <- readRDS(paste("../data/dialects/", corpus, "_data.rds", sep=""))

F2 = bf(zF2 ~ 
          context*stressed_vowel +
          (1|p|word) + (1+context*stressed_vowel|q|speaker))

model <- brm(F2,
             data=dataset, 
             file=paste("../models/", corpus, "_u_min_F2", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)