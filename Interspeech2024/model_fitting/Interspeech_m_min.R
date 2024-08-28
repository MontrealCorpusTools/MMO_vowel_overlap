# script for multivariate "minimal" model

library(brms)
library(lme4)

# Possible values for corpus: 
# "England-South"
# "North-America"
# "Scotland"
# "US-South"

corpus <- "US-South" # change the corpus name for each dialect  

dataset <- readRDS(paste("../data/dialects", corpus, "_data.rds", sep=""))

F1 = bf(zF1 ~ 
          context*stressed_vowel +
          (1|p|word) + (1+context*stressed_vowel|q|speaker))

F2 = bf(zF2 ~ 
          context*stressed_vowel +
          (1|p|word) + (1+context*stressed_vowel|q|speaker))

model <- brm(F1 + F2 + set_rescor(TRUE),
             data=dataset, 
             file=paste("../models/", corpus, "_m_min", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)