# script for multivariate "expanded" model

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
          context*stressed_vowel*log_duration +
          # by-word random intercepts are nested under by-following-consonant
          # random intercepts
          (1|p|word:following_cons) + (1|k|following_cons) + 
          (1+context*stressed_vowel|q|speaker)) +
  # allow sigma to vary for each speaker x vowel x context
  lf(sigma ~ stressed_vowel*context + (1+stressed_vowel*context|r|speaker))

F2 = bf(zF2 ~ 
          context*stressed_vowel*log_duration +
          (1|p|word:following_cons) + (1|k|following_cons) + (1+context*stressed_vowel|q|speaker)) +
  lf(sigma ~ stressed_vowel*context + (1+stressed_vowel*context|r|speaker))

model <- brm(F1 + F2 + set_rescor(TRUE),
             data=dataset, 
             file=paste("../models/", corpus, "_m_exp", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)