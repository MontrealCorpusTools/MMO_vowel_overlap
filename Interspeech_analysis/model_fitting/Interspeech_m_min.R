library(brms)
library(lme4)

args <- commandArgs(trailingOnly = TRUE)
corpus <- args[1]

dataset <- readRDS(paste("data/eval_2_revisions/", corpus, "_data.rds", sep=""))
F1 = bf(zF1 ~ 
          context*stressed_vowel +
          (1|p|word) + (1+context*stressed_vowel|q|speaker))

F2 = bf(zF2 ~ 
          context*stressed_vowel +
          (1|p|word) + (1+context*stressed_vowel|q|speaker))

model <- brm(F1 + F2 + set_rescor(TRUE),
             data=dataset, 
             file=paste("models/Interspeech/", corpus, "_m_m1", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)