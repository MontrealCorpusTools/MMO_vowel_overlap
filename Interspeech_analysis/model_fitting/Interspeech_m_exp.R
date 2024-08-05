library(brms)
library(lme4)

args <- commandArgs(trailingOnly = TRUE)
corpus <- args[1]

dataset <- readRDS(paste("data/eval_2_revisions/", corpus, "_data.rds", sep=""))
F1 = bf(zF1 ~ 
          context*stressed_vowel*log_duration +
          (1|p|word:following_cons) + (1|k|following_cons) + (1+context*stressed_vowel|q|speaker)) +
  lf(sigma ~ stressed_vowel*context + (1+stressed_vowel*context|r|speaker))

F2 = bf(zF2 ~ 
          context*stressed_vowel*log_duration +
          (1|p|word:following_cons) + (1|k|following_cons) + (1+context*stressed_vowel|q|speaker)) +
  lf(sigma ~ stressed_vowel*context + (1+stressed_vowel*context|r|speaker))

model <- brm(F1 + F2 + set_rescor(TRUE),
             data=dataset, 
             file=paste("models/Interspeech/", corpus, "_m_m2", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)