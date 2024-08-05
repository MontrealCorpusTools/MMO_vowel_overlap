library(brms)
library(lme4)

args <- commandArgs(trailingOnly = TRUE)
corpus <- args[1]

dataset <- readRDS(paste("data/eval_2_revisions/", corpus, "_data.rds", sep=""))
F2 = bf(zF2 ~ 
          context*stressed_vowel*log_duration +
          (1|p|word:following_cons) + (1|k|following_cons) + (1+context*stressed_vowel|q|speaker)) +
  lf(sigma ~ stressed_vowel*context + (1+stressed_vowel*context|r|speaker))

model <- brm(F2,
             data=dataset, 
             file=paste("models/Interspeech/", corpus, "_u_m2_F2", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)