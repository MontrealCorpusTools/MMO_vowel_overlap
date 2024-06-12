library(brms)
library(lme4)
library(splines)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
corpus <- args[1]

dataset <- readRDS(paste("data/eval_2_revisions/", corpus, "_data.rds", sep="")) %>%
  mutate(birthyear = birthyear %>% as.numeric()) %>%
  filter(!is.na(birthyear), !is.na(gender)) %>%
  mutate(birthyear_centered = (birthyear-mean(birthyear))/sd(birthyear))
F1 = bf(zF1 ~ 
          context*stressed_vowel*log_duration +
          context*stressed_vowel*ns(birthyear_centered, 4) +
          context*stressed_vowel*gender +
          (1|p|word:following_cons) + (1|k|following_cons) + (1+context*stressed_vowel|q|speaker)) +
  lf(sigma ~ stressed_vowel*context + (1+stressed_vowel*context|r|speaker))

model <- brm(F1,
             data=dataset, 
             file=paste("models/Interspeech/", corpus, "_u_m6_F1", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)