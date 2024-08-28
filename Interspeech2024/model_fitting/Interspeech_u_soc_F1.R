# script for univariate F1 "social predictors" model

# NOTE: the social predictors are not present in the publicly-available data!
# Therefore, you will not be able to fit this model without being granted access
# to the full dataset. However, these models were not reported on in the
# Interspeech paper and have been known to have convergence issues. This script
# is included in the repository for educational purposes only. Please contact
# the researchers if you have further questions.

library(brms)
library(lme4)
library(splines)
library(dplyr)

# Possible values for corpus: 
# "England-South"
# "North-America"
# "Scotland"
# "US-South"

corpus <- "US-South" # change the corpus name for each dialect 

dataset <- readRDS(paste("../data/dialects/", corpus, "_data.rds", sep="")) %>%
  mutate(birthyear = birthyear %>% as.numeric()) %>%
  filter(!is.na(birthyear), !is.na(gender)) %>%
  mutate(birthyear_centered = (birthyear-mean(birthyear))/sd(birthyear))

F1 = bf(zF1 ~ 
          context*stressed_vowel*log_duration +
          # we don't expect the effect of birthyear to be linear, so we use a 
          # spline, by way of the `ns()` function. Need to have the "splines" 
          # package loaded.
          context*stressed_vowel*ns(birthyear_centered, 4) +
          context*stressed_vowel*gender +
          # by-word random intercepts are nested under by-following-consonant
          # random intercepts
          (1|p|word:following_cons) + (1|k|following_cons) + 
          (1+context*stressed_vowel|q|speaker)) +
  # allow sigma to vary for each speaker x vowel x context
  lf(sigma ~ stressed_vowel*context + (1+stressed_vowel*context|r|speaker))

model <- brm(F1,
             data=dataset, 
             file=paste("../models/", corpus, "_u_soc_F1", sep=""), 
             prior = c(prior(lkj(1.5), class = cor)), 
             chains = 4, cores = 4, iter = 6000)