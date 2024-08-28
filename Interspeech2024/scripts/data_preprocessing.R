library(tidyverse)
library(arm)

# Download the formants files for each of the corpora and put the files in Interspeech2024/data
# 
# Hastings: https://osf.io/3jv5n
# Sounds of the city: https://osf.io/3jv5n
# Sunset: https://osf.io/wdxeq 
# SLAAP NorthTown Anglo: https://osf.io/kpz7j
# SLAAP NorthTown Latinx: https://osf.io/t79yx 
# 
# **Rename the Sunset file** from 'spade-Sunset_formants.csv' to 'spade-Sunset_formants_whitelisted.csv'. 
# 
# Then, run the code chunk to save the cleaned datasets in the proper places to access them for fitting models. 

corpus_names <- c("SLAAP-NorthTown-Anglo",
                  "SLAAP-NorthTown-Latinx", 
                  "Sunset", 
                  "Hastings", 
                  "SOTC")

final_word_list <- readRDS('../data/Unisyn_words.rds')


filter_data <- function(corpus_name, unisyn_group){
  data <- read.csv(paste("../data/spade-", corpus_name, "_formants_whitelisted.csv", 
                         sep = "")) %>%
    group_by(speaker) %>%
    mutate(zF1 = as.numeric(scale(F1)),
           zF2 = as.numeric(scale(F2))) %>%
    ungroup() %>%
    mutate(word = tolower(word)) %>%
    rename(stressed_vowel=paste("unisynPrimStressedVowel2", unisyn_group, 
                                sep="_")) %>%
    inner_join(final_word_list, by=c("word", "stressed_vowel")) %>%
    filter(UnisynPrimStressedVowel1==stressed_vowel) %>%
    dplyr::select(speaker, duration, word, stressed_vowel, 
                  following_cons, context, syllable_type, F1, F2, zF1, zF2) %>%
    mutate(corpus=corpus_name)
}


Sunset_data <- filter_data("Sunset", "gam")
Hastings_data <- filter_data("Hastings", "rpx")
SOTC_data <- filter_data("SOTC", "edi")
SLAAP_NorthTown_Anglo_data <- filter_data("SLAAP-NorthTown-Anglo", "sca")
SLAAP_NorthTown_Latinx_data <- filter_data("SLAAP-NorthTown-Latinx", "gam")

data_all <- bind_rows(Sunset_data,
                      Hastings_data,
                      SOTC_data,
                      SLAAP_NorthTown_Anglo_data,
                      SLAAP_NorthTown_Latinx_data) %>%
  mutate(dialect=case_when(
    corpus=='Hastings' ~ 'England South',
    corpus=='Sunset' ~ 'North America',
    corpus=='SOTC' ~ 'Scotland', 
    corpus %in% c('SLAAP-NorthTown-Anglo', 'SLAAP-NorthTown-Latinx') ~ 'US South')) %>%
  group_by(dialect) %>%
  mutate(log_duration = arm::rescale(log(duration)),
         context = factor(context),
         # gender = factor(gender), # public data doesn't necessarily include gender
         syllable_type = factor(syllable_type),
         stressed_vowel = factor(stressed_vowel)) %>%
  ungroup()

# helmert code factors
contrasts(data_all$context) <- contr.helmert(2)
contrasts(data_all$syllable_type) <- contr.helmert(2)
contrasts(data_all$stressed_vowel) <- contr.helmert(2)
# excluding gender for now because I don't know what to do about "not recorded";
# gender isn't included in model 2, but might need to address this if I fit 
# models with gender included as a predictor

# save them all together
data_all %>% saveRDS(file="../data/all_corpora_Interspeech_analysis.rds")

# save each corpus individually
data_all %>%
  group_by(dialect) %>%
  group_walk(~ saveRDS(.x, file=paste("../data/dialects/",
                                      # replace white space with hyphen so that
                                      # there is no whitespace in filename
                                      str_replace(.y$dialect, ' ', '-'), 
                                      "_data.rds", sep="")))