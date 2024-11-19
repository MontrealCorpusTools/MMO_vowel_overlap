library(tidyverse)

# We'll be using the Raleigh corpus from the SPADE project. Download the Raleigh
# dataset at from OSF https://osf.io/79jgu and put the file in the data
# folder.

raleigh_data_full <- read.csv("data/spade-Raleigh_formants_whitelisted.csv")

# To make this tutorial run-able on a personal laptop, we'll just use the ten
# speakers who have the most data.

# make a vector containing the names of the 10 speakers with the most data
top_10_speakers <- raleigh_data_full %>%
  count(speaker) %>% # the number of tokens per speaker 
  mutate(rank=dense_rank(desc(n))) %>% # rank number of tokens with most=1
  filter(rank <= 10) %>% # filter out everyone except the top 10 speakers
  pull(speaker) # convert speaker column to a vector

# filter the original dataset to only include top 10 speakers 
raleigh_data_top10 <- raleigh_data_full %>%
  filter(speaker %in% top_10_speakers) 

# we won't be using the full dataset anymore, so let's get rid of it to 
# save a little memory
rm(raleigh_data_full)

# Before narrowing down to the two vowels of interest (/ɪ/ and /ɛ/), we want to
# Lobanov normalize F1 and F2 for each speaker using all of their vowel tokens
# i.e., take a z-score of F1 and F2 across all vowel tokens for each speaker.

raleigh_data_norm <- raleigh_data_top10 %>%
  group_by(speaker) %>% # vowels are normalized separately for each speaker
  mutate(across(c(F1, F2), ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x)))

# Now, let's narrow down the dataset to the tokens and quantities we're interested in. We want stressed tokens of /ɪ/ and /ɛ/.

raleigh_data <- raleigh_data_norm %>%
  # pick out just syllables with primary stress
  filter(syllable_stress==1) %>%
  # grab the /ɪ/ and /ɛ/ tokens.
  # match the unisyn label to the phone label for a quality check
  filter((phone_label=="EH1" & UnisynPrimStressedVowel1=="e") | 
           (phone_label=="IH1" & UnisynPrimStressedVowel1=="i")) %>%
  # make sure that UR in this dialect corresponds to lexical set 
  # (more important when there is more than one dialect)
  filter(unisynPrimStressedVowel2_sca==UnisynPrimStressedVowel1) %>%
  # label the context according to following consonant
  mutate(context = case_when(
    following_phone %in% c("B", "CH", "D", "DH", "F", "G", "HH", "JH", "K",
                           "L", "P", "R", "S", "SH", "T", "TH", "V", "W",
                           "Y", "Z", "ZH" ) ~ "oral", # oral consonants
    following_phone %in% c("N", "M", "NG") ~ "nasal", # nasal consonants
    TRUE ~ "other")) %>% 
  # get rid of everything that isn't an oral or nasal consonant
  filter(context != "other") %>% 
  # get rid of voiced prevalar contexts (can include, but would want 
  # following consonant included in model as a predictor or random effect)
  filter(following_phone!="NG", following_phone!="G") %>%
  # make vowel and context factors 
  mutate(vowel = factor(UnisynPrimStressedVowel1,
                        levels=c('i', 'e')),
         context = factor(context,
                          levels=c('oral', 'nasal'))) %>%
  # make sure that there aren't extraneous levels
  droplevels() %>%
  # keep just the columns we're interested in
  dplyr::select(speaker, vowel, context, word, F1, F2)

# Save the dataset
raleigh_data %>% saveRDS("data/Raleigh_subset.rds")
raleigh_data %>% write_csv("data/Raleigh_subset.csv")
