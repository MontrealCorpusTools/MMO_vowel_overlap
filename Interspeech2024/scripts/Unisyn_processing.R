# PLEASE READ: This script accesses files that are not in the github repository. 
# It is included for transparency, but unless you have access to the UNISYN 
# files, you will not be able to run this script. The file that this script 
# saves, is in the Interspeech2024/data folder, and is all you need in order to 
# do the preprocessing for all following analysis. If you have further 
# questions, please contact the researchers.

# read in unisyn dictionaries for each dialect group
unisyn_aae <- read.csv("../data/aae_keysymbols.csv")
unisyn_can <- read.csv("../data/can_keysymbols.csv")
unisyn_edi <- read.csv("../data/edi_keysymbols.csv")
unisyn_gam <- read.csv("../data/gam_keysymbols.csv")
unisyn_lds <- read.csv("../data/lds_keysymbols.csv")
unisyn_rpx <- read.csv("../data/rpx_keysymbols.csv")
unisyn_sca <- read.csv("../data/sca_keysymbols.csv")

# combine all of the dialect groups together
unisyn_cmb <- bind_rows(`aae` = unisyn_aae,
                        `can` = unisyn_can,
                        `edi` = unisyn_edi,
                        `gam` = unisyn_gam,
                        `lds` = unisyn_lds,
                        `rpx` = unisyn_rpx,
                        `sca` = unisyn_sca,
                        .id = "dialect_group")

# read in the dictionary with the actual phonetic transcription of words. It is 
# formatted as a character vector with one word per entry, so we need to use a 
# regular expression to turn it into a usable dataframe
unisyn_xxx <- read_lines("../data/unilex") %>%
  str_extract('(.+?):[^\\s].+:\\s(.+)\\s:', group = c(1, 2)) %>% 
  as.data.frame() %>%
  # make the column names meaningful
  rename(Word='V1', Transcription='V2')

unisyn_stressed_vowels <- unisyn_xxx$Transcription %>%
  # extract the stressed vowel, the syllable boundary ('.', if it exists), 
  # and the following consonant
  str_extract('\\*\\s([^\\s]+)\\s(=?\\.?=?)\\s?([^\\s]+)', group = c(1, 2, 3)) %>%
  as.data.frame %>%
  rename(stressed_syll='V1', syll_boundary='V2', following_cons='V3')

# combine the transcription with the vowel and following consonant
unisyn_dict <- unisyn_xxx %>%
  bind_cols(unisyn_stressed_vowels) %>%
  group_by(Word, stressed_syll) %>%
  # take the first transcription of each word
  summarise(Word = first(Word),
            Transcription = first(Transcription),
            stressed_syll = first(stressed_syll), 
            syll_boundary = first(syll_boundary),
            following_cons = first(following_cons)) %>%
  ungroup()

# get just the words where the stressed vowel is /IH/ or /EH/ in all dialects
words_I_E_prelim <- unisyn_cmb %>%
  filter(UnisynPrimStressedVowel1=="i" | 
           UnisynPrimStressedVowel1=="e") %>% 
  filter(UnisynPrimStressedVowel1 == UnisynPrimStressedVowel2) %>% 
  mutate(word_pron = paste(Word, WordId))


# we have seven dialects: get rid of words where there are not exactly 7 of each 
# pronunciation
words_to_elim <- words_I_E_prelim %>%
  group_by(word_pron) %>%
  count() %>%
  filter(n != 7) %>%
  ungroup()

# get rid of the words in the "eliminate" list
words_I_E <- words_I_E_prelim %>%
  filter(!(word_pron %in% words_to_elim$word_pron)) %>%
  dplyr::select(Word, WordId, UnisynPrimStressedVowel1, UnisynPrimStressedVowel2) %>%
  unique()

aae_I_E <- words_I_E_prelim %>% # take any example dialect in order to do a setdiff
  filter(!(word_pron %in% words_to_elim$word_pron)) %>%
  filter(dialect_group == "aae") %>%
  dplyr::select(Word, WordId, UnisynPrimStressedVowel1, UnisynPrimStressedVowel2)

duplicated_words <- words_I_E %>%
  setdiff(aae_I_E)

words_to_inspect <- words_I_E_prelim %>% 
  filter(Word %in% duplicated_words$Word)
# this turns out to be a list of words where there are two possible stress 
# patterns, and some dialects have one stress pattern, and other dialects have 
# the other. The exception is cigarette and cigarettes, where all dialects have 
# both stress patterns, but they're ordered differently. I'm just getting rid of 
# all of these words.

semifinal_wordlist <- words_I_E %>%
  filter(!(Word %in% duplicated_words$Word)) %>%
  group_by(Word) %>%
  summarise(Word = first(Word), stressed_syll = first(UnisynPrimStressedVowel1)) %>%
  left_join(unisyn_dict, by=c("Word", "stressed_syll")) %>%
  filter(!(following_cons %in% c("@", "[.", "[p1]", "r", NA)))

# final_word_counts <- semifinal_wordlist %>%
#   group_by(Word) %>%
#   count() %>%
#   filter(n > 1)
# # this returns an empty dataframe, which is exactly what I want!

cons_counts <- semifinal_wordlist %>%
  group_by(following_cons) %>%
  count()

weird_cons <- cons_counts %>%
  filter(n <= 5)

cons_inspect <- semifinal_wordlist %>%
  filter(following_cons %in% weird_cons$following_cons)

# use the words from cons_inspect to fix the transcriptions
final_word_list <- semifinal_wordlist %>%
  mutate(following_phone = case_when(
    following_cons == "k/g" ~ "k", # derivations of "exit"
    following_cons == "dh/th" ~ "th", # derivations of "with", as well as "scythia" 
    following_cons == "(z" ~ "z", # jesuit(s)
    following_cons == "ll" ~ "l", # llanelli
    Word %in% c("said", "unsaid", "gainsaid") ~ "d",
    Word == "says" ~ "z",
    TRUE ~ following_cons)) %>%
  mutate(context = case_when(
    following_phone %in% c("m", "n", "ng") ~ "nasal",
    TRUE ~ "oral")) %>%
  mutate(syllable_type = case_when(
    syll_boundary %in% c(".", "=.=") ~ "open",
    TRUE ~ "closed")) %>%
  dplyr::select(Word, stressed_syll, following_phone, context, syllable_type) %>%
  rename(word="Word", stressed_vowel="stressed_syll", following_cons="following_phone")

final_word_list %>% saveRDS('../data/Unisyn_words.rds')
