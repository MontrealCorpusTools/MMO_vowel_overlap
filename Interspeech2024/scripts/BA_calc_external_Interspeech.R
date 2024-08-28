library(brms)
library(dplyr)
library(stringr)
library(adehabitatHR)

bhatt <- function (F1, F2, vowel) 
{
  vowel_data <- droplevels(data.frame(vowel))
  
  sp_df <- SpatialPointsDataFrame(cbind(F1, F2), vowel_data)
  tryCatch(
    expr = {kerneloverlap(sp_df, method='BA')[1,2]}, 
    error = function(e){NA})
}

df_names <- dir("data/BA_calcs/Interspeech") %>%
  str_remove(".rds")

bhattacharyya_affs_sp <- data.frame()
bhattacharyya_affs_av <- data.frame()

for (corpus_name in df_names[1:(length(df_names)/2)]) {
  # corpus_name = str_remove(model, "_m2.rds")
  df_av <- readRDS(paste("data/BA_calcs/Interspeech/", corpus_name, sep="")) # (paste("data/BA_calcs/BA_sp_input", corpus_name, "m2.rds", sep="_"))
  df_sp <- readRDS(paste("data/BA_calcs/Interspeech/", str_replace(corpus_name, "_av_", "_sp_" ), sep="")) # (paste("data/BA_calcs/BA_av_input", corpus_name, "m2.rds", sep="_"))
  
  bhattacharyya_affs_sp_int <- df_sp %>%
    group_by(speaker, model_type, model_structure, context, .draw) %>%
    summarise(bhatt_aff = bhatt(zF1, zF2, stressed_vowel)) %>%
    ungroup() %>%
    mutate(corpus = str_remove(corpus_name, "BA_av_input_"))
  bhattacharyya_affs_sp <- bind_rows(bhattacharyya_affs_sp, bhattacharyya_affs_sp_int)
  
  bhattacharyya_affs_av_int <- df_av %>%
    group_by(model_type, model_structure, context, .draw) %>%
    summarise(bhatt_aff = bhatt(zF1, zF2, stressed_vowel)) %>%
    ungroup() %>%
    mutate(corpus = str_remove(corpus_name, "BA_av_input_"))
  bhattacharyya_affs_av <- bind_rows(bhattacharyya_affs_av, bhattacharyya_affs_av_int)
  
  
  saveRDS(bhattacharyya_affs_sp, "data/BA_calcs/Interspeech/bhattacharyya_affs_sp.rds")
  saveRDS(bhattacharyya_affs_av, "data/BA_calcs/Interspeech/bhattacharyya_affs_av.rds")
}