library(brms)
library(dplyr)
library(tidyr)
library(stringr)
library(tidybayes)

rep_df <- function(df, n_reps)
{
  df_repeated <- do.call("rbind", replicate(n_reps, df, simplify = FALSE))
}

# by-speaker fitted values for each corpus to put in big dataframe
get_fitted_sp <- function(nd2, model, num_draws, corpus_name, model_type, model_structure){
  fitted_sp_int <- nd2 %>% 
    add_epred_draws(model, 
                    re_formula = ~(1 + context * stressed_vowel | 
                                     q | speaker),
                    ndraws = num_draws) %>%
    ungroup() 
  if (model_type=="m"){
    fitted_sp_int <- fitted_sp_int %>% pivot_wider(names_from=".category", values_from='.epred')
  }
  fitted_sp_int <- fitted_sp_int %>% mutate(corpus = corpus_name, model_type = model_type, model_structure = model_structure)
  }


# average-speaker fitted values for each corpus to put in big dataframe
get_fitted_av <- function(nd1, model, num_draws, corpus_name, model_type, model_structure){
  fitted_av_int <- nd1 %>% 
    add_epred_draws(model, 
                    re_formula = NA,
                    ndraws = num_draws) %>%
    ungroup()
  if (model_type=="m"){
    fitted_av_int <- fitted_av_int %>% pivot_wider(names_from=".category", values_from='.epred')
  }
  fitted_av_int <- fitted_av_int %>% mutate(corpus = corpus_name, model_type = model_type, model_structure = model_structure)
  }

# by-speaker predicted values for each corpus to put in big dataframe
get_predicted_sp <- function(nd2, model, num_draws, corpus_name, model_type, model_structure){
  predicted_sp_int <- nd2 %>% 
    add_predicted_draws(model, 
                        re_formula = ~(1 + context * stressed_vowel | 
                                         q | speaker),
                        ndraws = num_draws) %>%
    ungroup()
  if (model_type=="m"){
    predicted_sp_int <- predicted_sp_int %>% pivot_wider(names_from=".category", values_from='.prediction')
  }
  predicted_sp_int <- predicted_sp_int %>% mutate(corpus = corpus_name, model_type = model_type, model_structure = model_structure)
  }


# average-speaker predicted values for each corpus to put in big dataframe
get_predicted_av <- function(nd1, model, num_draws, corpus_name, model_type, model_structure){
  predicted_av_int <- nd1 %>% 
    add_predicted_draws(model, 
                        re_formula = NA,
                        ndraws = num_draws) %>%
    ungroup()
  if (model_type=="m"){
    predicted_av_int <- predicted_av_int %>% pivot_wider(names_from=".category", values_from='.prediction')
  }
  predicted_av_int <- predicted_av_int %>% mutate(corpus = corpus_name, model_type = model_type, model_structure = model_structure)
  }

# by-speaker predictions with multiple points per draw to calculate BA
get_predicted_sp_BA <- function(nd2, model, num_points, num_draws, corpus_name, model_type, model_structure){
  predicted_sp_BA <- nd2 %>% 
    rep_df(num_points) %>%
    add_predicted_draws(model,
                        re_formula = ~(1 + context * stressed_vowel | 
                                         q | speaker),
                        ndraws = num_draws) %>%
    ungroup()
  if (model_type=="m"){
    predicted_sp_BA <- predicted_sp_BA %>% pivot_wider(names_from=".category", values_from='.prediction')
  }
  predicted_sp_BA <- predicted_sp_BA %>% mutate(corpus = corpus_name, model_type = model_type, model_structure = model_structure)
  }

# average speaker predictions with multiple points per draw to calculate BA
get_predicted_av_BA <- function(nd1, model, num_points, num_draws, corpus_name, model_type, model_structure){
  predicted_av_BA <- nd1 %>% 
    rep_df(num_points) %>%
    add_predicted_draws(model, 
                        re_formula = NA,
                        ndraws = num_draws) %>%
    ungroup()
  if (model_type=="m"){
    predicted_av_BA <- predicted_av_BA %>% pivot_wider(names_from=".category", values_from='.prediction')
  }
  predicted_av_BA <- predicted_av_BA %>% mutate(corpus = corpus_name, model_type = model_type, model_structure = model_structure)
}


num_points <- 1000 # the number of points used to calculate BA
num_draws <- 100 # the number of points used to calculate error bars 
# each BA comes from one draw)

filepath <- "models/Interspeech"
model_dir <- dir(filepath) %>%
  data.frame() %>%
  filter(!str_detect(., "m6"))
model_dir <- model_dir[,1]

predicted_sp <- data.frame()
predicted_av <- data.frame()
fitted_sp <- data.frame()
fitted_av <- data.frame()

i <- 1
while (i <= length(model_dir)) {
  model <- model_dir[i]
  model_parts <- str_split_fixed(model, "_", 6)
  # 1: corpus name
  # 2: dialect country
  # 3: dialect label
  # 4: model type (m or u)
  # 5: mode structure (m1, m2, m6; not currently working for m6)
  # 6: formant (F1 or F2, only for univariate models)
  corpus_name <- model_parts[,1]
  model_type <- model_parts[,4]
  
  m <- readRDS(paste(filepath, "/", model, sep=""))
  if (str_detect(model_parts[,5], "m1")){
    nd1 <- expand_grid(context = levels(m$data$context),
                       stressed_vowel = levels(m$data$stressed_vowel))
    nd2 <- expand_grid(context = levels(m$data$context), 
                       stressed_vowel = levels(m$data$stressed_vowel),
                       speaker = unique(m$data$speaker))
    model_structure <- "m1"
  }
  if (str_detect(model_parts[,5], "m2")){
    nd1 <- expand_grid(context = levels(m$data$context),
                       stressed_vowel = levels(m$data$stressed_vowel),
                       log_duration = 0)
    nd2 <- expand_grid(context = levels(m$data$context), 
                       stressed_vowel = levels(m$data$stressed_vowel),
                       log_duration = 0,
                       speaker = unique(m$data$speaker))
    model_structure <- "m2"
  }

  
  # set.seed(192)
  fitted_sp_int <- get_fitted_sp(nd2, m, num_draws, corpus_name, model_type, model_structure)
  fitted_av_int <- get_fitted_av(nd1, m, num_draws, corpus_name, model_type, model_structure)
  predicted_sp_int <- get_predicted_sp(nd2, m, num_draws, corpus_name, model_type, model_structure)
  predicted_av_int <- get_predicted_av(nd1, m, num_draws, corpus_name, model_type, model_structure)
  predicted_sp_BA <- get_predicted_sp_BA(nd2, m, num_points, num_draws, corpus_name, model_type, model_structure)
  predicted_av_BA <- get_predicted_av_BA(nd1, m, num_points, num_draws, corpus_name, model_type, model_structure)
  
  if (model_type == "u"){
    i <- i+1
    # set.seed(192)
    model <- model_dir[i]
    m <- readRDS(paste(filepath, "/", model, sep=""))
    fitted_sp_F2 <- get_fitted_sp(nd2, m, num_draws, corpus_name, model_type, model_structure)
    fitted_av_F2 <- get_fitted_av(nd1, m, num_draws, corpus_name, model_type, model_structure)
    predicted_sp_F2 <- get_predicted_sp(nd2, m, num_draws, corpus_name, model_type, model_structure)
    predicted_av_F2 <- get_predicted_av(nd1, m, num_draws, corpus_name, model_type, model_structure)
    predicted_sp_BA_F2 <- get_predicted_sp_BA(nd2, m, num_points, num_draws, corpus_name, model_type, model_structure)
    predicted_av_BA_F2 <- get_predicted_av_BA(nd1, m, num_points, num_draws, corpus_name, model_type, model_structure)
    
    fitted_sp_int <- fitted_sp_int %>%
      inner_join(fitted_sp_F2, by=colnames(fitted_sp_int)[!colnames(fitted_sp_int)==".epred"]) %>%
      rename(zF1=".epred.x", zF2=".epred.y")
    fitted_av_int <- fitted_av_int %>%
      inner_join(fitted_av_F2, by=colnames(fitted_av_int)[!colnames(fitted_av_int)==".epred"]) %>%
      rename(zF1=".epred.x", zF2=".epred.y")
    predicted_sp_int <- predicted_sp_int %>%
      inner_join(predicted_sp_F2, by=colnames(predicted_sp_int)[!colnames(predicted_sp_int)==".prediction"]) %>%
      rename(zF1=".prediction.x", zF2=".prediction.y")
    predicted_av_int <- predicted_av_int %>%
      inner_join(predicted_av_F2, by=colnames(predicted_av_int)[!colnames(predicted_av_int)==".prediction"]) %>%
      rename(zF1=".prediction.x", zF2=".prediction.y")
    predicted_sp_BA <- predicted_sp_BA %>%
      inner_join(predicted_sp_BA_F2, by=colnames(predicted_sp_BA)[!colnames(predicted_sp_BA)==".prediction"]) %>%
      rename(zF1=".prediction.x", zF2=".prediction.y")
    predicted_av_BA <- predicted_av_BA %>%
      inner_join(predicted_av_BA_F2, by=colnames(predicted_av_BA)[!colnames(predicted_av_BA)==".prediction"]) %>%
      rename(zF1=".prediction.x", zF2=".prediction.y")
  }
  
  # update dataframes
  predicted_sp <- bind_rows(predicted_sp, predicted_sp_int)
  predicted_av <- bind_rows(predicted_av, predicted_av_int)
  fitted_sp <- bind_rows(fitted_sp, fitted_sp_int)
  fitted_av <- bind_rows(fitted_av, fitted_av_int)
  
  # save individual dataframes for BA calculations
  saveRDS(predicted_sp_BA, paste("data/BA_calcs/Interspeech/BA_sp_input", 
                                 corpus_name, model_type, model_structure, 
                                 sep="_"))
  saveRDS(predicted_av_BA, paste("data/BA_calcs/Interspeech/BA_av_input", 
                                 corpus_name, model_type, model_structure, 
                                 sep="_"))
  
  i <- i+1
}

saveRDS(predicted_sp, "data/predicted_dataframes/Interspeech/predicted_sp.rds")
saveRDS(predicted_av, "data/predicted_dataframes/Interspeech/predicted_av.rds")
saveRDS(fitted_sp, "data/predicted_dataframes/Interspeech/fitted_sp.rds")
saveRDS(fitted_av, "data/predicted_dataframes/Interspeech/fitted_av.rds")