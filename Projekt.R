library(bayesrules)
library(bayesplot)
library(tidyverse)
library(psych)
library(ggcorrplot)
library(glue)
library(rstanarm)
library(broom.mixed)
library(tidybayes)
library(loo)

raw_data <- read.csv("DATASET_BAYES_IMDB.csv")

genre_manipulation <- raw_data %>%
  # 1. Odstraníme duplicity způsobené režiséry (jako předtím)
  group_by(tconst) %>%
  slice(1) %>%
  ungroup() %>%
  
  # 2. ZDE JE ZMĚNA: Rozdělíme žánry do více řádků
  # Film s žánry "Horror,Sci-Fi" bude mít teď dva řádky
  separate_rows(genres, sep = ",") %>%
  
  mutate(genres = as.factor(genres))
final_dataset <- genre_manipulation
# Kontrola: Počet řádků by měl být vyšší než počet unikátních filmů
print(nrow(genre_manipulation)) 
print(n_distinct(genre_manipulation$tconst))

#pridani filtru pro zanry ktere definuji alespon 20 filmu protoze male vyorky delaji problem v cv
final_dataset_20 <- final_dataset %>%
  # Seskupit podle sloupce s žánry
  group_by(genres) %>% 
  
  # Necháme jen žánry, co mají alespoň 20 filmů
  filter(n() >= 20) %>% 
  
  ungroup() %>%
  
  mutate(genres = droplevels(factor(genres)))

#-----------------------------------------------------------------------------
#models
#-----------------------------------------------------------------------------

model_start <- stan_glmer(
  averageRating ~ runtimeMinutes + (1 | genres), 
  data = final_dataset_20,
  family = gaussian,
  
  # Priory
  prior_intercept = normal(6, 2.5),
  prior = normal(0, 2.5, autoscale = TRUE),
  chains = 4, iter = 5000*2, cores = 4,
)
pp_check(model_start)
prior_summary(model_start)
mcmc_trace(model_start)
mcmc_dens_overlay(model_start)
mcmc_acf(model_start)

# Výpis vlivu žánrů (to nás zajímá)
model_startranef <- ranef(model_start)$genre
test <- ranef(model_start)
VarCorr(model_start)


model_2 <- stan_glmer(
  averageRating ~ runtimeMinutes + (1 | genres) + numVotes, 
  data = final_dataset_20,
  family = gaussian,
  
  # Priory
  prior_intercept = normal(6, 2.5),
  prior = normal(0, 2.5, autoscale = TRUE),
  chains = 4, iter = 5000*2, cores = 6,
)
pp_check(model_2)
prior_summary(model_2)
mcmc_trace(model_2)
mcmc_dens_overlay(model_2)
mcmc_acf(model_2)
model_2ranef <- ranef(model_2)$genre

model_3 <- stan_glm(
  averageRating ~ runtimeMinutes + numVotes + genres, 
  data = final_dataset_20,
  family = gaussian,
  
  # Priory
  prior_intercept = normal(6, 2.5),
  prior = normal(0, 2.5, autoscale = TRUE),
  chains = 4, iter = 5000*2, cores = 4,
)
pp_check(model_3)
prior_summary(model_3)
mcmc_trace(model_3)
mcmc_dens_overlay(model_3)
mcmc_acf(model_3)

model_4 <- stan_glmer(
  averageRating ~ runtimeMinutes + (1 | genres) + (1 | tconst), 
  data = final_dataset_20,
  family = gaussian,
  
  # Priory
  prior_intercept = normal(6, 2.5),
  prior = normal(0, 2.5, autoscale = TRUE),
  chains = 4, iter = 5000, cores = 4,
)

cv_1 <- prediction_summary_cv(
  model = model_start,
  data  = final_dataset_20,
  k     = 10,
  group = "genres"
)

cv_2 <- prediction_summary_cv(
  model = model_2,
  data  = final_dataset_20,
  k     = 10,
  group = "genres"
)

cv_3 <- prediction_summary_cv(
  model = model_3,
  data  = final_dataset_20,
  k     = 10
)

cv_1
cv_2
cv_3

loo_1 <- loo(model_start)
loo_2 <- loo(model_2)
loo_3 <- loo(model_3)

loo_1$estimates["elpd_loo", ]
loo_2$estimates["elpd_loo", ]
loo_3$estimates["elpd_loo", ]

# Direct comparison
loo_compare(loo_1, loo_2, loo_3)

