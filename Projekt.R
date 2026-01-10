library(rstanarm)
library(bayesrules)
library(tidyverse)
library(tidybayes)

raw_data <- read.csv("~/Downloads/DATASET_BAYES_IMDB.csv")

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

# Pozor: Tento model poběží déle (minuty), protože má tisíce parametrů (pro každý film jeden)
model_complete <- stan_glmer(
  averageRating ~ runtimeMinutes + (1 | genres) + (1 | tconst), 
  data = final_dataset,
  family = gaussian,
  
  # Priory
  prior_intercept = normal(6, 2.5),
  prior = normal(0, 2.5, autoscale = TRUE),
  
  # Pro zrychlení můžete zkusit méně iterací nebo jader
  chains = 4, iter = 5000, cores = 4,
)
pp_check(model_complete)
prior_summary(model_complete)
mcmc_trace(model_complete)
mcmc_dens_overlay(model_complete)
mcmc_acf(model_complete)

# Výpis vlivu žánrů (to nás zajímá)
# (Nebudeme vypisovat tconst, protože to by byl seznam 3000 filmů)
ranef(model_complete)$genre