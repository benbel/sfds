library(tidyverse) 
library(nimble) 
library(coda) 
library(ggmcmc) 

n_obs_binomial <- 100

set.seed(123) 

true_theta_binomial <- 0.25

obs_binomial <- rbinom(
  n = n_obs_binomial, 
  size = 1,
  prob = true_theta_binomial
  )

nimble_code_binomial <- nimbleCode(
  {
    # Priors
    theta ~ dbeta(a0, b0)
    # Génération des données
    for(i in 1:n){ # n sera la taille des données 
      # (à préciser dans l'appel du code)
      x[i] ~ dbinom(size = 1, prob = theta)
      # Ici, on stipule que y est un vecteur de composantes indépendantes.
    }
  }
  )
  
nimble_model_binomial <- nimbleModel(
  code = nimble_code_binomial,
  constants = list(
    n = n_obs_binomial,
    a0 = 1,
    b0 = 1),
  data = list(x = obs_binomial)
  )

nimble_sampler_binomial <- buildMCMC(nimble_model_binomial)
compiled_binomial <- compileNimble(nimble_model_binomial, nimble_sampler_binomial)
ech_post_binomial <- runMCMC(compiled_binomial$nimble_sampler_binomial, 
                             samplesAsCodaMCMC = TRUE) # Mise au format coda
# Mise au format data.frame adapté au ggplot
ech_post_binomial_df <- ggs(ech_post_binomial)
print(ech_post_binomial_df) # Regardez le résultat!

ggplot(ech_post_binomial_df) +
  aes(x = value) +
  geom_density(aes(color = "Empirical posterior")) +
  geom_vline(xintercept = true_theta_binomial, 
             color = "red", linetype = 2) + # Vraie valeur
  # On peut ajouter la vraie distribution a posteriori qui est connue ici
  stat_function(fun = function(x) dbeta(x, 
                                        shape1 = 1 + sum(obs_binomial), 
                                        shape2 = 1 + sum(1 - obs_binomial)),
                aes(color = "True posterior")) +
  labs(x = expression(Valeur~de~theta),
       y = "Densité", color = "")


ech_post_binomial_df$value %>% mean()
(ech_post_binomial_df$value > 0.35) %>% mean()
