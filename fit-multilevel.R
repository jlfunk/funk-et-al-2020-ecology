library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)

args <- commandArgs(trailingOnly = T)
index <- as.integer(args[1])
stopifnot(index >= 1)

data.file <- "/storage/Dropbox/Macphunk/jen-drought-and-fitness/REC_raw_all_nooutlier2.csv"
traits <- c(
  "canopy",
  "g",
  "leaf.n",
  "lma",
  "phis2",
  "photo",
  "rdiam",
  "rld",
  "rtd",
  "srl",
  "wp",
  "wue"
)
stopifnot(index <= length(traits))
trait <- traits[index]

df <- read_csv(data.file) %>%
  select(Species, Treatment, fitness, one_of(traits)) %>%
  mutate(
    Treatment = factor(Treatment, levels = c("L", "M", "H"), ordered = T),
    ) %>%
  ## -- normalize fitness, and each trait, within each species
  group_by(Species) %>%
  mutate(
    fitness = (fitness - mean(fitness, na.rm = T)) / sd(fitness, na.rm = T),
    ) %>%
  mutate(
    across(traits, ~ (. - mean(., na.rm = T)) / sd(., na.rm = T))
  )


message(glue("Building model for [{trait}]"))
frm <- as.formula(glue("fitness ~ 0 + {trait} + ( 0 + {trait} | Species) + ( 1 + {trait} | Treatment )"))
bfit <- brm(
  data = df,
  family = gaussian,
  formula = frm,
  prior = c(
    prior(normal(0, 2), class = b),
    prior(cauchy(0, 1), class = sd)
  ),
  iter = 5000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 99
)



neff_ratio(bfit) %>%
  mcmc_neff() +
  theme_fivethirtyeight()
ggsave(glue("results/{trait}-neff-dx.pdf"), width = 8, height = 10)

mcmc_plot(bfit, pars = c("^r_", "^b_", "^sd_")) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme_fivethirtyeight()
ggsave(glue("results/{trait}-multilevel.pdf"), width = 8, height = 10)

saveRDS(bfit, glue("results/{trait}-fitted.rds"))
