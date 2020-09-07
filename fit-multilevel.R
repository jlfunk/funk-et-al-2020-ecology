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
  "leaf.n",
  "lma",
  "photo",
  "rdiam",
  "rld",
  "rtd"
)
stopifnot(index <= length(traits))
trait <- traits[index]

df <- read_csv(data.file) %>%
  select(Species, Treatment, fitness, one_of(traits)) %>%
  mutate(
    Treatment = factor(Treatment, levels = c("L", "M", "H"), ordered = T),
    ## -- normalize each trait globally
    leaf.n = (leaf.n - mean(leaf.n, na.rm = T)) / sd(leaf.n, na.rm = T),
    lma = (lma - mean(lma, na.rm = T)) / sd(lma, na.rm = T),
    photo = (photo - mean(photo, na.rm = T)) / sd(photo, na.rm = T),
    rdiam = (rdiam - mean(rdiam, na.rm = T)) / sd(rdiam, na.rm = T),
    rld = (rld - mean(rld, na.rm = T)) / sd(rld, na.rm = T),
    rtd = (rtd - mean(rtd, na.rm = T)) / sd(rtd, na.rm = T),
  ) %>%
  ## -- normalize fitness within each species
  group_by(Species) %>%
  mutate(
    fitness = (fitness - mean(fitness, na.rm = T)) / sd(fitness, na.rm = T),
  )

message(glue("Building model for [{trait}]"))
frm <- as.formula(glue("fitness ~ 1 + {trait} + ( 1 + {trait} | Species) + ( 1 + {trait} | Treatment )"))
bfit <- brm(
  data = df,
  family = gaussian,
  formula = frm,
  prior = c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
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
ggsave(glue("{trait}-neff-dx.pdf"), width = 8, height = 10)

mcmc_plot(bfit, pars = c("^r_", "^b_", "^sd_")) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme_fivethirtyeight()
ggsave(glue("{trait}-multilevel.pdf"), width = 8, height = 10)

saveRDS(bfit, glue("{trait}-fitted.rds"))
