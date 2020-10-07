library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)

args <- commandArgs(trailingOnly = T)
index <- as.integer(args[1])
stopifnot(index >= 1)

data.file <- "./REC_raw_all_nooutlier2.csv"
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
  mutate(
    across(traits, ~ (. - mean(., na.rm = T)) / sd(., na.rm = T))
  )


message(glue("Building anova model for [{trait}]"))
frm <- as.formula(glue("{trait} ~ (1 | Species * Treatment)"))
bfit <- brm(
  data = df,
  family = gaussian,
  formula = frm,
  prior = c(
    prior(normal(0, 2), class = Intercept),
    prior(cauchy(0, 1), class = sd)
  ),
  ## iter = 5000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 99
)

mcmc_plot(bfit, type = "neff") +
  theme_fivethirtyeight()
ggsave(glue("results/{trait}-neff-anova.pdf"), width = 8, height = 10)

mcmc_plot(bfit, type = "intervals", pars = c("^sd_")) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme_fivethirtyeight() +
  ggtitle(glue("Variance Partitioning for [{trait}]"))
ggsave(glue("results/{trait}-anova.pdf"), width = 8, height = 4)

saveRDS(bfit, glue("results/{trait}-anova.rds"))
