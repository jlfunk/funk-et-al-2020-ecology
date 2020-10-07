library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)
library(purrr)

traits <- c(
  ## leaf traits
  "canopy",
  "g",
  "leaf.n",
  "lma",
  "phis2",
  "photo",
  "wp",
  "wue",
  ## root traits
  "rdiam",
  "rld",
  "rtd",
  "srl"
)

entity.order <- c(
  "Species",
  "Treatment",
  "Species:Treatment"
)


fits <- list()
for (trait in traits) {
  fits[[trait]] <- readRDS(glue("results/anova/{trait}-anova.rds"))
}

df <- fits %>%
  purrr::map_dfr(mcmc_intervals_data, .id = "trait") %>%
  filter(str_detect(parameter, "^sd_")) %>%
  extract(parameter, c("entity"), "sd_([A-Za-z:]+)__Intercept") %>%
  mutate(entity = factor(entity, levels = rev(entity.order), ordered = T)) %>%
  mutate(trait = recode_factor(trait,
    photo = "A[area]",
    g = "g",
    leaf.n = "Leaf~N",
    phis2 = "Phi[PSII]",
    ##
    wp = "Psi[leaf]",
    wue = "WUE",
    lma = "LMA",
    rtd = "RTD",
    ##
    rld = "RLD",
    srl = "SRL",
    rdiam = "R[diam]",
    canopy = "Canopy~volume",
    .ordered = T
  ))


ggplot(df, aes(x = m, y = entity)) +
  facet_wrap(~trait, ncol = 4, label = "label_parsed") +
  geom_point(shape = 3, size = 3) +
  geom_linerange(aes(xmin = ll, xmax = hh)) +
  geom_linerange(aes(xmin = l, xmax = h), size = 2.0) +
  theme_fivethirtyeight() +
  theme(
    panel.spacing = unit(2, "lines"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(),
    axis.title.y = element_blank()
  ) +
  xlab("Estimated standard deviation of effects")

ggsave("results/figure-variance-partitioning.pdf", width = 8.5, height = 8.5 * 0.60)
