library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)
library(purrr)

traits <- c(
  "leaf.n",
  "lma",
  "photo",
  "rdiam",
  "rld",
  "rtd"
)

traits.alt <- c(
  "canopy",
  "g",
  "phis2",
  "srl",
  "wp",
  "wue"
)

entity.order <- c(
  "Avba",
  "Brma",
  "Brni",
  "Mepo",
  ##
  "Arca",
  "Enca",
  "Isme",
  "Saap",
  ##
  "L",
  "M",
  "H"
)


## -- FIXFIX: couldn't figure out how to extract the "keys" from this vector, so
##    redundantly provided the `entity.order` vector above
entity.labels <- c(
  "Arca" = expression(italic("A. californica")),
  "Avba" = expression(italic("A. barbata")),
  "Brma" = expression(italic("B. madritensis")),
  "Brni" = expression(italic("B. nigra")),
  "Enca" = expression(italic("E. californica")),
  "Isme" = expression(italic("I. menziesii")),
  "Mepo" = expression(italic("M. polymorpha")),
  "Saap" = expression(italic("S. apiana")),
  ##
  "L" = "Water: 50%",
  "M" = "Water: 100%",
  "H" = "Water: 150%"
)

fits <- list()
for (trait in traits) {
  fits[[trait]] <- readRDS(glue("results/no-intercept-normal-0-2-scale-within-species/{trait}-fitted.rds"))
}

df <- fits %>%
  purrr::map_df(mcmc_intervals_data) %>%
  filter(str_detect(parameter, "^r_")) %>%
  filter(!str_detect(parameter, "Intercept")) %>%
  extract(parameter, c("entity", "trait"), ".*\\[([[:alnum:]]+),([a-z.]+)\\]") %>%
  mutate(entity = factor(entity, levels = rev(entity.order), ordered = T)) %>%
  mutate(trait = recode_factor(trait,
    photo = "A[area]~-~Repro",
    leaf.n = "Leaf~N~-~Repro",
    lma = "LMA~-~Repro",
    rtd = "RTD~-~Repro",
    rld = "RLD~-~Repro",
    rdiam = "R[diam]~-~Repro",
    .ordered = T
  )) %>%
  mutate(
    slope_class = if_else(entity %in% c("L", "M", "H"), "treatment", "trait")
  )

ggplot(df, aes(x = m, y = entity, color = slope_class)) +
  facet_wrap(~trait, label = "label_parsed") +
  geom_point(shape = 3, size = 3) +
  geom_linerange(aes(xmin = ll, xmax = hh)) +
  geom_linerange(aes(xmin = l, xmax = h), size = 2.0) +
  scale_color_tableau() +
  scale_y_discrete(labels = entity.labels) +
  theme_fivethirtyeight() +
  theme(panel.spacing = unit(2, "lines"), plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"), strip.background = element_rect(fill = "white")) +
  guides(color = FALSE)
ggsave("results/figure-posterior-intervals-for-key-traits.pdf", width = 8.5, height = 11 * 0.6)


fits <- list()
for (trait in traits.alt) {
  fits[[trait]] <- readRDS(glue("results/no-intercept-normal-0-2-scale-within-species/{trait}-fitted.rds"))
}

df <- fits %>%
  purrr::map_df(mcmc_intervals_data) %>%
  filter(str_detect(parameter, "^r_")) %>%
  filter(!str_detect(parameter, "Intercept")) %>%
  extract(parameter, c("entity", "trait"), ".*\\[([[:alnum:]]+),([0-9a-z.]+)\\]") %>%
  mutate(entity = factor(entity, levels = rev(entity.order), ordered = T)) %>%
  mutate(trait = recode_factor(trait,
    canopy = "Canopy~volume~-~Repro",
    g = "g~-~Repro",
    phis2 = "Phi[PSII]~-~Repro",
    srl = "SRL~-~Repro",
    wp = "Psi[leaf]~-~Repro",
    wue = "WUE~-~Repro",
    .ordered = T
  )) %>%
  mutate(
    slope_class = if_else(entity %in% c("L", "M", "H"), "treatment", "trait")
  )


ggplot(df, aes(x = m, y = entity, color = slope_class)) +
  facet_wrap(~trait, label = "label_parsed") +
  geom_point(shape = 3, size = 3) +
  geom_linerange(aes(xmin = ll, xmax = hh)) +
  geom_linerange(aes(xmin = l, xmax = h), size = 2.0) +
  scale_color_tableau() +
  scale_y_discrete(labels = entity.labels) +
  theme_fivethirtyeight() +
  theme(panel.spacing = unit(2, "lines"), plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"), strip.background = element_rect(fill = "white")) +
  theme(panel.spacing = unit(2, "lines")) +
  guides(color = FALSE)
ggsave("results/figure-posterior-intervals-for-alt-traits.pdf", width = 8.5, height = 11 * 0.6)
