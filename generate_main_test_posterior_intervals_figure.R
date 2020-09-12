library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)
library(purrr)

traits <- c(
  ## "canopy",
  ## "g",
  "leaf.n",
  "lma",
  ## "phis2",
  "photo",
  "rdiam",
  "rld",
  "rtd"
  ## "srl",
  ## "wp",
  ## "wue"
)

plot.order <- c(
  "Arca",
  "Avba",
  "Brma",
  "Brni",
  "Enca",
  "Isme",
  "Mepo",
  "Saap",
  ##
  "L",
  "M",
  "H"
)

trait.labels <- list(
  leaf.n = "Leaf N",
  lma = "LMA"
)

fits <- list()
for (trait in traits) {
  fits[[trait]] <- readRDS(glue("results/no-intercept-normal-0-2/{trait}-fitted.rds"))
}

df <- fits %>%
  purrr::map_df(mcmc_intervals_data) %>%
  filter(str_detect(parameter, "^r_")) %>%
  filter(!str_detect(parameter, "Intercept")) %>%
  extract(parameter, c("entity", "trait"), ".*\\[([[:alnum:]]+),([a-z.]+)\\]") %>%
  mutate(entity = factor(entity, levels = rev(plot.order), ordered = T)) %>%
  mutate(trait = recode(trait,
    leaf.n = "Leaf~N",
    lma = "LMA", photo = "A[area]", rdiam = "Root~Diameter", rld = "Root~Length~Depth", rtd = "Root~Total~Depth"
  ))

ggplot(df, aes(x = m, y = entity)) +
  facet_wrap(~trait, label = "label_parsed") +
  geom_point(shape = 3, size = 4) +
  geom_linerange(aes(xmin = ll, xmax = hh)) +
  geom_linerange(aes(xmin = l, xmax = h), size = 2.5) +
  theme_fivethirtyeight() +
  ## xlim(-1, 1) |
  ## geom_pointrange(aes(y=m, ymin=l, ymax=h), color="blue") +
  theme(panel.spacing = unit(2, "lines"))
