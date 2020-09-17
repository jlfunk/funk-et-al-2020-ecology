library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)

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
  ## -- normalize each trait globally
  mutate(
    across(traits, ~ (. - mean(., na.rm = T)) / sd(., na.rm = T))
  ) %>%
  ## -- normalize fitness within each species
  group_by(Species) %>%
  mutate(
    fitness = (fitness - mean(fitness, na.rm = T)) / sd(fitness, na.rm = T),
  )


bfit <- readRDS("results/normal-0-10/photo-fitted.rds")
bfit <- readRDS("results/no-intercept-normal-0-2/photo-fitted.rds")
bfit <- readRDS("results/srl-fitted.rds")

nd <-
  expand_grid(
    srl = seq(-3, 3, 0.5),
         Treatment  = c("L", "M", "H"),
         Species    = unique(df$Species),
         ) %>%
  mutate(Treatment = factor(Treatment, levels=c("L", "M", "H"), ordered = T))
nd.grand <-
  expand_grid(srl = seq(-3, 3, 0.5))


(
  all_fitted <-
    fitted(bfit,
           newdata = nd) %>%
    as_tibble()
)
(
  grand_fitted <-
    fitted(bfit,
           newdata = nd.grand,
           re_formula = NA) %>%
    as_tibble()
)


ggplot(cbind(nd, all_fitted), aes(x=srl, y=Estimate)) +
  facet_grid(Species ~ Treatment) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "lightblue") +
  geom_line() +
  geom_line(data = cbind(nd.grand, grand_fitted), aes(x=srl, y=Estimate), color="red", alpha=0.5, linetype="dashed") +
  geom_point(data = df, aes(x=srl, y=fitness), alpha=0.5) +
  xlab("srl") +
  ylab("fitness") +
  theme_clean()

ggsave("results/no-intercept-normal-0-2/ppc-photo.pdf", width = 8.5, height = 11)


