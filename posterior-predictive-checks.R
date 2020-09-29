library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)

args <- commandArgs(trailingOnly = T)
index <- as.integer(args[1])
stopifnot(index >= 1)

trait.map <- list(
canopy = expression(Standardized ~ Canopy~volume),
g      = expression(Standardized ~ g),
leaf.n = expression(Standardized ~ Leaf~N),
lma    = expression(Standardized ~ LMA),
phis2  = expression(Standardized ~ Phi[PSII]),
photo  = expression(Standardized ~ A[area]),
wp     = expression(Standardized ~ Psi[leaf]),
wue    = expression(Standardized ~ WUE),
rdiam  = expression(Standardized ~ R[diam]),
rld    = expression(Standardized ~ RLD),
rtd    = expression(Standardized ~ RTD),
srl    = expression(Standardized ~ SRL)
)

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
  ## -- normalize each trait within species
  group_by(Species) %>%
  mutate(
    across(traits, ~ (. - mean(., na.rm = T)) / sd(., na.rm = T))
  ) %>%
  ## -- normalize fitness within each species
  group_by(Species) %>%
  mutate(
    fitness = (fitness - mean(fitness, na.rm = T)) / sd(fitness, na.rm = T),
    )


bfit <- readRDS(glue("results/no-intercept-normal-0-2-scale-within-species/{trait}-fitted.rds"))

nd <-
  expand_grid(
    trait = seq(-3, 3, 0.5),
    Treatment = c("L", "M", "H"),
    Species = unique(df$Species),
  ) %>%
  mutate(Treatment = factor(Treatment, levels = c("L", "M", "H"), ordered = T)) %>%
  rename(!!trait:=trait)
nd.grand <-
  expand_grid(trait = seq(-3, 3, 0.5)) %>%
  rename(!!trait:=trait)



all_fitted <-
  fitted(bfit,
    newdata = nd
  ) %>%
  as_tibble()

grand_fitted <-
  fitted(bfit,
    newdata = nd.grand,
    re_formula = NA
  ) %>%
  as_tibble()



ggplot(cbind(nd, all_fitted) %>%
       mutate(Treatment = fct_recode(Treatment, `50%`="L", `100%`="M", `150%`="H")),
       aes_string(x = trait, y = "Estimate")) +
  facet_grid(Species ~ Treatment) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "lightblue") +
  geom_line() +
  geom_line(data = cbind(nd.grand, grand_fitted), aes_string(x = trait, y = "Estimate"), color = "red", alpha = 0.5, linetype = "dashed") +
  geom_point(data = df %>% mutate(Treatment = fct_recode(Treatment, `50%`="L", `100%`="M", `150%`="H")), aes_string(x = trait, y = "fitness"), alpha = 0.5) +
  xlab(trait.map[[trait]]) +
  ylab("Reproductive Allocation") +
  theme_clean()

ggsave(glue("results/ppc/posterior-predictive-check-{trait}.pdf"), width = 8.5, height = 11)
