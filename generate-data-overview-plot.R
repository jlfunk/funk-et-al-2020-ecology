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
  mutate(Treatment = fct_recode(Treatment, `50%`="L", `100%`="M", `150%`="H")) %>%
  ## -- normalize fitness within each species
  group_by(Species) %>%
  mutate(
    fitness = (fitness - mean(fitness, na.rm = T)) / sd(fitness, na.rm = T),
    ) %>%
  ## -- normalize each trait within each species
  group_by(Species) %>%
  mutate(
    across(all_of(traits), ~ (. - mean(., na.rm = T)) / sd(., na.rm = T))
  ) %>%
  identity

ggplot(df %>%
       pivot_longer(one_of(c(traits, "fitness"))) %>%
       mutate(Species = factor(Species)) %>%
       mutate(name = recode_factor(name,
                                    photo = "A[area]",
                                    g = "g",
                                    phis2 = "Phi[PSII]",
                                    leaf.n = "Leaf~N",
                                    wp = "Psi[leaf]",
                                    wue = "WUE",
                                    lma = "LMA",
                                    rtd = "RTD",
                                    rld = "RLD",
                                    srl = "SRL",
                                    rdiam = "R[diam]",
                                    canopy = "Canopy~volume",
                                    fitness = "Repro",
                                    .ordered=T
                                   ),
              Species = recode_factor(Species,
                                      Arca = "italic(A.~californica)",
                                      Avba = "italic(A.~barbata)",
                                      Brma = "italic(B.~madritensis)",
                                      Brni = "italic(B.~nigra)",
                                      Enca = "italic(E.~californica)",
                                      Isme = "italic(I.~menziesii)",
                                      Mepo = "italic(M.~polymorpha)",
                                      Saap = "italic(S.~apiana)",
                                      .ordered=T
                                      )),
       aes(x=Treatment, y=value)) +
  facet_grid(name ~ Species, label="label_parsed") +
  geom_boxplot(width=0.3) +
  ylab("Standardized Trait Value")

ggsave(glue("results/data-overview.pdf"), width = 8.5, height = 11)
