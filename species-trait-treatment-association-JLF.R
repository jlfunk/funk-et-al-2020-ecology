library(tidyverse)
library(glue)
library(ggthemes)

traits <- c(
  "photo",
  "logg",
  "phis2",
  "leaf.n",
  "wp",
  "logwue",
  "loglma",
  "logrtd",
  "logrld",
  "logsrl",
  "logrdiam",
  "logcanopy"
)

response <- "logfitness"


data.file <- "./REC_raw_all_nooutlier2.csv"

df <- readr::read_csv(data.file) %>%
  mutate(
    Treatment = factor(Treatment, levels = c("L", "M", "H"), ordered = T),
  ) %>%
  mutate(
    logrld = log(rld),
    logsrl = log(srl),
    logrdiam = log(rdiam),
    logrtd = log(rtd),
    logfitness = log(fitness),
    loglma = log(lma),
    logg = log(g),
    logwue = log(wue),
    logcanopy = log(canopy)
  ) %>%
  select(one_of(traits), Species, Treatment, response) %>%
  ## z-transform within species and two treatments
  group_by(Species, Treatment) %>%
  mutate_at(c(traits, response), scale) %>%
  ungroup()

species.labels <- c("Avba", "Brma", "Brni", "Mepo", "Arca", "Enca", "Isme", "Saap")

# annuals <- c('Avba', 'Mepo', 'Brma', 'Brni')
# perennials <- c('Arca', 'Enca', 'Isme', 'Saap')
# treatment.labels <- c('L', 'M', 'H')

# response.name <- "reproductive"
# -- original order from somewhere, preserving for posterity
# trait.names <- c("rgr.t", "a.t", "lma.t", "wp.t",
#                 "leaf.n.t", "g.t", "e.t", "phis2.t",
#                 "wue.t", "rld.t", "rld.canopy.t",
#                 "srl.t", "rdiam.t")
# -- given in the order they're intended to appear in the table rows
# trait.names <- c("a", "final_canopy_vol", "g", "leaf.n", "wp",
#                 "lma", "phis2", "rdiam", "rld", "srl",
#                 "wue")

# trait.names <- c("photo", "g", "phis2", "leaf.n", "wp", "wue", "lma",
#                 "rtd",  "rld",
#                 "srl", "rdiam", "final_canopy_vol")
# trait.names <- rev(trait.names)

trait.names.map <- c(
  "photo" = expression(A[area]),
  "logcanopy" = "Canopy volume",
  "logg" = "g",
  "leaf.n" = "Leaf N",
  "wp" = expression(Psi[leaf]),
  "loglma" = "LMA",
  "phis2" = expression(Phi[PSII]),
  "logrdiam" = expression(R[diam]),
  "logrld" = "RLD",
  "logrtd" = "RTD",
  "logsrl" = "SRL",
  "logwue" = "WUE"
)

transform.trait.names <- function(tn) {
  trait.names.map[[tn]]
}


pos.color <- rgb(8 / 255, 119 / 255, 189 / 255)
neg.color <- rgb(245 / 255, 147 / 255, 34 / 255)

# new graphic
response <- "logfitness"
cor.df <- data.frame(species = NA, treatment = NA, trait = NA, cor.val = NA, cor.p = NA)
for (species in species.labels) {
  for (treatment in c("L", "M", "H")) {
    df.sub <- subset(df, Species == species & Treatment == treatment)
    for (trait in traits) {
      result <- cor.test(df.sub[[trait]], df.sub[[response]], method = "pearson", exact = F)
      cor.val <- result[["estimate"]]
      cor.p <- result[["p.value"]]
      cor.df <- rbind(cor.df, c(species, treatment, trait, cor.val, cor.p))
    }
  }
}
cor.df <- cor.df %>%
  drop_na() %>%
  mutate(
    species = factor(species),
    treatment = factor(treatment, levels = c("L", "M", "H"), ordered = T),
    trait = factor(trait, levels = rev(traits), ordered = T),
    cor.val = as.numeric(cor.val),
    cor.p = as.numeric(cor.p)
  ) %>%
  mutate(treatment = recode_factor(treatment,
    L = "50",
    M = "100",
    H = "150",
    .ordered = T
  )) %>%
  mutate(species = recode_factor(species,
    Avba = "italic(A.~barbata)",
    Brma = "italic(B.~madritensis)",
    Brni = "italic(B.~nigra)",
    Mepo = "italic(M.~polymorpha)",
    ##
    Arca = "italic(A.~californica)",
    Enca = "italic(E.~californica)",
    Isme = "italic(I.~menziesii)",
    Saap = "italic(S.~apiana)",
    .ordered = T
  ))


ggplot(cor.df, aes(x = treatment, y = trait, color = cor.val, size = abs(cor.val))) +
  facet_grid(~species, label = "label_parsed") +
  geom_point() +
  ## scale_shape_manual(values = c(21, 19)) +
  xlab("") +
  ylab("") +
  ## guides(shape=FALSE) +
  scale_y_discrete(labels = trait.names.map) +
  scale_color_gradient2(low = neg.color, mid = "white", high = pos.color, midpoint = 0.0, limits = c(-1, 1)) +
  ## scale_color_manual(values = c("TRUE" = pos.color, "FALSE"= neg.color)) +
  guides(
    color = guide_colorbar(title = "Pearson Correlation"),
    size = guide_legend(title = "Pearson Correlation Absolute Value")
  ) +
  theme(
    panel.spacing = unit(2, "lines"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
  ) +
  theme_fivethirtyeight() +
  theme(
    ## panel.spacing = unit(2, "lines"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    ## -- hide gray box behind size guide
    legend.key = element_blank()
  )

ggsave("results/figure-trait-correlations.pdf", width = 10.5, height = 11 * 0.5)
