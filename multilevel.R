library(tidyverse)
library(brms)
library(bayesplot)

df <- read_csv("~/Dropbox/Macphunk/jen-drought-and-fitness/REC_raw_all_nooutlier2.csv") %>%
  select(Species, Treatment, photo, fitness) %>%
  mutate(
    Treatment=factor(Treatment, levels = c("L", "M", "H"), ordered = T),
    photo=(photo - mean(photo, na.rm=T)) / sd(photo, na.rm=T),
    ) %>%
  group_by(Species) %>%
  mutate(
    fitness=(fitness - mean(fitness, na.rm=T)) / sd(fitness, na.rm=T),
    )

df %>% summary

(ggplot(df, aes(x=photo, y=fitness))
  + facet_grid(Species ~ Treatment)
  + geom_point()
  + geom_smooth(method = lm)
)


## does photo look normalish?
(ggplot(df, aes(sample=photo))
  + facet_grid(Species ~ Treatment)
  + geom_qq()
  + geom_qq_line(color="blue")
)

## and fitness?
(ggplot(df, aes(sample=fitness))
  + facet_grid(Species ~ Treatment)
  + geom_qq()
  + geom_qq_line(color="blue")
)


b1.0 <-
  brm(data = df, family = gaussian,
      fitness ~ 1 + photo + (1 | Species) + (1 | Treatment),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 6000, warmup = 2000, chains = 4, cores = 1,
      control = list(adapt_delta = 0.99),
      seed = 99)

post <- posterior_samples(b1.0, add_chain = T)

post %>%
  select(-lp__, -iter) %>%
  mcmc_trace(facet_args = list(ncol = 4)) +
  scale_x_continuous(breaks = c(0, 2500, 5000)) +
  theme(legend.position = c(.75, .06))

neff_ratio(b1.0) %>%
  mcmc_neff()


mcmc_plot(b1.0, pars = c("^r_", "^b_", "^sd_")) +
  theme(axis.text.y = element_text(hjust = 0))
