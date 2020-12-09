## Analyses and Figures for Funk et al, Ecology (2021)

This repo contains the source code to reproduce the analyses and figures for the
manuscript "Plant traits are differentially linked to performance in a semi-arid
ecosystem", to be published in *Ecology* in 2021.

### Requirements

- A unix-style system that can run `make`.
- Installation of the `R` interpreter, and these packages:
  - `bayesplot`
  - `brms`
  - `ggthemes`
  - `glue`
  - `tidyverse`
- (For conversion from PDF to TIFF) `ImageMagick`


### Usage

Consult the [Makefile](./Makefile) to see the available recipes.

For example, running:

    make multilevel-fits

will reproduce the "Model 2" multilevel fits from the paper.


The scripts beginning with `fit` perform the analyses, and the scripts beginning
with `generate` create the figures.
