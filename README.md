## Analyses and Figures for Funk et al. Ecology (2021)

This repo contains the data and source code to reproduce the analyses and figures
for the manuscript "Plant traits are differentially linked to performance in a semi-arid
ecosystem", to be published in *Ecology* in 2021.

### Dependencies

- Installation of the `R` interpreter, and these packages:
  - `bayesplot`
  - `brms`
  - `ggthemes`
  - `glue`
  - `tidyverse`
- (Optional: To use the Makefile) A unix-style system that can run `make`.
- (Optional: For conversion from PDF to TIFF) `ImageMagick`


### Usage

The R scripts beginning with `fit` perform the analyses, and the scripts
beginning with `generate` create the figures.

Consult the [Makefile](./Makefile) for available recipes, and for guidance on
invoking the R scripts.

For example, running:

    make multilevel-fits

will reproduce the "Model 2" multilevel fits from the paper.
