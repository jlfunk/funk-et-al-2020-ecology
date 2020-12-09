# -- MODELS
.PHONY: multilevel-fits
multilevel-fits: fit-multilevel.R
	parallel -j3 Rscript ./fit-multilevel.R {} ::: 1 2 3 4 5 6 7 8 9 10 11 12

.PHONY: anova-fits
anova-fits: fit-anova.R
	parallel -j3 Rscript ./fit-anova.R {} ::: 1 2 3 4 5 6 7 8 9 10 11 12


# -- FIGURES
.PHONY: figures
figures: \
	ppc \
	results/figure-anova-trait-effects.pdf \
	results/figure-posterior-intervals-for-alt-traits.pdf \
	results/figure-posterior-intervals-for-key-traits.pdf \
	results/figure-trait-correlations.pdf \
	results/figure-variance-partitioning.pdf


results/figure-variance-partitioning.pdf: generate-variance-partitioning-figure.R
	Rscript $^

results/figure-anova-trait-effects.pdf: generate-anova-trait-effects-figure.R
	Rscript $^

results/figure-trait-correlations.pdf: generate-species-trait-treatment-association-figure.R
	Rscript $^

results/figure-posterior-intervals-for-key-traits.pdf results/figure-posterior-intervals-for-alt-traits.pdf: generate-key-traits-posterior-intervals-figure.R
	Rscript $^

.PHONY: ppc
ppc: generate-ppc-figures.R
	parallel Rscript $^ {} ::: 1 2 3 4 5 6 7 8 9 10 11 12


# -- PDF -> TIFF conversion
.PHONY: tiffs
tiffs: results/figure-posterior-intervals-for-key-traits.tiff \
  results/figure-posterior-intervals-for-alt-traits.tiff \
  results/figure-variance-partitioning.tiff

results/figure-posterior-intervals-for-key-traits.tiff: results/figure-posterior-intervals-for-key-traits.pdf
	convert -density 2400 $< -resize 5% $@

results/figure-posterior-intervals-for-alt-traits.tiff: results/figure-posterior-intervals-for-alt-traits.pdf
	convert -density 2400 $< -resize 5% $@

results/figure-variance-partitioning.tiff: results/figure-variance-partitioning.pdf
	convert -density 2400 $< -resize 5% $@

results/figure-trait-correlations.tiff: results/figure-trait-correlations.pdf
	convert -density 2400 $< -resize 5% $@


.PHONY: clean
clean:
	rm -rf results/*.pdf results/*.tiff
