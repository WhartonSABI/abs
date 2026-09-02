.PHONY: install snapshot test test-revealed-policy pipeline fixed-clock-smoke fixed-clock-full report dashboard outputs clean

install:
	Rscript --vanilla -e 'dir.create(".Rlib", showWarnings = FALSE); install.packages("renv", lib = ".Rlib")'
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::snapshot(prompt = FALSE, dev = TRUE)'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

test-revealed-policy:
	Rscript -e 'testthat::test_dir("tests/testthat", filter = "revealed", stop_on_failure = TRUE)'

pipeline:
	Rscript -e 'targets::tar_make()'

fixed-clock-smoke:
	ABS_FIXED_CLOCK_PROFILE=smoke ABS_FIXED_CLOCK_REFRESH_TARGETS=false Rscript scripts/run_fixed_clock_confirmation_1d.R

fixed-clock-full:
	Rscript scripts/run_fixed_clock_confirmation_1d.R

report:
	quarto render report/index.qmd
	cp report/index.pdf output/pdf/mlb-abs-challenge-run-value.pdf

dashboard:
	Rscript scripts/build_dashboard_assets.R

outputs: pipeline report dashboard

clean:
	Rscript -e 'targets::tar_destroy(destroy = "objects", ask = FALSE)'
