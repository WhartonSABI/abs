.PHONY: install snapshot test pipeline report dashboard outputs clean

install:
	Rscript --vanilla -e 'dir.create(".Rlib", showWarnings = FALSE); install.packages("renv", lib = ".Rlib")'
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::snapshot(prompt = FALSE, dev = TRUE)'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

pipeline:
	Rscript -e 'targets::tar_make()'

report:
	quarto render report/index.qmd

dashboard:
	Rscript scripts/build_dashboard_assets.R

outputs: pipeline report dashboard

clean:
	Rscript -e 'targets::tar_destroy(destroy = "objects", ask = FALSE)'
