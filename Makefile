.PHONY: install snapshot test pipeline report clean

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

clean:
	Rscript -e 'targets::tar_destroy(destroy = "objects", ask = FALSE)'
