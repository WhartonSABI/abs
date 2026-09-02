.PHONY: install snapshot test test-perception test-revealed-policy pipeline perception-setup perception-pilot perception-full perception-report revealed-policy-pilot revealed-policy-full revealed-policy-report fixed-clock-smoke fixed-clock-full report dashboard outputs clean

install:
	Rscript --vanilla -e 'dir.create(".Rlib", showWarnings = FALSE); install.packages("renv", lib = ".Rlib")'
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::snapshot(prompt = FALSE, dev = TRUE)'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

test-perception:
	Rscript -e 'testthat::test_dir("tests/testthat", filter = "continuous", stop_on_failure = TRUE)'

test-revealed-policy:
	Rscript -e 'testthat::test_dir("tests/testthat", filter = "revealed", stop_on_failure = TRUE)'

perception-setup:
	Rscript -e 'if (!requireNamespace("cmdstanr", quietly = TRUE)) install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org")); dir.create("data/processed/stan", recursive = TRUE, showWarnings = FALSE); source("scripts/functions/perception/continuous_model_utils.R"); source("scripts/functions/perception/continuous_perception_workflow.R"); if (!dir.exists(continuous_cmdstan_path())) cmdstanr::install_cmdstan(dir = dirname(continuous_cmdstan_path()), version = continuous_cmdstan_version(), cores = 4); configure_continuous_cmdstan()'

pipeline:
	Rscript -e 'targets::tar_make()'

perception-pilot: pipeline
	ABS_PERCEPTION_PROFILE=pilot Rscript -e 'targets::tar_make(script = "_targets_perception.R", store = "_targets_perception")'

perception-full: pipeline
	ABS_PERCEPTION_PROFILE=full Rscript -e 'targets::tar_make(script = "_targets_perception.R", store = "_targets_perception")'

revealed-policy-pilot:
	ABS_REVEALED_POLICY_PROFILE=pilot Rscript analysis/policy/run_revealed_challenge_policy_1d.R

revealed-policy-full:
	ABS_REVEALED_POLICY_PROFILE=full Rscript analysis/policy/run_revealed_challenge_policy_1d.R

revealed-policy-report:
	quarto render report/perception.qmd

fixed-clock-smoke:
	ABS_FIXED_CLOCK_PROFILE=smoke ABS_FIXED_CLOCK_REFRESH_TARGETS=false Rscript analysis/policy/run_fixed_clock_confirmation_1d.R

fixed-clock-full:
	Rscript analysis/policy/run_fixed_clock_confirmation_1d.R

perception-report: revealed-policy-report

report:
	quarto render report/index.qmd
	cp report/index.pdf output/pdf/mlb-abs-challenge-run-value.pdf

dashboard:
	Rscript scripts/build_dashboard_assets.R

outputs: pipeline report dashboard

clean:
	Rscript -e 'targets::tar_destroy(destroy = "objects", ask = FALSE)'
