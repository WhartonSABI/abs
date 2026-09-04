.PHONY: install snapshot test pipeline public-data fixed-clock-smoke fixed-clock-full clean paper-figures paper paper-check paper-clean

PAPER_DIR := paper
PAPER_TEX := $(PAPER_DIR)/main.tex
PAPER_PDF := $(PAPER_DIR)/main.pdf
PAPER_LOG := $(PAPER_DIR)/main.log
PAPER_AUX := $(PAPER_DIR)/main.aux
PAPER_REVIEW_PDF := output/pdf/abs-challenge-policy-review.pdf
PAPER_MAX_BODY_PAGES ?= 10
PAPER_IDENTIFY_RE ?= /Users/|/vast/
-include internal/anonymity.mk

install:
	Rscript --vanilla -e 'dir.create(".Rlib", showWarnings = FALSE); install.packages("renv", lib = ".Rlib")'
	Rscript -e 'renv::restore(prompt = FALSE)'

snapshot:
	Rscript -e 'renv::snapshot(prompt = FALSE, dev = TRUE)'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

pipeline:
	Rscript -e 'targets::tar_make()'

public-data:
	Rscript scripts/export_public_data.R

fixed-clock-smoke:
	ABS_FIXED_CLOCK_PROFILE=smoke ABS_FIXED_CLOCK_INPUT_DIR=data/analysis ABS_FIXED_CLOCK_REFRESH_TARGETS=false Rscript scripts/run_fixed_clock_confirmation_1d.R

fixed-clock-full:
	ABS_FIXED_CLOCK_INPUT_DIR=data/analysis ABS_FIXED_CLOCK_REFRESH_TARGETS=false Rscript scripts/run_fixed_clock_confirmation_1d.R

paper-figures:
	Rscript paper/build_figures.R

paper: paper-figures
	cd $(PAPER_DIR) && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex
	mkdir -p $(dir $(PAPER_REVIEW_PDF))
	cp $(PAPER_PDF) $(PAPER_REVIEW_PDF)

paper-check: paper
	@if logs="$(PAPER_LOG)"; \
		[ ! -f "$(PAPER_DIR)/main.blg" ] || logs="$$logs $(PAPER_DIR)/main.blg"; \
		grep -En '(^! |LaTeX Error:|Package [^:]+ Error:|Emergency stop|Fatal error|Citation .* undefined|Reference .* undefined|There were undefined (citations|references)|Label\(s\) may have changed|Overfull \\[hv]box|No file .*\.bbl)' $$logs; then \
		echo "Paper check failed: resolve the LaTeX diagnostics above."; \
		exit 1; \
	fi
	@body_pages=$$(sed -n 's/^\\newlabel{last-content-page}{{[^}]*}{\([0-9][0-9]*\)}.*/\1/p' "$(PAPER_AUX)" | tail -n 1); \
		[ -n "$$body_pages" ] || { \
			echo "Paper check failed: add \\label{last-content-page} immediately before the reference-page break."; \
			exit 1; \
		}; \
		echo "Body pages before references: $$body_pages (limit $(PAPER_MAX_BODY_PAGES))"; \
		[ "$$body_pages" -le "$(PAPER_MAX_BODY_PAGES)" ] || { \
			echo "Paper check failed: body exceeds $(PAPER_MAX_BODY_PAGES) pages before references."; \
			exit 1; \
		}
	@tmp_text=$$(mktemp); \
		trap 'rm -f "$$tmp_text"' EXIT HUP INT TERM; \
		if command -v pdftotext >/dev/null 2>&1; then \
			pdftotext "$(PAPER_REVIEW_PDF)" "$$tmp_text"; \
		elif command -v gs >/dev/null 2>&1; then \
			gs -q -dNOPAUSE -dBATCH -sDEVICE=txtwrite -sOutputFile="$$tmp_text" "$(PAPER_REVIEW_PDF)"; \
		else \
			echo "Paper check failed: install Poppler (pdftotext) or Ghostscript to inspect PDF text."; \
			exit 1; \
		fi; \
		if command -v pdfinfo >/dev/null 2>&1; then \
			pdfinfo "$(PAPER_REVIEW_PDF)" >> "$$tmp_text"; \
		fi; \
		source_files=$$(find "$(PAPER_DIR)" -maxdepth 2 -type f \( -name '*.tex' -o -name '*.bib' -o -name '*.R' \) -print); \
		if grep -Eni '$(PAPER_IDENTIFY_RE)' $$source_files "$$tmp_text"; then \
			echo "Paper check failed: the review artifacts contain an identifying string."; \
			exit 1; \
		fi
	@echo "Paper checks passed: LaTeX diagnostics, page limit, and anonymous-review scan."

paper-clean:
	cd $(PAPER_DIR) && latexmk -C main.tex
	rm -f $(PAPER_REVIEW_PDF)

clean:
	Rscript -e 'targets::tar_destroy(destroy = "objects", ask = FALSE)'
