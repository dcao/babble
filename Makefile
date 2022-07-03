all: fmt build clippy doc test
check: check-fmt build clippy doc test

fmt:
	cargo fmt --all

check-fmt:
	$(if ${CI}, @echo "::group::Check fmt")
	cargo fmt --all -- --check
	$(if ${CI}, @echo "::endgroup::")

clippy:
	$(if ${CI}, @echo "::group::Clippy")
	cargo clippy --workspace --all-targets
	$(if ${CI}, @echo "::endgroup::")

doc:
	$(if ${CI}, @echo "::group::Docs")
	cargo doc --workspace --document-private-items
	$(if ${CI}, @echo "::endgroup::")

build:
	$(if ${CI}, @echo "::group::Build")
	cargo build --workspace --all-targets
	$(if ${CI}, @echo "::endgroup::")

test:
	$(if ${CI}, @echo "::group::Test")
	cargo test --workspace --all-targets
	$(if ${CI}, @echo "::endgroup::")

bench:
	cargo bench --workspace --all-targets

.PHONY: all check fmt check-fmt clippy doc build test bench


### plots

define CSVS
	harness/data_gen/list-beam-400-lps-1-rounds-20.csv
    harness/data_gen/physics-beam-400-lps-1-rounds-20.csv
endef

PDFS = $(patsubst harness/data_gen/%.csv,harness/plots/%.pdf,$(CSVS))

.PHONY: plots
plots: $(PDFS)

harness/plots/%.pdf: harness/scripts/plot-lines.py harness/data_gen/%.csv
	$^ $@



