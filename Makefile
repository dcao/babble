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

DOMAINS=list physics text logo origami towers
PDFS = $(patsubst %,harness/plots/%.pdf,$(DOMAINS))
CSVS = $(patsubst %,harness/data_gen/%.csv,$(DOMAINS))
RUST_SRC = $(shell find src -name "*.rs")

.PHONY: plots
plots: $(PDFS)

harness/data_gen/%.csv: $(RUST_SRC)
	cargo run --bin=benchmark --release -- --domain=$* -o $@
harness/plots/%.pdf: harness/scripts/plot-lines.py harness/data_gen/%.csv
	$^ $@



