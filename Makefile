all: fmt build clippy doc test

fmt:
	cargo fmt --all

clippy:
	cargo clippy --workspace

doc:
	cargo doc --workspace --document-private-items

build:
	cargo build --workspace --all-targets

test:
	cargo test --workspace --all-targets

bench:
	cargo bench --workspace --all-targets

.PHONY: all fmt clippy doc build test bench
