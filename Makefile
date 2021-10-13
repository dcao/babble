all: fmt build clippy doc test
check: check-fmt build clippy doc test

fmt:
	cargo fmt --all

check-fmt:
	$(if ${CI}, @echo "::group::check-fmt")
	cargo fmt --all -- --check
	$(if ${CI}, @echo "::endgroup::")

clippy:
	cargo clippy --workspace --all-targets

doc:
	cargo doc --workspace --document-private-items

build:
	cargo build --workspace --all-targets

test:
	cargo test --workspace --all-targets

bench:
	cargo bench --workspace --all-targets

.PHONY: all check fmt check-fmt clippy doc build test bench
