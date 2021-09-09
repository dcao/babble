all:
	cargo build --workspace --all-targets
	cargo doc --workspace --document-private-items
	cargo clippy --workspace
	cargo fmt --all

test:
	cargo test --workspace --all-targets

bench:
	cargo bench --workspace --all-targets

.PHONY: all test bench
