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

.PHONY: all check fmt check-fmt clippy doc build test bench plots plots-quick cogsci-table clean

plots: harness/scripts/plot.py
	$^

plots-quick: harness/scripts/plot.py
	$^ physics

cogsci-table: harness/scripts/cogsci-table.py
	$^

clean:
	rm -rf harness/data_gen/cache/