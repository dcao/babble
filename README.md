# Babble [![ci-status-badge]][ci-status] [![license-badge]][license]

[ci-status]: https://github.com/dcao/babble/actions/workflows/ci.yml?query=branch%3Amain
[ci-status-badge]: https://img.shields.io/github/workflow/status/dcao/babble/CI?style=for-the-badge
[license]: https://github.com/dcao/babble/blob/main/LICENSE
[license-badge]: https://img.shields.io/github/license/dcao/babble?style=for-the-badge

Experimental library learning using anti-unification of e-graphs.

## Building

``` shellsession
$ git clone https://github.com/dcao/babble.git
$ cd babble
$ make
```

## Examples
Learning `filter`:

``` shellsession
$ cargo run --bin=list -- examples/filter-list.bab
```

Learning nested functions:

``` shellsession
$ cargo run --bin=smiley -- examples/nested-functions.bab
```
