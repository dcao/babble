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

## How it works

As a simple example, consider the following list program (with size 29):

```
(list
 (cons 0 (cons 0 (cons 0 empty)))
 (cons 1 (cons 1 (cons 1 empty)))
 (cons 2 (cons 2 (cons 2 empty)))
 (cons 3 (cons 3 (cons 3 empty))))
```

Here babble learns the following compressed program (with size 23):

```
(lib f8 (λ (cons $0 (cons $0 (cons $0 empty)))) 
  (list (@ f8 0) (@ f8 1) (@ f8 2) (@ f8 3)))
```

Here is the algorithm it follows:

First it adds the initial expression to an e-graph.

**Step 1: Anti-unification**

Now for each pair of e-classes it tries to anti-unify them, and if successful, generate *abstraction rewrites*.
AU works by picking two e-nodes with same constructor as long as possible, and returning an AU variable otherwise.
We say that AU succeeds if it returns a term that is not just an AU variable.
For example:

```
AU (cons 0 (cons 0 (cons 0 empty))) (cons 1 (cons 1 (cons 1 empty))) = (cons ?x (cons ?x (cons ?x empty))) -- suceess
AU (cons 0 empty)                   empty                            =  ?x                                 -- fail
```

