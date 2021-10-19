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

To this end, it first it adds the initial expression to an e-graph,
and then goes through the following steps.

**Step 1: Anti-unification**

Babble anti-unifies (AU) each pair of e-nodes in the e-graph in a bottom-up fashion to avoid recomputing AU for subterms
(this is actually based on DFTA instersection).
For example:

```
AU empty                            empty                            = empty
AU 0                                1                                = ?x01            -- indexed by the e-classes of 0 and 1
AU (cons 0 empty)                   (cons 1 empty)                   = cons ?x01 empty -- reuses AU results for subterms
...
AU (cons 0 (cons 0 (cons 0 empty))) (cons 1 (cons 1 (cons 1 empty))) = (cons ?x01 (cons ?x01 (cons ?x01 empty)))
```

**Step 2: Generate abstraction rewrites**

Now for each anti-unification result that is not just a variable or constant term,
babble generates a rewrite rule that would turn matching terms 
into an application of a newly-defined library function.

For example, the AU result `(cons ?x01 (cons ?x01 (cons ?x01 empty)))`
gives rise to the rewrite rule:

```
(cons ?x01 (cons ?x01 (cons ?x01 empty)))  ->  (fun f (lambda (cons $0 (cons $0 (cons $0 empty)))) (f ?x01))
```

(the name `f` will be fresh for each rule).
This rule, when applied, will result in rewriting each one of the three-element lists in the original term
into a definition of a new library function followed by its application to the appropriate argument (i.e. `0`, `1`, `2`).

**Step 3: Merging function definitions**

In the next step, we give egg all the generated abstraction rewrites
together with some fixed rules that propagate `fun` definitions up the term
and merge different `fun`s with the same body
when they are propagated from two sides of a `cons`, `list`, or some other constructor.
