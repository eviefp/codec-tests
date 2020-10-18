# codecs


Start using this as a STRING parsing library. Get away from XML and json.


Next time:
- [ ] Factor out things that don't depend on XML to the common module
- [ ] Combinators for string parsing
- [ ] Tests/examples for string parsing


## Things that I think are awkward and I would like to solve
- add helpers to ditch unit values
     - problem: do we change `element` to be `(a, (b, c))` instead of `(a, b, c)`?
     - if not, we'll need a ton of "ditch unit" functions
     - if we do, we make element more awkward :(

- `//` is also a bit awkward (but not as bad as we thought!)
