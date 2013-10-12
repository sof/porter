## Haskell implementation of Porter's original stemming algorithm.

  http://snowball.tartarus.org/algorithms/porter/stemmer.html

Arguably a bit tidier than the example Haskell version given on
the Snowball web pages.

The test input was shamelessly taken from kristopolous' fine
Javascript implementation,

  https://github.com/kristopolous/Porter-Stemmer

### To build:

    foo$ runhaskell Setup configure
    foo$ runhaskell Setup build
    ...
    foo$ dist/build/checker/checker
    ...
    # Should you so wish...
    foo$ runhaskell Setup install


--sof 10/2013.
