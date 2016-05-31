# lock-comonad
Toying around with representing locks and scoping rules as indexed (co)monads in Haskell.

The basic model is that we want to model access to a resource that can be opened and closed, and want to check these accesses statically. There are of course myriad ways of doing this; this is an attempt at expressing these constraints in the Haskell type system. This relies on several extensions, the most important of which are type families and type-level lists.

Very much a work in progress; this is only intended as a minor curiousity.
