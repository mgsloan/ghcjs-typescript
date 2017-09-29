**ghcjs-typescript** implements some aspects of TypeScript's type
system, embedded within GHC's type system.  It attempts to follow the
TypeScript specification, but this is not guaranteed, and there are
still quite a few things left to do (see TODO.md).

**ghcjs-typescript-convert** implements a converter from typescript
definition files (.d.ts) to Haskell modules.  These modules make use
of **ghcjs-typescript** to write their definitions.  Currently, its
test-suite attempts to convert all of the
[DefinitelyTyped](https://github.com/borisyankov/DefinitelyTyped) repo
of typescript definitions.  The goal is to be able to generate
packages which require very little manual work to use, but we're not
quite there yet.

# Project Status

I'm starting to think that this isn't the most pragmatic way to do TypeScript
FFI.  Here's what I'm thinking:

## Alternative approach to TypeScript FFI

I think it makes sense to follow the lead of inline-c / inline-r, and instead
handle interfacing with TypeScript definition files by writing chunks of code in
TypeScript.

At first, this would be done by having TypeScript files in the `js-files` of
your ghcjs project (see
[issue #393](https://github.com/ghcjs/ghcjs/issues/393)). Then, we can
automatically generate FFI bindings to the definitions in these files.
Interfaces become newtypes, and properties / methods become functions. Some
polyvariadic tricks can be used to handle overloaded functions and "rest"
parameters.

It might also generate code for casting up the inheritance hierarchy. Even
though TypeScript has structural subtyping, we can still validly pretend like
it's nominative, and get a subset of the casts considered valid.

Later, we can have a TypeScript quasiquoter which allows for compiling little
bits of typescript into FFI bindings which have types inferred by the TypeScript
compiler. I thought of this idea before embarking on the ghcjs-typescript
approach, but I didn't like that there's no good way for type information from
Haskell code to be provided to the TypeScript snippets. For example, if I have a
local variable `x` of type `Node`, I'd really like to be able to write
`[ts| x.childNodes|]` and have it know that it is type `NodeList`.  Instead,
you'll need to write something like [ts| (x:Node).childNodes |].

This TH design decision makes sense - otherwise TH would get info that depends
on the implementation specifics of typechecking. However, it would be quite nice
if we could attempt to reify some type info for local variables. I'll consider
adding such an "unsafe" API to TH in the future.

I think the implementation of the QuasiQuoter could turn out quite nicely. GHCJS
already runs TH splices by compiling them to javascript and running in Node. The
TypeScript compiler is written in TypeScript, which means that we can also load it
into Node!

## Issues with the current approach

Here are the main issues that bug me about the current approach:

1) Perfectly implementing typescript's subtyping and assignability rules will
take time, and it's a moving target. What I've got already does most of it
right, but I know it's not all the way there (lacks support for generics, etc).

2) I haven't benchmarked things yet, but I suspect that in some real-world
situations you could encounter some very slow compilation due to typechecking.

3) Much care needs to be taken to make sure the error messages aren't
inscrutable.  Even with this extra effort, the errors aren't so great because
[type level programming needs an error function](https://ghc.haskell.org/trac/ghc/ticket/9637).

4) It isn't currently very convenient to program with. With enough type hackery
and code generation, we could end up with some very nice user code. For example,
by using polyvariadic functions rather than passing in an hlist of arguments.
However, this is putting complicated type system stuff atop complicated type
system stuff. I'm concerned it'd make the errors even trickier.
