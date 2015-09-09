**ghcjs-typescript** implements some aspects of TypeScript's type
system.  It attempts to follow the TypeScript specification, but this
is not guaranteed, and there are still quite a few things left to do
(see TODO.md).

**ghcjs-typescript-convert** implements a converter from typescript
definition files (.d.ts) to Haskell modules.  These modules make use
of **ghcjs-typescript** to write their definitions.  Currently, its
test-suite attempts to convert all of the
[DefinitelyTyped](https://github.com/borisyankov/DefinitelyTyped) repo
of typescript definitions.  The goal is to be able to generate
packages which require very little manual work to use, but we're not
quite there yet.
