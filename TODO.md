* Should objects be ordered?

* Function types

* Call and construct signatures
   - Specialization (section 3.8.2.4) + string literal types

* Index signatures

* Actual interfaces for primitives

* Finish implementing rules for assignability: (from TSS)

  * M is a non-specialized call or construct signature and S has an
  apparent call or construct signature N where, when M and N are
  instantiated using type Any as the type argument for all type
  parameters declared by M and N (if any),

    - the signatures are of the same kind (call or construct),

    - M has a rest parameter or the number of non-optional parameters in
      N is less than or equal to the total number of parameters in M,

    - for parameter positions that are present in both signatures, each
      parameter type in N is assignable to or from the corresponding
      parameter type in M, and

    - the result type of M is Void, or the result type of N is
      assignable to that of M.

  * M is a string index signature of type U and S has an apparent string
  index signature of a type that is assignable to U.

  * M is a numeric index signature of type U and S has an apparent
  string or numeric index signature of a type that is assignable to U


* Concerns for code generation from TSS:

  * The apparent members of an enum type are the apparent members of the
  global interface type 'Number'.

  * The apparent members of an object type T are the combination of the
    following:

    * The declared and/or inherited members of T.

    * The properties of the global interface type 'Object' that aren't hidden
      by properties with the same name in T.

    * If T has one or more call or construct signatures, the properties of the
      global interface type 'Function' that aren't hidden by properties with
      the same name in T.
