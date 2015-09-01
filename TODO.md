* Should objects be ordered?

* Handle type "identical" relationship properly

* Handle recursive types (3.10.7)

* Tuple types

* Function type

* Support "Rest" parameter in invoke functions

* Function overloading

  - Specialized signatures

* Actual interfaces for primitives

* Follow index signature rules:
  (for extends, TSS(7.1)) All properties of the interface must
  satisfy the constraints implied by the index signatures of the
  interface as specified in section 3.8.4.

  (TSS 3.8.4)

  * String index signatures, specified using index type string, define
  type constraints for all properties and numeric index signatures in
  the containing type. Specifically, in a type with a string index
  signature of type T, all properties and numeric index signatures must
  have types that are assignable to T.

  * Numeric index signatures, specified using index type number, define
  type constraints for all numerically named properties in the
  containing type. Specifically, in a type with a numeric index
  signature of type T, all numerically named properties must have types
  that are assignable to T.

* Rules to revisit:

  * (TSS 7.1) The instance type (section 3.6.1) of the declared
  interface must be assignable (section 3.10.4) to each of the base
  type references

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

  * 8.3.3 Automatic constructors
