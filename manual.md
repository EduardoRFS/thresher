# Thresher Stuff

A good model is that Thresher is another language, such that when embedding Grain into Thresher the semantics are preserved. In practice, the way that we actually define the Grain semantics will be through Thresher.

Keep in mind, some of the details of Thresher are not properly handled, as they're never produced by the Grain compiler.

## More Ideas

Lightweight unit tests using dependent types.

## Pipeline

```rust
// success
Grain AST -> Thresher AST -[Typer]> Typed Thresher

```

## Glossary

- **Term**: A term is the building blocks of the system, it can be a variable, a function, a pair or even another type, in Thresher all terms including types have a type.
- **Type**: A type is any term that the type is a "universe", think pair type, function type, but it may also just be a variable that has the type of a universe.
- **Dependent Type**: A dependent type is a type that depends on a term, in general here it just means that the type of the body of the function may reference the input, polymorphic functions are instances of dependent types, modules and functors are often instances of those also.
- **Boxed Type**: It means a type that has a known representation, in general it is provides a unified way of passing different things around. It is especially useful for polymorphic functions.

## Thresher Language

Thresher is a dependently-typed language, with pairs, records, enums and some forms of subtyping(TODO: which forms?).

The dependently-typed aspect is really not that accessible, but it aid the implementation but having a unified language, such that the semantics of the term and type-level are the same.

```rust
Term (M, N)
Type (A, B) ::=
  | Type // type universe
  | Data // runtime universe
  | (M : A) // type annotation
  | x // var
  | x = M; N // let
  | Box(A) // box type
  | box(M) // box value
  | unbox(M) // unbox
  | P -> M // function type
  | P => M // function
  | M(N) // apply
  | <A> -> M // implicit function type
  | <A> => M // implicit function
  | M<A> // explicit apply of implicit function
  | [] // unit type
  | [] // unit
  | [l : A, B] // pair type
  | [l = M, N] // pair
  | M.0 // fst
  | M.1 // snd
  | {} // empty type
  | {} // empty value
  | { l : A; B } // record type
  | { l = M; N } // record
  | M.l // record access
  | ⊥ // never type
  | (`l(A) | B) // enum type
  | `l(M) // enum value
  | match(M) { ...C }

Case (C) ::=
  | P when M => N // case

Pattern (P) ::=
  | x // var
  | box(P) // box
  | [] // unit
  | [P, Q] // pair
  | {} // empty record
  | { l = P; Q } // record
  | `l(P) // enum
```

### Universes

Universes in dependent type theory are types of types, they're also related to "kinds". In Thresher they're used to ensure that everything has a type, to ensure separation between types and terms and for polymorphic reasons.

Anything that it's kind is `Type` only exists in the runtime, and anything that it's kind is `Data` is a term. This is not exposed to the user and it's subject to change. Everything in Type also has no size.

Notice that the kind-level is simply typed `M : A : K`, K is always going to be either `Type` or `Data` as such the compiler can rely on it to extract the values.

```rust
Type : Type // the type of things on the type level
Data : Type // the type of things on the runtime

//  same as, type id('a) = 'a
Id : ((A : Data) -> Data : Type)
  = (A : Type) => A;
// more or less, (type a) => (x : a) => x;
id = (A : Data) => (x : A) => x;

((x : A) -> B : K) : K
// if any of them is data, the whole thing is data/
[x : A : K_A, y : B : K_B] : min(K_A, K_B)
```

This is probably the biggest cost payed by this approach of unifying everything, we then need to be able to separate what is a value from a type to not mess up the compilation and carry types in the runtime.

### Boxing

Everything in Thresher is unboxed by default, this is used to avoid the need for n-ary things, like n-ary functions, tuples and records.

This also makes inference for pairs and records through row polymorphism much easier.

The main issue of this approach, is that the size of an object is not known at compile time, making impossible to pass it around, this is handled by ensuring that the Grain translation never produces a term with an unknown size.

Any boxed value has constant size(the size of a word), not all boxed values are pointers, as Grain uses tagged pointers.

```rust
Box(Unit) // size Word
[x : Box(Unit), y : Box(Unit), Unit] // 2 * Word

// polymorphic functions can take boxed values
id = <A>(x : Box(A)) => x;
```

### Functions & Implicit Functions

Functions in Thresher replace both functions in Grain, but also Functors in OCaml and any form of polymorphism.

```rust
// explicit polymorphism
id : (A : Data, x : Box(A)) -> Box(A)
  = (A : Data, x : Box(A)) => x;

x : Box(String) = id(Box(String), box("hello"))

// implicit polymorphism
id : <A : Data>(x : Box(A)) -> Box(A)
  = <A : Data>(x : Box(A)) => x;

// then calling it
x : Box(String) = id(box("hello"));

// functors / first-class modules
show = (S : { A : Data; show : (x : Box(A)) -> String; }, x : Box(S.A)) => x;
```

In logics they make "universal quantification", the cat people call them "dependent product types".

### Unit & Pairs

Unit and pairs in Thresher replace tuples in Grain. As they're unboxed, this doesn't incur in any additional runtime cost.

Unit: A value of size 0, doesn't exist in the runtime, unlike the grain unit which is equivalent to `Box(Unit)`, it is mostly used to indicate "no return" and "no parameter", as a placeholder, but it also exists to simplify the row polymorphic system.

Pair is a value where the size is the sum of the sizes of its elements.

Invariant: the second element of a pair must be a pair or unit.

```rust
T(()) == []
// the first element must be boxed
T((A, ...R)) == [l : Box(T(A)); T(R)]
```

A side bonus is that a tuple of size 1 is allowed, as it is a pair + unit, this can be used to achieve "named values".

TODO: check with the Grain team if they have an usage for this.

```rust
f : (x : Nat, y : Nat) -> [z : Nat, []];
```

On logics, because of universes dependent pairs act like "existential quantification", they're also called "dependent sum types" by the cat people, and are useful to describe sum types.

```rust
// a pair where the type of the second element depends on the first element
// think GADTs
T = [A : Data, x : A, []];

// but you can also depend on values, like the Either sum type
Either = (A, B) => [
  tag : Bool;
  content :
    match(tag) {
    | true => A
    | false => B
    };
];
```

### Empty & Records

Like pairs records are also split in two, empty and records, this is useful for row polymorphism. As Grain only expose nominal records, the records machinery is mostly used due to type inference reasons.

The main difference from records to pairs is that records ignore the order of labels.

Empty: Isomorphic to Unit, it is a value of size 0 and doesn't exist in the runtime, it only exists to simplify the row polymorphic system.

Record: Pack a valu with a label value where the size is the sum of the sizes of its elements.

A big advantage is that anonymous records can be represented, this also includes nested records, this is especially useful for things like JSON, it's up to the Grain team to levearage it or not.

Invariant: all the labels in a sequence of record must be unique.
Invariant: the second element of a record must be a record or unit.

```rust
T({}) == {}
// the first element must be boxed
T({ l : A, ...R }) == {l : Box(T(A)), T(R)}

// this is true for records
{ x : A, y : B, {} } == { y : B, x : A, {} }

// notice the _R for record rows
// this is also useful for nominal types reasons
M.l : { l : _A; _R }
```

Similar to pairs, records are also "existential types" and "dependent sum types".

## Never & Enum

Enums are also split in two, never and enum, this is also useful for row polymorphism. Similar to records Grain only expose nominal enums, the enums machinery is mostly used due to type inference reasons.

Never doesn't have any value, it is a contradiction, as such it never exists in the runtime, and it only appear for things like exceptions and infinite loops. It is mostly used to refute branches but also to simplify the row polymorphic system.

Enum is used to add a label to discriminate between values, the size of an enum is unknown unless all the elements have the same known size, there is no implicit padding supported. They're in the system mostly due to improve inference and avoid duplicated labels.

Similarly to records, a big advantage is that anonymous enums can be represented, this also includes nested enums.

Invariant: the second element of an enum must be a enum or never.
Invariant: all the labels in a sequence of enum must be unique.

```rust
T(•) == ⊥
T(`l | ...R) == `l([]) | T(R)
T(`l(A), ...R) == `l(A) | T(R)

// this is true for enum
`x(A) | `y(B) == `y(B) | `x(A)

// notice the _R for enum rows
// this is also useful for nominal types reasons
`x(A) : `x(A) | _R = M; N
```

## Nominal & Nested Nominal

Instead of having some types to be nominal and others to be structural, Thresher has a structural type system and a way of pack types and values, such that they act nominally.

The way that this works is that substitution during unification doesn't work for nominal types. As such they get stuck and you can only see the variable.

The way that values for nominal types are introduced is by making them "weak".

```rust
User_A = Nominal({ id : Nat; name : String; });
User_B = Nominal({ id : Nat; name : String; });

((M : User_A) : User_B) // fails because User_A is not the same as User_B

// keep in mind, everything is bound to a let anyway
User = Nominal({
  id : Nat;
  name : Nominal({
    first : String;
    last : String;
  });
});

// same as
User_Name = Nominal({
  first : String;
  last : String;
});
User = Nominal({
  id : Nat;
  name : User_Name;
});
```

## Grain to Thresher

TODO: this is work in progress

```rust
// TD = Declaration
// TT = Type
// TE = Expression
// TP = Pattern
// TC = Case

TT('a. B) |-> <A : Data> -> TT(B)
TT((A, ...R)) |-> [Box(TT(A)); TT(R)]
TT((...A) -> B) |-> TT(A) -> Box(TT(B))
TT({ x : A; ...R }) |-> { x : Box(TT(A)); TT(R) }
TT(`x(A) | ...R) |-> `x(TT(A)) | TT(R)

TE(x) |-> x
TE(P = M; N) |-> P = TE(M); TE(N)
TE((...P) => M) |-> TP(P) => box(TE(M))
TE(M(N)) |-> TE(M)(TE(N))
TE(()) |-> []
TE((M, ...R)) |-> [box(TE(M)); TE(R)]
// TODO: with
TE({}) |-> {}
TE({ x = M; ...R }) |-> { x = box(TE(M)); TE(R) }
TE(`x(M)) |-> `x(TE(M))
TE(match(M) { ...C }) |-> match(TE(M)) { TC(C) }

TP(_)
TP(x) |-> x
TP(()) |-> []
TP((P; ...R)) |-> [TP(P); TP(R)]
TP({}) |-> {}
TP({ x = P; ...R }) |-> { x = TP(P); TP(R) }
TP((P : A)) |-> (TP(P) : TT(A))
TP(`x(P)) |-> `x(TP(P))
TP(P | Q) |-> TP(P) | TP(Q)
TP(P as x) |-> TP(P) as x

TC(•) |-> •
TC(P => M | R) |-> TP(P) when true => TE(M) | TC(R)
TC(P when G => M | R) |-> TP(P) when TE(G) => TE(M) | TC(R)


TP({ x = P; ...R }) |-> ({ TP(P); TP(R) } : { x : _A; _B })
```
