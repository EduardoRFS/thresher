# Thresher

## Speed

- counting variables for instantiation
- try lazy instantiation
- try no instantiation, accumulate the offset
- https://okmij.org/ftp/ML/generalization.html

## Binders

id to distinguish between variables in the same binder
rank to distinguish between binders positions in the same kind
kind to distinguish between all the binders kind(exists and forall)

## Playground

````rust
Constants (C) ::=
  | Int
  | Float
  | Number
Mono (S, T) ::=
  | C
  | a
  | (x : S) -> T
  | (x : S, y : T);
Poly (A, B) ::=
  | T
  | forall a : K. A
  | forall a extends C. A;
Pattern (P) ::=
  | x
  | (P : A)
  | (P1, P2);
Expression (E) ::=
  | x
  | P => E
  | E1 E2
  | let P = E1; E2
  | (E : A);

type id = forall a. a -> a

f : id = _;
type x
(f : x -> x)


x => _
mut a. ref(a )

type S = (module )

a\-1 -> a\-1


received : forall. b -> c -> b
expected : forall. a -> a -> a

received : _k -> (forall. () -> b) -> _k
expected : _k -> (() -> _k) -> _k

received : _k -> (forall. b -> b -> b) -> _k
expected : _k -> (forall. a -> _k -> a) -> _k

(a\-2 -> a\-2) -> a\-1

received : forall a. a -> a
expected : Int -> Int

received : forall a. a -> a
expected : _b -> _b

(forall a. a\-1 -> a\-1) -> ()

Γ |- A : Type
----------------------
Γ |- x ⇐ A -| Γ, x : A

Γ |- P ⇐ A -| Δ
---------------------
Γ |- (P : A) ⇒ A -| Δ

Γ |- P ⇒ A -| Δ  Δ |- Q ⇒ B -| Θ
--------------------------------
Γ |- (P, Q) ⇒ (A, B) -| Θ


(f : forall mut a. a -> a) =>

_a -> _a
_a -> (_a -> ()) -> ()

not_b = not b;
not_b | true => _ | false => _

(x : (x : A) -> _) => _


()
(x : Nat)

(x : Nat) => (x : Nat, y : Nat)
f = () : (x : Nat, y : Nat) = _;
(y, x) = f ();
(x = y, y = x) = f ();

(x) : (incr : Nat) => (x + 1)

a b.

let { x as a } = _;


(x : A) -> A // arrow

f => (
  x = 1;
  x + 1
);

{
  x : A;

}

∀x. T

(M) {M} [M] <M> |M|
(|M|) {|M|} [|M|] <|M|>
// )M( }M{ ]M[ >M<

f ('a. 'a)

'a. 'a

User = { id : Nat; name : String; };

{ id : Nat; ...R } <: User
User <: { id : Nat; ...R }

f : (x : Number) -> Number
f : <A <: Number>(x : Number) -> Number
f : (x : Nat | String) -> Nat | String


(x : R) -> R
(x : Nat | ...R) -> Nat |...R

(x : Nat) -> Nat | ...R

Nat | ..._R
Nat | ..._R  ≡ Nat | String

step = (p, x, k) => p * x + k;


f : () -> Nat | 'A = _;
x : Int = f ();

f : () -> Type (1 | 'A) = _;

x : Int & 'B = f ()
Type 1 : Type 2
∀l. Type (l | 1) : Type 2


a =
(x : ) ->

f : (f : Nat -> Nat) -> _ = (f : Nat -> Int) => _;

f : (f : Nat -> Nat) -> _ = ((f : Nat -> Int) : Nat -> Nat) => _;


#(M : Nat -> Int)

#((f : Nat -> Int) : Nat -> Int)
#(M : Nat -> Int)
#(P ⇒ A ⇐ B)
#(P : A)
#(#(P : A))

((P : A) : )

(f : Nat -> Int) : Nat -> Nat
(M : Int -> _ : Nat -> Nat)


Number = Nat | Int;

add : Nat -> Nat;
add : Int -> Int;


add : (A <: Number). A -> A


add : (A & Number) -> A;


_A & Number

f : () -> Nat;

f : (A <: Nat). () -> A;

f : () -> A | Nat;

Int = Nat | Neg;

x : _B & Int = (1 : _A | Nat);


f : (A <: Number). A;

add : (A <: Number). A;

add : (A & Number) -> A


add : (_A & Number) -> A


_A & Number ≡ _B | Nat
Nat & Number ≡ Nat | Nat

_A ≡ _B
_B ≡ Nat
_B ≡ Nat


Type 0

(refl : (x : Nat) => Nat == (x : Nat) => Int)

check annot r:((x : Nat) -> Nat) e:_A
check annot r:((x : Nat) -> Nat) e:((x : _B) -> _C)
check annot r:((x : Nat) -> Nat) e:((x : _B) -> _C)


(x : Nat) -> Nat ≡ (x : _B) -> _C
Nat ≡ _B

((M : Int -> String) : String -> String)


add : <A extends Number>(x : A, y : A) -> A;
add : (x : A & Number, y : A) -> A;


Γ |- P : A -| Δ  Δ |- M : B
---------------------------
Γ |- P => M : (P : A) -> B


Γ |- P => M : (P : _A) -> _B

Γ |- M : (x : A) -> B  Γ |- N : A
---------------------------------
Γ |- M N : B


(x : Nat) => x N

(x : Nat) => x

x => M |-> (((x : _A) => (M : _B)) : (x : _A) -> _B)

x => x |-> (((x : _A) => (M : _B)) : (x : _A) -> _B)

(((x : _A) => (M : _B)) : (x : _A) -> _B)

((M : Nat -> Nat) : Option Int)

Int -> Int

_A -> _B

'a. 'a -> 'a

weak / hole
univ / meta

_ -> _

exists x. t
free
bound

free
bound
![alt text](image.png)

exists A. A -> A
A -> A

forall A. A -> A
_A -> _A

expected : exists A. A -> A
received : forall A. A -> A

expected : exists A. A -> A
received : forall A. A -> A


add : <A <: Number>(l : A, r : A) -> A;

add : (x : Number, y : Number) -> Number;
add : (x : Number & _A, y : Number & _B) -> Number & _C;


add : <A>(x : Number & A, y : Number & A) -> A;

choose : <A>(x : A) -> (y : A) -> A = _;

choose : (x : _A) -> (y : _B) -> _A | _B;

choose 1 : (y : _B) -> Nat | _A
choose (1 : Nat)

add : <A>(x : A & Number) -> (y : A & Number) -> A = _;
add : <A <: Number>(x : A) -> (y : A) -> A = _;

add : (x : _A & Number) -> (y : _B & Number) -> _A | _B = _

id : <A>(x : A & Number) -> A = _;


Number = Nat | Float;

Nat == Number
Nat == Nat | Float

_B | Nat == _A & Number

_B | Nat == _A & Nat;


_B | Nat == _A & Nat;


A <: B
B <: A
A == B


A == C  B == D
--------------
A | B == C | D

A == D  B == C
--------------
A | B == C | D


A | B | C == B | C | A

----------------
Nat | Int != Int

Nat | Int == Nat | Int


String & Number =

Nat | Int == Int | Nat

Weak { link; binder }
Univ { link; binder }
Inst


∀A. A -> A
∀A. A -> B -> A

∀A. A -> A
A#1 -> A#1

Poly (σ) =
  | ∀. τ
Mono (τ) =
  | α
  | σ -> τ

   |----|
∀. A -> A#1

∀A. A -> A -> B -> A

   |------------|
   |-------------------------------------|
∀. Bind("A") -> Free(-1) -> Bind("B") -> Free(-2)



∀A. ∀B. -2 -> -2 -> -1 -> -2

∀. 0 -> -1 -> 0 -> -2

[] |- ∀. 0 -> -1 -> 0 -> -2


[] |- ∀. Bind("A") -> Free(-1) -> Bind("B") -> Free(-2)
[] |- Bind("A") -> Free(-1) -> Bind("B") -> Free(-2)
["A" := _A] |- Free(-1) -> Bind("B") -> Free(-2) -| _A ->
["A" := _A] |- Bind("B") -> Free(-2) -| _A -> _A
["B" := _B; "A" := _A] |- Free(-2) -| _A -> _A -> _B
["B" := _B; "A" := _A] |- -| _A -> _A -> _B -> _A


<A>(x : )

_A <: { x : A }

_A == { x : A; y : A }

{ x : A | _R }


T = ∀. 'a -> 'a
S = Nat -> Nat


Γ |- M ⇒ M A ?

Γ |- S₁ ⇒ S₂ : Type  Γ |- T₁ ⇒ T₂ : Type
----------------------------------------
Γ |- S₁ -> T₁ ⇒ S₂ -> T₂ : Type


Γ |- M ⇒ _A -> _B  Γ |- N ⇐ _A
------------------------------
Γ |- M N ⇒ B


Γ |- M ⇒ _A -> _B  Γ |- N ⇐ _A
------------------------------
Γ |- M N ⇒ B


(T = Int; (1 : T))

(1 : _R) ⇐ Int

_R <: Nat

_R <: Int

Γ |- M ⇒ B  (M : B :> A)  B <: A
--------------------------------
Γ |- M ⇐ A

_R == Nat
_R <: Int


[M ⇒ B ⇐ A]

(M ⇒ _A ⇐ _B)

(x => x) 1 == 1

((M : Int -> Int) : Int -> String)


'a 'b. 'a -> 'b -> 'b
'a 'b. 'b -> 'a -> 'a


forall a. a -> a
forall a b. a -> b -> a

'a -> 'a
'a -> 'b -> 'a

(f : ∀. 'a -> 'a) => _

let f = (x : forall a. a -> a) => (x 1, x "a");

type t = {
  x : int;
  something : {
    id : nubmer;
    name : string;
  }
}
type t =
  F of { x : int }

type t<'a, 'b> = ('a, 'b);

type id<a> = a;
type t<a, b>;

t<int, int>
t<int, int>

let f = (x : forall a. a -> a) => (x 1, x "a");

let id : ∀. 'a -> 'a = x => x;

Γ |- M : A  Γ |- N : B
----------------------
Γ |- (M, N) : (A, B)


(f : id) => _

(f : ⊥) => _
(f : forall a. a) => _


(-> string int)
(-> int int)

let (x : string, y) : (int * int) = _

((m : int -> int) : int -> string)
((m : int -> int) : int -> string)
(M : A)

let id : <A>(x : A) => A = <A>(x : A) : A => x;

User = {

};

'a <: { id : int }

List.map ((user : User.t) => user.id) [User.blaine]

(f : <a>(x : a) -> a) => _

type t<mut a> = '_b;

let sequence x y = y;
let id : _a -> _a = sequence ();

record Ref<a> {
  mut content: a
}

make : mut a. (length: Number, item: a) => Array<a>




User = { x : Nat; y : String; };

user = { id = 1; name = "Meta"; } : ({ x : Nat; y : String; } | _A)

Γ |- M : A  Γ |- Y : B
------------------------------------
Γ |- (x = M, y = N) : (x : A, y : B)

Γ |- M : B  (A <: B)
--------------------
Γ |- (M : A) : A

Γ |- M : S  Γ |- N : T
(S <: (x : A) -> B)  (T <: A)
-----------------------------
Γ |- M N : B

{ x : Nat; y : String; } | _A :> User


M = {
  T = A;
  U = B;
}

Term ::=
  | x
  | M(A)
Type ::=
  | A -> B


((M : String -> Int) : Int -> Int)


size(_ : Type | Mut) == 1 // word
size(A -> B : Unboxed) == 1 // word
size((x : A,) : Unboxed) == 1 // word


(A -> B) -> C
A -> B -> C -> [D -> E] ≡ (A, B, C) -> D -> E
A -> B -> (C, D) ≡ (A, B) -> (C, D)
(A, (B, (C, [(D, E)]))) ≡ (A, B, C, (D, E))


(A, _B : Row)
(A, [_B])

A -> B ->


(A -> B -> C) ≡


All types have a simple kind, they're all known statically, so the compiler can trivially use them.

Kind (K) ::=
  // boxed value
  | Val
  // raw value
  | Raw

Type (A, B) ::=
  | (A : K_A) -> (B : K_B)
  | (A : K_A, B : K_B);

// so this
A -> ((B -> C) : Raw)
// is the same as
(A, B) -> C
// the curried version is
A -> ((B -> C) : Val)
// which is the same as
A -> B -> C

// same for pairs
(A, ((B, C) : Raw))
// same as
(A, B, C)
// but the val
(A, ((B, C) : Val))
// same as
(A, (B, C))

This makes everything a LOT easier, but we have a kind system, but well we had one anyway before, just that now we also use it for more things.

I will also be using it for type constructors and what not, but this is a simplified version to understand the general idea.


```typescript
type T<A, B> = _;
// same as
type T<(A, B)> = _;


````

```rust
[v - x] -| B ⇐ Row
-------------------------
[v] -| { x : A | B } : Row

B : Row [v + x]
-------------------------
{ x : A | B } : Row [v]

B : Row [v + x]
-------------------------
{ x : A | B } : Row [v + x]

B == T

T & { x : A }
-------------------------
{ x : A | B } : Row [v + x]

B == #pair ⇐ Raw
-------------------------
(A, B) : Raw

// fails
forall T. (f : T & { x : A }) -> _
// works
forall (T <: { x : A }). (f : T & { x : A }) -> _

forall (T : Val). (H : T <: {x : A}) -> (f : T & { x : A }) -> _

name = forall (T <: { name : _ | T }).
  (user : { name : _ | T }) => user.name;

-------------
{ x : A | B }


incr : Nat -> Nat;
incr : Int -> Int;

incr : T <: Nat | Int -> T -> T


R : Row
T = { name : _ | R };

_T == { name : _ | () }

T == { name : A; { id : Nat | () } }

(A, B, C) <: (A, B, ())



(A, B, C, D, ()) <: (A, B, C, ())
(A, B, C, D, ⊤) <: (A, B, C, ⊤)

// TODO: named tuples
A -> B -> C
(A, B) -> C

(A, B) -> B
A -> B -> C

A -> (B -> C : Raw)
A -> (B -> C)

(A, ((B, C) : Raw, ()))
(A, (B, (C, ())))
(A, B, (C, •))
(A, B, C)

A -> B -> C -> •

(A, B : Val)
(A, (B : Raw, ()))
(A, (B : Raw), ())

{x : A & {}}
{}
Record = ⊤

() : _

{} : _

B : Record
---------------
{ x : A & B } :


A :
---------------
{ x : A } : Row

(A, (B, C) : Val)

•

Record = • | { x : A & R }
Pair = • | (x : A, R)
Pair = • | (x : A, R)

{ x : A }
{ x : A & B }



p : (A, (B, • : Raw))

(x, (y, z)) = p;

r = {};

let x = set(1)

p : (A, (B, C) : Raw);


{} = M;
record Empty {}
enum T {

}

(x : A, y : B)

(A, B) == { 0 : A; 1 : B }

_T = { name : A & {} }
_T == { id : Nat & { name : A & {} } };


Type : Kind

A : Type

module M {
  A : Type;
  x : A;
}
p : (x : A, y : B, ());

p.1

_T = { 0 : A & {} }
_T == { 1 : Nat & { 0 : A & {} } };

{ 0; 1 } = M;

{ x : A & B }
{ x : A & { y : B & • } }

A of () | B of () | ⊥


(A, (((B, C) : Raw), ()))
(A, (B, (C, ())))

⊥

T = A of () | B of () | ⊥;

forall T. T -> (A, T)

(A, ()) == (A)

(A, B, C, ())
(A, (B, ((), •)))

(A, •)
(A, •)

(M : { x : A | • }) :> ()
(M : { x : A | () }) :> ()

⊤ <: (D, ⊤)
() <: (D, ⊤)

⊥
()
A | B


{ x : A | () } :>

A | B | ⊥
(M : A | ⊤) :> ⊤
(M : A | ⊥) :> A



"red" | "green" | "blue" | ⊥

_T = "red" | "green" | ⊥

// produces
_T = "red"
// handles
_T = "red" | "green";

P = "red" | "green" | "blue" | ⊥;

_T == "red" | "green" | "blue" | ⊥
red | green | ⊥ == "red" | "green" | "blue" | ⊥

red | green | ()


#open<"red" | "green" | ⊥>
#open<"red" | "green" | ⊥>



enum M {

}

match x with

_T = { name : _ | () };





{ name : _ | () }

(A, ((B, C) : Raw, ()))
(A, (B, C) : Raw)
(A, B, C) ≡ (A, B, C, ())

(A, (B, C) : Raw, ())

// subtyping
Pair = () | (A, _ <: Pair);
Record = () | { x : A | R <: Record }

// row polys
Raw


(_, _ : _A & Pair)
(_, _ : _A & Pair)

R <: { x : A | () }

{ x : A | { y : B | () } }
{ x : A; y : B | R }
{ x : A | { y : B | () } }

(A, (B, C) : Raw)
// TODO: do we need to distingish those?
(A, B, C)
(A, (B, (C, ())))

(A, ((B, C) : Raw))


forall (R <: { x : A }). (R : { x : A }) -> _
forall (R : Row [x]). { x : A | R }


M : A  N : B
-------------------------
{ x = M | N } ⇐ { x : A | B }


Pair = () | (A, _ : Pair)

Γ |- A : Type  Γ |- B : Pair
-------------------------------------
Γ |- (A, B) : Pair  Γ |- (A, B) : Raw


Γ |- A : Type  Γ |- B : Record
-----------------------------------------------------
Γ |- { x : A & B } : Record  Γ |- { x : A & B } : Raw

-------------------------------
Γ |- {} : Record  Γ |- {} : Raw


Γ |- A : Pair  Γ |- B : Raw
---------------------------
Γ |- A -> B : Val

Γ |- A : Pair  Γ |- B : Raw
---------------------------
Γ |- A -> B : Val

Number = Nat | Int | Float;
_T = Int;

_T == Nat

_T = Number;
add : 'T = Number; T -> T -> T;

add : Number -> Number -> Number;

match _T {
| Float => _
| Nat => _
}

_R = { x : A & B };


x : Nat | = 1;

Nat <: Int
Nat <: Float
Nat <: Number
Int <: Float
Int <: Number
Float <: Number


A : Type
--------------
(A, ()) : Type


(A, (B, ()))
(A, B, ())

(x : (), y : ())
(A, ())
(A, ((), ()))


(x : A, _B)
{ x : A & _B }

(x : A, _B) as _B
(x : A, (y : B, ()))


(() : Unboxed, ())
() == ((), ())

A == (x : A, ())

Γ |- A : Pair  Γ |- B : Raw
---------------------------
Γ |- A -> B : Type

Nat of { x : A; y : B }
// TODO: unboxed functions
Nat of (A -> B : Unboxed)


A => B
(x : A) -> B

T = #opaque()


forall A.
T = ()

Number =
  | Natural // missing
  | Uint64
  | Uint32
  | Uint16
  | Uint8
  | Integer
  | Int64
  | Int32
  | Int16
  | Int8
  | Rational
  | Float64
  | Float32



T = ∀.


Id = B => ∀A. A -> B -> A;
Id = ∀A. B => A -> B -> A;


(f : forall a. (forall b. b -> a) -> a) => _;

X = () => forall b. b;
T = forall a. X () -> a;

((Either A) B)


F = A => (A, A);
G = B => (B, B);

L = F(K);
R = G(K);

L == R
F(K) == G(K)
(K, K) == (K, K)

T = K;
L == F(T)

f = x => M;
forall a. X -> a
T = forall a. a -> a;

id : T = _;

x = id("a");
y = id(1);


(f : T) => _;


(x : Option(A)) => _
(x : Option(A)) => _
Option(A) == Option(B)


// TODO: injectivity bit
Option : !Val -> Val;

!Option(A) |-> Some of A | None

Option(B) == Some of A | None
f<x>

Option
T =

...(x : A, )


p : (x : Int32, y : Int32);
M

Γ |- B : Type
---------------
Γ |- (x : A, B)

{ x : A }

| x of A
| y of <P> -> P(x)

A -> (B, ())

(x : A, () : Raw)


_T == (x : A, _R)

(x : A, ())

A of { x : A }


...(x : A, ())

Box(Box())
Box()

T : Raw = ();

b|A| == _A

b|A| == b|_A : Raw|

(x : Raw)

T = n|(x : A) -> B|;
T = n|b|(x : A) -> B||;


f : (x : A) -> B = x => B;
f : T = x => B;

(Box(A, B), Box(C, D))
(x : A)


(x : _A, y ) -> B
A -> B


T () = forall b. b;
forall a. T () -> a

(x : _A, y : B)
(x : _A, (y : B, ()))

'a -> 'a

Int : Type;

T = Box(A);
Unbox(T) == T.einner

Forall(A)

A : Boxed + Mutable + Nominal


Raw

box(box(box(T)))
forall()
nominal()

Int = nominal(_);
Nat = nominal(Int);

forall a. (forall b. b -> a -> b) -> a -> a

x = 1;
f : Box<Number -> Number> = y => x + 1;
p : Box<(Number, Number)> = (1, 2)

p : Box<(Number, Number)> = (1, 2)

unbox (Number -> Number)

X of
A -> B

T = 'a => ∀. 'b -> 'a -> 'b;
∀. T<'a> -> 'a -> 'a


T = ∀. 'b -> 'b;
∀. (∀. 'b -> 'b) -> 'a -> 'a

∀. T<'a> -> 'a -> 'a

T = nominal(_);
U = nominal(T)

nominal(box())

forall(T) == T  (fv(T) == ∅)

forall(forall(B)) == forall(B)

box(A)
forall(A)
nominal(A)



nominal(box(A))
box(nominal(A))
nominal(box(A))

T =

t = _;
(t : Raw)

r = [header, int]
x = [header, int, r]


Γ |- T : Raw + Mut
-----------------------
Γ |- Ref<T> : Raw + Mut

Γ |- T : Raw + F
---------------------
Γ |- Box<T> : Val + F

<A : Val>(x : A) -> _;

Val <: Raw
(x : Number, ())

_A

size(Ref<M : Raw>) == size(M)
size(Box<M>) == Word



id = x =>
<A : Val + Mut> =>

T<'a as box<'k>> = 'a -> 'a;
T<'r as { x : _ & _ }> = { y : _ & 'r };
T<'r as { x : _ & _ }> =
  'r == { x : _ & _ };
  { y : _ & 'r };

id = x => x;

k = box<'a>. k -> k


'k : * -> *. 'k -> 'k
('k as x => 'M) -> 'k

'M<'a> -> ('a -> 'M<'b>) -> 'M<'b>


bind : ('M as x => 'R)<'a> -> ('a -> 'M<'b>) -> 'M<'b>

'M == x => 'R. 'R[x := 'a] -> ('a -> 'R[x := 'b]) -> 'R[x := 'b]


(M : (x : A) -> B) ≡ (x => M(x) : (x : A) -> B)





Γ |- M : (x : A) -> B  Γ |- N : A
---------------------------------
Γ |- M(N) : B[x := N]


Γ |- M : B  Γ |- N : A
-----------------------------
Γ |- x => M : B[x := N]


S : (x : Nat) -> Nat
Z : Nat

Γ, x : A |- M : B  Γ |- N : A
-----------------------------
Γ |- (x => M)(N) : B[x := N]

Γ |- N : A |-> M
----------------
Γ |- M : A

Γ |- S : (x : Nat) -> Nat |-> x => S(x)
---------------------------------------
Γ |- x => S(x) : (x : Nat) -> Nat  Γ |- Z : Nat
-----------------------------------------------
Γ |- (x => S(x))(Z) : Nat
--------------------------
Γ |- S(Z) |-> (x => S(x))(Z)
---------------------------
Γ |- S(Z)

S(Z)



Option = A => #nominal(T);
Option = #nominal(A => T);
Either(A)(B)

(_A, _B)



K = Box<Int32>;
T<K>

K -> K

T<'a : Val> = 'a -> 'a;

T<'a : Val> = 'a -> 'a;

(T = a => a == box('k); a -> a)


( -> a : Raw) ->

A -> B

f : A -> B -> C

f(x)(y)

r = f[0](f + 1, x);
r[0](r + 1, y)

f[0](f + 1, x)

('A as x => R) ->

Id = 'A -> 'A;
// same as
Id = ('A as El(_)). 'A -> 'A;
// also, arrows requires El on both sides
Id = El('A) -> El('A);
// also works for other kinds
Pure = ('M as x => El(_)). 'A -> 'M('A);


A == El(_)  B == El(_)
----------------------
A -> B

----------
bool(true)

-----------
bool(false)


---------------
T(K(_) -> K(_))

K(K())


(x => M)
'a. 'a -> 'a


set : raw a. ref<a> -> a -> unit;
set : box a. ref<a> -> a -> unit;
set : mut a. ref<a> -> a -> unit;
set : ref<mut<r>> -> r -> unit;

Int == box<'Int_r>

------------
A == mut<_>

'a


('a as box<mut<_>>)

A <: mut<A>



received : 'r | box<'Int_r>
expected : 'a & box<mut<_>>

type('r, 'a)
'a <: box<mut<>>

box<'a> == mut<box<'a>>>

box<mut<_>> == mut<box<_>>?


mut a. ref<a>
box<mut<_>> == mut<box<_>>?


received : box<Int_r>
expected : mut<'a>

set(_, 1 : mut<'a>)


set : 'a as mut<_>. ref<'a> -> 'a -> unit;

set : box a. ref<a> -> a -> unit;

f : 'a. Int -> 'a -> 'a;

f : 'a. ('b. 'b) -> 'a -> 'a;

f : 'a. Int<()> -> 'a -> 'a;

_A == M
M == M
x ==
forall a. (forall b. b -> a) -> a;

_A : Type -> Type;
(_A as 'x => 'f['x])
(_B as 'x => 'g['x])

pure : 'M.
a : Type -> Type;
b : Type -> Type;

a as 'x. 'x => f['x]
(_A as 'x => 'f['x])

_A == a
Id<'a> = 'a -> 'a;
id : 'a. Id<'a>;

Id<'a * 'b>

Number = Nat | Int | Float;
incr : 'a as Number | 'r. 'a -> 'a
incr : 'a as Number | 'r. 'a -> 'a
incr : Number & 'r -> 'r

incr : Number('r) -> Number('r);

add : Number('a) -> Number('a) -> Number('a);

add : Nat -> Int -> Int;

add : Number('_x) -> Number('a) -> Int;

add : Number('_z) -> Number('_z) -> Number('_z)

add : Number(Nat) -> Number('_y as Int) -> Number(Int)


incr : Number('x. 'x as Nat) -> Number('x. 'x as Nat)

add : 'x. Number('x) -> Number('x) -> Int;

make : Number(Nat) -> _;

add : Number a => a -> a -> a;
add : a as Number -> a -> a;
add : a extends Number. a -> a -> a;
add : Number<a> -> Number<a> -> Number<a>;

add : Nat -> Nat -> Nat;
add : Nat -> Int -> Nat;
add : Int -> Nat -> Nat;
add : Int -> Int -> Int;


add : Number<a> -> Number<a> -> Number<a>;

add : a as Number -> a -> a;

add : number a.

incr : 'x. Number('x as Nat) -> Number('x. 'x as Nat)
add : Number('_x as Nat) -> Number('_y as Int) -> Number(Int)

{ x : A & r }



'x. 'x as Nat == Int

'_x as Nat

'_x as Nat | _ == '_y as Nat | Int | _
'_x as Nat == '_y as Int

Nat


add (_ : Nat) (_ : Int) : Number(Int)
add (_ : Nat) (_ : Int)

add : ('_a as Number) -> '_a -> '_a

add : Nat -> Nat -> Nat
add : Nat -> Int -> Int
add : Int -> Nat -> Int



x : Number('_x as Nat)

Number_Int = Nat | Int
Number = Number_Int | Float;

received : '_x as Nat | 'r
expected : '_a as Number

add

incr :

V_number == V_nat

Number()
1 : Number(Nat)


received : Nat | 'r
expected : Nat | Int | Float | 'b;

// another way for algebraic subtyping, by indexing row poly
// I'm not sure if all lattice's are supported tho
Nat <: Int
Int <: Rat
Rat <: Number
Float32 <: Float32
Float64 <: Number

// this is similar to precedence in parsing
type L_Nat = [ `Nat]
type L_Int = [ #L_Nat | `Nat ]
type L_Rat = [ #L_Int | `Rat ]
type L_Float = [ `Float ]
type L_Float = [ `Float ]
type L_Number = [ #L_Rat | #L_Float ]

// the predefined types
type I_Number<'a>
type Nat<'r> = I_Number<[ `Rat of [ `Int of [ `Nat of [ #'r ] ] ] ]>
type Int<'r> = I_Number<[ `Rat of [ `Int of [ #'r ] ] ]>
type Rat<'r> = I_Number<[ `Rat of [ #'r ] ]>
type Float32<'r> = I_Number<[ `Float64 of [ `Float32 of [ #'r ] ] ]>
type Float64<'r> = I_Number<[ `Float64 of [ #'r ] ]>
type Number<'r> = I_Number<[ `Float64 of [ #'r ] ]>

// this almost works
(1 : I<[ `Nat | #'_r]>)
// but annotations can make some weird values
(1 : I_Number<[ `Nat | `Not_a_number]>)
// so you hide the I_Number and use the predefined types
(1 : '_x as Nat)
// now the internal rows are always inferred
add : 'a as Number -> 'a -> 'a;
// which is the same as
add : 'a as I_Number<[ #'r ]> -> 'a -> 'a;

// example
(add 1)
// instantiation
add : '_a as I_Number<[ #'_a_r ]> -> '_a -> '_a
1 : '_b as I_Number<[ `Nat | #'r]>
// unifies
'_a as I_Number<[ #'_a_r ]> == '_b as I_Number<[ `Nat | #'r]>
'_a_r == [ `Nat | #'r]
'_a == '_b
// then
(add 1) : '_a as I_Number<[ `Nat | #'_r]> -> '_a
// if you generalize
(add 1) : 'a as I_Number<[ `Nat | #'r]>. 'a -> 'a
// or with the aliases
(add 1) : 'a as Nat. 'a -> 'a



(1 : '_x as Nat)
// which is the same as

incr ()

incr =

// so you need to bound

incr : 'a as [ #L_Number | #'r ] -> 'a;
1 : '_a as Number<[ `Nat | #'_ ]


// what if you try
incr `Not_number
// then
(incr `Not_number : [ #L_Number | `Not_number ])


incr : '_a as [ #L_Number | #'r ] -> 'a;


add : 'a as [ #L_Number | #'r ] -> 'a -> 'a;



type Rat<a> = a as [> L_rat | r ] ;
type Number<a> = a as [> L_Number | r ] ;

<#Nat | #Int | #Rat | #Float32 | #Float64>


f : L_Rat<a> -> L_Rat<a>;


1 : L_Number<Nat | _r>
1.2 : L_Number<Rat | _r>


(1 : L_Number(Nat | _r))

Number()

fix : μx. x -> unit;

fix()

unfold(fix)(fix)

fix(fix)


A = () -> A;
B = () -> () -> B;

f : A -> ()
g : B -> ()

type A = () -> A;
type B = () -> B;

type X = Int;

((1 : X) : Int)
((1 : Int) : X)

|- () -> A == () -> () -> B
|- A == () -> B
A == () -> B |- () -> A == () -> B
A == () -> B |- A == B
A == () -> B |- () -> A == () -> () -> B
A == () -> B |- A == () -> B


x = a;

{ x : A & 'r }

_a = { x : A & _a }
_f = x => _;

'_f = 'x => 'r['x]

pure : 'M. 'a -> 'M<'a>;
pure : 'M. 'a -> 'r['a];

M : Type -> Type

'M as 'x. 'x => M['x]
'r['a] == f['b]
f['a] == f['b]

{ x : A & '_l } == { y : B & '_r }

{ x : A & '_l } == { y : B & '_r }
'_f<N>

x = nominal(T);

enum T {
  X({ var : Int })
}


Γ |- Q : () | (A, B) | 'R
---------------------
Γ |- (P, Q) : (A, 'R)

Γ |- P : A  Γ |- Q : {} & R
----------------------------------
Γ |- { x = P & Q } : { x : A & R }

{ x :}

{ y; x } = (M : { x : A & { y : B & _r } });

{ x : A & { y : B & _r } }

{ x : A & _r }
_r


()

box(mut(m))

∀.

'a
box x.
mut x.

[x = M; N] : [A; x => B]


[M, N] : [A, B]
(x : A) => M : (x : A) -> B

f : (x : A) -> B
#eta((x : A) -> B, f)


[M, N, K] == box([M, [N, [K, []]]])
[M, [N, [K, []]]] == box([M, box([N, box([K, []])])])


'a -> 'a

box('a) -> box('a)


(x : 'a. 'a -> 'a) => _

('x. )


('r as {}). { x : A & 'r }

user.id

expected : { name : 'c | 'd }
received : { id : 'a | '_b }

expected : { name : 'c | {} }
received : { id : 'a | '_b }

expected : P({ name : 'c | {} })
received : P({ id : 'a | '_b })

({ id : Nat | {} }) -> Nat

({ id : Nat | '_A }) -> Nat



expected : { id : Nat | {} }
received : { id : Nat; name : String; {} }

expected : { id : Nat; 'r. 'r }
received : { id : Nat; name : String; 'r. 'r }

{ id = M; } : { id : Nat; name : String; ('r) }


Ref : mut 'x. (A : 'x) -> Type;
ref : 'x. (x : 'x) -> Ref(Mut('x));

Ref : (A : Mut(Type)) -> Type;

'x. 'x -> 'x

<x : Type> -> x -> x


Sort(Type)
Sort(Data)



[x : A : Sort('a), B(x) : Sort('b)] : Sort('c)

[x : A : (Type | 'a), B(x) : (Type | 'a)] : Sort('a)

{
  type t
  x : t
}

id = <A : Data>(x : A) : A => x;


(A : Data $ 0, x : A) => x
a : [
  A : Data,
  x : A
];
b : [A : Data, x : A];

p : a == b;
p.0 : a.0 == b.0

fst()
{
  T : Data,
  x : T : Data
}


(M : A) :> B // cast M of A to B
// because M was known to be A it can be cast back
((M : A) :> B) :> A

(in(M : A) : B) // cast M of A to B
// you can only do out if it's an in
(out(in(M)) : A) // cast back to A

Data
Type


(x : A)
min(Type, Data) == min(Data, Type) == Data

min('a, Type) |-> 'a
min(Type, 'a) == 'a

min('a, Data) |-> Data
min(Data, 'a) |-> Data

----------------------
min('a, 'b) == Data

'a == Type  'b == Type
----------------------
min('a, 'b) == Type

min(Data, 'b)
min(S1, S2)

min(S1, S2) == Data

S1 == Data  S2 == Data
----------------------
min(S1, S2) == Data

f : (A : Type & 'a, B : 'a) : 'a

A : Type | '_c

(A : Type | '_c) :> Type & '_a

Type | '_c == Type & '_a


f(A : Type, _)

add : (x : Number as '_a, y : 'a) -> 'a;

Nat as '_b == Number as '_a

Nat as '_b == Number as '_a


expected : Nat as '_b
β
'_b == '_a

add(1 : Nat as '_b, _ )
f(Type, _ )


⇑(A : Data) : Type
⇓(A : Type) : Data

(in⇓(M : A) : )
(out⇓(A : Type)) :

(x : ⇑A : Type) ->
[x : A : Data, y : _] : Type

Type | '_a

f(A : Type | '_a, ) : Type

[x : A : El('a), B(x) : El('b)] : El(min('a, 'b))
(x : A) ->

(x : A) -> B(x)

M :

Int
Nat
Float
Number

add : (x : 'a. Nat as 'a, y : '_a) -> 'a;

left : 'a. Nat as 'a
right : Nat as '_b

left : 'a. 'a -> 'a
right : '_b -> '_b

add(1 : )

Int as '_a == Nat as '_b

Number as '_a == Int as '_b
Int as '_b == Number as '_a

'_a == Int as '_b

1 : Nat as 'b

add(1 : Nat as '_b, -1)

add(-1 : Int as '_b, 1)

add : (x : Number & 'a, y : Number & 'a) -> 'a | Number;

//


(x : A : 's)
A : Data
A : Data | Type


Mut : Type;

Ref : (A : Mut) -> Data;
ref : <A>(x : A) -> Ref(A);
write :

ref()

Sort(min('a, 'b))

Sort(_A)

unbox(box())

(`x(l) | `y(l)) = a;

match(a) {
  `x(l) => _;
  `y(l) => _;
}


add : (x : Number & 'a, y : 'a) -> 'a;

Number = Number(`Number | `Int | `Nat | `Float);
Int = Number(`Int | `Nat);
Nat = Number(`Nat);
Float = Number(`Float);

received : Number(`Nat | 'b)
expected : Number(`)
'a <: 'b <: 'c

add(1 : Nat | '_b, -1)


User = Nominal({ id : Nat; name : String });

received : { id : Nat; name : String } & User;
expected : { id : Nat; '_r } | '_a

M.id


match(x) {
  (P) => _
}
expected : Nominal()



{ A : Type; x : A; y : B }
{ y : B; A : Type; x : A;  }

// works
[A\0 : Type; x\1 : A; y\2 : B]
[y\0 : B; A\1 : Type; x\2 : _A]

[A\1 : Type; x\2 : A; y\2 : B]
[y\2 : B; A\1 : Type; x\2 : _A]

A\1 == _A\2

// fails
[A\0 : Type; x\1 : A; y\2 : B]
[y\0 : B; x\1 : _A; A\2 : Type]

[A\2 : Type; x\1 : A; y\2 : B]
[y\2 : B; x\1 : _A; A\2 : Type]

A\2 == _A\1

{ x = M; { y = N; _R } }

{ x : A; _ } as '_R

<: '_M <: {}

r => r.x + r.y

{ x : A; _ } as '_a
{ y : A; _ } as '_b
'_a == '_c
'_b == '_c

User = Nominal({ id : Nat; name : String });

received : { id : Nat; name : String } | User
expected : { id : Nat; _ } | _'a

{ y : A; _ } == Record({x}) as 'a
'a == { y : A; _ }


{ x : A; _ } as '_a
{ x : A; { y : A; _ } }

{ x : A; { y : A; _ } } as '_c


{ x : A; y : A; _ } as '_R <: '_M <: {}

received : { y : B; _ } | '_N
expected : {} & '_M

{ y : B; _ } <: {}


received : { y : B; _ } | '_N
expected : {} & '_M

{ y : B; _ }

{ x = M; N }




_R_1 := { x : A; _R_1 }


{ x : A; _R }

Term (M, N) ::=
  |


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



(P | Q) = M;

match(M) {

}

f : (..._A) -> _B
app = (f, x, y) => f(x, y)



if() {
  return
}


r : { x : A; y : B; _C } = { x = M; y = N; _K };
u : { y : B; x : A; _D } = { y = N; x = M; _R };

f :Q

r == u


(a, b, c, d) |-> box([a, [b, [c, [d, []]]]])

{ x : A; {} }
{ x : A; y : B; } |-> Box({ x : A; { y : B; {} } })

id : 'a. 'a box -> 'a box
  = fun x -> x;

show = (S : { A : Data; show : (x : Box(A)) -> String; }, x : Box(S.A)) => x;

type user = {
  x : A;
  y : B;
};

User = Nominal(Box({
  x : A;
  y : B;
}));

[]
[A, B]

{}
{ x : A; B }

|
`l(A) | B


{ x : A; _R }

((a, b) => M) : '_a -> '_b


((P) => M) : A -> B
((P : A) => (M : B))

((a, b) => M) : (A, B) -> C
((a : A, b : B) => (M : C))

[Box(A), Box(B), Box(C), Box(D)] -> B

f : (A, B, C, D) -> B
  = (a, b, c, d) => _

(a, b, c, d) |-> box([a, [b, [c, [d, []]]]])

[a, [b, [c, [d, []]]]]

type term = Term({
  desc : term_desc;
  type_ : value;
  loc : Location.t;
})

type term = Term({
  mutable desc : term_desc;
  type_ : value;
  loc : Location.t;
})

Term = Nominal(
  `Term({
    desc : Ref(term_desc);
    type_ : value;
    loc : Location.t;
  })
);

User = Nominal({
  id : Nat;
  name : Nominal({
    first : String;
    last : String;
  });
});

Term2 = Nominal(
  `Term2({
    descx : term_desc;
    type_ : value;
    loc : Location.t;
  })
);

add : <A extends Number>(x : A, y : A) -> A;

//
add : (x : Int, y : Int) -> Int;
add : (x : Nat, y : Nat) -> Nat;


abstract type Color = Number



Color = int;

type 'a t = 'a

let x : int t = 1

add : (x : Number & 'a, y : Number & 'a) -> Number & 'a;

incr = (x) => add(x, 1);

add(1.2, Nat) : Rat


Term
term.Term


Color = Nominal(Number);

type 'a t = 'a

t = (A : Type) => A;

x : t(Int) = 1;


x : Color = 1;

T = [[Nat, String] Number];


'a = Nat;

A = [Nat, Number];
B = A;

((M : A) : B)


T_L = [Nat, String];
T = [T_L, Number];

#test(f(1) == 2);

x : M.A = 1;

Type : Type
Data : Type


Nat : Data
M : (A : K)


[A, _R]

name_P = User;
name = (u : name_P) => u.name;

User == name_P

User = { id : Nat; name : String };
name(user : User)
{
  A : Data = Nat;
  x : A : Data = 1;
};

[x : A, []]

(x : Nat, y : Nat) -> Nat


user =
  (r : { name : String; 'R }, m : { name : String; 'R }) => r;

T = { name : String; {} }; (r : T, m : T) -> T

add = (a : [< Number] as 'R, b : 'R) -> 'R;

[< `nat | `int > '_R]

[< `nat | `int > '_R] == [< > `nat | '_A]

Number('a) = Bottom(`Number)
Nat('a) = Number(_);

1 : Nat('a)

Number = Nat | Int | Float;

received : Nat | '_b
expected : Number & '_a



add(-1, 2)


A & B
```
