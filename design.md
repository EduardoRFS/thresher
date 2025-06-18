```rust
Type ::=
  | x
  | A -> B
  | forall x <: S. M
  | forall x. M
  | exists x. M

Code ::=
Term ::=
  | x
  | let x = M; N
  | Type
  | A -> B
  |


let x = (M, N); K

x ≡
  #inst(x)
(M N : E) ≡
  f = _A -> _B;
  ((M : _A -> _B)(N : _A) : _B)

module M { B } ≡
  M : {

  } = B

Meta
Type : Meta;
Mut : Meta;

type User {
  id : Nat;
  name : String;
};

User = #type({
  id : Nat;
  name : String;
});


name = user => user.name;

name = (user : _A & { name : String; }) => user.name;
```
