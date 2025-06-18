Named records like variants?

```rust
let User { id; name } = user;
```

```rust
let x = ({
  record Person { name: String, age: Number }
  let x = 1
  x + 1
})

let f : 'a. 'a -> 'a
```

```rust
Γ |- type x = A -| Δ
Γ |- let x = M -| Δ
```

Stress test if contravariance and higher-rank types preserve names.
