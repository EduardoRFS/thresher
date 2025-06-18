const weird: (id: <A>(x: A) => A) => [number, string] = (
  (a) => (id) =>
    [id(1), id("a")]
)();
