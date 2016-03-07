# Nested Functors

See example for automatic mapping of functions over arbitrarily nested lists:

```haskell
data Person = Person { name :: String, age :: Integer, gender :: String, status :: String } deriving Show

let persons = fromList' [Person {name="Alice", age=20, gender="F", status="Good"}, Person {name="Bob", age=18, gender="M", status="Good"}, Person {name="Chuck", age=16, gender="M", status="Bad"}] :: NList N1 Person

persons <<$$>> select (age)
-- [20,18,16]

persons <<$$>> groupby (gender) <<$$>> select (age)
-- [[20],[18,16]]

persons <<$$>> groupby (gender) <<$$>> groupby (status) <<$$>> select (age)
-- [[[20]],[[18],[16]]]
```

Note that `select`, `filterby`, `orderby`, `groupby` work regardless of nesting depth. You can also append or remove nesting levels:
```haskell
persons <<$$>> groupby (gender) <<$$>> select (age) <<$$>> produce (\x -> [0..x])
-- [[[0..20]],[[0..18],[0..16]]]

persons <<$$>> groupby (gender) <<$$>> select (age) <<$$>> reduce (sum)
-- [20, 34]
```

Advanced examples:
```haskell
persons <<$$>> groupby (gender) <<$$>> select (age) <<$$>> reduce (sum &&& mean)
-- [(20,20.0),(34,17.0)]

persons <<$$>> filterby(age >= 18) <<$$>> groupby (gender) <<$$>> select (age * 2)
-- [[40],[36]]

let abg = persons <<$$>> groupby (gender) <<$$>> select (age) in ((/) <$> (abg <<$$>> select (realToFrac)) <<*>> (abg <<$$>> reduce (mean)))
-- [[1.0],[1.0588235294117647,0.9411764705882353]]
```

The operators `<<$>>`, `<<*>>` are generic versions of `<$>` in Functor and `<*>` in Applicative.
```haskell
<<$>> :: (NList m a -> NList (m+x) b) -> NList n a -> NList (n+x) b
<<*>> :: (NList m (a -> b)) -> NList n a -> NList (max m n) b
pure' :: (NList 0 a)
```
