# Nested Functors

### Usage

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

### Details

The operators `<<$>>`, `<<*>>` are generic versions of `<$>` in Functor and `<*>` in Applicative, `<<$$>>` and `<<**>>` are flipped.
```haskell
<<$>> :: (NList m0 a -> NList m1 b) -> NList n0 a -> NList n1 b -- where n1 = n0 + (m1 - m0)
<<*>> :: (NList m0 (a -> b)) -> NList n0 a -> NList n1 b -- where n1 = max(n0, m0) 
pure' :: a -> NList 0 a
```

Examples (Pseudocode):
```haskell
select :: (a -> b) -> (NList 0 a -> NList 0 b) -- <<$>> :: (NList 0 a -> NList 0 b) -> NList n a -> NList n b
orderby :: (Ord b) => (a->b) -> (NList 1 a -> NList 1 a) -- <<$>> :: (NList 1 a -> NList 1 a) -> NList n a -> NList (n+0) a
groupby :: (Ord b) => (a->b) -> (NList 1 a -> NList 2 a) -- <<$>> :: (NList 1 a -> NList 2 a) -> NList n a -> NList (n+1) a
```
