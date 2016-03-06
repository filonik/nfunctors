# Nested Functors

See example for automatic mapping of functions over arbitrarily nested lists:

```haskell
data Person = Person { name :: String, age :: Integer, gender :: String, status :: String } deriving Show

let persons = fromList' [Person {name="Alice", age=20, gender="F", status="Good"}, Person {name="Bob", age=18, gender="M", status="Good"}, Person {name="Chuck", age=16, gender="M", status="Bad"}] :: NList N1 Person

persons `select` age
-- [20,18,16]

persons `groupby` gender `select` age
-- [[20],[18,16]]

persons `groupby` gender `groupby` status `select` age
-- [[[20]],[[18],[16]]]


Note that "`select` age" works regardless of nesting depth. You can also append or remove nesting levels:

persons `groupby` gender `select` age `produce` (\x -> [0..x])
-- [[[0..20]],[[0..18],[0..16]]]

persons `groupby` gender `reduce` (sumof age)
-- [20, 34]
```
