import GHCi.Message (EvalResult(EvalException))
--
-- Introduction to functional programming in Haskell
--
-- 5: Types
--

-- type aliases with type

-- type String = [Char]
type Name = String
type Size = Integer
type Age = Integer

age1 :: Age
age1 = 26

size1 :: Size
size1 = 183

bool1 = (age1 == size1) -- works )-:


-- newtype

newtype Size' = Size' Integer
newtype Age' = Age' Integer

age2 = Age' 26
size2 = Size' 26
-- bool2 = (age2 == size2) -- does not work (-:


-- data

data Person = PersonConstructor Name EyeColor
-- PersonConstructor is a Constructor,
--   a function that takes values (of type Name and EyeColor)
--   and constructs a value (of type Person)

-- multiple constructors ("cases")
data EyeColor =
        Brown
    |   Blue
    |   Red -- Vampires only

sebastian :: Person
sebastian = PersonConstructor "Sebastian" Brown

-- pattern matching
printEyeColor :: Person -> String
printEyeColor (PersonConstructor _ Brown) = "You have brown eyes!"
printEyeColor (PersonConstructor _ Blue)  = "Deine blauen Augen..."
printEyeColor (PersonConstructor _ Red)   = "You are a vampire!"


-- recursive data type

-- custom integer list, works just like [Integer]
data MyIntegerList = EmptyIntegerList | IntegerHead Integer MyIntegerList

sum' (EmptyIntegerList) = 0
sum' (IntegerHead x list) = x + sum' list

-- does not work:
-- data MyList a = EmptyList | Head a MyList
-- MyList is a type constructor, not a type

-- works just like [a] for any a
data MyList a = EmptyList | Head a (MyList a)

-- node-labeled trees
data Tree a = Leaf | Node a (Tree a) (Tree a)

treeMap _ (Leaf) = Leaf
treeMap f (Node x t1 t2) = Node (f x) (treeMap f t1) (treeMap f t2)

-- option-type (similar to null in Java)
data Maybe' a = Nothing' | Just' a

-- map of key-value pairs
type Map k v = [(k,v)]

-- keys need to be comparable using ==, see later
lookup' :: Eq k => Map k v -> k -> Maybe v
lookup' [] _ = Nothing
lookup' ((k',v):xs) k = if k == k' then Just v else lookup' xs k

mommyTable :: Map Name Name
mommyTable = [("Bob","Anna"),("Anna","Mathilda"),("John","Eve")]

example1 = lookup' mommyTable "Bob"
example2 = lookup' mommyTable "Eve"

-- grandmother' :: Name -> Maybe Name
-- grandmother' p =
--     let
--         mother = lookup' mommyTable p
--     in
--         lookup' mommyTable mother
-- -- type error: Maybe Name vs. Name

grandmother :: Name -> Maybe Name
grandmother p =
    case lookup' mommyTable p  of
        Nothing     -> Nothing
        Just mother -> lookup' mommyTable mother
-- use pattern matching: ugly but it works!
-- monads provide a nicer solution (later)


-- subtle difference between newtype and data
data WeirdData = WeirdData Integer
newtype WeirdNewtype = WeirdNewtype Integer

subtle1 = case undefined of
     WeirdData _ -> 1

subtle2 = case undefined of
     WeirdNewtype _ -> 1


-- records
-- usage discouraged
data Person' =
    Person'
    {
        name :: String,
        age :: Int
    }

horst :: Person'
horst = Person' { name = "Horst", age = 68 }
