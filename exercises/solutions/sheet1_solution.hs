{-# LANGUAGE NoMonomorphismRestriction #-}

-- for dp'''
import Control.Applicative

--
-- Introduction to functional programming in Haskell
--
-- Exercise sheet 1
--
-- Solution
--

--
-- Exercise 1
--

e1 :: [Bool]
e1 = [True]

e2 :: [a]
e2 = []

e3 :: a -> a
e3 = \ x -> x

e4 :: a -> b -> (b, a)
e4 = \ x y -> (y, x)

e5 :: a -> a -> [a]
e5 = \ x y -> [y, x]

e6 :: (Eq a) => [a] -> [a]
e6 = \ x -> (if x == [] then x else x)

e7 :: [a] -> a
e7 = head

e8 :: [a] -> [a]
e8 = \ xs -> tail xs

e9 :: [a] -> [a]
e9 = \ xs -> tail (tail xs)

f :: Bool -> a -> a -> a
f x y z = if x then y else z

g :: (Eq a) => a -> a -> a -> a
g x y z = if x == y then y else z


data Tree a = Leaf | Node [a] (Tree a) (Tree a)

h :: Tree Char -> String
h Leaf = "c"
h (Node "x" l r) = "x"
h (Node x l r) = x

i :: Tree a -> b -> b
i (Node x l r) = id

j :: Tree a -> Tree a
j (Node [] l r) = l

k :: Maybe a -> Maybe b
k Nothing = Nothing

l :: Tree a -> [a]
l (Node a b c) = a

m :: Tree a -> Tree Int
m (Node x l r) = Node [length x] (m l) (m r)


--
-- Exercise 2
--

-- compute the permutations of a list (h:t)
--   compute the permutations of t recursively
--   then insert h at every possible position
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (h:t) = insertEverywhere h (permutations' t)
    where
        insertEverywhere :: a -> [[a]] -> [[a]]
        insertEverywhere x li = concatMap (insertEverywhere' x) li
        insertEverywhere' :: a -> [a] -> [[a]]
        insertEverywhere' x []     = [[x]]
        insertEverywhere' x (y:ys) =
            (x:y:ys) : (map (\z -> y:z) (insertEverywhere' x ys))


-- helper function for taking the first element of a list that satisfied a given predicate
takeFirst :: (a -> Bool) -> [a] -> a
takeFirst _ [] = error "none found"
takeFirst p (h:t) = if p h then h else takeFirst p t

-- predicate for checking if a list is sorted
isSorted [] = True
isSorted [_] = True
isSorted (x:y:t) = x < y && isSorted (y:t)

-- take the first sorted permutation
idioticSort list = takeFirst isSorted (permutations' list)


--
-- Exercise 3
--

flatmMap :: (a -> [b]) -> [a] -> [b]
-- flatmMap = concatMap
-- flatmMap f = concat . map f
flatmMap f [] = []
flatmMap f (x:xs) = (f x) ++ (flatmMap f xs)

partition' :: (a -> Bool) -> [a] -> ([a],[a])
-- partition' p list = (filter p list, filter (not . p) list)
partition' p []    = ([],[]) -- <(^_^)>
partition' p (h:t) = if p h then (h:left,right) else (left,h:right)
    where
        (left,right) = partition' p t

length' :: [a] -> Int
length' = foldl (\i _ -> i+1) 0

minimumWrt :: Ord b => (a -> b) -> [a] -> a
minimumWrt val [] = error "Empty list has no minimum"
minimumWrt val [x] = x
minimumWrt val (x:y:t) =
    if val x <= val y
    then minimumWrt val (x:t)
    else minimumWrt val (y:t)

-- insertation
sortWrt :: (a -> a -> Bool) -> [a] -> [a]
sortWrt _ [] = []
sortWrt comparator (h:t) = insert comparator h (sortWrt comparator t)
    where
        insert c x []     = [x]
        insert c x (y:ys) = if c x y then x : y : ys else y : (insert c x ys)

-- try out this comparator
-- does using it make sense?
isDisibleBy :: Integer -> Integer -> Bool
isDisibleBy n m = m `mod` n == 0


--
-- Exercise 4
--

-- we use integers for variables
type Variable = Integer

data Literal =
        Pos Variable
    |   Neg Variable
    deriving (Eq)

newtype Clause = C [Literal]

newtype Formula = F [Clause]

-- constants
falseFormula = F [ C [] ]
trueFormula = F []

-- obtain the polarity of a literal
data Polarity = Positive | Negative
    deriving (Eq)

polarity :: Literal -> Polarity
polarity (Pos _) = Positive
polarity (Neg _) = Negative

-- extract the variable from a literal
variable :: Literal -> Variable
variable (Pos n) = n
variable (Neg n) = n

-- negate a literal
neg :: Literal -> Literal
neg (Pos n) = Neg n
neg (Neg n) = Pos n


-- helper data type for satisfiability checking
data Result a =
        Unsatisfiable  -- formula is unsatisfied
    |   Satisfied      -- formula is already satisfied
    |   Undetermined a -- formula is not yet determined to be unsatisfiable or satisfied

-- compute F[L]
assignToLiteral :: Literal -> Literal -> Result Literal
assignToLiteral formula literal
    -- variables not equal, formula not affected
    | variable formula /= variable literal  = Undetermined formula
    -- variables equal, formula is affected
    | polarity formula == polarity literal  = Satisfied -- formula is now satisfied
    | otherwise                             = Unsatisfiable -- formula is now unsatisfied


assignToClause :: Clause -> Literal -> Result Clause
-- the empty clause is always unsatisfiable
assignToClause (C []) literal = Unsatisfiable
assignToClause (C literals) literal =
    -- first map assign_to_literal over the literals, then reduce using foldl
    foldl helper Unsatisfiable $ map (\ x -> assignToLiteral x literal) literals
        where
            helper :: Result Clause -> Result Literal -> Result Clause
            -- as soon as one part of the clause is satisfied, the whole clause is satisfied
            helper Satisfied     _             = Satisfied
            helper _             Satisfied     = Satisfied
            -- ignore unsatisfiable parts of the clause
            helper clause        Unsatisfiable       = clause
            helper Unsatisfiable (Undetermined lit)  = Undetermined $ C [lit]
            helper (Undetermined (C literals)) (Undetermined lit) = Undetermined (C $ literals ++ [lit])


assign :: Formula -> Literal -> Result Formula
-- the empty formula is always satisfied
assign (F []) _ = Satisfied
-- similar to assign_to_clause
assign (F clauses) literal =
    foldl helper Satisfied $ map (\x -> assignToClause x literal) clauses
        where
            helper :: Result Formula -> Result Clause -> Result Formula
            -- as soon as one clause of the formula is unsatisfiable, the whole formula is unsatisfiable
            helper Unsatisfiable _             = Unsatisfiable
            helper _             Unsatisfiable = Unsatisfiable
            -- ignore satisfied parts of the formula
            helper formula       Satisfied             = formula
            helper Satisfied     (Undetermined clause) = Undetermined $ F [clause]
            helper (Undetermined (F clauses)) (Undetermined clause) = Undetermined (F $ clauses ++ [clause])

unitLiteral :: Formula -> Maybe Literal
unitLiteral (F [])            = Nothing
unitLiteral (F ((C [lit]):_)) = Just lit
unitLiteral (F (_:clauses))   = unitLiteral (F clauses)

-- function that extracts all literals
literals :: Formula -> [Literal]
literals (F []) = []
literals (F ((C c):cs)) = c ++ literals (F cs)

pureLiteral :: Formula -> Maybe Literal
pureLiteral formula = head' pures
    where
        head' :: [a] -> Maybe a
        head' (h:_) = Just h
        head []    = Nothing
        pures = filter (\x -> not $ (neg x) `elem` candidates) candidates
        candidates = literals formula


sat :: Formula -> Bool
-- wrap the formula
sat formula = dp $ Undetermined formula

dp :: Result Formula -> Bool
dp (Satisfied) = True   -- the formula is satisfiable
dp (Unsatisfiable) = False -- the formula is unsatisfiable
dp (Undetermined formula) =
    -- apply unit, then pure, then split with the first literal
    case unitLiteral formula of
        Just uliteral -> dp (assign formula uliteral)
        Nothing ->
            case pureLiteral formula of
                Just pliteral -> dp (assign formula pliteral)
                Nothing ->
                    let
                        fliteral = head $ literals formula
                    in
                        (dp $ assign formula fliteral) || (dp $ assign formula (neg fliteral))


-- alternative version that produces a satisfying assignment
sat' :: Formula -> Maybe [Literal]
sat' formula = dp' [] (Undetermined formula)

-- keep track of candidate assignment
dp' :: [Literal] -> Result Formula -> Maybe [Literal]
dp' assignment (Satisfied)  = Just assignment
dp' _          (Unsatisfiable) = Nothing
dp' assignment (Undetermined formula) =
        -- apply unit, then pure, then split with the first literal
    case unitLiteral formula of
        Just uliteral -> dp' (uliteral:assignment) (assign formula uliteral)
        Nothing ->
            case pureLiteral formula of
                Just pliteral -> dp' (pliteral:assignment) (assign formula pliteral)
                Nothing ->
                    let
                        fliteral = head $ literals formula
                        negfliteral = neg fliteral
                    in
                        -- try to assign the literal first, if that doesn't work, try the negation
                        case dp' (fliteral:assignment) (assign formula fliteral) of
                            Nothing -> dp' (negfliteral:assignment) (assign formula negfliteral)
                            x -> x

-- do you think this code is ugly?
-- it can be made nicer using the fact that Maybe is a monad and
-- using the the function <|> for the class Alternative from Control.Applicative
sat'' :: Formula -> Maybe [Literal]
sat'' formula = dp'' [] (Undetermined formula)

dp'' :: [Literal] -> Result Formula -> Maybe [Literal]
dp'' assignment (Satisfied)  = Just assignment
dp'' _          (Unsatisfiable) = Nothing
dp'' assignment (Undetermined formula) =
    -- try unit first
    do
        literal <- unitLiteral formula
        dp'' (literal:assignment) (assign formula literal)
    <|>
    -- if that fails, try pure
    do
        literal <- pureLiteral formula
        dp'' (literal:assignment) (assign formula literal)
    <|>
    -- if that fails, try split with the first literal
    let
        literal = head $ literals formula
    in
        dp'' (literal:assignment) (assign formula literal)
    <|>
    -- if that fails, try split with the negation of the first literal
    let
        literal = neg $ head $ literals formula
    in
        dp'' (literal:assignment) (assign formula literal)


-- optional functions for printing the formulas in a nice way
instance Show Literal where
    show (Pos n) = show n
    show (Neg n) = "¬" ++ show n

instance Show Clause where
    show (C literals) = foldl (\x y -> if null x then y else x ++ " v " ++ y) "" (map show literals)

instance Show Formula where
    show (F clauses) =
        foldl (\x y -> if null x then y else x ++" ^ " ++ y )
            ""
            (map ((\x -> "("++x++")") . show) clauses)

-- Some test cases
testFormula = F [ C [Pos 1, Pos 2], C [Pos 3, Neg 2], C [Neg 3] ]

testLiteralsExpr = literals testFormula
testLiteralsResult = [Pos 1, Pos 2, Pos 3, Neg 2, Neg 3]
testLiterals = testLiteralsExpr == testLiteralsResult

testPureExpr = pureLiteral testFormula
testPureResult = Just $ Pos 1
testPure = testPureExpr == testPureResult

testUnitExpr = unitLiteral testFormula
testUnitResult = Just $ Neg 3
testUnit = testUnitExpr == testUnitResult

testSatExpr = sat' testFormula
testSatResult = Just [Pos 1, Neg 2, Neg 3]
testSat = testSatExpr == testSatResult

tests = [testLiterals, testPure, testUnit, testSat]
