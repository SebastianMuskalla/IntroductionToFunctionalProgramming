--
-- Introduction to functional programming in Haskell
--
-- 3 Lists
--

-- if a is a type, then [a] is the corresponding list type

emptyIntList :: [Integer]
emptyIntList = []

singletonIntList :: [Integer]
singletonIntList = [1]

someIntList :: [Integer]
someIntList = [1,3,3,7]

someIntList' :: [Integer]
someIntList' = [1,3,7,3]

b1 = someIntList == someIntList'

b2 = null emptyIntList

b3 = null someIntList

-- decomposition
i1 = head someIntList

i2 = head emptyIntList

il1 = tail someIntList

il2 = tail emptyIntList

-- cons (:) operator
il3 = 1 : [3,3,7]

-- concatenation (++) operation
il4 = [1,3] ++ [3,7]

il5 = [1] ++ [3,3,7]

-- Inductive definition of finite lists
il6 = 1 : 3 : 3 : 7 : []

-- get the fifth element (zero indexed)
i5 = [0,1,2,3,4,5,6,7,8,9,10] !! 5

-- fancy notation
il7 = [1..10]

il8 = reverse il7

-- lists of lists
ill :: [[Integer]]
ill = [[],[0],[1,2],[3,4,5]]

il9 = head ill

il10 = concat ill

-- String is a type synonom for Char-List (type String = [Char])
-- both notations can be used
string1 = "Hallo"
string2 = ['H','a','l','l','o']
string3 = 'H' : 'a' : 'l' : 'l' : 'o' : []

-- lexicographic ordering
b4 = ("Anna" < "Bob") && ("Bob" < "Charlie")

-- reverse function
-- if-then-else
reverse' :: [Integer] -> [Integer]
reverse' list =
    if null list
    then []
    else reverse' (tail list) ++ [head list]

-- guards
reverse'' list
    | null list = []
    | otherwise = reverse'' (tail list) ++ [head list]

-- pattern matching
-- the pattern (h:t) patern decomposes lists into its first element (head, h) and the rest of the list (tail, t)
reverse''' [] = []
reverse''' (h:t) = reverse''' t ++ h

-- more pattern matching
addPairwise :: [Integer] -> [Integer]
addPairwise [] = []
addPairwise [x] = error "Input has even length"
addPairwise (x:y:t) = (x+y) : addPairwise t

isSorted :: [Integer] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:t) = x < y && isSorted (y:t)

-- is this nicer?
isSorted' list = (null list) || (null (tail list)) || ( (head list < (head (tail list))) && isSorted (tail list) )

length' :: [Integer] -> Int
length' [] = 0
length' (_:t) = 1 + length' t

elem' :: Integer -> [Integer] -> Bool
elem' _ [] = False
elem' x (h:t) = (x == h) || elem' x t
-- elem' x (x:t) = ... -- this type of pattern does not work

removeDuplicates [] = []
removeDuplicates (x:t) =
    if x `elem` (removeDuplicates t) then removeDuplicates t else x : (removeDuplicates t)

-- version that keeps the order
removeDuplicates' :: [Integer] -> [Integer]
removeDuplicates' = helper []
-- equivalent to
-- removeDuplicates' list = helper [] list
    where
        helper seen [] = seen
        helper seen (x:xs)
            | x `elem` seen = helper seen xs
            | otherwise     = helper (seen ++ [x]) xs
-- check what happens if we remove the type signature

quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (x:t) = (quickSort smaller) ++ [x] ++ (quickSort greater)
    where
        smaller = filterSmallerThan x t
        greater = filterGreaterThan x t
        filterSmallerThan :: Integer -> [Integer] -> [Integer]
        filterSmallerThan k [] = []
        filterSmallerThan k (h:rest) = if h <= k then h : (filterSmallerThan k rest) else filterSmallerThan k rest
        filterGreaterThan k [] = []
        filterGreaterThan k (h:rest) = if h >= k then h : filter_rest  else filter_rest
            where
                filter_rest = filterGreaterThan k rest

take' :: Int -> [Integer] -> [Integer]
take' 0 _     = []
take' _ []    = []
take' n (h:t) = h : (take' (n-1) t)

drop' :: Int -> [Integer] -> [Integer]
drop' 0 list  = list
drop' _ []    = []
drop' n (h:t) = drop' (n-1) t

mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort firstHalf) (mergeSort secondHalf)
    where
        len = length list
        firstHalf  = take (div len 2) list
        secondHalf = drop (div len 2) list
        merge [] slist = slist
        merge slist [] = slist
        merge (x:xs) (y:ys)
            | x <= y    = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys

-- More pattern matching!
not' :: Bool -> Bool
not' True = False
not' False = True

-- selectors for 3-tuples
first :: (Integer,Integer,Integer) -> Integer
first  (a,b,c) = a
second (_,b,_) = b
third  (_,_,c) = c

-- pattern matching inside the function body using case ... of
digitToInteger :: Char -> Integer
digitToInteger c =
    case c of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        _   -> error "provide a digit"

-- alternative implementation
digitToInteger' :: Char -> Int
digitToInteger' c =
    if not (c `elem` ['0'..'9'])
    then error "provide a digit"
    else fromEnum c - fromEnum '0'
-- check what happens if you remove
--   signature
--   backticks around elem
--   both


-- Compare: reverse' "Hallo" vs. reverse'' "Hallo"
-- Compare: mergesort "Sebastian" vs. quicksort Sebastian
-- what happens?

-- later: nicer implementation of quicksort using higher order functions
quickSort' [] = []
quickSort' (x:t) = (quickSort' $ filter (<=x) t) ++ [x] ++ (quickSort' $ filter (>= x) t)

-- one can implement of quicksort using list comprehension (which relies on the list monad (later))
quickSort'' [] = []
quickSort'' (x:t) = quickSort'' [ y | y <- t, y <= x ] ++ [x] ++ quickSort'' [ y | y <- t, y >= x ]
