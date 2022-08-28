--
-- Introduction to functional programming in Haskell
--
-- 12: Parsing using monads
--

-- language pragma that allows us to specify the signature for functions
-- in the definitions of instances of type classes
-- for the sake of clarity
{-# LANGUAGE  InstanceSigs #-}

import Control.Monad
import Control.Applicative

-- Idea:
-- A parser for objects of type a
-- takes a string
-- and returns a list of possible parse results,
-- each result consisting of a parsed object, and the remaining string that has not been processed.
-- If parsing fails, we return the empty list.
newtype Parser a = Parser (String -> [(a,String)])

-- apply a parser
apply :: Parser a -> String -> [(a,String)]
apply (Parser p) = p

-- a parse is only successful if it is complete (there is no remaining string) and it is unambiguous
parse :: Parser a -> String -> a
parse p s =
    case filter (\(x,r) -> null r) (apply p s) of
        []       -> error "no complete parse"
        [(x,"")] -> x
        _        -> error "ambiguous complete parse"

-- basic example:
-- parse the first Char of a String
charParser :: Parser Char
charParser = Parser
  (
    \x  -> case x of
        ""    -> []
        (h:t) -> [(h,t)]
  )


-- let us turn Parser into a Monad

instance Functor Parser where
    -- apply the function to the parsed object
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser (\ s -> map (\ (x,y) -> (f x,y)) (apply p s))

instance Applicative Parser where
    -- parser that doesn't actually modify the string
    pure :: a -> Parser a
    pure x = Parser (\s -> [(x,s)])
    (<*>) :: (Parser (a -> b)) -> (Parser a) -> (Parser b)
    (<*>) = ap

-- ap: implementation of <*> for any monad, available from Control.Monad
ap' :: Monad m => m (a -> b) -> m a -> m b
ap' mfun mval =
    do
        f <- mfun
        x <- mval
        return $ f x

-- this implements <*> using >>=, and saves us the effort of doing it by hand


instance Monad Parser where
    -- execute the first Parser, evaluate the function, then executed the returned Parser
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser
        (
          \ s ->
            do -- this is using the list monad
                (x,remainder) <- apply p s
                (y,remainder2) <- apply (f x) remainder
                return (y,remainder2)
        )

-- it will be helpful to make Parser also a MonadPlus and an Alternative

-- MonadPlus: Monads that also support choice and failure.
-- class Monad m => MonadPlus m where
--    mzero :: m a                -- failure
--    mplus :: m a -> m a -> m a  -- choice

instance MonadPlus Parser where
    -- parser that always fails
    mzero :: Parser a
    mzero = Parser (\s -> [])
    -- parser that tries two possibilities
    mplus :: (Parser a) -> (Parser a) -> (Parser a)
    mplus p1 p2 = Parser (\s -> (apply p1 s) ++ (apply p2 s))

-- Alternative: Applicatives that support combining computations
-- class Applicative m => Alternative m where
--     empty :: m a                -- empty computation
--     (<|>) :: m a -> m a -> m a  -- combine two computations

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

-- parse a String of arbitrary length
-- i.e. when we apply the parser, we will get every possible combination of parsed prefix and remaining string
stringParser :: Parser String
stringParser = pure "" -- length 0
               <|>
               do
                    c <- charParser
                    string <- stringParser
                    return (c:string)

-- try
-- apply stringParser "test"

-- we can also define the following more general functions

-- turns a Parser for a into a Parser of zero or more occurrences of a
star :: Parser a -> Parser [a]
star p =
    return []
    <|>
    plus p

-- turns a Parser for a into a Parser of one or more occurrences of a
plus :: Parser a -> Parser [a] -- many
plus p =
    do
        x <- p
        xs <- star p
        return (x:xs)

-- using star, we can also define stringParser' in the following way
stringParser' :: Parser String
stringParser' = star charParser

-- we can use StringParser to define a parser for natural Numbers
naturalParser' :: Parser Integer
naturalParser' =
    do
        string <- stringParser
        guard (not $ null string)
        guard (onlyNumbers string)
        return (read string)
    where
        onlyNumbers = foldl (\x y -> x && y `elem` ['0'..'9']) True
        -- explicit definition without foldl:
        -- onlyNumbers "" = True
        -- onlyNumbers (h:t) = h `elem` ['0'..'9'] && onlyNumbers t

-- where guard is the following function that aborts a computation when the condition is not satisfied

-- guard :: Alternative m -> Bool -> m ()
-- guard False = mzero
-- guard True  = return ()

convertNumber :: String -> Integer
convertNumber []     = error "empty number"
convertNumber string = convertNumber' $ reverse string
    where
        convertNumber' []      = 0
        convertNumber' ('0':t) = 10 * convertNumber' t
        convertNumber' ('1':t) = 10 * convertNumber' t + 1
        convertNumber' ('2':t) = 10 * convertNumber' t + 2
        convertNumber' ('3':t) = 10 * convertNumber' t + 3
        convertNumber' ('4':t) = 10 * convertNumber' t + 4
        convertNumber' ('5':t) = 10 * convertNumber' t + 5
        convertNumber' ('6':t) = 10 * convertNumber' t + 6
        convertNumber' ('7':t) = 10 * convertNumber' t + 7
        convertNumber' ('8':t) = 10 * convertNumber' t + 8
        convertNumber' ('9':t) = 10 * convertNumber' t + 9
        convertNumber' (c:t)   = error $ "Illegal Character: " ++ [c]
        -- better implementation
        -- convertNumber' (c:t) =
        --     let
        --         enum = fromEnum c - fromEnum '0'
        --     in
        --         if enum < 0 || enum > 9
        --         then error $ "Illegal Character: " ++ [c]
        --         else  10 * convertNumber' t + toInteger enum

-- by the way:
-- with the guard function, and the list monad, we can understand list comprehension!

li1 = [ x | x <- [1..100], x*x <= 20]
-- is equivalent to
li2 =
    do
        x <- [1..100]
        guard (x*x <= 20)
        return x

-- our natural parser is highly inefficient
-- it will try out every prefix of the string,
-- even though we know that if one prefix contains an illegal character, then any other prefix will

-- better version that always parses the largest prefix just containing numbers
naturalParser :: Parser Integer
naturalParser =
    Parser
    (
        \ s ->
        let
            onlyNumbers = foldl (\x y -> x && y `elem` ['0'..'9']) True
            numString = largestPrefixSatisfying onlyNumbers s
            restString = drop (length numString) s
        in
            if null numString
            then [] -- parsing has failed
            else [(convertNumber numString, restString)]
    )

-- function for finding the largest prefix of a list that satisfies a prefix
largestPrefixSatisfying :: ([a] -> Bool) -> [a] -> [a]
largestPrefixSatisfying predicate list = largestPrefixSatisfying' predicate list []
    where
        -- version that tracks the prefix
        largestPrefixSatisfying' :: ([a] -> Bool) -> [a] -> [a] -> [a]
        largestPrefixSatisfying' _ [] prefix = prefix
        largestPrefixSatisfying' p (h:t) prefix
            | p (prefix ++ [h]) = largestPrefixSatisfying' p t (prefix ++ [h])
            | otherwise         = prefix

-- parse a specific token
token :: Char -> Parser ()
token c = Parser
    (
        \x -> case x of
            "" -> []
            (h:t) ->
                if h == c
                then
                    [((),t)]
                else
                    []
    )

-- parse a list of tokens
tokens :: String -> Parser ()
tokens ""    = return ()
tokens (h:t) = token h >> tokens t

-- >> is a variant of >>= that discards the result of the command on the left-hand side
--   mx >> my
-- its signature is
--   (>>) :: Monad m => m a -> m b -> m b

-- parser for Integer
integerParser :: Parser Integer
integerParser =
        -- try to parse a positive number
        naturalParser
        <|>
        -- try to parse '-', then parse a positive number, multiply the result with -1
        (token '-' >> (fmap (* (-1)) naturalParser))


-- let us parse some more complex objects


-- Example 1: Parsing arithmetic expressions

-- data type for such expressions
data Arith =
        Add   Arith Arith
    |   Mult  Arith Arith
    |   Minus Arith Arith
    |   Number Integer

-- show these expressions using the usual mathematic symbols
instance Show Arith where
    show (Add l r)   = "(" ++ show l ++ " + " ++ show r ++ ")"
    show (Mult l r)  = "(" ++ show l ++ " * " ++ show r ++ ")"
    show (Minus l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
    show (Number n)  = show n

-- evaluation function
eval :: Arith -> Integer
eval (Add   x y)    = (eval x) + (eval y)
eval (Mult  x y)    = (eval x) * (eval y)
eval (Minus x y)    = (eval x) - (eval y)
eval (Number i)     = i

-- some test cases
testexpr = Mult (Add (Number (-100)) (Number 106)) (Number 7)
teststr = show testexpr
testapply = apply arithParser teststr
testparse = parse arithParser teststr

-- let us write parser
arithParser :: Parser Arith
arithParser =
    -- parser for the case Number Integer
    (
        do
            i <- integerParser
            return $ Number i
    )
    <|>
    -- parser for the case Add Arith Arith
    (
        do
            token '('
            x <- arithParser -- :: Arith
            tokens " + "
            y <- arithParser -- :: Arith
            token ')'
            return $ Add x y
    )
    <|>
    -- parser for the case Mult Arith Arith
    (
        do
            token '('
            x <- arithParser -- :: Arith
            tokens " * "
            y <- arithParser -- :: Arith
            token ')'
            return $ Mult x y
    )
    <|>
    -- parser for the case Minus Arith Arith
    (
        do
            token '('
            x <- arithParser -- :: Arith
            tokens " - "
            y <- arithParser -- :: Arith
            token ')'
            return $ Minus x y
    )


-- Example 2: Parsing a context-free grammar

-- consider the grammar
-- S -> X | a S b
-- X -> c X | d X | epsilon

-- data types for the grammar
data S = SX X | SASB A S B
data X = XC C X | XD D X | Empty
data A = A
data B = B
data C = C
data D = D

instance Show A where
    show A = "a"
instance Show B where
    show B = "b"
instance Show C where
    show C = "c"
instance Show D where
    show D = "d"
instance Show X where
    show (XC c x) = show c ++ show x
    show (XD d x) = show d ++ show x
    show (Empty)  = ""
instance Show S where
    show (SX x) = show x
    show (SASB a s b) = show a ++ show s ++ show b

aParser = token 'a' >> return A
bParser = token 'b' >> return B
cParser = token 'c' >> return C
dParser = token 'd' >> return D

xParser =
    do
        cParser
        x <- xParser
        return (XC C x)
    <|>
    do
        dParser
        x <- xParser
        return (XD D x)
    <|>
        return Empty

sParser =
    do
        x <- xParser
        return (SX x)
    <|>
    do
        token 'a'
        s <- sParser
        token 'b'
        return (SASB A s B)


test1 = "aacdcddbb" -- in the language

test2 = "aacdacdbb" -- not in the language (illegal a in the middle)

test3 = "aacccdddbbb" -- not in the language (number of a's and b's doesn't match)

-- try parsing these, e.g.
-- parse sParser test1


-- Example 3: Parsing an expression

-- the grammar from Example 2 generates expressions of the shape
-- a^n (c + d)^* b^n
-- where ^* means zero or more occurrences
-- and ^n means n-fold repetition (for some number n that is the same for both a^n and b^n)

exprParser :: Parser String
exprParser =
    do
        token 'a'
        s <- exprParser
        token 'b'
        return ("a" ++ s ++ "b")
    <|>
        fmap concat innerParser
            where
                innerParser = star ((token 'c' >> return "c") <|> (token 'd' >> return "d"))
