{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Applicative hiding (Const)
import Control.Monad.State.Lazy

--
-- Introduction to functional programming in Haskell
--
-- Exercise sheet 2
--
-- Solution
--


--
-- Exercise 1
--

data Exception a =
        Exception String
    |   Result a
    deriving (Show)

-- make it a monad
instance Functor Exception where
    fmap _ (Exception s) = Exception s
    fmap f (Result x) = Result $ f x

instance Applicative Exception where
    pure x = Result x
    (Exception s) <*> _ = Exception s
    _ <*> (Exception s) = Exception s
    (Result f) <*> (Result x) = Result $ f x

instance Monad Exception where
    (Exception s) >>= _ = Exception s
    (Result x) >>= f = f x


--
-- Exercise 2
--

data Arith = Const Integer | Div Arith Arith
    deriving (Eq,Show)

eval :: Arith -> Integer
eval (Const n) = n
eval (Div l r) = (eval l) `div` (eval r)

good :: Arith
good = Div (Div (Const 1932) (Const 2)) (Const 23)
bad :: Arith
bad = Div (Const 1) (Const 0)

-- trivial monad
newtype Burrito a = Burrito a
    deriving (Eq,Show,Ord)

instance Functor Burrito where
    fmap f (Burrito x) = Burrito (f x)

instance Applicative Burrito where
    pure x = Burrito x
    (Burrito f) <*> (Burrito x) = Burrito (f x)

instance Monad Burrito where
    (Burrito x) >>= f = f x

-- using the trivial monad
evalWithTrivial :: Arith -> Burrito Integer
evalWithTrivial (Const n) = pure n
evalWithTrivial (Div l r) =
    do
        n <- evalWithTrivial l
        m <- evalWithTrivial r
        return (n `div ` m)


-- using the Exception monad
evalWithException :: Arith -> Exception Integer
evalWithException (Const n) = pure n
evalWithException (Div l r) =
    do
        n <- evalWithException l
        m <- evalWithException r
        if m == 0 then
            Exception "Division by 0"
        else
            return (n `div ` m)


-- using the State Int monad
evalWithState' :: Arith -> State Int Integer
evalWithState' (Const n) = pure n
evalWithState' (Div l r) =
    do
        n <- evalWithState' l
        m <- evalWithState' r
        x <- get
        put (x+1)
        return (n `div ` m)

evalWithState expr = runState (evalWithState' expr) 0

-- using the IO monad
evalWithIO :: Arith -> IO Integer
evalWithIO (Const n) = pure n
evalWithIO (Div l r) =
    do
        n <- evalWithIO l
        m <- evalWithIO r
        let result = n `div ` m
        putStrLn $ show l ++ " / " ++ show r ++ " = " ++ show result
        return result


--
-- Exercise 3
--

data Expression =
        Constant Integer
    |   Add Expression Expression
    |   Subtract Expression Expression
    |   Multiply Expression Expression
    |   Divide Expression Expression

-- ((1 + 2) + (3 * (4 - (2 / 3))))
testexpr = Add
            (Constant 2)
            (Multiply
                (Constant 3)
                (Subtract (Constant 4) (Divide (Constant 2) (Constant 3))))

-- Show instance for Expression
instance Show Expression where
    show (Constant n)   = show n
    show (Add l r)      = "(" ++ show l ++ " + " ++ show r ++ ")"
    show (Subtract l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
    show (Multiply l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
    show (Divide l r)   = "(" ++ show l ++ " / " ++ show r ++ ")"

data BExpression = T Term | A BExpression Term | S BExpression Term -- recursion, addition, subtraction
data Term        = F Factor | M Term Factor | D Term Factor  -- recursion, multiplication, division
data Factor      = C Integer | B BExpression -- expression in brackets

-- 1 + 2 + 3 * (4 - 2 / 3)
testbexpr =
    A (A (T $ F $ C 1) (F $ C 2))
      (M (F $ C 3) (B $ S (T $ F $ C 4) (D (F $ C 2) (C 3))))

-- Show instance for BExpression
instance Show BExpression where
    show (T term) = show term
    show (A l r) = show l ++ " + " ++ show r
    show (S l r) = show l ++ " - " ++ show r

instance Show Term where
    show (F factor) = show factor
    show (M l r) = show l ++ " * " ++ show r
    show (D l r) = show l ++ " / " ++ show r

instance Show Factor where
    show (C n) = show n
    show (B expr) = "(" ++ show expr ++ ")"


-- helper functions
convertFactor (C n) = (Constant n)
convertFactor (B bexpr) = convert bexpr

convertTerm (F f) = convertFactor f
convertTerm (M f1 f2) = Multiply (convertTerm f1) (convertFactor f2)
convertTerm (D f1 f2) = Divide  (convertTerm f1) (convertFactor f2)

-- the actual convert function
convert :: BExpression -> Expression
convert (T t) = convertTerm t
convert (A t1 t2) = Add (convert t1) (convertTerm t2)
convert (S t1 t2) = Subtract (convert t1) (convertTerm t2)


-- taken from the lecture material
newtype Parser a = Parser (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (Parser p) = p

parse :: Parser a -> String -> a
parse p s =
    case filter (\(x,r) -> null r) (apply p s) of
        []       -> error "no complete parse"
        [(x,"")] -> x
        _        -> error "ambiguous complete parse"

charParser :: Parser Char
charParser = Parser
  (
    \x  -> case x of
        ""    -> []
        (h:t) -> [(h,t)]
  )

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser (\ s -> map (\ (x,y) -> (f x,y)) (apply p s))

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> [(x,s)])
    (<*>) :: (Parser (a -> b)) -> (Parser a) -> (Parser b)
    (<*>) = ap

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' mfun mval =
    do
        f <- mfun
        x <- mval
        return $ f x

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser
        (
          \ s ->
            do
                (x,remainder) <- apply p s
                (y,remainder2) <- apply (f x) remainder
                return (y,remainder2)
        )

instance MonadPlus Parser where
    mzero :: Parser a
    mzero = Parser (\s -> [])
    mplus :: (Parser a) -> (Parser a) -> (Parser a)
    mplus p1 p2 = Parser (\s -> (apply p1 s) ++ (apply p2 s))

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

stringParser :: Parser String
stringParser = pure ""
               <|>
               do
                    c <- charParser
                    string <- stringParser
                    return (c:string)

star :: Parser a -> Parser [a]
star p =
    return []
    <|>
    plus p

plus :: Parser a -> Parser [a]
plus p =
    do
        x <- p
        xs <- star p
        return (x:xs)

naturalParser' :: Parser Integer
naturalParser' =
    do
        string <- stringParser
        guard (not $ null string)
        guard (onlyNumbers string)
        return (read string)
    where
        onlyNumbers = foldl (\x y -> x && y `elem` ['0'..'9']) True

convertNumber :: String -> Integer
convertNumber []     = error "empty number"
convertNumber string = convertNumber' $ reverse string
    where
        convertNumber' :: String -> Integer
        convertNumber' []    = 0
        convertNumber' (c:t) =
            let
                enum = fromEnum c - fromEnum '0'
            in
                if enum < 0 || enum > 9
                then error $ "Illegal Character: " ++ [c]
                else  10 * convertNumber' t + toInteger enum

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
            then []
            else [(convertNumber numString, restString)]
    )

largestPrefixSatisfying :: ([a] -> Bool) -> [a] -> [a]
largestPrefixSatisfying predicate list = largestPrefixSatisfying' predicate list []
    where
        largestPrefixSatisfying' :: ([a] -> Bool) -> [a] -> [a] -> [a]
        largestPrefixSatisfying' _ [] prefix = prefix
        largestPrefixSatisfying' p (h:t) prefix
            | p (prefix ++ [h]) = largestPrefixSatisfying' p t (prefix ++ [h])
            | otherwise         = prefix

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

tokens :: String -> Parser ()
tokens ""    = return ()
tokens (h:t) = token h >> tokens t

integerParser :: Parser Integer
integerParser =
        -- try to parse a positive number
        naturalParser
        <|>
        -- try to parse '-', then parse a positive number, multiply the result with -1
        (token '-' >> (fmap (* (-1)) naturalParser))


-- now we can go on!

-- modified grammar
-- bexpr  ::= term (addop term)*
-- term   ::= factor (multop factor)*
-- factor ::= integer | '(' bexpr ')'
-- addop  ::= '+' | -'
-- multop ::= '*' | '/'

-- recall:
-- data BExpression = T Term | A Term BExpression | S Term BExpression
-- data Term        = F Factor | M Factor Term | D Factor Term
-- data Factor      = C Integer | B BExpression

-- helper data type
data Operation = OpPlus | OpMinus | OpTimes | OpDivide

-- parser that discards whitespace exhaustively
whitespaceParser :: Parser ()
whitespaceParser = Parser ( \ string -> [((),discardWhitespacePrefix string)])

discardWhitespacePrefix :: String -> String
discardWhitespacePrefix []      = []
discardWhitespacePrefix (' ':t) = discardWhitespacePrefix t
discardWhitespacePrefix string = string

bExpressionParser :: Parser BExpression
bExpressionParser =
    do
        whitespaceParser
        term <- termParser
        tuples <- star tupleParser
        return $ combine term tuples
    where
        tupleParser :: Parser (Operation, Term)
        tupleParser =
            do
                whitespaceParser
                char <- token '+'
                term <- termParser
                return (OpPlus, term)
            <|>
            do
                whitespaceParser
                char <- token '-'
                term <- termParser
                return (OpMinus, term)
        combine :: Term -> [(Operation, Term)] -> BExpression
        combine term tuples = combine' term (reverse tuples)
        combine' :: Term -> [(Operation, Term)] -> BExpression
        combine' term [] = T term
        combine' term ((op,term2):t) =
            case op of
                OpPlus  -> A (combine' term t) term2
                OpMinus -> S (combine' term t) term2
                _       -> error "invalid operator"

termParser :: Parser Term
termParser =
    do
        whitespaceParser
        factor <- factorParser
        tuples <- star tupleParser
        return $ combine factor tuples
    where
        tupleParser :: Parser (Operation, Factor)
        tupleParser =
            do
                whitespaceParser
                char <- token '*'
                factor <- factorParser
                return (OpTimes, factor)
            <|>
            do
                whitespaceParser
                char <- token '/'
                factor <- factorParser
                return (OpDivide, factor)
        combine :: Factor -> [(Operation, Factor)] -> Term
        combine factor tuples = combine' factor (reverse tuples)
        combine' factor [] = F factor
        combine' factor ((op,factor2):t) =
            case op of
                OpTimes  -> M (combine' factor t) factor2
                OpDivide -> D (combine' factor t) factor2
                _        -> error "invalid operator"

factorParser :: Parser Factor
factorParser =
    do
        whitespaceParser
        n <- integerParser
        return $ C n
    <|>
    do
        whitespaceParser
        token '('
        whitespaceParser
        bexpr <- bExpressionParser
        whitespaceParser
        token ')'
        return $ B bexpr


-- some test cases
testStrings = ["1 + 2 + 3", "1 + 2 * 3", "1 + 2 + 3 * (4 - 2 / 3)"]
expectedResults = ["((1 + 2) + 3)", "(1 + (2 * 3))", "((1 + 2) + (3 * (4 - (2 / 3))))"]
testCase0 = show $ convert $ parse bExpressionParser (testStrings !! 0)
test0 = testCase0 == (expectedResults !! 0)
testCase1 = show $ convert $ parse bExpressionParser (testStrings !! 1)
test1 = testCase1 == (expectedResults !! 1)
testCase2 = show $ convert $ parse bExpressionParser (testStrings !! 2)
test2 = testCase2 == (expectedResults !! 2)
tests = [test0, test1, test2]
