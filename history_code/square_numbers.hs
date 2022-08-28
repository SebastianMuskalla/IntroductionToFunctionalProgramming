numbers = [1..]
square = \ x -> x*x
squareNumbers = map square numbers
firstSquares n = take n squareNumbers

main =
    do
        inputString <- getLine
        print $ firstSquares (read inputString :: Int)
