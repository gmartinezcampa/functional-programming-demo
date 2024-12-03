-- printAMessage function
printAMessage :: Show a => a -> IO ()
printAMessage x = putStrLn (show x)

-- division function
division :: Double -> Double -> Maybe Double
division x y
    | x == y    = Nothing
    | otherwise = Just (x / y)

-- factorial function
factorial :: Int -> Int
factorial n
    | n == 0 || n == 1 = 1
    | otherwise        = product [1..n]

-- factList function
factList :: Int -> [Int]
factList n = [factorial i | i <- [1..n]]

-- merge function
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Main function to test all the functions
main :: IO ()
main = do
    -- Testing printAMessage
    printAMessage "Hello World!"

    -- Testing division
    let z = division 1 2
    let w = division 1 1
    let g = division 6 2
    print $ "z: " ++ show z ++ ", w: " ++ show w ++ ", g: " ++ show g

    -- Testing factorial
    let a = factorial 1
    let b = factorial 7
    print $ "a: " ++ show a ++ ", b: " ++ show b

    -- Testing factList
    let testList = factList 5
    print $ "testList: " ++ show testList

    -- Testing merge
    let merged = merge [1, 3, 6] [2, 4, 5, 6, 7]
    print $ "merge: " ++ show merged
