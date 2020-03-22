import Data.List

main = do
        line <- getLine
        let inputArray = map read (words line) :: [Int]
        putStrLn $ show $ findLongest inputArray
        putStrLn $ show $ length $ createLongest inputArray

findLongest :: [Int] -> Int
findLongest inputArr = findLongest' inputArr 1 1

findLongest' :: [Int] -> Int -> Int -> Int
findLongest' [] curr max = max
findLongest' (x:[]) curr max = max
findLongest' (x:y:xs) curr max = if x < y && (even x) /= (even y) 
                                 then (if curr == max then (findLongest' (y:xs) (curr+1) (curr+1)) else (findLongest' (y:xs) (curr+1) max))
                                 else (findLongest' (y:xs) 1 max)

createLongest :: [Int] -> [Int]
createLongest inputArr = createLongest' $ sort inputArr

createLongest' :: [Int] -> [Int]
createLongest' [] = []
createLongest' [x] = [x]
createLongest' (x:y:xs) = if x < y && (even x) /= (even y) then x:(createLongest' (y:xs)) else (createLongest' (y:xs))
