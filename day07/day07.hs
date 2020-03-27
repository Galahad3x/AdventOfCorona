main = do
    -- putStrLn $ show $ calculateCycleLength 1 5 8
    let start = [34]
    mainloop start 

mainloop arr = if (snd $ findCycle arr) == [] then do mainloop (arr++[(calculateNext 91 999 (last arr))]) else do putStrLn $ show $ length $ snd $ findCycle arr

calculateCycleLength :: Int -> Int -> Int -> Int
calculateCycleLength x y firstN = 0

calculateNext :: Int -> Int -> Int -> Int
calculateNext x y n = if even n then n `div` 2 + x else 3*n + y

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)

findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys

