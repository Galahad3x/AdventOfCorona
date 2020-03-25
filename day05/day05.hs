

main = do
    line <- getLine
    let alphas = map read (words line)
    let vaccines = calculateVaccines alphas
    let newvac = testVaccines vaccines alphas
    putStrLn $ show $ newvac
    let vaccinestext = "[" ++ (printvaccines newvac)
    putStrLn $ show $ vaccinestext

printvaccines :: [Int] -> String
printvaccines [] = "]"
printvaccines (x:xs) = (show x) ++ ", " ++ (printvaccines xs)

testVaccines :: [Int] -> [Int] -> [Int]
testVaccines = testVacc 0

testVacc :: Int -> [Int] -> [Int] -> [Int]
testVacc cont vacc alpha = if cont == 1000 then vacc else testVacc (cont+1) (modVacc vacc alpha) alpha

modVacc :: [Int] -> [Int] -> [Int]
modVacc = modVacc' 0

modVacc' :: Int -> [Int] -> [Int] -> [Int]
modVacc' _ vac [] = vac
modVacc' _ vac [x] = vac
modVacc' pos vac alp@(ax:ay:axs)
    | ax > ay = if ((vac!!pos) == (vac!!(pos+1))) then modVacc' (pos+1) (set (pos) ((vac!!(pos))+1) vac) (ay:axs) else modVacc' (pos+1) vac (ay:axs)
    | otherwise = modVacc' (pos+1) vac (ay:axs) 

calculateVaccines :: [Int] -> [Int]
calculateVaccines [] = []
calculateVaccines [x] = [1]
calculateVaccines lis@(x:y:xs) = calculateVacc' (0::Int) lis (longList::[Int])
    where longList = take 1000 [1,1..]

calculateVacc' :: Int -> [Int] -> [Int] -> [Int]
calculateVacc' _ [] vacc = vacc
calculateVacc' _ [x] vacc = vacc
calculateVacc' pos (x:y:xs) vacc
    | y > x = calculateVacc' (pos+1) (y:xs) (set (pos+1) ((vacc!!(pos))+1) vacc) 
    | y < x = if (vacc!!pos) > (vacc!!(pos+1)) then calculateVacc' (pos+1) (y:xs) vacc else calculateVacc' (pos+1) (y:xs) (set (pos) ((vacc!!pos)+1) vacc)
    | otherwise = calculateVacc' (pos+1) (y:xs) vacc

set' :: Int -> Int -> a -> [a] -> [a]
set' _ _ _ [] = []
set' ind curr new arr@(x:xs)
    | curr > ind = arr
    | ind == curr = new:(set' ind (curr+1) new xs)
    | otherwise = x:(set' ind (curr+1) new xs)

set :: Int -> a -> [a] -> [a]
set ind new arr = (set' ind 0 new arr)
