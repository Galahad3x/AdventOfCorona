import Data.List

main = do
    line <- getLine
    putStrLn $ encryptedMessage $ intercalate "" (words line)

encryptedMessage :: String -> String
encryptedMessage mess = encryption (csqrt (length mess)) (longmess mess (csqrt (length mess)))

encryption :: Int -> String -> String
encryption col mess = if (length mess) == (col*col) then encrip mess col True else encrip mess col False

longmess :: String -> Int -> String
longmess mesg con = if (length mesg) == (con*(con-1)) || (length mesg) == (con*con) then mesg else longmess (mesg ++ [' ']) con

encrip :: String -> Int -> Bool -> String
encrip mess col roweqcol = if roweqcol == True then encr2 0 0 col mess else encr 0 0 col mess

encr :: Int -> Int -> Int -> String -> String
encr rc cc col mess
    | rc == 0 = if cc == col then [] else (mess!!(cc)):(encr (rc+1) cc col mess)
    | rc /= 0 && rc < (col-1) = (mess!!(rc*col + cc)):(encr (rc+1) cc col mess)
    | rc == (col-1) = if cc == (col-1) then [] else (' ':(mess!!(cc+1)):(encr 1 (cc+1) col mess))
    | otherwise = []

encr2 :: Int -> Int -> Int -> String -> String
encr2 rc cc col mess
    | rc == 0 = if cc == col then [] else (mess!!(cc)):(encr2 (rc+1) cc col mess)
    | rc /= 0 && rc < (col) = (mess!!(rc*col + cc)):(encr2 (rc+1) cc col mess)
    | rc == (col) = if cc == (col-1) then [] else (' ':(mess!!(cc+1)):(encr2 1 (cc+1) col mess))
    | otherwise = []

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

csqrt :: Int -> Int
csqrt = ceiling . sqrt . fromIntegral
