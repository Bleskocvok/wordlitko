#!/usr/bin/env runghc


module Main where


import System.Environment ( getArgs, getProgName )
import System.Exit
import Control.Monad ( forM_, (<$!>) )
import Data.Functor ( (<&>) )
import Data.Char ( isLetter, toLower )
import Data.List ( group, sort, sortBy, sortOn, nub )
import Data.Foldable ( foldr' )
import Data.Array as A ( Array, elems, (!), listArray )


data Color = Green | Yellow | Gray
           deriving ( Show, Eq )

data Clue = Clue !Color !Int !Char !Bool
          deriving ( Show, Eq )


getC :: Clue -> Char
getC (Clue _ _ c _) = c


-- EFFICIENT DATA TYPE FOR REPRESENTING A 5-LETTER WORD
type Word5 = Array Int Char

toWord :: String -> Word5
toWord = A.listArray (0, 4)

fromWord :: Word5 -> String
fromWord = A.elems

present :: Char -> Word5 -> Bool
present = elem

isAt :: Int -> Char -> Word5 -> Bool
isAt i c = (c ==) . (A.! i)


main :: IO ()
main = do
    args <- getArgs
    case args of
        (rules : dataFile : _) -> solve rules dataFile
        _ -> do prog <- getProgName
                putStrLn $ "usage: " ++ prog ++ " CLUES DATABASE_PATH"
                exitFailure


solve :: String -> String -> IO ()
solve input database = do
    words <- parseData <$!> getLines database
    let rules = nub $ parseClues input
        filtered = applyClues rules words
        every = if len > 50 then len `div` 50 else 1
            where len = length filtered
        chosen = everyNth every filtered
        sorted = fromWord <$!> orderBest chosen filtered
    forM_ sorted putStrLn


parseClues :: String -> [Clue]
parseClues = assignElsewhere . parseClues' 0
    where
        parseClues' i str = case str of
            ('.'     : xs) -> parseClues' (i + 1) xs
            (' '     : xs) -> parseClues' i xs
            ('\n'    : xs) -> parseClues' i xs
            ('\r'    : xs) -> parseClues' i xs
            ('!' : c : xs) -> Clue Gray   i (toLower c) False : parseClues' i xs
            ('^' : c : xs) -> Clue Yellow i (toLower c) False : parseClues' i xs
            (      c : xs) -> Clue Green  i (toLower c) False : parseClues' i xs
            _              -> []


parseData :: [String] -> [Word5]
parseData = sortUnique . (toWord <$!>) . map (map toLower) . filterLen 5 . validate


getLines :: FilePath -> IO [String]
getLines file = readFile file <&> lines


validate :: [String] -> [String]
validate = map $ filter isLetter


filterLen :: Int -> [String] -> [String]
filterLen l = filter ((l ==) . length)


sortUnique :: [Word5] -> [Word5]
sortUnique = (head `map`) . group . sort


getClues :: Word5 -> Word5 -> [Clue]
getClues guess chosen = aew $! zipWith3 oneChar [0 ..] (fromWord guess)
                                                      (fromWord chosen)
    where
        aew = assignElsewhere
        oneChar i g c
            | g == c = Clue Green i c False
            | g `present` chosen = Clue Yellow i g False
            | otherwise = Clue Gray i g False

        -- TODO: account for repeated letters in chosen
        -- counts = (\c -> (c, length $ filter (c ==) lst)) `map` lst
        --     where lst = fromWord chosen


median :: (Fractional b, Integral a) => [a] -> b
median [] = 1
median [x] = fromIntegral x
median lst = fromIntegral $ ceiling $ fromIntegral (atLift2 (+) (mid - 1) lst) / 2
    where
        mid = length lst `div` 2
        atLift2 _ _ []           = error "empty list"
        atLift2 f 0 (x : y : xs) = f x y
        atLift2 f i (_     : xs) = atLift2 f (i - 1) xs


evaluate :: Fractional a => [Word5] -> Word5 -> a
evaluate wrds a = mean
    where
        remain ges cho = applyClues (getClues ges cho) wrds
        simulate wrds ges = remain ges `map` wrds
        getMean = median . sort . (length `map`)
        mean = getMean $ simulate wrds a


everyNth _ [] = []
everyNth n (x : xs) = x : everyNth n rest
    where rest = drop (n - 1) xs


orderBest :: [Word5] -> [Word5] -> [Word5]
orderBest chosen wrds = ((fst `map`) . sortOn snd) (zip wrds vals)
    where
        vals = evaluate chosen `map` wrds

assignElsewhere :: [Clue] -> [Clue]
assignElsewhere clues = assign `map` clues
    where
        assign (Clue Gray i c ew) = Clue Gray i c (elsewhere c clues)
        assign clue = clue
        elsewhere c = any yellOrGreen . filter ((c ==) . getC)
        yellOrGreen (Clue col _ _ _) = case col of
                                    Yellow -> True
                                    Green  -> True
                                    _      -> False

-- format:
-- .A.^N.


applyClues :: [Clue] -> [Word5] -> [Word5]
applyClues rls lst = foldr' (\r acc -> filter (accept rls r) acc) lst rls
-- alternate implementation that feels like it should run faster, but doesn't
-- applyClues rls = filter (\w -> all (\r -> accept rls r w) rls)


accept :: [Clue] -> Clue -> Word5 -> Bool
accept rls (Clue col i c elsewhere) = case col of
    Green  -> isAt i c
    Yellow -> \str -> not (isAt i c str) && present c str
    Gray   ->
        if elsewhere
        then not . isAt i c
        else not . present c
