#!/usr/bin/env runghc


module Main where


import Data.Functor ( (<&>) )
import Control.Monad ( forM_, (<$!>) )
import Data.Char ( isLetter, toLower )
import Data.List ( group, sort, sortBy, sortOn )
import System.Environment ( getArgs )
import Data.Foldable ( foldr' )

-- apparently not in Prelude, therefore not portable necessarily
import Data.Array as A ( Array, elems, (!), listArray )


-- CONSTANTS
dataFile :: String
dataFile = "data.txt"


data Rule = Green  !Int !Char
          | Yellow !Int !Char
          | Gray   !Int !Char
          deriving ( Show, Eq )


getC :: Rule -> Char
getC r = case r of
    Green  _ c -> c
    Yellow _ c -> c
    Gray   _ c -> c


-- EFFICIENT DATA TYPE FOR REPRESENTING A 5-LETTER WORD
type Word5 = Array Int Char

toWord :: String -> Word5
toWord = A.listArray (0, 4)

fromWord :: Word5 -> String
fromWord = A.elems

present :: Char -> Word5 -> Bool
present = elem

isAt :: Int -> Char -> Word5 -> Bool
isAt i ch = (ch ==) . (A.! i)



solve :: String -> IO ()
solve input = do
    words <- parseData <$!> getLines dataFile
    let rules = parseRules input
        filtered = applyRules rules words
        sorted = fromWord <$!> orderBest filtered
    forM_ sorted putStrLn


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do putStr "enter clues: "
                 rules <- getLine
                 solve rules
        (rules : _) -> solve rules


parseRules :: String -> [Rule]
parseRules = parseRules' 0
    where
        parseRules' i str = case str of
            ('.'      : xs) -> parseRules' (i + 1) xs
            (' '      : xs) -> parseRules' i xs
            ('\n'     : xs) -> parseRules' i xs
            ('\r'     : xs) -> parseRules' i xs
            ('!' : ch : xs) -> Gray   i (toLower ch) : parseRules' i xs
            ('^' : ch : xs) -> Yellow i (toLower ch) : parseRules' i xs
            (      ch : xs) -> Green  i (toLower ch) : parseRules' i xs
            _               -> []


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


getRules :: Word5 -> Word5 -> [Rule]
getRules guess chosen = zipWith3 oneChar [0 ..] (fromWord guess)
                                                (fromWord chosen)
    where
        oneChar i g c
            | g == c = Green i c
            | g /= c && g `elem` chosen = Yellow i g
            | otherwise = Gray i g


median :: (Fractional b, Integral a) => [a] -> b
median [] = 1
median [x] = fromIntegral x
-- median lst = fromIntegral $ ceiling $ fromIntegral (lst !! (mid - 1) + lst !! mid) / 2
median lst = fromIntegral $ ceiling $ fromIntegral (atLift2 (+) (mid - 1) lst) / 2
    where
        mid = length lst `div` 2
        atLift2 _ _ []           = error "empty list"
        atLift2 f 0 (x : y : xs) = f x y
        atLift2 f i (_     : xs) = atLift2 f (i - 1) xs


evaluate :: Fractional a => [Word5] -> Word5 -> a
evaluate wrds a = am
    where
        remain ges cho = applyRules (getRules ges cho) wrds
        simulate wrds ges = remain ges `map` wrds
        getMean = median . (length `map`)
        am = getMean $ simulate wrds a


orderBest :: [Word5] -> [Word5]
orderBest wrds = ((fst `map`) . sortOn snd) (zip wrds vals)
    where
        vals = evaluate wrds `map` wrds


-- format:
-- .A.^N.


applyRules :: [Rule] -> [Word5] -> [Word5]
applyRules rls lst = foldr' (\r acc -> filter (accept rls r) acc) lst rls


accept :: [Rule] -> Rule -> Word5 -> Bool
accept rls r = case r of
    Green  i ch -> isAt i ch
    Yellow i ch -> \str -> present ch str && not (isAt i ch str)
    Gray   i ch ->
        if elsewhere ch rls
        then not . isAt i ch
        else not . present ch
    where
        elsewhere ch = not . null . filter yellOrGreen . filter ((ch ==) . getC)
        yellOrGreen r = case r of Yellow _ _ -> True
                                  Green  _ _ -> True
                                  _          -> False

