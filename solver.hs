#!/usr/bin/env runghc


module Main where


import Data.Functor ( (<&>) )
import Control.Monad ( forM_ )
import Data.Char ( isLetter, toLower )
import Data.List ( group, sort, sortBy, sortOn )
import System.Environment ( getArgs )


data Rule = Green Int Char
          | Yellow Int Char
          | Gray Char
          deriving ( Show )


solve :: String -> IO ()
solve input = do
    words <- parseData <$> getLines "data.txt"
    let rules = parseRules input
        filtered = applyRules rules words
        sorted = orderBest filtered
    forM_ sorted putStrLn


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do rules <- getLine
                 solve rules
        (rules : _) -> solve rules


parseRules :: String -> [Rule]
parseRules = parseRules' 0
    where
        parseRules' i ('.'      : xs) = parseRules' (i + 1) xs
        parseRules' i (' '      : xs) = parseRules' i xs
        parseRules' i ('\n'     : xs) = parseRules' i xs
        parseRules' i ('\r'     : xs) = parseRules' i xs
        parseRules' i ('!' : ch : xs) = Gray     (toLower ch) : parseRules' i xs
        parseRules' i ('^' : ch : xs) = Yellow i (toLower ch) : parseRules' i xs
        parseRules' i (ch : xs)       = Green  i (toLower ch) : parseRules' i xs
        parseRules' _ _ = []


parseData :: [String] -> [String]
parseData = sortUnique . map (map toLower) . filterLen 5 . validate


getLines :: FilePath -> IO [String]
getLines file = readFile file <&> lines


validate :: [String] -> [String]
validate = map $ filter isLetter


filterLen :: Int -> [String] -> [String]
filterLen l = filter ((l ==) . length)


sortUnique :: [String] -> [String]
sortUnique = map head . group . sort


getRules :: String -> String -> [Rule]
getRules guess chosen = zipWith3 oneChar [0 ..] guess chosen
    where
        oneChar i g c
            | g == c = Green i c
            | g /= c && g `elem` chosen = Yellow i g
            | otherwise = Gray g


median :: (Fractional b, Integral a) => [a] -> b
median [] = 1
median [x] = fromIntegral x
median lst = fromIntegral $ ceiling $ fromIntegral (lst !! (mid - 1) + lst !! mid) / 2
    where
        mid = length lst `div` 2


evaluate :: Fractional a => [String] -> String -> a
evaluate wrds a = am
    where
        remain ges cho = applyRules (getRules ges cho) wrds
        simulate wrds ges = remain ges `map` wrds
        getMean = median . (length `map`)
        am = getMean $ simulate wrds a


orderBest :: [String] -> [String]
orderBest wrds = ((fst `map`) . sortOn snd) (zip wrds vals)
    where
        vals = evaluate wrds `map` wrds


-- format:
-- .A.^N.


allWords :: Int -> [String]
allWords 1 = (: []) <$> ['a' .. 'z']
allWords n = [ ch : xs | ch <- ['a' .. 'z'],
                         xs <- allWords $ n - 1 ]


applyRules :: [Rule] -> [String] -> [String]
applyRules rls lst = foldr applyRule lst rls


applyRule :: Rule -> [String] -> [String]
applyRule r =
    filter $ case r of
        Green i ch -> isAt i ch
        Yellow i ch -> \str -> present ch str && not (isAt i ch str)
        Gray ch -> not . present ch


present :: Char -> String -> Bool
present = elem


isAt :: Int -> Char -> String -> Bool
isAt i ch = (ch ==) . (!! i)

