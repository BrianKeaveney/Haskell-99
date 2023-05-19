{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Foldable (Foldable(foldl))
import Data.List

main :: IO ()
main = putStrLn $ show $ mylast [1,2,3,4,5] 

data ListElm a = Int | Char | String

mylast :: [a] -> a
mylast list = 
    case list of
        [x]-> x
        elm : rest -> mylast rest

myButLast :: [a] -> a
myButLast list =
    case list of
        [x, _] -> x
        elm : rest -> myButLast rest

elementAt :: [a] -> Int -> a
elementAt list index = 
    case index of
       1 ->  head list
       _ -> elementAt (tail list) $ index -1

myLength :: [a] -> Int
myLength = 
  foldl (\a b -> a+1) 0 
  
myReverse :: [a] -> [a]
myReverse list =
    case list of 
        [] -> []
        head : rest -> myReverse rest ++ [head]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list =
    list == myReverse list

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten list =
    case list of 
        Elem val -> [val]
        List [] -> []
        List list -> concatMap flatten list


compress :: String -> String 
compress =
    foldl (\wordAcc letter -> 
            if wordAcc == "" then letter : wordAcc
            else if last wordAcc /= letter
                then wordAcc++[letter]
            else wordAcc
        ) "" 

pack :: [Char] -> [String]
pack [] = []
pack (char: characters) =
    let str = char : takeWhile (== char) characters
        remaining = dropWhile (== char) characters
    in str : pack remaining

encode :: String -> [(Int, Char)]
encode str =
    let groups = pack str
    in map (\w -> (length w, head w)) groups

-- First 10 complete!!!

data GroupType = Multiple Int Char | Single Char deriving (Show) 

encodeModified :: String -> [GroupType]
encodeModified = 
    map (\(count, char) ->
        if count == 1 then Single char
        else Multiple count char
    ) . encode 

decodeModified :: [GroupType] -> String
decodeModified = concatMap (\case 
    Multiple count char -> replicate count char
    Single char -> [char]) 

encodeDirect :: String -> [GroupType]
encodeDirect "" = []
encodeDirect str =
    let char = head str
        rest = dropWhile (== char) str
        count = length str - length rest 
    in if count == 1 then Single char : encodeDirect rest else Multiple count char : encodeDirect rest 


dupli :: [a] -> [a]
dupli list =
    case list of
        [] -> []
        elm : rest -> elm : elm : dupli rest

replicate' :: a -> Int -> [a]
replicate' _ 0 = []
replicate' elm count = let newCount = count -1
    in elm : replicate' elm newCount

repli :: [a] -> Int -> [a]
repli list num =
    case list of 
        [] -> []
        elm : rest -> replicate' elm num ++ repli rest num

dropEvery :: [a] -> Int -> [a]
dropEvery list num = drop' list num 1
  where drop' [] _ _ = []  
        drop' (elm : rest) n index
          | index `mod` n /= 0 = elm : drop' rest n (index + 1)
          | otherwise = drop' rest n (index + 1)


split :: [a] -> Int -> ([a], [a])
split list = splitHelper list [] 
    where splitHelper :: [a] -> [a] -> Int -> ([a], [a])
          splitHelper l nl 0 = (nl, l)
          splitHelper (elm:rest) newList n = splitHelper rest (newList++[elm]) $ n-1

slice :: [a] -> Int -> Int -> [a]
slice list start end = helper list start (1 + end - start)
    where helper _ _ 0 = [] 
          helper (elm:rest) 1 n = elm : helper rest 1 (n-1)
          helper (elm:rest) n m = helper rest (n-1) m

rotate' :: [a] -> Int -> [a]
rotate' list num 
    | num > 0 = helper list [] num
    | num == 0 = list
    | otherwise = helper list [] $ length list + num
    where 
        helper list acc 0 = list ++ acc
        helper (elm:rest) acc n = helper rest (acc++[elm]) $ n-1

removeAt' :: [a] -> Int -> (a, [a])
removeAt' list index 
    | index > 0 = helper list [] index
    | otherwise = error "index out of bounds"
    where
        helper (elem:rest) acc 1 = (elem, acc++rest)
        helper (elem:rest) acc idx = helper rest (elem:acc) $ idx-1

-- 20 Done and Dusted!!

