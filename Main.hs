import Data.Char
import System.IO
import System.Environment
import System.Exit

isStringDigit :: [Char] -> Bool --verifie si la chaine de caractéres est uniquement composé de nombre
isStringDigit [] = True
isStringDigit (x:xs)
    | x == '-' = isStringDigit(xs)
    | isDigit x == False = False
    | otherwise = isStringDigit(xs)

splitOn :: (a -> Bool) -> [a] -> [[a]] -- Sépare un String en fonction d'un caractére séparateurs
splitOn _ [] = []
splitOn f l@(x:xs)
    | f x = splitOn f xs
    | otherwise = let (h,t) = break f l in h:(splitOn f t)

checkSndPart :: [String] -> Bool --Regarde s'il n'y a que des chiffres dans le arguments fournis
checkSndPart [] = True
checkSndPart (x:xs)
    | isStringDigit x == True = checkSndPart xs
    | otherwise = False

checkArray :: [String] -> Bool --Regarde s'il n'y a que des fonctions autorisé dans le tableau
checkArray [] = True
checkArray (x:xs) = 
    if x /= "sa" && x /= "sb" && x /= "sc" && x /= "pa" && x /= "pb" && x /= "ra" && x /= "rb" && x /= "rr" && x /= "rra" && x /= "rrb"  && x /= "rrr" then
        False
    else
        checkArray xs

startCheck :: [String] -> [String] -> Bool --Commence le checks des différents arguments
startCheck a b = do
    if checkArray a == True && checkSndPart b == True then
        True
    else
        False

swapFunction :: [String] -> [String] --Inverse les deux premier éléments de la liste donnée
swapFunction [] = []
swapFunction (x:[]) = (x:[])
swapFunction (x:y:xs) = (y:x:xs)

rotateFunction :: [String] -> [String] --le premier élément devient le dernier élément de la liste donnée
rotateFunction [] = []
rotateFunction (a:[]) = (a:[])
rotateFunction (x:xs) = xs ++ [(x:xs)!!0]

sndRotateFunction :: [String] -> [String] --le dernier élément devient le premier de la liste donnée
sndRotateFunction [] = []
sndRotateFunction (a:[]) = (a:[])
sndRotateFunction a = [last a] ++ init a

pushFunction :: [String] -> [String] -> [String] --prend le premier élément de la liste et le met dans la deuxième liste
pushFunction [] snd = snd
pushFunction fst snd = [fst!!0] ++ snd

sortList :: [String] -> [String] -> [String] -> [String] --Commence les opérations sur les chiffres donnés en arguments
sortList [] a b
    | length b /= 0 = ["failed"]
    | otherwise = a
sortList l a b
    | l!!0 == "sa" = sortList (drop 1 l) (swapFunction a) b
    | l!!0 == "sb" = sortList (drop 1 l) a (swapFunction b)
    | l!!0 == "sc" = sortList (drop 1 l) (swapFunction a) (swapFunction b)
    | l!!0 == "pa" = sortList (drop 1 l) (pushFunction b a) (drop 1 b)
    | l!!0 == "pb" = sortList (drop 1 l) (drop 1 a) (pushFunction a b)
    | l!!0 == "ra" = sortList (drop 1 l) (rotateFunction a) b
    | l!!0 == "rb" = sortList (drop 1 l) a (rotateFunction b)
    | l!!0 == "rr" = sortList (drop 1 l) (rotateFunction a) (rotateFunction b)
    | l!!0 == "rra" = sortList (drop 1 l) (sndRotateFunction a) b
    | l!!0 == "rrb" = sortList (drop 1 l) a (sndRotateFunction b)
    | otherwise = sortList (drop 1 l) (sndRotateFunction a) (sndRotateFunction b) --rrr

verifieList :: [String] -> IO () --Verifier que la liste est bien triée dans l'odre croissant
verifieList [] = putStr "OK"
verifieList a
    | a!!0 == "failed" = putStr "KO"
verifieList (x:[]) = putStr "OK"
verifieList (x:xs)
    | (read x :: Int) > (read (head xs) :: Int) = putStr "KO"
    | otherwise = verifieList xs

main :: IO () --début du programme
main = do
    args <- getArgs
    prearg <- getLine
    let array = splitOn (==' ') prearg
    if startCheck array args == False then
        exitWith (ExitFailure 84)
    else do
        let new_list = sortList array args []
        verifieList new_list
