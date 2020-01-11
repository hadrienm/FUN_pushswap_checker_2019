import Data.Char
import System.IO
import System.Environment
import System.Exit
import Data.List

isStringDigit :: [Char] -> Bool --verifie si la chaine de caractéres est uniquement composé de nombre
isStringDigit [] = True
isStringDigit (x:xs)
    | x == '-' = isStringDigit(xs)
    | isDigit x == False = False
    | otherwise = isStringDigit(xs)

checkSndPart :: [String] -> Bool --Regarde s'il n'y a que des chiffres dans les arguments fournis
checkSndPart [] = True
checkSndPart (x:xs)
    | isStringDigit x == True = checkSndPart xs
    | otherwise = False

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

printPb :: Int -> [String] -> [String]
printPb a op
    | a == 0 = op
    | otherwise = printPb (a - 1) (op++["pa"])

sortList :: [String] -> [String] -> [String] -> [String] --Commence les opérations sur les chiffres donnés en arguments
sortList a b op
    | verifieList a == True && length b == 0 = op
sortList [] b op = sortList (reverse b) [] (printPb (length b) op) -- "pa" passse toute la liste b dans la liste a TODO reverse
sortList (a:[]) b op = sortList (drop 1 (a:[])) (pushFunction (a:[]) b) (op++["pb"])
sortList a b op
    | (read (a!!0)::Int) > (read (a!!1)::Int) && (length b >= 2 && (read (b!!0)::Int) < (read (b!!1)::Int)) = sortList (swapFunction a) (swapFunction b) (op++["sc"])
    | (read (a!!0)::Int) > (read (a!!1)::Int) = sortList (swapFunction a) (b) (op++["sa"])
    | length b >= 2 && (read (b!!0)::Int) < (read (b!!1)::Int) = sortList a (swapFunction b) (op++["sb"])
    | otherwise = sortList (drop 1 a) (pushFunction a b) (op++["pb"])

verifieList :: [String] -> Bool --Verifier que la liste est bien triée dans l'odre croissant
verifieList [] = False
verifieList (x:[]) = True
verifieList x
    | (read (x!!0)::Int) > (read (x!!1)::Int) = False
    | otherwise = verifieList (tail x)

main :: IO () --début du programme
main = do
    args <- getArgs
    if checkSndPart args == False then
        exitWith (ExitFailure 84)
    else do
        let operation_list = sortList args [] []
        print (intercalate " " operation_list)  
