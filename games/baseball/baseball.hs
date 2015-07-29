import System.IO
import System.Random
import Data.List

main = do
    gen <- getStdGen
    let answer = makeAnswer gen
    play answer

play :: Int -> IO ()
play answer = do
    try <- guess
    if try == answer then
        putStrLn "right answer!"
    else if try == 0 then
        putStrLn "exit."
    else do
        let (s,b) = getStrikeAndBall try answer
        putStrLn $ (show s) ++ " strike," ++ (show b) ++ " ball"
        play answer

guess :: IO Int
guess = do
    putStrLn "guess the answer : "
    input <- getLine
    let inputList = reads input
    if inputList == [] then
        return 0
    else if validInput (head inputList) then
        return $ fst (head inputList)
    else do
        putStrLn "invalid input."
        guess
        
makeAnswer :: StdGen -> Int
makeAnswer gen = h*100 + t*10 + o
    where [h,t,o] = go [] $ randomRs (0,9) gen
          go [a,b] (r:rs) = if r /= a && r /= b then [a,b,r] else go [a,b] rs
          go [a] (r:rs) = if r /= a then go [a,r] rs else go [a] rs
          go [] (r:rs) = if r == 0 then go [] rs else go [r] rs

validInput :: (Int, String) -> Bool
validInput (v,"") = if (length . nub $ vs) == 3 then True else False
    where vs = valueToList v
validInput _ = False

valueToList :: Int -> [Int]
valueToList 0 = [0]
valueToList v 
    | v < 10 = [v]
    | otherwise = (v `mod` 10):valueToList (v `div` 10) 

getStrikeAndBall :: Int -> Int -> (Int, Int)
getStrikeAndBall try answer = (strike, ball)
    where t = valueToList try
          a = valueToList answer
          strike = length $ filter (\(x,y) -> x == y) (zip t a)
          ball = length (filter (\x -> x `elem` a) t) - strike