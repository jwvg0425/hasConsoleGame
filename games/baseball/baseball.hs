import System.IO
import System.Random

makeAnswer :: StdGen -> Int
makeAnswer gen = h*100 + t*10 + o
    where [h,t,o] = go [] $ randomRs (0,9) gen
          go [a,b] (r:rs) = if r /= a && r /= b then [a,b,r] else go [a,b] rs
          go [a] (r:rs) = if r /= a then go [a,r] rs else go [a] rs
          go [] (r:rs) = if r == 0 then go [] rs else go [r] rs

main = do
    gen <- getStdGen
    let answer = makeAnswer gen
    guess answer
    
guess :: Int -> IO ()
guess answer = do
    putStrLn . show $ answer