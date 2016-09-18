-- simplistic test program executing an Eden process and a GC

import System.Environment
import System.IO

import Control.Parallel.Eden.EdenConcHs

import System.Mem(performGC)

-- two time-waster functions
wasteTime :: Integer -> Integer
wasteTime n = foldl1 (+) (zipWith gcd xs (map nfib25 xs))
    where xs = 1:[3,5..n]
          nfib25 n = nfib (10 + n `mod` 16) -- nfib on arg between 10 and 25

nfib :: Integer -> Integer
nfib n | n < 2 = 1
       | otherwise = 1 + nfib (n-1) + nfib (n-2)

main = do args <- getArgs

          hSetBuffering stdout NoBuffering

          let limit = if null args then 30 else read (head args)
              list  = map (+10) [1..limit]
              

          putStrLn ("mapping a remote computation on " 
                    ++ show (take 5 list) ++ "...")

          let (r:rs) = spawnAt [selfPe,2] (repeat $ process (map wasteTime))
                                          (replicate 2 list)

          putStrLn ("First result is " ++ show (head r))

          putStrLn "Doing a GC"
          performGC

          putStrLn ("Printing result: " ++ show r)

          putStrLn ("Other result  :  " ++ show (head rs))
