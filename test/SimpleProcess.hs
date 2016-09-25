-- simplistic test program executing an Eden process and a GC

import System.Environment
import System.IO
import Control.Parallel.Eden

import System.Mem(performGC)

-- two time-waster functions
wasteTime :: Integer -> Integer
wasteTime n = foldl1 (+) (zipWith gcd xs (map nfib40 xs))
    where xs = 1:[3,5..n]
          nfib40 n = nfib (10 + n `mod` 15) -- nfib on arg between 10 and 24

nfib :: Integer -> Integer
nfib n | n < 2 = 1
       | otherwise = 1 + nfib (n-1) + nfib (n-2)

main = do args <- getArgs

          hSetBuffering stdout NoBuffering

          let limit = if null args then 30 else read (head args)
              list  = map (+10) [1..limit]
              quiet = length args > 1 && args!!1 == "-q" 

          let [r,r2] = spawnAt [selfPe,2] (repeat $ process (map wasteTime))
                                          (replicate 2 list)

          if not quiet then
              do putStrLn ("mapping a remote computation on " 
                           ++ show (take 5 list) ++ "...")

                 putStrLn ("First result is " ++ show (head r))
                 
                 putStrLn "Doing a GC"
          else rnf (head r) `pseq` return ()
          performGC

          if not quiet then
              do putStrLn ("Printing result: " ++ show r)
                 putStrLn ("Other result  :  " ++ show r2)
          else rnf (r,r2) `pseq` return ()
          if r == r2 then putStrLn "OK" else error "FAILED"
