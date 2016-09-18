{-# LANGUAGE CPP #-}
-- testing process spawning which depends on a process result

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

          let r1s:_ = spawn [process $ map wasteTime ] [list]
              r2s   = spawn (map mkProc r1s) $
                             repeat 0 -- wanna use repeat () , but see below
              -- mkProc x = process $ const $ show x
              -- this variant sends prematurely (const or unit optimised away)
              mkProc :: Show a => a -> Process Int String
              mkProc x = process $  (\n -> if n == 0 then show x else "-")
#ifndef QUIET
          putStrLn ("First process maps a computation on " 
                    ++ show list ++ 
                    ",\n then secondary processes are spawned on results.")

          putStrLn ("First process' result starts by " ++ show (head r1s))
          putStrLn ("Secondary results:  " ++ unwords r2s)
#else
          (rnf (head r1s,unwords r2s) `pseq` putStrLn ("Done")
#endif
