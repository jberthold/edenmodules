-- fork an IO action on an RTS-chosen PE, using sendData (with modes)

import Control.DeepSeq(rnf)

import Control.Parallel.Eden.ParPrim

import System.IO
import System.Environment

-- future main: testing different primitive operations separately
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let size  = if null args then 2 ^ 16 else read (head args)
      trace = if not (null args) && args!!1 == "-q"
              then const (return ()) else putStrLn
                   
  sequence_  [ f size trace | f <- allTests ]

type IOTest = Integer -> (String -> IO ()) -> IO ()

allTests :: [ IOTest ]
allTests = [ \_ report -> putStrLn "empty test, OK"
           , testcreateC   -- create does not crash
           , testconnect   -- connectToPort does not crash
           -- , forkProc      -- use sendData in Instantiate mode for forking
           -- , testsendData  -- create chan, fork print thread, connect, send
           --                 -- (main thread waits for child to terminate)
           , dataTransfer -- process-like, but without input channel
           -- , streamTransfer -- sending a list from the child process
           ]

-- calling the primitives in isolation
testcreateC :: IOTest
testcreateC _ report = do (c, _) <- createC
                          (c :: ChanName' ()) `seq`
                              report "Survived createC"

testconnect :: IOTest
testconnect _ report = do (c, _) <- createC
                          connectToPort (c :: ChanName' ())
                          report "survived connectToPort"

forkProc :: IOTest
forkProc = undefined

testsendData :: IOTest
testsendData = undefined

-- process-like: spawn using sendData, connect, sendData (Integer)
-- but not using a reply channel for input
dataTransfer :: IOTest
dataTransfer size report = do
          report ("Remote computation with size " ++ show size)
          (port, placeholder) <- createC
          sendData (Instantiate 0) $
                      do n <- selfPe
                         report ("thread on PE " ++ show n)
                         connectToPort port
                         report ("doing work on PE " ++ show n)
                         let result :: Integer
                             result = doWork size
                         rnf result `seq`
                           -- report ("remote result " ++ show result)
                           sendData Data result
                         report "thread done"

          report "spawned remote work, waiting for result"
          report ("result received :" ++ show placeholder)
          report "doing some local work now..."
          let local = doWork size
          report (show local)
          if local == placeholder then putStrLn "OK"
             else error "FAILED"

doWork :: Integer -> Integer
doWork n = foldl1 (+) (zipWith gcd xs $ tail xs)
             where xs = [0..n]

