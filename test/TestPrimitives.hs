-- fork an IO action on an RTS-chosen PE, using sendData (with modes)

import Control.DeepSeq(rnf)

import Control.Parallel.Eden.ParPrim

import System.IO
import System.Environment
import Text.Printf
import Control.Concurrent(threadDelay)

-- future main: testing different primitive operations separately
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let size  = if null args then 2 ^ 16 else read (head args)
      trace = if length args > 1 && args!!1 == "-v"
              then putStrLn 
              else \msg -> rnf msg `seq` return ()
  sequence_  [ f size trace | f <- allTests ]

type IOTest = Integer -> (String -> IO ()) -> IO ()

allTests :: [ IOTest ]
allTests = [ testcreateC   -- create does not crash
           , testconnect   -- connectToPort does not crash
           , forkProc      -- use sendData in Instantiate mode for forking
           , testsendData  -- create chan, fork print thread, connect, send
                           -- (main thread waits for child to terminate)
                           -- (process-like, but without input channel)
           , streamTransfer -- sending a list from the child process
           ]

-- calling the primitives in isolation
testcreateC :: IOTest
testcreateC _ report = do printf "createC"
                          (c, _) <- createC
                          (c :: ChanName' ()) `seq`
                              report "Survived createC"
                          printf " - OK\n"

testconnect :: IOTest
testconnect _ report = do printf "connect"
                          (c, _) <- createC
                          connectToPort (c :: ChanName' ())
                          report "survived connectToPort"
                          printf " - OK\n"
forkProc :: IOTest
forkProc size report = do
          printf "forkProc"
          report "forking a child process via sendData (Instantiate 0)"
          report (printf "remote PE should print its PE ID and %d" size)
          let printSize :: IO ()
              printSize = selfPe >>= \pe ->
                          printf "\tforked on PE %d: size %d" pe size
          sendData (Instantiate 0) printSize
          threadDelay 500
          report "Survived sendData (child process output should be visible)"
          printf " - OK\n"

-- process-like: spawn using sendData, connect, sendData (Integer)
-- but not using a reply channel for input
testsendData :: IOTest
testsendData size report = do 
          printf "testSendData"
          report ("Remote computation with size " ++ show size)
          (port, placeholder) <- createC
          sendData (Instantiate 0) $
                      do n <- selfPe
                         (selfPe >>= printf "\tdata thread on PE %d")
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
          if local == placeholder then printf " - OK\n"
             else error "FAILED"

streamTransfer :: IOTest
streamTransfer size report = do
          printf "streamTransfer"
          report ("sending stream of data, size " ++ show size)
          (port, placeholder) <- createC
          sendData (Instantiate 0) $
                      do (selfPe >>= printf "\tstream thread on PE %d")
                         connectToPort port
                         mapM_ (sendData Stream) [0..size] 
                         sendData Data ([]::[Integer])
                         report "thread done"

          report "spawned remote work, waiting for result"
          report ("head received :" ++ show (head placeholder))
          if size == last placeholder then printf " - OK\n"
             else error "FAILED"

doWork :: Integer -> Integer
doWork n = foldl1 (+) (zipWith gcd xs $ tail xs)
             where xs = [0..n]

