{-# OPTIONS -XScopedTypeVariables -XCPP -XMagicHash -XBangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Eden.ParPrimConcHs
-- Copyright   :  (c) Philipps Universitaet Marburg 2005-2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  eden@mathematik.uni-marburg.de
-- Stability   :  beta
-- Portability :  not portable
--
-- Provides primitive functions for explicit distributed functional programming.
-- This version: simulates primitives by Concurrent Haskell
-- (can serve as specification of primitives semantics)
--
-- Depends on GHC.
--
-- Eden Group Marburg ( http:\/\/www.mathematik.uni-marburg.de/~eden )
--
-----------------------------------------------


module Control.Parallel.Eden.ParPrimConcHs
    (noPe, selfPe     -- system information    :: Int
     , ChanName'      -- primitive channels (abstract in Eden module and outside)
     , fork           -- forking conc. threads :: IO () -> IO ()
     , createC        -- creating placeholders :: IO (ChanName' a, a)
     , connectToPort  -- set thread's receiver :: ChanName' a -> IO ()
     , sendData       -- sending data to recv. :: Mode -> a -> IO ()
     , Mode(..)       -- send modes:  implemented:
                      --      1 - connect (no graph needed)
                      --      2 - stream  (list element)
                      --      3 - single  (single value)
                      --      4 - rFork   (receiver creates a thread, different ports)
                      -- additional payload (currently only for rFork) in high bits
     , simInitPes
              )
   where

import GHC.Base(unsafeCoerce# )

import qualified Data.Map as Map -- collides with prelude functions
import Data.Map(Map)
import Data.Maybe(fromJust)

import System.IO.Unsafe
import Control.Concurrent
import GHC.Conc(numCapabilities)

import Data.Dynamic

-- Concurrent-Haskell simulation of Eden PrimOps
----------------------------------------------------------
-- tracing
trace :: String -> IO ()
#ifdef TRACE
trace msg = do me <- myThreadId
               (pe,p,_) <- myInfo
               putStrLn (show (pe,p,me) ++ msg)
#else
trace _ = return ()
#endif

----------------------------------------------------------

---- Simulation specials ----

-- global ID supply for process IDs and Channel IDs:
--    (CAF trick, evaluated, i.e. created, by first usage)
{-# NOINLINE idSupply #-}
idSupply :: MVar Int
idSupply = unsafePerformIO (newMVar 1)

-- pulling a fresh channel/process ID:
freshId :: IO Int
freshId = do i <- takeMVar idSupply
             putMVar idSupply (i+1)
             return i

-- process and thread book-keeping:

-- (PE, processID, Maybe connected channel)
type ThreadInfo = (Int,Int,Maybe Int)

-- global thread table: ID -> ThreadInfo
--    (first time called: created with first thread as an entry)
{-# NOINLINE thrs #-}
thrs :: MVar (Map ThreadId (Int,Int,Maybe Int))
thrs = unsafePerformIO (myThreadId >>= \tid ->
          newMVar (Map.insert tid (1,1,Nothing) Map.empty ))

-- retrieving own thread information
myInfo :: IO ThreadInfo
myInfo = do tid        <- myThreadId
            thrMap     <- readMVar thrs
            case Map.lookup tid thrMap of
              Nothing -> error (show tid ++ " not found!")
              Just x  -> return x

-- retrieving the channel a thread has connected to
myChan :: IO Int
myChan = do (_,_,c) <- myInfo
            case c of
              Nothing -> do tid <- myThreadId
                            error (show tid ++ " not connected!")
              Just x  -> return x

-- when thread finished:
removeThread :: ThreadId -> IO ()
removeThread tid = do trace ("Kill " ++ show tid)
                      thrMap <- takeMVar thrs
                      putMVar thrs (Map.delete tid thrMap)


-- table of open channels, and channel lookup
-- Channels are MVars, but cast to Dynamic as they may carry various types
-- (to test the 1:1 restriction, we save past senders for stream comm.)

{-# NOINLINE chs #-}
chs :: MVar (Map Int (Maybe ThreadId, Dynamic))
chs = unsafePerformIO (newMVar Map.empty)

-- for Connect messages: only register the calling thread as the sender
registerSender :: Int -> IO ()
registerSender i
    = do cMap <- takeMVar chs
         tid  <- myThreadId
         case Map.lookup i cMap of
            Nothing      -> error $ "missing MVar for Id " ++ show i
            Just (t,var) -> if (t == Nothing || t == Just tid)
                              then do putMVar chs
                                        (Map.insert i (Just tid,var) cMap)
                              else error ("duplicate connect message: "
                                          ++ show tid ++ "->"
                                          ++ show i)

-- for receiving messages, removes the channel (Data message)
getRemoveCVar :: Typeable a => Int -> IO (MVar a)
getRemoveCVar i = do 
  cMap <- takeMVar chs
  case Map.lookup i cMap of
    Nothing       -> error ("missing MVar for Id "
                            ++ show i)
    Just (_,varD) -> do putMVar chs (Map.delete i cMap)
                        return (fromDyn varD errA)
        where errA = error $ "failed to get MVar holding "
--                             ++ show (typeOf (undefined :: a))


-- for receiving stream messages, updates the channel, checks the sender
updateGetCVar :: Typeable a => MVar a -> Int -> IO (MVar a)
updateGetCVar newVar i
    = do cMap <- takeMVar chs
         tid  <- myThreadId
         let newVarD = toDyn newVar
         case Map.lookup i cMap of
           Nothing      -> error $ "missing MVar for Id " ++ show i
           Just (t,varD)-> if (t == Nothing || t == Just tid)
                             then do putMVar chs
                                       (Map.insert i (Just tid,newVarD) cMap)
                                     return (fromDyn varD errA)
                             else error "1:1 restriction violated"
             where errA = error $ "Failed to get MVar holding " 
--                                  ++ show (typeOf (undefined :: a))

-- holds number of PEs simulated (can be changed using simInitPes function
{-# NOINLINE pesVar #-}
pesVar :: MVar ([Int],())
pesVar = unsafePerformIO (newMVar (placementList,()))
    where placementList = leftrotate peNums
          peNums = if numCapabilities == 1
                       then [1..4]  -- arbitrary default: 4 PEs
                       else [1..numCapabilities]

leftrotate :: [a] -> [a]
leftrotate [] = []
leftrotate (x:xs) = xs ++ [x]

simInitPes :: Int -> IO ()
simInitPes pes | pes < 1 = error "invalid number of PEs requested"
               | otherwise = do (_,test) <- takeMVar pesVar
                                trace ("Init. with " ++ show pes ++ " PEs.")
                                test `seq` -- protect against double init.
                                   putMVar pesVar
                                     ([2..pes+1],error "double simInitPes")


-- round-robin placement:
choosePe :: IO Int
choosePe = do pe          <- selfPe
              trace "choosing PE"
              (list,test) <- takeMVar pesVar
              let place = list!!(pe-1)
                  pes   = length list
                  new = if place == pes then 1 else place+1
                  newList = take (pe-1) list ++ new:drop pe list
              putMVar pesVar (newList,test)
              trace "chosen"
              return place

--------- Primitives simulation ----------

-- the following is exported:

-- system information
{-# NOINLINE noPe #-}
noPe :: IO Int
noPe = do (p,_) <- readMVar pesVar
          return (length p)

-- place processes in round-robin manner
{-# NOINLINE selfPe #-}
selfPe :: IO Int
selfPe = do (pe,_,_) <- myInfo
            return pe

-- abstract outside!
data ChanName' a = Chan Int Int Int
   deriving (Show)

-- tweaking fork primop from concurrent haskell... (not returning threadID)
{-# NOINLINE fork #-}
fork :: IO () -> IO ()
fork action = do (pe,p,_) <- myInfo
                 trace ("new thread")
                 tMap <- takeMVar thrs
                 tid <- forkIO action'
                 putMVar thrs (Map.insert tid (pe,p,Nothing) tMap)
                 trace ("forked! ID=" ++ show tid)
    where action' = do tid <- myThreadId
                       trace ("run thread " ++ show tid)
                       action
                       removeThread tid

-- creation of one placeholder and one new inport
-- returns consistent channel type (channel of same type as data)
createC :: Typeable a => IO ( ChanName' a, a )
createC = do (!pe,!p,_) <- myInfo
             !i <- freshId
             -- Bang patterns make sure all components of ChanName' are
             -- evaluated when channel is demanded. We get rnf = rwhnf for
             -- ChanName' outside. The real primitive does this by nature.
             var <- newEmptyMVar
             trace ("new channel in " ++ show (pe,p) ++ ", ID=" ++ show i)
             cList <- takeMVar chs
             let x = unsafePerformIO $ readMVar var
             putMVar chs (Map.insert i (Nothing, toDyn var) cList)
             trace "channel created!"
             return (Chan pe p i, x )

-- connect a thread to a channel
connectToPort :: ChanName' a -> IO ()
connectToPort (Chan _ _ cid)
                   = do tid <- myThreadId
                        tlist <- takeMVar thrs
                        putMVar thrs (Map.update newC tid tlist)
      where newC (pe, p, _) = Just (pe, p, Just cid)

-- send modes for sendData
data Mode = Connect -- announce sender at receiver side (no graph needed)
          | Data    -- data to send is single value
          | Stream  -- data to send is element of a list/stream
          | Instantiate Int -- data is IO(), receiver to create a thread for it

sendData :: Typeable a => Mode -> a -> IO ()
sendData Connect _ = do ch <- myChan
                        registerSender ch

sendData Data d = do cd <- myChan
                     var <- getRemoveCVar cd
                     putMVar var d

sendData Stream d = do cd <- myChan
                       v2 <- newEmptyMVar
                       var <- updateGetCVar v2 cd
                       let rest = unsafePerformIO $ readMVar v2
                       putMVar var (d : rest)

sendData (Instantiate maybePe) d
         = do newPid <- freshId
              pes <- noPe
              pe <- if maybePe == 0 then choosePe
                        else return (1+((maybePe-1) `mod` pes))
              trace ("new process on PE " ++ show pe)
              tlist <- takeMVar thrs
              tid <- forkIO action
              putMVar thrs (Map.insert tid (pe,newPid,Nothing) tlist)
              trace ("process,thread: " ++ show (newPid,tid))
    where action = do tid <- myThreadId
                      trace ("process starting")
                      fromJust (cast d :: Maybe (IO ()))
                      removeThread tid
