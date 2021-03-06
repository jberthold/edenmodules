{-# OPTIONS -XCPP -fno-warn-unused-binds -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Eden.Edi
-- Copyright   :  (c) Philipps Universitaet Marburg 2005-2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  eden@mathematik.uni-marburg.de
-- Stability   :  beta
-- Portability :  not portable
--
-- The low-level parallel functional language:
--      EDen Implementation language
--
-- This module defines a thin layer of type-checking wrappers
-- over the parallel primitives implemented in ParPrim.hs.
--
-- Depends on the Eden Compiler.
--
-- Eden Group Marburg
--

module Control.Parallel.Eden.Edi
-- interface:
   (fork,           -- :: IO () -> IO (), from conc.hs, without ThreadID
    spawnProcessAt, -- :: Int -> IO () -> IO ()
    spawnArgsProcessAt, -- ::NFData a =>  Int -> (a -> IO ()) -> a -> IO ()
    ChanName',      -- EdI channel type
    createC,        -- :: IO (ChanName' a,a) , prim.Op.
    createCs,       -- :: Int -> IO ([ChanName' a],[a])
    sendWith,       -- :: (Strategy a) -> ChanName' a -> a -> IO ()
    sendNF,         -- :: NFData a => ChanName' a -> a -> IO ()
    sendStreamWith, -- :: (Strategy a) -> ChanName' [a] -> [a] -> IO ()
    sendNFStream,   -- :: NFData a => ChanName' [a] -> [a] -> IO ()
    noPe, selfPe,   -- :: IO Int
    NFData(..), -- reexported from Control.Deepseq
    using, r0, rseq, rdeepseq, seqList, seqFoldable
                -- reexported from Control.Seq (sequential strategies)
                -- selection rationale: same export as Eden module
   )
   where

#if defined(NOT_PARALLEL)
import Control.Parallel.Eden.ParPrimConcHs as ParPrim
#else
import Control.Parallel.Eden.ParPrim as ParPrim
#endif

import Data.Typeable

import Control.DeepSeq(NFData(..))
import Control.Seq (Strategy, using,
                       r0, rseq, rdeepseq, seqList, seqFoldable)

-- Helper function: Despite its name, seq does not guarantee sequence! We
-- specialise on strategy application (unit type) and use a trivial comparison
-- case.
pseq :: () -> b -> b -- strategy application -> b -> b
pseq strat_x y = if strat_x == () then y else error "Impossible case!"
infixr 0 `pseq`
-- We could import this from Control.Parallel.Strategies, but want to
-- decouple the code from that module


-- Process Creation:
--------------------
spawnProcessAt :: Int -> IO () -> IO () -- forces IO() type!
spawnProcessAt pe action = sendData (Instantiate pe) action

-- additional: force evaluation of arguments (uncurried version)
spawnArgsProcessAt :: NFData a => Int -> (a -> IO()) -> a -> IO ()
spawnArgsProcessAt pe argsAction args
               = (rnf args `seq`
                  sendData (Instantiate pe) (argsAction args))

-- Communication:
-----------------

instance NFData (ChanName' a) where rnf x = seq x ()
-- Can only be created by ParPrim* operations, so, fine with rwhnf.


-- creation of n channels in one call, "safe" evaluation
createCs :: Typeable a =>
            Int -> IO ([ChanName' a],[a])
createCs n | n >= 0 = do list <- sequence (replicate n createC)
                         let (cs, vs) = unzip list
                         rnf cs `pseq` -- channels fully evaluated
                         -- spine vs `seq` -- value list spine (optional)
                            return (cs,vs)
           | otherwise = error "createCs: n < 0"


-- Evaluation / Communication:
------------------------------
sendWith :: Typeable a =>
            Strategy a -> ChanName' a -> a -> IO ()
--  Strategy a => a -> ()
sendWith strat c d = connectToPort c >>
                     (strat d `pseq` sendData Data d)

-- sendChan with evaluation, without Connect message
sendNF :: Typeable a =>
          NFData a => ChanName' a -> a -> IO ()
sendNF = sendWith rnf

sendStreamWith :: Typeable a =>
                  (a -> ()) -> ChanName' [a] -> [a] -> IO ()
--  Strategy a => a -> ()
sendStreamWith strat c xs = connectToPort c >>
                            send xs
    where send l@[]   = sendData Data l
          send (y:ys) = strat y `pseq` sendData Stream y >>
                         send ys

sendNFStream :: Typeable a =>
                NFData a => ChanName' [a] -> [a] -> IO ()
sendNFStream = sendStreamWith rnf

--------------------------------------------------------------

-- JUNKYARD:

-- monadic evaluation control
rnfM :: NFData a => a -> IO ()
rnfM x = case rnf x of { () -> return () } -- works as well
-- rnfM x = rnf x `pseq` return ()
-- rnfM  = return . rnf -- doznwork: returns _unevaluated_ (rnf x)

-- send without evaluation or Connect message
sendVia :: Typeable a =>
           ChanName' a -> a -> IO ()
sendVia c d = connectToPort c >>
              sendData Data d

-- send with NF evaluation and Connect message
connectSendNFvia :: Typeable a =>
                    NFData a => ChanName' a -> a -> IO ()
connectSendNFvia c d = connectToPort c >>
                       sendData Connect d >>
                       rnfM d >>
                       sendData Data d

-- sendStream: Connect message followed by element-wise NF evaluation/send
sendStreamNFvia :: (Typeable a, NFData a) =>
                   ChanName' [a] -> [a] -> IO ()
sendStreamNFvia c d = connectToPort c >>
                      sendData Connect d >>
                      sendStream' d
    where sendStream'  l@[] = sendData Data l
          sendStream' (x:xs)= rnfM x >>
                              sendData Stream x >>
                              sendStream' xs
