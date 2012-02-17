{-# OPTIONS -fglasgow-exts -cpp -XBangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Eden.ParPrim
-- Copyright   :  (c) Philipps Universitaet Marburg 2005-2010
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  eden@mathematik.uni-marburg.de
-- Stability   :  beta
-- Portability :  not portable
--
-- Provides primitive functions for explicit distributed functional programming.-- Base module, importing PrimOps => exporting IO actions
-- Depends on the Eden Compiler.
-- Eden Project, JB
-- 
-----------------------------------------------

module Control.Parallel.Eden.ParPrim(
     noPe, selfPe     -- system information    :: Int
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
	      ) 
   where


import GHC.IO(IO(..))
import GHC.Base(Int#, Int(..), (+#), 
		fork#, expectData#, connectToPort#, sendData#
	       )

import Foreign.C(CInt)
import GHC.Ptr(Ptr)
import Foreign.Storable(Storable(peek))

----------------------------------------------------------
-- IO wrappers for primitive operations:
--
-- all primitives are implemented out-of-line,
-- wrappers should all be of type * -> IO (...)
--
-- (eden implementation can work with unsafePerformIO)
---------

-- system information
{-# NOINLINE noPe #-}
noPe :: IO Int
{-# NOINLINE selfPe #-}
selfPe :: IO Int

foreign import ccall "&nPEs" nPEs :: Ptr CInt
foreign import ccall "&thisPE" thisPE :: Ptr CInt
noPe = do n <- peek nPEs
          return (fromIntegral n)
selfPe = do n <- peek thisPE
            return (fromIntegral n)

-------------------------

-- not for export, only abstract type visible outside
data ChanName' a = Chan Int# Int# Int#
                deriving Show

-- tweaking fork primop from concurrent haskell... (not returning threadID)
{-# NOINLINE fork #-}
fork :: IO () -> IO ()
fork action = IO (\s -> case (fork# action s) of 
                          (# s' , _ #) -> (# s' , () #)
                 )

-- creation of one placeholder and one new inport
{-# NOINLINE createC #-}
-- returns consistent channel type (channel of same type as data)
createC :: IO ( ChanName' a, a )
createC = IO (\s -> case (expectData# s) of 
                     (# s',i,p, bh #) -> case selfPe_ s' of
                                            (# s'', I# pe #) ->
                                                (# s'',(Chan pe p i, bh) #)
             )
    where (IO selfPe_) = selfPe

-- TODO: wrap creation of several channels in RTS? (see eden5::createDC# )
--       (would save foreign call overhead, but hard-wire more into RTS)

{-# NOINLINE connectToPort #-}
connectToPort_ :: Int# -> Int# -> Int# -> IO ()
connectToPort_ pe proc i 
    = IO (\s -> case (connectToPort# pe proc i s) of
	                   s' -> (# s', () #)
	 )

connectToPort :: ChanName' a -> IO ()
connectToPort (Chan p proc i) = connectToPort_ p proc i

-- send modes for sendData
data Mode = Connect -- announce sender at receiver side (no graph needed)
	  | Data    -- data to send is single value
	  | Stream  -- data to send is element of a list/stream
	  | Instantiate Int -- data is IO(), receiver to create a thread for it
decodeMode :: Mode -> Int
decodeMode Connect         = 1
decodeMode Stream          = 2
decodeMode Data            = 3
decodeMode (Instantiate n) = let k = 4 + n*8
                             in -- k `seq` -- needed to pass NF to PrimOp?
			        k
-- decodeMode other = error "sendData: no such mode"

{-# NOINLINE sendData #-}
sendData :: Mode -> a -> IO ()
sendData mode d 
    = IO (\s -> case (sendData# m d s) of 
	                   s' -> (# s', () #)
	 )
      where !(I# m) = decodeMode mode
