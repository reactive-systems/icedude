-----------------------------------------------------------------------------
-- |
-- Module      :  SPI
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Serial Peripheral Interface
-- 
-----------------------------------------------------------------------------

module SPI
  ( transferIO
  , setSpeed
  , setMode
  ) where

-----------------------------------------------------------------------------

import Data.Word
  ( Word8
  )

import Control.Monad.State
  ( get
  , put
  , when  
  )
    
-----------------------------------------------------------------------------

import Data
  ( PortNumber
  , ST(..)  
  , OP
  , Size  
  , Mode  
  )
  
import Commands
  ( Command(..)
  , SubCommand(..)  
  )

import Utils
  ( sendCmd
  , verify
  , i2W4
  , w42I
  , readData
  , writeData  
  , splitBlocks
  )  

-----------------------------------------------------------------------------

-- | Transfers data over the interface.

transferIO
  :: [Word8] -> Size -> OP [Word8]

transferIO bs n = do
  initInterface 0

  let
    i = max 0 $ n - length bs
    bs' = bs ++ replicate i 0x00

  (sa,_) <- sendCmd SPI ASSCS [0x00, 0x00] 
  verify (sa == 0)
    "assertion failed"

  let x = [0x00, 0x00, 0x00, if n > 0 then 0x01 else 0x00] 
          ++ i2W4 (length bs')
    
  (ss,_) <- sendCmd SPI START x 

  verify (ss == 0)
    "start failed"

  rs <- transfer [] n $ splitBlocks 64 bs'

  (se,y) <- sendCmd SPI END [0x00,0x00] 

  if se >= 192 then do
    verify (length y == 8)
      "Incompatible response"

    let
      (f,s) = splitAt 4 y
      vo = w42I f
      vi = w42I s
    
    verify (vo == length bs' && vi == n)
      "Read/Write failure"
      
  else if se >= 128 then do
    verify (length y == 4)
      "Incompatible response"
    verify (w42I y == length bs')
      "Read failure"

  else when (se >= 64) $ do 
    verify (length y == 4)
      "Incompatible response"

    verify (w42I y == n)
      "Read failure"

  (sc,_) <- sendCmd SPI ASSCS [0x00, 0x01]
  verify (sc == 0)
    "clear failed"      
    
  return $ concat rs

  where
    transfer a i xs = case xs of
      [] 
        | i > 64 -> do
          x <- readData 64
          transfer (x:a) (i - 64) []
        | i > 0 -> do
          x <- readData i
          return $ reverse $ x : a
        | otherwise -> 
          return $ reverse a
      (y:yr) -> do
        writeData y
        if i > 64 then do
          x <- readData 64
          transfer (x:a) (i - 64) yr
        else if i > 0 then do
          x <- readData i
          transfer (x:a) 0 yr
        else
          transfer a 0 yr
  
-----------------------------------------------------------------------------

-- | Sets the speed of the interface.

setSpeed
  :: Int -> OP ()

setSpeed n = do
  initInterface 0
  
  (s,x) <- sendCmd SPI SPEED (0x00 : i2W4 n)

  verify (s == 0 && length x == 4)
    "Speed setting failed "

  verify (w42I x == n)
    ("Speed count set correctly" ++ show x)

-----------------------------------------------------------------------------

-- | Sets the mode of the interface.

setMode
  :: Mode -> OP ()

setMode v = do
  initInterface 0

  (s,x) <- sendCmd SPI MODE [0x00, v] 
  verify (s == 0 && null x)
    "Mode setting failed"

-----------------------------------------------------------------------------

initInterface
  :: PortNumber -> OP ()

initInterface pn = do
  st <- get
  case fSPIPort st of
    Just _  -> return ()
    Nothing -> do
      (so, x) <- sendCmd SPI OPEN [pn] 
      verify (so == 0 && null x)
        "Opening SPI port failed"

      put st {
        fSPIPort = Just pn,
        cleanup = do
          (sc, y) <- sendCmd SPI CLOSE [pn] 
          verify (sc == 0 && null y)
            "Closing SPI port failed"
          cleanup st
        }

-----------------------------------------------------------------------------    
