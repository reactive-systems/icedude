-----------------------------------------------------------------------------
-- |
-- Module      :  VIO
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Digilent Vritual I/O Expansion Debugging Interface.
-- 
-----------------------------------------------------------------------------

module VIO
  ( readRegister
  , writeRegister
  ) where

-----------------------------------------------------------------------------

import Data.Word
  ( Word8
  )
  
import Control.Monad.State
  ( get
  , put
  , unless
  )
  
import Control.Exception
  ( assert
  )

-----------------------------------------------------------------------------

import Data
  ( Register
  , ST(..)      
  , OP
  )
  
import Utils
  ( i2W
  , w42I
  , verify
  , readData
  , writeData
  , pureSendCmd
  , sendCmd    
  )
  
import Commands
  ( Command(..)
  , SubCommand(..)  
  )  

-----------------------------------------------------------------------------

-- | Reads 'n' bytes starting at register 'reg'.

readRegister
  :: Register -> Int -> OP [Word8]

readRegister reg n = do
  -- connect to the inteface
  initInterface

  -- send read request
  (sc,x) <- sendCmd COMM READ [0x00, reg, i2W n, 0x00, 0x00, 0x00] 
            
  verify (sc == 0 && null x)
    "Read register error"

  -- read the data
  rs <- readData $ min 16 $ fromIntegral n

  -- verify transfer count  
  tc <- transferCount
  assert (tc < 0) $ return ()

  verify (n + fromIntegral tc == 0)
    "Invalid number of registers transferted"

  return rs

-----------------------------------------------------------------------------

-- | Writes the data 'bs' at register 'reg'.  

writeRegister
  :: Register -> [Word8] -> OP ()

writeRegister reg bs = do
  assert (length bs < 128) $ return ()

  -- connect to the interface
  initInterface

  -- send write request
  (sc,x) <- sendCmd COMM WRITE [0x00, reg, i2W (length bs), 0x00, 0x00, 0x00]
            
  verify (sc == 0 && null x)
    "Write register error"

  -- write the data
  writeData bs

  -- verify transfer count  
  tc <- transferCount
  assert (tc > 0) $ return ()
  
  verify (tc == 1)
    "Invalid number of registers transferted"

-----------------------------------------------------------------------------

transferCount
  :: OP Int

transferCount = do
  initInterface
  
  (s,rs) <- sendCmd COMM TCNT [0x00] 

  if s >= 128 then do
    verify (length rs == 4)
      "No tranfer count received"
    return $ w42I rs
  else if s >= 64 then do
    verify (length rs == 4) 
      "No tranfer count received"
    return $ negate $ w42I rs
  else
    return 0
  
-----------------------------------------------------------------------------

initInterface
  :: OP ()

initInterface = do
  st <- get
  unless (fCommOpen st) $ do
    pureSendCmd COMM OPEN [0x00]
    put st {
      fCommOpen = True,
      cleanup = do
        pureSendCmd COMM CLOSE [0x00]
        cleanup st
      }

-----------------------------------------------------------------------------    
