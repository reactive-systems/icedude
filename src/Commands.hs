-----------------------------------------------------------------------------
-- |
-- Module      :  Commands
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Command line interface parameters.
-- 
-----------------------------------------------------------------------------

module Commands 
  ( Command(..)
  , SubCommand(..)
  , cmd2W
  , sub2W
  ) where

-----------------------------------------------------------------------------

import Data.Word
  ( Word8 )

-----------------------------------------------------------------------------

-- | Selection command, which determines the interface.

data Command =
    GPIO  
  | COMM
  | SPI      

-----------------------------------------------------------------------------

-- | Operation command, which determines the action.

data SubCommand =
    OPEN
  | CLOSE
  | WRITE    
  | READ
  | TCNT
  | MODE
  | SPEED
  | ASSCS  
  | START
  | END
  | DIR
  | VALUE

-----------------------------------------------------------------------------

-- | Converts commands into binary.

cmd2W
  :: Command -> Word8

cmd2W cmd = case cmd of
  GPIO -> 0x03
  COMM -> 0x04  
  SPI  -> 0x06

-----------------------------------------------------------------------------

-- | Converts sub-commands into binary.

sub2W
  :: SubCommand -> Word8

sub2W cmd = case cmd of
  OPEN  -> 0x00
  CLOSE -> 0x01
  WRITE -> 0x04  
  READ  -> 0x05
  TCNT  -> 0x85
  MODE  -> 0x05
  SPEED -> 0x03
  ASSCS -> 0x06
  START -> 0x07
  END   -> 0x87
  DIR   -> 0x04
  VALUE -> 0x06

-----------------------------------------------------------------------------
