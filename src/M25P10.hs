-----------------------------------------------------------------------------
-- |
-- Module      :  SPI
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- M25P10 Memory Interface.
--
-----------------------------------------------------------------------------

module M25P10
  ( wakeup
  , getId
  , writeEnable
  , writeDisable
  , getStatus
  , waitUntilDone
  , chipErase
  , readContent
  , pageProgram
  ) where

-----------------------------------------------------------------------------

import Data.Word
  ( Word8
  )

import Control.Monad
  ( void
  , when
  )

import Control.Monad.State
  ( lift
  )

import Control.Concurrent
  ( threadDelay
  )

import Control.Exception
  ( assert
  )

-----------------------------------------------------------------------------

import Data
  ( OP
  , ManufacturerID
  , MemTypeID
  , MemCapacity
  , Address
  , Size
  )

import Utils
  ( i2W3
  )

-----------------------------------------------------------------------------

import qualified SPI

-----------------------------------------------------------------------------

-- | Instructions of the M25P10.

data Instruction =
    WREN      -- ^ Write Enable
  | WRDI      -- ^ Write Disable
  | RDID      -- ^ Read Identification
  | RDSR      -- ^ Read Status Register
  | WRSR      -- ^ Write Status Register
  | READ      -- ^ Read Data Bytes
  | FAST_READ -- ^ Read Data Bytes at Higher Speed
  | PP        -- ^ Page Program
  | SE        -- ^ Sector Erase
  | BE        -- ^ Bulk Erase
  | DP        -- ^ Deep Power-down
  | RES       -- ^ Release from Deep Power-down

-----------------------------------------------------------------------------

-- | Converts an instruction into a single byte.

x2W
  :: Instruction -> Word8

x2W ins = case ins of
  WREN      -> 0x06
  WRDI      -> 0x04
  RDID      -> 0x9F
  RDSR      -> 0x05
  WRSR      -> 0x01
  READ      -> 0x03
  FAST_READ -> 0x0B
  PP        -> 0x02
  SE        -> 0xD8
  BE        -> 0xC7
  DP        -> 0xB9
  RES       -> 0xAB

-----------------------------------------------------------------------------

-- | Shortcut to transfer instructions via the SPI.

transfer
  :: Instruction -> Int -> OP [Word8]

transfer ins = SPI.transferIO [x2W ins]

-----------------------------------------------------------------------------

-- | Releases the M25P10 from Deep Power-down.

wakeup
  :: OP ()

wakeup =
  void $ transfer RES 0

-----------------------------------------------------------------------------

-- | Returns the identification data of the M25P10.

getId
  :: OP (ManufacturerID, MemTypeID, MemCapacity)

getId = do
  bs <- transfer RDID 4
  let [_, b1, b2, b3] = bs
  return (b1, b2, b3)

-----------------------------------------------------------------------------

-- | Enables to write data to the M25P10.

writeEnable
  :: OP ()

writeEnable =
  void $ transfer WREN 0

-----------------------------------------------------------------------------

-- | Disables to write data to the M25P10.

writeDisable
  :: OP ()

writeDisable =
  void $ transfer WRDI 0

-----------------------------------------------------------------------------

-- | Returns the status of the M25P10.

getStatus
  :: OP Word8

getStatus = do
  bs <- transfer RDSR 2
  return $ head $ tail bs

-----------------------------------------------------------------------------

-- | Waits until a pending operation of the M25P10 finished.

waitUntilDone
  :: OP ()

waitUntilDone = do
  w <- getStatus
  when (w > 0) $ do
    lift $ threadDelay 100000
    waitUntilDone

-----------------------------------------------------------------------------

-- | Erases all content saved on the M25P10.

chipErase
  :: OP ()

chipErase = do
  writeEnable
  void $ transfer BE 0
  waitUntilDone

-----------------------------------------------------------------------------

-- | Reads 'n' bytes from the M25P10 starting at the given address.

readContent
  :: Address -> Size -> OP [Word8]

readContent addr n = do
  let is = [x2W FAST_READ] ++ (i2W3 addr) ++ [0x00]
  bs <- SPI.transferIO is $ n + 5
  return $ snd $ splitAt 5 bs

-----------------------------------------------------------------------------

-- | Writes a page of 256 bytes to the M25P10 starting at the given
-- address.

pageProgram
  :: Address -> [Word8] -> OP ()

pageProgram addr bs = do
  writeEnable

  assert (addr `mod` 256 == 0) $ return ()
  assert (length bs <= 256) $ return ()

  void $ SPI.transferIO ([x2W PP] ++ (i2W3 addr) ++ bs) 0

  waitUntilDone

-----------------------------------------------------------------------------
