-----------------------------------------------------------------------------
-- |
-- Module      :  Data
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Global data sturctures and values used by the application.
-- 
-----------------------------------------------------------------------------

module Data
  ( MemoryOperation(..)
  , Configuration(..)
  , ST(..)
  , OP  
  , CmdStatus  
  , PortNumber
  , Register
  , Direction
  , Value
  , ManufacturerID
  , MemTypeID
  , MemCapacity
  , Address
  , Size
  , Mode   
  , defaultCfg
  , timeout
  , vendorId
  , productId
  , boardType
  , m25p10MemSize  
  ) where

-----------------------------------------------------------------------------

import Data.Word
  ( Word8
  )
  
import System.USB
  ( Device
  , DeviceHandle
  , DeviceDesc
  , ConfigDesc  
  , InterfaceDesc
  , EndpointDesc
  )
  
import Control.Monad.State
  ( StateT
  )  

-----------------------------------------------------------------------------

-- | Return status of a command
type CmdStatus = Word8

-- | Register to be read or written
type Register = Word8

-- | Connection Port
type PortNumber = Word8

-- | Transfer direction
type Direction = Word8

-- | Value to be read or written
type Value = Word8

-- | Manufacturer Id
type ManufacturerID = Word8

-- | Memory Type
type MemTypeID = Word8

-- | Memory Capacity
type MemCapacity = Word8

-- | Memory Address
type Address = Int

-- | Transfer Size
type Size = Int

-- | SPI Mode
type Mode = Word8

-----------------------------------------------------------------------------

-- | Operation modi.

data MemoryOperation =
    ReadVIO Register
  | WriteVIO (Register,Word8)
  | VerifyVIO (Register, Word8)
  | ReadFLASH FilePath
  | WriteFLASH FilePath
  | VerifyFLASH FilePath

-----------------------------------------------------------------------------

-- | Configuration structure containing all command line settings.

data Configuration = Configuration
  { cHelp :: Bool
  , cList :: Bool
  , cId :: Maybe String
  , cErase :: Bool
  , cVerbose :: Bool
  , cQuiet :: Bool
  , cMemOpt :: Maybe MemoryOperation
  } 

-----------------------------------------------------------------------------

-- | Operation State Monad 

type OP a = StateT ST IO a

-- | State of the operation state monad

data ST = ST
  { config :: Configuration
  , device :: Device
  , fCommOpen :: Bool
  , fGPIOOpen :: Bool      
  , fSPIPort :: Maybe Word8
  , fReset :: Bool  
  , handle :: Maybe DeviceHandle
  , dDevice :: DeviceDesc
  , dConfig :: ConfigDesc
  , dIface :: InterfaceDesc
  , dCmdIn :: EndpointDesc
  , dCmdOut :: EndpointDesc
  , dDataIn :: EndpointDesc
  , dDataOut :: EndpointDesc
  , cleanup :: OP ()
  }

-----------------------------------------------------------------------------

-- | The default configuration.

defaultCfg
  :: Configuration

defaultCfg =
  Configuration
  { cHelp = False
  , cList = False
  , cId = Nothing
  , cErase = False
  , cVerbose = False
  , cQuiet = False
  , cMemOpt = Nothing
  }

-----------------------------------------------------------------------------

-- | Timeout of read / write requests in milliseconds.
timeout :: Int
timeout = 100

-- | Expected vendor ID of the iCE40.
vendorId :: String
vendorId = "1443"

-- | Expected product ID of the iCE40.
productId :: String
productId = "0007"

-- | Board type, expected to be returned by the device.
boardType :: String
boardType = "iCE40"

-- | M25P10 memory size in bytes.
m25p10MemSize :: Int
m25p10MemSize = 131072

-----------------------------------------------------------------------------
