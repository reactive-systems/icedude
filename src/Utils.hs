-----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Utility Functions.
-- 
-----------------------------------------------------------------------------

module Utils where

-----------------------------------------------------------------------------

import Data.Word
  ( Word8
  )
  
import System.IO
  ( hFlush
  , stdout  
  )
  
import System.USB
  ( Status(..)
  , DeviceHandle
  , EndpointAddress
  , openDevice
  , closeDevice  
  , setConfig
  , claimInterface
  , releaseInterface
  , interfaceNumber
  , readBulk
  , writeBulk
  , endpointAddress
  )

import Control.Monad.State
  ( get
  , put
  , when  
  , unless
  , lift
  )
  
import Control.Exception
  ( assert
  )

import Data.ByteString
  ( pack
  , unpack
  )

-----------------------------------------------------------------------------

import Data
  ( Configuration(..)
  , ST(..)    
  , CmdStatus
  , OP    
  , timeout
  )  

import Commands
  ( Command
  , SubCommand
  , cmd2W
  , sub2W
  )  

-----------------------------------------------------------------------------

-- | Splits a list into multiple blocks of the given lenght 'n'.

splitBlocks
  :: Int -> [Word8] -> [[Word8]]

splitBlocks n = splitBlocks' [] 
  where
    splitBlocks' a xs = case xs of
      [] -> reverse a
      _  -> let (x,y) = splitAt n xs 
           in splitBlocks' (x : a) y

-----------------------------------------------------------------------------

-- | Converts an integer into a list of four bytes.

i2W4
  :: Int -> [Word8]

i2W4 n = 
  [ i2W $ n `mod` 256
  , i2W $ (n `div` 256) `mod` 256
  , i2W $ (n `div` (256 * 256)) `mod` 256
  , i2W $ (n `div` (256 * 256 * 256)) `mod` 256
  ]
  
-----------------------------------------------------------------------------

-- | Converts an integer into a list of three bytes.
  
i2W3
  :: Int -> [Word8]

i2W3 n = 
  [ i2W $ n `mod` 256
  , i2W $ (n `div` 256) `mod` 256
  , i2W $ (n `div` (256 * 256)) `mod` 256 
  ]
  
-----------------------------------------------------------------------------  

-- | Converts a small integer into a single byte.

i2W
  :: Int -> Word8

i2W x =  assert (x < 256) $ fromIntegral x         

-----------------------------------------------------------------------------

-- | Converts a list of 4 bytes into an integer.

w42I
  :: [Word8] -> Int

w42I bs =
  let [b0,b1,b2,b3] = map fromIntegral bs
  in assert (length bs == 4) $ b0 + 256 * (b1 + 256 * (b2 + 256 * b3))

-----------------------------------------------------------------------------

-- | Shortcut prcoedure to verify a given statement.

verify
  :: Bool -> String -> OP ()

verify b str =
  unless b $ do
    st <- get
    cleanup st
    error str

-----------------------------------------------------------------------------

-- | Safe error message.

sError
  :: String -> OP a

sError str = do
  st <- get
  cleanup st
  error str    

-----------------------------------------------------------------------------

-- | Opens a connection to a device and returns it's handle.    

getHandle
  :: OP DeviceHandle

getHandle = do
  st <- get
  case handle st of
    Just h  -> return h
    Nothing -> do
      h <- lift $ openDevice $ device st
      lift $ setConfig h (Just 1)
      lift $ claimInterface h (interfaceNumber $ dIface st)
      put st {
        handle = Just h,
        cleanup = do
          lift $ releaseInterface h (interfaceNumber $ dIface st)
          lift $ closeDevice h
          cleanup st
        }
      return h

-----------------------------------------------------------------------------

-- | Read some data from an endpoint.

readbulk
  :: EndpointAddress -> Int -> OP [Word8]

readbulk ep n = do
  h <- getHandle
  (bs,status) <- lift $ readBulk h ep n timeout
  case status of
    TimedOut  -> error "Read bulk timeout"
    Completed -> return $ unpack bs

-----------------------------------------------------------------------------

-- | Write some data from to endpoint.    

writebulk
  :: EndpointAddress -> [Word8] -> OP ()

writebulk ep bs = do
  h <- getHandle
  (s,status) <- lift $ writeBulk h ep (pack bs) timeout
  case status of
    TimedOut  -> error "Write bulk timeout"
    Completed -> when (s /= length bs) $ error "Write bulk incomplete"

-----------------------------------------------------------------------------

-- | Read some data from the Command-In endpoint.

readCmd
  :: Int -> OP [Word8]

readCmd n = do
  st <- get
  readbulk (endpointAddress $ dCmdIn st) n 

-----------------------------------------------------------------------------

-- | Write some data to the Command-Out endpoint.  

writeCmd
  :: [Word8] -> OP ()

writeCmd bs = do
  st <- get
  writebulk (endpointAddress $ dCmdOut st) $ bs 

-----------------------------------------------------------------------------
  
-- | Read some data from the Data-In endpoint.  

readData
  :: Int -> OP [Word8]

readData n = do
  st <- get
  readbulk (endpointAddress $ dDataIn st) n

-----------------------------------------------------------------------------
  
-- | Write some data to the Data-Out endpoint.    

writeData
  :: [Word8] -> OP ()

writeData bs = do
  st <- get
  writebulk (endpointAddress $ dDataOut st) bs 

-----------------------------------------------------------------------------

-- | Sends a command while ignoring the result.
  
pureSendCmd
  :: Command -> SubCommand -> [Word8] -> OP ()

pureSendCmd cmd sub bs = 
  let w = i2W $ length bs + 2
  in writeCmd $ w : cmd2W cmd : sub2W sub : bs

-----------------------------------------------------------------------------

-- | Sends a command and verifies the result.

sendCmd
  :: Command -> SubCommand -> [Word8] -> OP (CmdStatus, [Word8])

sendCmd cmd sub bs = do
    writeCmd (i2W (length bs + 2) : cmd2W cmd : sub2W sub : bs)
    xs <- readCmd 16

    case xs of
      []     -> error "Received message to small"
      size:rx -> do
      
        verify (fromIntegral size == length rx)
          "Incorrect response"
        
        case rx of
          []   -> error "Incorrect response"
          y:yr -> return (y,yr)        
 
-----------------------------------------------------------------------------

-- | Printing shortcut.

report
  :: String -> OP ()

report str = do
  st <- get
  unless (cQuiet $ config st) $ lift $ do
    putStr str
    hFlush stdout        

-----------------------------------------------------------------------------

-- | Printing shortcut including a newline.    

reportLn
  :: String -> OP ()

reportLn str = do
  st <- get
  unless (cQuiet $ config st) $ lift $ do
    putStrLn str
    hFlush stdout            

-----------------------------------------------------------------------------

-- | Debuggin shortcut.    

debug
  :: String -> OP ()

debug str = do
  st <- get
  when (cVerbose $ config st) $ lift $ do
    putStr str
    hFlush stdout            

-----------------------------------------------------------------------------
    
-- | Debuggin shortcut including a newline.

debugLn
  :: String -> OP ()

debugLn str = do
  st <- get
  when (cVerbose $ config st) $ lift $ do
    putStrLn str
    hFlush stdout            
  
-----------------------------------------------------------------------------
