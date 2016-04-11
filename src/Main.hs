-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
-- 
-- Main program.
-- 
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Numeric
  ( showHex
  )

import Data.Word
  ( Word8
  )

import Data.List
  ( sort
  , find
  )
  
import Data.Maybe
  ( catMaybes
  , isJust
  , fromJust
  )
  
import System.USB
  ( Device
  , ControlSetup(..)
  , Recipient(..)
  , RequestType(..)
  , USBException(..)
  , TransferDirection(..)  
  , newCtx
  , getDevices
  , getDeviceDesc
  , getConfigDesc    
  , deviceVendorId
  , deviceProductId
  , transferDirection
  , endpointAddress
  , interfaceEndpoints    
  , endpointNumber
  , readControlExact
  , deviceNumConfigs
  , configInterfaces  
  )
  
import System.Exit
  ( exitSuccess
  )
  
import System.Directory
  ( doesFileExist
  )
  
import System.Environment
  ( getArgs
  , getProgName  
  )
  
import Control.Monad.State
  ( when
  , unless
  , get
  , void
  , lift
  , liftM  
  , runStateT
  , execStateT  
  )
    
import Control.Monad.Trans.Maybe
  ( MaybeT
  , runMaybeT  
  )
  
import Control.Exception
  ( catch
  , assert
  )

import Data
  ( m25p10MemSize
  )  

-----------------------------------------------------------------------------

import qualified Data.Vector as V
import qualified Data.ByteString as B

-----------------------------------------------------------------------------

import Data
  ( MemoryOperation(..)
  , Configuration(..)
  , ST(..)
  , OP
  , timeout
  , vendorId
  , productId
  , boardType
  , defaultCfg  
  )
  
import Utils
  ( report
  , reportLn
  , debug
  , debugLn
  , verify
  , splitBlocks
  , getHandle
  , sError  
  )  

-----------------------------------------------------------------------------

import qualified SPI
import qualified VIO
import qualified GPIO 
import qualified M25P10 

-----------------------------------------------------------------------------

-- | Main program.

main
  :: IO ()

main = do
  c <- getCfg

  when (cHelp c) $ do
    prHelp
    exitSuccess

  devs <- findICE40Devices c

  if cList c then do
    
    unless (cQuiet c) $ do
      putStr "Looking for iCE40 devices: "
      if null devs then
        putStrLn "no device found"
      else if length devs == 1 then
        putStrLn "1 device found"
      else do
        putStr $ show $ length devs
        putStrLn " devices found"

    mapM_ (\(x,_) -> putStrLn ("  * " ++ x)) devs
    mapM_ (\(_,x) -> runStateT (get >>= cleanup) x) devs
    
  else case devs of
    []          -> 
      unless (cQuiet c) $ putStrLn "No device found."
    [(dId,st)] -> 
      if isJust (cId c) && dId /= fromJust (cId c) 
      then do
        void $ runStateT (cleanup st) st
        error ("No such device: " ++ fromJust (cId c))
      else do
        ste <- execStateT processDevice st
        void $ runStateT (cleanup ste) ste
    _     -> case cId c of
      Nothing -> do
        unless (cQuiet c) $ putStrLn "Multiple devices found:"
        mapM_ (\(x,_) -> putStrLn ("  * " ++ x)) devs
        mapM_ (\(_,x) -> runStateT (cleanup x) x) devs
        unless (cQuiet c) $ 
          putStrLn "Select a specific device using the \"-d\" option."
      Just dId -> case find (\(y,_) -> y == dId) devs of
          Nothing  -> do
            mapM_ (\(_,x) -> runStateT (cleanup x) x) devs
            error ("No such device: " ++ dId)
          Just (_,st) -> do
            let xs = filter (\(y,_) -> y /= dId) devs
            mapM_ (\(_,x) -> runStateT (cleanup x) x) xs
            ste <- execStateT processDevice st
            void $ runStateT (cleanup ste) ste

-----------------------------------------------------------------------------

processDevice
  :: OP ()

processDevice = do
  st <- get
  
  when (cErase (config st)) erase

  case cMemOpt (config st) of
    Nothing -> return ()
    Just mo -> case mo of
      
      ReadVIO r       -> do
        [x] <- VIO.readRegister r 1
        let s = showHex x ""
            
        if length s == 1 then
          lift $ putStrLn (" 0x0" ++ s)
        else
          lift $ putStrLn (" 0x" ++ s)        

      WriteVIO (r,d)  ->
        VIO.writeRegister r [d]

      VerifyVIO (r,d) -> do
        [x] <- VIO.readRegister r 1
        lift $ putStrLn $ if x /= d 
        then "Verification failed."
        else "Verification successful."

      ReadFLASH fp        -> do
        readImage fp

      WriteFLASH fp      -> do
        bs <- filecontent fp
        flashFile bs

      VerifyFLASH fp     -> do
        bs <- filecontent fp
        verifyFile bs

  where
    filecontent file = do
      b <- lift $ doesFileExist file
      unless b $ error ("File does not exist: " ++ file)
      c <- lift $ B.readFile file
      return $ B.unpack c
  
-----------------------------------------------------------------------------    

connectDevice
  :: OP ()

connectDevice = do
  report "Connecting to device ..."
  
  GPIO.reset

  SPI.setSpeed 4000000
  SPI.setMode 0x00

  M25P10.wakeup
  
  (mId, tId, ca) <- M25P10.getId
  verify (mId == 0x20 && tId == 0x20 && ca == 0x11)
    "\nIncompatible Memory Device (M25P10 expected)"

  reportLn " DONE"

-----------------------------------------------------------------------------    

erase 
  :: OP ()

erase = do
  connectDevice 

  report "Erasing flash ..."

  M25P10.chipErase

  reportLn " DONE"

-----------------------------------------------------------------------------

flashFile
  :: [Word8] -> OP ()

flashFile bs = do
  connectDevice

  st <- get
  unless (cErase (config st)) erase

  report "Writing image ..."
  
  bs' <- align bs

  let
    xs = splitBlocks 256 bs'
    ys = zip [0,256..(length xs - 1) * 256] xs

  mapM_ (uncurry M25P10.pageProgram) ys

  reportLn " DONE"

  verifyFile bs'

  where
    align (0x7E:0xAA:0x99:0x7E:_) = return bs
    align (0xFF:0x00:xr) = skip xr
    align _ = do
      verify False " FAILED\n Invalid file format."
      return bs

    skip (0x00:0xFF:xr) = return xr
    skip (_:xr) = skip xr
    skip _ = do
      verify False " FAILED\n Invalid file format."
      return bs

-----------------------------------------------------------------------------

readImage
  :: FilePath -> OP ()

readImage fp = do
  connectDevice

  report "Reading image ..."

  rs <- readBlockwise [] 0 [] 0

  lift $ B.writeFile fp (B.pack rs)   

  reportLn " DONE"

  where
    invalid = sError " FAILED\n No valid image on device."

    readBlockwise :: [Word8] -> Int -> [Word8] -> Int -> OP [Word8]
    readBlockwise a i overlap csec = do
      verify (i < m25p10MemSize) " FAILED\n No valid image on device."
      bs <- M25P10.readContent i 256
      (rs,o,c,done) <- validate [] (overlap ++ bs) csec
      if done then
        return $ reverse $ rs ++ a
      else
        readBlockwise (rs ++ a) (i + 256) o c

    validate :: [Word8] -> [Word8] -> Int -> OP ([Word8], [Word8], Int, Bool)
    validate a (0x7E:0xAA:0x99:0x7E:br) 0 = validate (0x7E:0x99:0xAA:0x7E:a) br 3
    validate a (0xFF:0x00:br)           0 = validate (0x00:0xFF:a) br 1
    validate _ _                        0 = invalid

    validate a (0x00:0xFF:br) 1 = validate (0xFF:0x00:a) br 2
    validate a [0x00]         1 = return (a,[0x00],1,False)
    validate a []             1 = return (a,[],1,False)
    validate a (b:br)         1 = validate (b:a) br 1

    validate a (0x7E:0xAA:0x99:0x7E:br) 2 = validate (0x7E:0x99:0xAA:0x7E:a) br 3
    validate a [0x7E,0xAA,0x99]         2 = return (a,[0x7e,0xAA,0x99],2,False)
    validate a [0x7E,0xAA]              2 = return (a,[0x7e,0xAA],2,False)
    validate a [0x7E]                   2 = return (a,[0x7e],2,False)
    validate a []                       2 = return (a,[],2,False)
    validate _ _                        2 = invalid

    validate a (c:br) _ 
      | length br < fromIntegral c `mod` 16 = return (a,c:br,3,False)
      | c == 0x01 && head br == 0x06           = return (0x06:0x01:a,[],3,True)
      | otherwise                           = 
          let (rs,ls) = splitAt (fromIntegral c `mod` 16) br
          in validate (reverse rs ++ (c:a)) ls 3 
    validate a [] _ = return (a,[],3,False)

-----------------------------------------------------------------------------  

verifyFile
  :: [Word8] -> OP ()

verifyFile bs = do
  connectDevice
  
  report "Verifying ..."

  rs <- M25P10.readContent 0x00 $ length bs

  st <- get
  if cQuiet (config st) then
    when (rs /= bs) $ do
      cleanup st
      verificationFault rs
  else  
    verify (rs == bs) " FAILED"

  reportLn " DONE"

  where
    verificationFault rs 
      | length rs < length bs = 
        error "Verification failed (read image smaller than written image)"
      | length rs > length bs = 
        error "Verification failed (read image larger than written image)"
      | otherwise = 
        let cs = zip3 [0,1..length rs] rs bs 
        in case find (\(_,a,b) -> a /= b) cs of
          Nothing      -> error "strange"
          Just (i,a,b) -> 
            error $ "Verification failed ([" ++ show i ++ "] " 
                    ++ show a ++ " != " ++ show b ++ ")"

-----------------------------------------------------------------------------  

ctrlBoardType
  :: OP (Maybe String)

ctrlBoardType = 
  let cBrdType = ControlSetup Standard ToDevice 0xE2 0x00 0x00
  in do
    h <- getHandle
    lift $ catch
      (liftM (Just . filter (/= '"') . show . B.takeWhile (/= 0)) $
       readControlExact h cBrdType 8 timeout)
      ((\_ -> return Nothing) :: USBException -> IO (Maybe String))

-----------------------------------------------------------------------------    

ctrlSerial
  :: OP (Maybe String)

ctrlSerial =
  let cSerial = ControlSetup Standard ToDevice 0xE4 0x00 0x00
  in do
    h <- getHandle
    lift $ catch
      (liftM (Just . ('D' :) . drop 6 . filter (/= '"') .
              show . B.takeWhile (/= 0)) $
       readControlExact h cSerial 12 timeout)
      ((\ _ -> return Nothing) :: USBException -> IO (Maybe String))

-----------------------------------------------------------------------------

initDevice
  :: Configuration -> Device -> MaybeT IO ST

initDevice c dev = do
  lift $ when (cVerbose c ) $ putStr
    "  Checking interface ..."
  
  dDesc <- lift $ getDeviceDesc dev
  
  checkthat (deviceNumConfigs dDesc == 1)
    "Device has no configuration" 
  
  cDesc <- lift $ getConfigDesc dev 0

  let iifaces = V.toList $ configInterfaces cDesc
      
  checkthat (length iifaces == 1)
    "Device has no inteface" 
  
  let ifaces = V.toList $ head  iifaces
  checkthat (length ifaces == 1)
    "Device has no inteface" 
  
  let
    iDesc = head ifaces
    endpoints = V.toList $ interfaceEndpoints iDesc
  
  checkthat (length endpoints == 4)
    "Invalid number of endpoints" 

  checkthat ([1,2,3,4] == sort (map endpoint endpoints))
    "Some Endpoint is missing"

  let
    cmdIn = findEp 2 endpoints
    cmdOut = findEp 1 endpoints
    dataIn = findEp 4 endpoints
    dataOut = findEp 3 endpoints
    
  checkthat (tdir cmdIn == In)
    "Invalid transfer direction of endpoint 2"
    
  checkthat (tdir cmdOut == Out)
    "Invalid transfer direction of endpoint 1" 
    
  checkthat (tdir dataIn == In)
    "Invalid transfer direction of endpoint 4"
    
  checkthat (tdir dataOut == Out)
    "Invalid transfer direction of endpoint 3"

  lift $ when (cVerbose c ) $ putStrLn "  VALID"

  return ST {
    config = c,
    device = dev,
    handle = Nothing,
    fCommOpen = False,
    fGPIOOpen = False,
    fReset = False,
    fSPIPort = Nothing,
    dDevice = dDesc,
    dConfig = cDesc,
    dIface = iDesc,
    dCmdIn = cmdIn,
    dCmdOut = cmdOut,
    dDataIn = dataIn, 
    dDataOut = dataOut,
    cleanup = return ()
    }

  where
    tdir = transferDirection . endpointAddress
    
    endpoint = endpointNumber . endpointAddress
    
    findEp x = fromJust . find ((x ==) . endpoint)
    
    checkthat b str = do
      when (not b && cVerbose c) $ lift $ do
        putStr "  "
        putStr str
        putStrLn ". Skipping."
      unless b $ fail ""

-----------------------------------------------------------------------------     

findICE40Devices
  :: Configuration -> IO [(String,ST)]

findICE40Devices c = do
  ctx <- newCtx
  devs <- getDevices ctx
  devs' <- mapM isICE40USB $ V.toList devs
  return $ catMaybes devs'
  
  where
    isICE40USB dev = do
      desc <- getDeviceDesc dev
  
      if idToStr (deviceVendorId desc) /= vendorId ||
         idToStr (deviceProductId desc) /= productId then
        return Nothing
      else do
        when (cVerbose c) $ putStrLn 
          "Found potential iCE40 device."

        msti <- runMaybeT $ initDevice c dev

        case msti of
          Nothing  -> return Nothing
          Just sti -> do
            (info, ste) <- runStateT getDevInfo sti
 
            case info of
              Nothing -> do
                void $ execStateT (cleanup ste) ste
                return Nothing
              Just y  -> do
                when (cVerbose c) (putStrLn "")
                return $ Just (y,ste)

    getDevInfo = do
      debug "  Checking board type ..."

      btype <- ctrlBoardType
      case btype of
        Nothing -> do
          debugLn " Cannot read board id. Skipping."
          return Nothing
        Just x  -> 
          if x /= boardType then do
            debugLn " Missmatch. Skipping." 
            return Nothing
          else do
            debugLn " VALID"
            debug "  Reading serial id ..."
            serial <- ctrlSerial
 
            debugLn $ case serial of
              Nothing -> " Cannot read Serial. Skipping."
              Just _  -> "   VALID"

            return serial

    idToStr x =
      let str = showHex x ""
      in case length str of
        1 -> "000" ++ str
        2 -> "00" ++ str
        3 -> "0" ++ str
        _ -> str

-----------------------------------------------------------------------------

-- | Configuration parser.

getCfg
  :: IO Configuration

getCfg = do
  args <- getArgs

  if null args then
    return $ defaultCfg { cHelp = True }
    
  else case parseArguments defaultCfg args of
    Left err -> error err
    Right c -> return c

  where
    parseArguments a xs = case xs of
      x:y:xr -> case parseArgument a x $ Just y of
        Right (Right z) -> parseArguments z xr
        Right (Left z)  -> parseArguments z (y:xr)
        Left z          ->  Left z
      [x]    -> case parseArgument a x Nothing of
        Right (Right z) -> Right z        
        Right (Left z)  -> Right z
        Left z          -> Left z
      []     -> Right a

    parseArgument a argument next = case argument of
      "-h" -> simple $ a { cHelp = True }
      "-l" -> simple $ a { cList = True }
      "-e" -> simple $ a { cErase = True }
      "-v" -> simple $ a { cVerbose = True, cQuiet = False }
      "-q" -> simple $ a { cQuiet = True, cVerbose = False }
      "-d" -> case next of
        Nothing -> Left "[-d] Missing device ID."
        Just x  -> arg $ a { cId = Just x }
      "-U" -> case next of
        Nothing -> Left "[-U] Missing specification."
        Just sp -> case break (== ':') sp of
          ([],_)      -> Left ("[-U] Invalid format: " ++ sp)
          (c1,':':ct) -> case break (== ':') ct of
            ([],_)      -> Left ("[-U] Invalid format: " ++ sp)
            (_,[])      -> Left ("[-U] Invalid format: " ++ sp)
            (c2,':':c3) -> case c1 of
              "vio" -> case c2 of
                "r" -> arg $ a { cMemOpt = return $ ReadVIO $ read c3 }
                "w" -> case pp c3 of
                  Left x -> Left x
                  Right c -> arg $ a { cMemOpt = return $ WriteVIO c }
                "v" -> case pp c3 of
                  Left x -> Left x
                  Right c -> arg $ a { cMemOpt = return $ VerifyVIO c }
                x   -> Left ("[-U] Invaid operation: " ++ x)
              "flash"  -> case c2 of
                "r" -> arg $ a { cMemOpt = return $ ReadFLASH c3 }
                "w" -> arg $ a { cMemOpt = return $ WriteFLASH c3 }
                "v" -> arg $ a { cMemOpt = return $ VerifyFLASH c3 }                
                x   -> Left ("[-U] Invaid operation: " ++ x)
              x -> Left ("[-U] Unsupported memory type: " ++ x)
            _ -> assert False undefined
          _ -> assert False undefined
      x -> Left ("Invalid argument: " ++ x)
                            
    simple = Right . Left
    arg    = Right . Right

    pp x = case break (== '#') x of
      ([],y)    -> Left ("[-U] Invalid format: " ++ y)
      (y,'#':z) -> Right (read y, read z)
      _         -> assert False undefined
 
-----------------------------------------------------------------------------

-- | Print usage information.

prHelp
  :: IO ()

prHelp = do
  toolname <- getProgName
  mapM_ putStrLn 
    [ "Usage: " ++ toolname ++ " [OPTIONS]..."
    , ""
    , "A programming tool for the iCE40 FPGA evaluation boards produced by"
    , "LATTICE Semiconductor."
    , ""
    , "  -l,                             : List all supported devices."
    , "  -d <id>                         : Select a specific device."
    , "  -U <memtype>:r|w|v:<data>       : Memory operation specification."
    , ""
    , "    Supported memory types are:"
    , ""
    , "      vio      : Digilent Virtual I/O Expansion. One can read and write"
    , "                 registers of the interface. There corresponding meaning,"
    , "                 however, is device specific and, thus, not interpreted"
    , "                 by this tool"
    , ""  
    , "      flash    : The SPI PROM of the device."
    , ""
    , "    Supported operations are:"
    , ""
    , "      r        : Read device memory. If VIO is selected, then <data>"
    , "                 has to be a single one byte address, e.g., '0x4F'."
    , "                 If the flash ROM is selected, <data> has to be a "
    , "                 file path to which the read binary is written."
    , ""  
    , "      w        : Write device memory. If VIO is selected, then <data>"
    , "                 has to be of the form <addr>#<byte>, where both"
    , "                 <addr> and <byte> are given by a single byte."
    , "                 If the flash ROM is selected, <data> has to be the"
    , "                 file path to the to be programmed binary."
    , ""  
    , "      v        : Verify the device memory. The format of <data> is"
    , "                 similar to the one used for writing to the memory."
    , ""
    , "  -e,                             : Perform a chip erease."
    , "  -h,                             : Print this help and exit."
    , "  -v,                             : Verbose output."
    , "  -q,                             : Quiet output."
    , ""
    , "Tested on iCEblink40-HX1K Evaluation Kit."  
    ]  

----------------------------------------------------------------------------- 
