-----------------------------------------------------------------------------
-- |
-- Module      :  GPIO
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- General Purpose Input / Output Interface.
--
-----------------------------------------------------------------------------

module GPIO
  ( reset
  , releaseReset
  ) where

-----------------------------------------------------------------------------

import Control.Monad
  ( when
  , unless
  )

import Control.Monad.State
  ( get
  , put
  )

import Control.Exception
  ( assert
  )

-----------------------------------------------------------------------------

import Data
  ( ST(..)
  , OP
  )

import Commands
  ( Command(..)
  , SubCommand(..)
  )

import Utils
  ( sendCmd
  , pureSendCmd
  , verify
  )

-----------------------------------------------------------------------------

-- | Set the reset signal to reset the device.

reset
  :: OP ()

reset = do
  initInterface

  (sc,_) <- sendCmd GPIO DIR [0x00, 0x01, 0x00, 0x00, 0x00]
  verify (sc == 0)
    "Setting direction failed"

  pureSendCmd GPIO VALUE [0x00, 0x00, 0x00, 0x00, 0x00]

  st <- get
  put st {
    fReset = True,
    cleanup = do
      releaseReset
      cleanup st
    }

-----------------------------------------------------------------------------

-- | Release the reset signal to restart the device.

releaseReset
  :: OP ()

releaseReset = do
  st <- get
  assert (fGPIOOpen st) $ return ()

  when (fReset st) $ do
    (sc,_) <- sendCmd GPIO DIR [0x00, 0x00, 0x00, 0x00, 0x00]
    verify (sc == 0)
      "Setting direction failed"

    put st {
      fReset = False
      }

-----------------------------------------------------------------------------

initInterface
  :: OP ()

initInterface = do
  st <- get
  unless (fGPIOOpen st) $ do
    pureSendCmd GPIO OPEN [0x00]

    put st {
      fGPIOOpen = True,
      cleanup = do
        (sc, y) <- sendCmd GPIO CLOSE [0x00]
        verify (sc == 0 && null y)
          "Closing SPI port failed"
        cleanup st
      }

-----------------------------------------------------------------------------
