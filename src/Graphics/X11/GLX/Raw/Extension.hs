-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.GLX.Raw.Extension
-- Copyright   : (c) 2013 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- Internal support routines for GLX extensions
--
-----------------------------------------------------------------------------

module Graphics.X11.GLX.Raw.Extension (
    getProcAddress
  ) where

import Foreign.C
import Foreign.Marshal.Error
import Foreign.Ptr

import Graphics.X11.GLX.Raw.ARB.GetProcAddress

-- | Get a pointer to a GLX function
getProcAddress :: String -> IO (FunPtr a)
getProcAddress procName = withCString procName
  $ throwIf (== nullFunPtr) (const msg) . glXGetProcAddressARB
  where
    msg = "couldn't get the address for " ++ procName
