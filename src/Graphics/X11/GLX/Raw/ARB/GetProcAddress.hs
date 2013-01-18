{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.GLX.Raw.ARB.GetProcAddress
-- Copyright   : (c) 2013 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- All raw functions, tokens, and types for the ARB_get_proc_address extension
-- (see <http://www.opengl.org/registry/specs/ARB/get_proc_address.txt>)
--
-----------------------------------------------------------------------------

module Graphics.X11.GLX.Raw.ARB.GetProcAddress (
  -- * Functions
    glXGetProcAddressARB
  ) where

import Foreign.C
import Foreign.Ptr

-- Functions
foreign import ccall unsafe "glXGetProcAddressARB" glXGetProcAddressARB
  :: CString -> IO (FunPtr a)
