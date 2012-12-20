{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.GLX.Raw.Core13.Types
-- Copyright   : (c) 2012 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- All types from the GLX 1.3 core
--
-----------------------------------------------------------------------------

module Graphics.X11.GLX.Raw.Core13.Types (
  -- * GLX token types
    GLXStringName
  , GLXErrorCode

  , GLXDrawableTypeMask
  , GLXRenderTypeMask
  , GLXEventMask
  , GLXPbufferClobberMask

  , GLXAttribute
  , GLXAttributeToken
  , GLXConfigCaveat
  , GLXTransparentType
  , GLXXVisualType
  , GLXRenderType
  , GLXPbufferClobberEventType
  , GLXPbufferClobberDrawableType

  -- * GLX types
  , GLXContext(..)
  , GLXPixmap
  , GLXDrawable

  , GLXFBConfig(..)
  , GLXFBConfigID
  , GLXContextID
  , GLXWindow
  , GLXPbuffer

  -- * X11 types
  , Display(..)
  , EventType
  , Font
  , Pixmap
  , ScreenNumber
  , VisualInfo
  , Window
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Graphics.X11.Types
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.VisualInfo

-- Not actual types, but useful for defining tokens
type GLXStringName = CInt
type GLXErrorCode = CInt

type GLXDrawableTypeMask = CInt
type GLXRenderTypeMask = CInt
type GLXEventMask = CUInt
type GLXPbufferClobberMask = CUInt

type GLXAttribute = CInt
type GLXAttributeToken = GLXAttribute
type GLXConfigCaveat = GLXAttribute
type GLXTransparentType = GLXAttribute
type GLXXVisualType = GLXAttribute
type GLXRenderType = CInt
type GLXPbufferClobberEventType = EventType
type GLXPbufferClobberDrawableType = CInt

-- Types
newtype GLXContext = GLXContext (Ptr GLXContext)
  deriving (Eq, Show, Storable)
type GLXDrawable = XID
type GLXPixmap = XID

newtype GLXFBConfig = GLXFBConfig (Ptr GLXFBConfig)
  deriving (Eq, Show, Storable)
type GLXFBConfigID = XID
type GLXContextID = XID
type GLXWindow = XID
type GLXPbuffer = XID
