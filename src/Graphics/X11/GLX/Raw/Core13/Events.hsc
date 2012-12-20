{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.GLX.Raw.Core13.Events
-- Copyright   : (c) 2012 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- All events from the GLX 1.3 core
--
-----------------------------------------------------------------------------

module Graphics.X11.GLX.Raw.Core13.Events (
  -- * Event constants
    glx_PbufferClobber

  -- * Event types
  , GLXPbufferClobberEvent(..)
  ) where

import Control.Applicative
import Foreign.C
import Foreign.Storable

import Graphics.X11.GLX.Raw.Core13.Types

#include <GL/glx.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct { char x; t (y); }, y)

-- Event constants
glx_PbufferClobber :: EventType
glx_PbufferClobber = #const GLX_PbufferClobber

-- Event types
data GLXPbufferClobberEvent = GLXPbufferClobberEvent
  { glxPbufferClobberEvent_eventType :: EventType
  , glxPbufferClobberEvent_drawType :: GLXPbufferClobberDrawableType
  , glxPbufferClobberEvent_serial :: CULong
  , glxPbufferClobberEvent_sendEvent :: Bool
  , glxPbufferClobberEvent_display :: Display
  , glxPbufferClobberEvent_drawable :: GLXDrawable
  , glxPbufferClobberEvent_bufferMask :: GLXPbufferClobberMask
  , glxPbufferClobberEvent_auxBuffer :: CUInt
  , glxPbufferClobberEvent_x :: CInt
  , glxPbufferClobberEvent_y :: CInt
  , glxPbufferClobberEvent_width :: CInt
  , glxPbufferClobberEvent_height :: CInt
  , glxPbufferClobberEvent_count :: CInt
  }
  deriving (Show)

instance Storable GLXPbufferClobberEvent where
  sizeOf _ = #size GLXPbufferClobberEvent
  alignment _ = #alignment GLXPbufferClobberEvent
  peek p = GLXPbufferClobberEvent
    <$> #{peek GLXPbufferClobberEvent, event_type} p
    <*> #{peek GLXPbufferClobberEvent, draw_type} p
    <*> #{peek GLXPbufferClobberEvent, serial} p
    <*> #{peek GLXPbufferClobberEvent, send_event} p
    <*> (Display <$> #{peek GLXPbufferClobberEvent, display} p)
    <*> #{peek GLXPbufferClobberEvent, drawable} p
    <*> #{peek GLXPbufferClobberEvent, buffer_mask} p
    <*> #{peek GLXPbufferClobberEvent, aux_buffer} p
    <*> #{peek GLXPbufferClobberEvent, x} p
    <*> #{peek GLXPbufferClobberEvent, y} p
    <*> #{peek GLXPbufferClobberEvent, width} p
    <*> #{peek GLXPbufferClobberEvent, height} p
    <*> #{peek GLXPbufferClobberEvent, count} p
  poke = undefined
