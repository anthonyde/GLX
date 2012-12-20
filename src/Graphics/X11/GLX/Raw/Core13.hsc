{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.GLX.Raw.Core13
-- Copyright   : (c) 2012 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- All raw functions, tokens, types, and events from the GLX 1.3 core
--
-----------------------------------------------------------------------------

module Graphics.X11.GLX.Raw.Core13 (
  -- * Types
    module Graphics.X11.GLX.Raw.Core13.Types

  -- * Events
  , module Graphics.X11.GLX.Raw.Core13.Events

  -- * Tokens
  -- ** String names
  , glx_VENDOR
  , glx_VERSION
  , glx_EXTENSIONS

  -- ** Error codes
  , glx_BAD_SCREEN
  , glx_BAD_ATTRIBUTE
  , glx_NO_EXTENSION
  , glx_BAD_VISUAL
  , glx_BAD_CONTEXT
  , glx_BAD_VALUE
  , glx_BAD_ENUM

  -- ** Drawable type masks
  , glx_WINDOW_BIT
  , glx_PIXMAP_BIT
  , glx_PBUFFER_BIT

  -- ** Render type masks
  , glx_RGBA_BIT
  , glx_COLOR_INDEX_BIT

  -- ** Event masks
  , glx_PBUFFER_CLOBBER_MASK

  -- ** PbufferClobberEvent buffer masks
  , glx_FRONT_LEFT_BUFFER_BIT
  , glx_FRONT_RIGHT_BUFFER_BIT
  , glx_BACK_LEFT_BUFFER_BIT
  , glx_BACK_RIGHT_BUFFER_BIT
  , glx_AUX_BUFFERS_BIT
  , glx_DEPTH_BUFFER_BIT
  , glx_STENCIL_BUFFER_BIT
  , glx_ACCUM_BUFFER_BIT

  -- ** Attributes
  , glx_USE_GL
  , glx_BUFFER_SIZE
  , glx_LEVEL
  , glx_RGBA
  , glx_DOUBLEBUFFER
  , glx_STEREO
  , glx_AUX_BUFFERS
  , glx_RED_SIZE
  , glx_GREEN_SIZE
  , glx_BLUE_SIZE
  , glx_ALPHA_SIZE
  , glx_DEPTH_SIZE
  , glx_STENCIL_SIZE
  , glx_ACCUM_RED_SIZE
  , glx_ACCUM_GREEN_SIZE
  , glx_ACCUM_BLUE_SIZE
  , glx_ACCUM_ALPHA_SIZE
  , glx_CONFIG_CAVEAT
  , glx_X_VISUAL_TYPE
  , glx_TRANSPARENT_TYPE
  , glx_TRANSPARENT_INDEX_VALUE
  , glx_TRANSPARENT_RED_VALUE
  , glx_TRANSPARENT_GREEN_VALUE
  , glx_TRANSPARENT_BLUE_VALUE
  , glx_TRANSPARENT_ALPHA_VALUE
  , glx_VISUAL_ID
  , glx_SCREEN
  , glx_DRAWABLE_TYPE
  , glx_RENDER_TYPE
  , glx_X_RENDERABLE
  , glx_FBCONFIG_ID
  , glx_MAX_PBUFFER_WIDTH
  , glx_MAX_PBUFFER_HEIGHT
  , glx_MAX_PBUFFER_PIXELS
  , glx_PRESERVED_CONTENTS
  , glx_LARGEST_PBUFFER
  , glx_WIDTH
  , glx_HEIGHT
  , glx_EVENT_MASK
  , glx_PBUFFER_HEIGHT
  , glx_PBUFFER_WIDTH

  -- ** Generic attribute values
  , glx_NONE
  , glx_DONT_CARE

  -- ** Config caveats
  , glx_SLOW_CONFIG
  , glx_NON_CONFORMANT_CONFIG

  -- ** Transparent types
  , glx_TRANSPARENT_RGB
  , glx_TRANSPARENT_INDEX

  -- ** X visual types
  , glx_TRUE_COLOR
  , glx_DIRECT_COLOR
  , glx_PSEUDO_COLOR
  , glx_STATIC_COLOR
  , glx_GRAY_SCALE
  , glx_STATIC_GRAY

  -- ** Render types
  , glx_RGBA_TYPE
  , glx_COLOR_INDEX_TYPE

  -- ** PbufferClobberEvent event types
  , glx_DAMAGED
  , glx_SAVED

  -- ** PbufferClobberEvent drawable types
  , glx_WINDOW
  , glx_PBUFFER

  -- * Functions
  , glXChooseVisual
  , glXCreateContext
  , glXDestroyContext
  , glXMakeCurrent
  , glXCopyContext
  , glXSwapBuffers
  , glXCreateGLXPixmap
  , glXDestroyGLXPixmap
  , glXQueryExtension
  , glXQueryVersion
  , glXIsDirect
  , glXGetConfig
  , glXGetCurrentContext
  , glXGetCurrentDrawable
  , glXWaitGL
  , glXWaitX
  , glXUseXFont

  -- ** GLX 1.1 and later
  , glXQueryExtensionsString
  , glXQueryServerString
  , glXGetClientString

  -- ** GLX 1.2 and later
  , glXGetCurrentDisplay

  -- ** GLX 1.3 and later
  , glXChooseFBConfig
  , glXGetFBConfigAttrib
  , glXGetFBConfigs
  , glXGetVisualFromFBConfig
  , glXCreateWindow
  , glXDestroyWindow
  , glXCreatePixmap
  , glXDestroyPixmap
  , glXCreatePbuffer
  , glXDestroyPbuffer
  , glXQueryDrawable
  , glXCreateNewContext
  , glXMakeContextCurrent
  , glXGetCurrentReadDrawable
  , glXQueryContext
  , glXSelectEvent
  , glXGetSelectedEvent
  ) where

import Foreign.C
import Foreign.Ptr

import Graphics.X11.GLX.Raw.Core13.Events
import Graphics.X11.GLX.Raw.Core13.Types

#include <GL/glx.h>

-- Tokens
-- String names
#{enum GLXStringName,
  , glx_VENDOR = GLX_VENDOR
  , glx_VERSION = GLX_VERSION
  , glx_EXTENSIONS = GLX_EXTENSIONS
  }

-- Error codes
#{enum GLXErrorCode,
  , glx_BAD_SCREEN = GLX_BAD_SCREEN
  , glx_BAD_ATTRIBUTE = GLX_BAD_ATTRIBUTE
  , glx_NO_EXTENSION = GLX_NO_EXTENSION
  , glx_BAD_VISUAL = GLX_BAD_VISUAL
  , glx_BAD_CONTEXT = GLX_BAD_CONTEXT
  , glx_BAD_VALUE = GLX_BAD_VALUE
  , glx_BAD_ENUM = GLX_BAD_ENUM
  }

-- Drawable type masks
#{enum GLXDrawableTypeMask,
  , glx_WINDOW_BIT = GLX_WINDOW_BIT
  , glx_PIXMAP_BIT = GLX_PIXMAP_BIT
  , glx_PBUFFER_BIT = GLX_PBUFFER_BIT
  }

-- Render type masks
#{enum GLXRenderTypeMask,
  , glx_RGBA_BIT = GLX_RGBA_BIT
  , glx_COLOR_INDEX_BIT = GLX_COLOR_INDEX_BIT
  }

-- Event masks
#{enum GLXEventMask,
  , glx_PBUFFER_CLOBBER_MASK = GLX_PBUFFER_CLOBBER_MASK
  }

-- PbufferClobberEvent buffer masks
#{enum GLXPbufferClobberMask,
  , glx_FRONT_LEFT_BUFFER_BIT = GLX_FRONT_LEFT_BUFFER_BIT
  , glx_FRONT_RIGHT_BUFFER_BIT = GLX_FRONT_RIGHT_BUFFER_BIT
  , glx_BACK_LEFT_BUFFER_BIT = GLX_BACK_LEFT_BUFFER_BIT
  , glx_BACK_RIGHT_BUFFER_BIT = GLX_BACK_RIGHT_BUFFER_BIT
  , glx_AUX_BUFFERS_BIT = GLX_AUX_BUFFERS_BIT
  , glx_DEPTH_BUFFER_BIT = GLX_DEPTH_BUFFER_BIT
  , glx_STENCIL_BUFFER_BIT = GLX_STENCIL_BUFFER_BIT
  , glx_ACCUM_BUFFER_BIT = GLX_ACCUM_BUFFER_BIT
  }

-- Attributes
#{enum GLXAttributeToken,
  , glx_USE_GL = GLX_USE_GL
  , glx_BUFFER_SIZE = GLX_BUFFER_SIZE
  , glx_LEVEL = GLX_LEVEL
  , glx_RGBA = GLX_RGBA
  , glx_DOUBLEBUFFER = GLX_DOUBLEBUFFER
  , glx_STEREO = GLX_STEREO
  , glx_AUX_BUFFERS = GLX_AUX_BUFFERS
  , glx_RED_SIZE = GLX_RED_SIZE
  , glx_GREEN_SIZE = GLX_GREEN_SIZE
  , glx_BLUE_SIZE = GLX_BLUE_SIZE
  , glx_ALPHA_SIZE = GLX_ALPHA_SIZE
  , glx_DEPTH_SIZE = GLX_DEPTH_SIZE
  , glx_STENCIL_SIZE = GLX_STENCIL_SIZE
  , glx_ACCUM_RED_SIZE = GLX_ACCUM_RED_SIZE
  , glx_ACCUM_GREEN_SIZE = GLX_ACCUM_GREEN_SIZE
  , glx_ACCUM_BLUE_SIZE = GLX_ACCUM_BLUE_SIZE
  , glx_ACCUM_ALPHA_SIZE = GLX_ACCUM_ALPHA_SIZE
  , glx_CONFIG_CAVEAT = GLX_CONFIG_CAVEAT
  , glx_X_VISUAL_TYPE = GLX_X_VISUAL_TYPE
  , glx_TRANSPARENT_TYPE = GLX_TRANSPARENT_TYPE
  , glx_TRANSPARENT_INDEX_VALUE = GLX_TRANSPARENT_INDEX_VALUE
  , glx_TRANSPARENT_RED_VALUE = GLX_TRANSPARENT_RED_VALUE
  , glx_TRANSPARENT_GREEN_VALUE = GLX_TRANSPARENT_GREEN_VALUE
  , glx_TRANSPARENT_BLUE_VALUE = GLX_TRANSPARENT_BLUE_VALUE
  , glx_TRANSPARENT_ALPHA_VALUE = GLX_TRANSPARENT_ALPHA_VALUE
  , glx_VISUAL_ID = GLX_VISUAL_ID
  , glx_SCREEN = GLX_SCREEN
  , glx_DRAWABLE_TYPE = GLX_DRAWABLE_TYPE
  , glx_RENDER_TYPE = GLX_RENDER_TYPE
  , glx_X_RENDERABLE = GLX_X_RENDERABLE
  , glx_FBCONFIG_ID = GLX_FBCONFIG_ID
  , glx_MAX_PBUFFER_WIDTH = GLX_MAX_PBUFFER_WIDTH
  , glx_MAX_PBUFFER_HEIGHT = GLX_MAX_PBUFFER_HEIGHT
  , glx_MAX_PBUFFER_PIXELS = GLX_MAX_PBUFFER_PIXELS
  , glx_PRESERVED_CONTENTS = GLX_PRESERVED_CONTENTS
  , glx_LARGEST_PBUFFER = GLX_LARGEST_PBUFFER
  , glx_WIDTH = GLX_WIDTH
  , glx_HEIGHT = GLX_HEIGHT
  , glx_EVENT_MASK = GLX_EVENT_MASK
  , glx_PBUFFER_HEIGHT = GLX_PBUFFER_HEIGHT
  , glx_PBUFFER_WIDTH = GLX_PBUFFER_WIDTH
  }

-- Generic attribute values
glx_NONE :: GLXAttribute
glx_NONE = #const GLX_NONE

glx_DONT_CARE :: GLXAttribute
glx_DONT_CARE = #const GLX_DONT_CARE

-- Config caveats
#{enum GLXConfigCaveat,
  , glx_SLOW_CONFIG = GLX_SLOW_CONFIG
  , glx_NON_CONFORMANT_CONFIG = GLX_NON_CONFORMANT_CONFIG
  }

-- Transparent types
#{enum GLXTransparentType,
  , glx_TRANSPARENT_RGB = GLX_TRANSPARENT_RGB
  , glx_TRANSPARENT_INDEX = GLX_TRANSPARENT_INDEX
  }

-- X visual types
#{enum GLXXVisualType,
  , glx_TRUE_COLOR = GLX_TRUE_COLOR
  , glx_DIRECT_COLOR = GLX_DIRECT_COLOR
  , glx_PSEUDO_COLOR = GLX_PSEUDO_COLOR
  , glx_STATIC_COLOR = GLX_STATIC_COLOR
  , glx_GRAY_SCALE = GLX_GRAY_SCALE
  , glx_STATIC_GRAY = GLX_STATIC_GRAY
  }

-- Render types
#{enum GLXRenderType,
  , glx_RGBA_TYPE = GLX_RGBA_TYPE
  , glx_COLOR_INDEX_TYPE = GLX_COLOR_INDEX_TYPE
  }

-- PbufferClobberEvent event types
#{enum GLXPbufferClobberEventType,
  , glx_DAMAGED = GLX_DAMAGED
  , glx_SAVED = GLX_SAVED
  }

-- PbufferClobberEvent drawable types
#{enum GLXPbufferClobberDrawableType,
  , glx_WINDOW = GLX_WINDOW
  , glx_PBUFFER = GLX_PBUFFER
  }

-- Functions
foreign import ccall unsafe "glXChooseVisual" glXChooseVisual
  :: Display -> ScreenNumber -> Ptr GLXAttribute -> IO (Ptr VisualInfo)

foreign import ccall unsafe "glXCreateContext" glXCreateContext
  :: Display -> Ptr VisualInfo -> GLXContext -> Bool -> IO GLXContext

foreign import ccall unsafe "glXDestroyContext" glXDestroyContext
  :: Display -> GLXContext -> IO ()

foreign import ccall unsafe "glXMakeCurrent" glXMakeCurrent
  :: Display -> GLXDrawable -> GLXContext -> IO Bool

foreign import ccall unsafe "glXCopyContext" glXCopyContext
  :: Display -> GLXContext -> GLXContext -> CULong -> IO ()

foreign import ccall unsafe "glXSwapBuffers" glXSwapBuffers
  :: Display -> GLXDrawable -> IO ()

foreign import ccall unsafe "glXCreateGLXPixmap" glXCreateGLXPixmap
  :: Display -> Ptr VisualInfo -> Pixmap -> IO GLXPixmap

foreign import ccall unsafe "glXDestroyGLXPixmap" glXDestroyGLXPixmap
  :: Display -> GLXPixmap -> IO ()

foreign import ccall unsafe "glXQueryExtension" glXQueryExtension
  :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall unsafe "glXQueryVersion" glXQueryVersion
  :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall unsafe "glXIsDirect" glXIsDirect
  :: Display -> GLXContext -> IO Bool

foreign import ccall unsafe "glXGetConfig" glXGetConfig
  :: Display -> Ptr VisualInfo -> GLXAttributeToken -> Ptr GLXAttribute
  -> IO GLXErrorCode

foreign import ccall unsafe "glXGetCurrentContext" glXGetCurrentContext
  :: IO GLXContext

foreign import ccall unsafe "glXGetCurrentDrawable" glXGetCurrentDrawable
  :: IO GLXDrawable

foreign import ccall unsafe "glXWaitGL" glXWaitGL
  :: IO ()

foreign import ccall unsafe "glXWaitX" glXWaitX
  :: IO ()

foreign import ccall unsafe "glXUseXFont" glXUseXFont
  :: Font -> CInt -> CInt -> CInt -> IO ()

-- GLX 1.1 and later
foreign import ccall unsafe "glXQueryExtensionsString" glXQueryExtensionsString
  :: Display -> ScreenNumber -> IO CString

foreign import ccall unsafe "glXQueryServerString" glXQueryServerString
  :: Display -> ScreenNumber -> GLXStringName -> IO CString

foreign import ccall unsafe "glXGetClientString" glXGetClientString
  :: Display -> GLXStringName -> IO CString

-- GLX 1.2 and later
foreign import ccall unsafe "glXGetCurrentDisplay" glXGetCurrentDisplay
  :: IO Display

-- GLX 1.3 and later
foreign import ccall unsafe "glXChooseFBConfig" glXChooseFBConfig
  :: Display -> ScreenNumber -> Ptr GLXAttribute -> Ptr CInt
  -> IO (Ptr GLXFBConfig)

foreign import ccall unsafe "glXGetFBConfigAttrib" glXGetFBConfigAttrib
  :: Display -> GLXFBConfig -> GLXAttributeToken -> Ptr GLXAttribute
  -> IO GLXErrorCode

foreign import ccall unsafe "glXGetFBConfigs" glXGetFBConfigs
  :: Display -> ScreenNumber -> Ptr CInt -> IO (Ptr GLXFBConfig)

foreign import ccall unsafe "glXGetVisualFromFBConfig" glXGetVisualFromFBConfig
  :: Display -> GLXFBConfig -> IO (Ptr VisualInfo)

foreign import ccall unsafe "glXCreateWindow" glXCreateWindow
  :: Display -> GLXFBConfig -> Window -> Ptr GLXAttribute -> IO GLXWindow

foreign import ccall unsafe "glXDestroyWindow" glXDestroyWindow
  :: Display -> GLXWindow -> IO ()

foreign import ccall unsafe "glXCreatePixmap" glXCreatePixmap
  :: Display -> GLXFBConfig -> Pixmap -> Ptr GLXAttribute -> IO GLXPixmap

foreign import ccall unsafe "glXDestroyPixmap" glXDestroyPixmap
  :: Display -> GLXPixmap -> IO ()

foreign import ccall unsafe "glXCreatePbuffer" glXCreatePbuffer
  :: Display -> GLXFBConfig -> Ptr GLXAttribute -> IO GLXPbuffer

foreign import ccall unsafe "glXDestroyPbuffer" glXDestroyPbuffer
  :: Display -> GLXPbuffer -> IO ()

foreign import ccall unsafe "glXQueryDrawable" glXQueryDrawable
  :: Display -> GLXDrawable -> GLXAttributeToken -> Ptr GLXAttribute -> IO ()

foreign import ccall unsafe "glXCreateNewContext" glXCreateNewContext
  :: Display -> GLXFBConfig -> GLXRenderType -> GLXContext -> Bool
  -> IO GLXContext

foreign import ccall unsafe "glXMakeContextCurrent" glXMakeContextCurrent
  :: Display -> GLXDrawable -> GLXDrawable -> GLXContext -> IO Bool

foreign import ccall unsafe "glXGetCurrentReadDrawable"
  glXGetCurrentReadDrawable :: IO GLXDrawable

foreign import ccall unsafe "glXQueryContext" glXQueryContext
  :: Display -> GLXContext -> GLXAttributeToken -> Ptr GLXAttribute
  -> IO GLXErrorCode

foreign import ccall unsafe "glXSelectEvent" glXSelectEvent
  :: Display -> GLXDrawable -> GLXEventMask -> IO ()

foreign import ccall unsafe "glXGetSelectedEvent" glXGetSelectedEvent
  :: Display -> GLXDrawable -> Ptr GLXEventMask -> IO ()
