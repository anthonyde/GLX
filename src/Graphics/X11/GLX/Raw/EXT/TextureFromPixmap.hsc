{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.GLX.Raw.EXT.TextureFromPixmap
-- Copyright   : (c) 2013 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- All raw functions, tokens, and types for the EXT_texture_from_pixmap
-- extension (see
-- <http://www.opengl.org/registry/specs/EXT/texture_from_pixmap.txt>)
--
-----------------------------------------------------------------------------

module Graphics.X11.GLX.Raw.EXT.TextureFromPixmap (
  -- * Types
  -- ** Token types
    GLXTextureFormatEXT
  , GLXTextureTargetMaskEXT
  , GLXTextureTargetEXT

  , GLXBufferEXT

  -- * Tokens
  -- ** Attributes
  , glx_BIND_TO_TEXTURE_RGB_EXT
  , glx_BIND_TO_TEXTURE_RGBA_EXT
  , glx_BIND_TO_MIPMAP_TEXTURE_EXT
  , glx_BIND_TO_TEXTURE_TARGETS_EXT
  , glx_Y_INVERTED_EXT
  , glx_TEXTURE_FORMAT_EXT
  , glx_TEXTURE_TARGET_EXT
  , glx_MIPMAP_TEXTURE_EXT

  -- ** Texture formats
  , glx_TEXTURE_FORMAT_NONE_EXT
  , glx_TEXTURE_FORMAT_RGB_EXT
  , glx_TEXTURE_FORMAT_RGBA_EXT

  -- ** Texture target masks
  , glx_TEXTURE_1D_BIT_EXT
  , glx_TEXTURE_2D_BIT_EXT
  , glx_TEXTURE_RECTANGLE_BIT_EXT

  -- ** Texture targets
  , glx_TEXTURE_1D_EXT
  , glx_TEXTURE_2D_EXT
  , glx_TEXTURE_RECTANGLE_EXT

  -- ** Buffers
  , glx_FRONT_LEFT_EXT
  , glx_FRONT_RIGHT_EXT
  , glx_BACK_LEFT_EXT
  , glx_BACK_RIGHT_EXT
  , glx_FRONT_EXT
  , glx_BACK_EXT
  , glx_AUX0_EXT
  , glx_AUX1_EXT
  , glx_AUX2_EXT
  , glx_AUX3_EXT
  , glx_AUX4_EXT
  , glx_AUX5_EXT
  , glx_AUX6_EXT
  , glx_AUX7_EXT
  , glx_AUX8_EXT
  , glx_AUX9_EXT

  -- * Functions
  , glXBindTexImageEXT
  , glXReleaseTexImageEXT
  ) where

import Foreign.C
import Foreign.Ptr

import Graphics.X11.GLX.Raw.Core13.Types

#include <GL/glx.h>

##include "HsGLX.h"

-- Types
-- Token type
type GLXTextureFormatEXT = GLXAttribute
type GLXTextureTargetMaskEXT = CUInt
type GLXTextureTargetEXT = GLXAttribute

type GLXBufferEXT = CInt

-- Tokens
-- Attributes
#{enum GLXAttributeToken,
  , glx_BIND_TO_TEXTURE_RGB_EXT = GLX_BIND_TO_TEXTURE_RGB_EXT
  , glx_BIND_TO_TEXTURE_RGBA_EXT = GLX_BIND_TO_TEXTURE_RGBA_EXT
  , glx_BIND_TO_MIPMAP_TEXTURE_EXT = GLX_BIND_TO_MIPMAP_TEXTURE_EXT
  , glx_BIND_TO_TEXTURE_TARGETS_EXT = GLX_BIND_TO_TEXTURE_TARGETS_EXT
  , glx_Y_INVERTED_EXT = GLX_Y_INVERTED_EXT
  , glx_TEXTURE_FORMAT_EXT = GLX_TEXTURE_FORMAT_EXT
  , glx_TEXTURE_TARGET_EXT = GLX_TEXTURE_TARGET_EXT
  , glx_MIPMAP_TEXTURE_EXT = GLX_MIPMAP_TEXTURE_EXT
  }

-- Texture formats
#{enum GLXTextureFormatEXT,
  , glx_TEXTURE_FORMAT_NONE_EXT = GLX_TEXTURE_FORMAT_NONE_EXT
  , glx_TEXTURE_FORMAT_RGB_EXT = GLX_TEXTURE_FORMAT_RGB_EXT
  , glx_TEXTURE_FORMAT_RGBA_EXT = GLX_TEXTURE_FORMAT_RGBA_EXT
  }

-- Texture target masks
#{enum GLXTextureTargetMaskEXT,
  , glx_TEXTURE_1D_BIT_EXT = GLX_TEXTURE_1D_BIT_EXT
  , glx_TEXTURE_2D_BIT_EXT = GLX_TEXTURE_2D_BIT_EXT
  , glx_TEXTURE_RECTANGLE_BIT_EXT = GLX_TEXTURE_RECTANGLE_BIT_EXT
  }

-- Texture targets
#{enum GLXTextureTargetEXT,
  , glx_TEXTURE_1D_EXT = GLX_TEXTURE_1D_EXT
  , glx_TEXTURE_2D_EXT = GLX_TEXTURE_2D_EXT
  , glx_TEXTURE_RECTANGLE_EXT = GLX_TEXTURE_RECTANGLE_EXT
  }

-- Buffers
#{enum GLXBufferEXT,
  , glx_FRONT_LEFT_EXT = GLX_FRONT_LEFT_EXT
  , glx_FRONT_RIGHT_EXT = GLX_FRONT_RIGHT_EXT
  , glx_BACK_LEFT_EXT = GLX_BACK_LEFT_EXT
  , glx_BACK_RIGHT_EXT = GLX_BACK_RIGHT_EXT
  , glx_FRONT_EXT = GLX_FRONT_LEFT_EXT
  , glx_BACK_EXT = GLX_BACK_LEFT_EXT
  , glx_AUX0_EXT = GLX_AUX0_EXT
  , glx_AUX1_EXT = GLX_AUX1_EXT
  , glx_AUX2_EXT = GLX_AUX2_EXT
  , glx_AUX3_EXT = GLX_AUX3_EXT
  , glx_AUX4_EXT = GLX_AUX4_EXT
  , glx_AUX5_EXT = GLX_AUX5_EXT
  , glx_AUX6_EXT = GLX_AUX6_EXT
  , glx_AUX7_EXT = GLX_AUX7_EXT
  , glx_AUX8_EXT = GLX_AUX8_EXT
  , glx_AUX9_EXT = GLX_AUX9_EXT
  }

-- Functions
EXTENSION_PROC(glXBindTexImageEXT,
  Display -> GLXDrawable -> GLXBufferEXT -> Ptr GLXAttribute -> IO ())

EXTENSION_PROC(glXReleaseTexImageEXT,
  Display -> GLXDrawable -> GLXBufferEXT -> IO ())
