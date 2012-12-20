-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.GLX
-- Copyright   : (c) 2012 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- A collection of bindings for GLX 1.3
--
-----------------------------------------------------------------------------

module Graphics.X11.GLX (
  -- * Types
    module Graphics.X11.GLX.Raw.Core13.Types

  , Attribute(..)

  -- * Functions
  , chooseVisual
  , createContext
  , destroyContext
  , makeCurrent
  , copyContext
  , swapBuffers
  , createGLXPixmap
  , destroyGLXPixmap
  , queryExtension
  , queryVersion
  , isDirect
  , getConfig
  , getCurrentContext
  , getCurrentDrawable
  , waitGL
  , waitX
  , useXFont

  -- ** GLX 1.1 and later
  , queryExtensionList
  , queryServerString
  , getClientString

  -- ** GLX 1.2 and later
  , getCurrentDisplay

  -- ** GLX 1.3 and later
  , chooseFBConfig
  , getFBConfigAttrib
  , getVisualFromFBConfig
  , createWindow
  , destroyWindow
  , createPixmap
  , destroyPixmap
  , createPbuffer
  , destroyPbuffer
  , queryDrawable
  , createNewContext
  , makeContextCurrent
  , getCurrentReadDrawable
  , queryContext
  , selectEvent
  , getSelectedEvent
  ) where

import Control.Applicative
import Data.Maybe
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31.Types
import Graphics.X11.Xlib.Extras (none, xFree)

import Graphics.X11.GLX.Raw
import Graphics.X11.GLX.Raw.Core13.Types

-- | Return 'Just' a value if it satisfies the given predicate, or 'Nothing'
-- otherwise.
justSat :: (a -> Bool) -> a -> Maybe a
justSat p x
  | p x = Just x
  | otherwise = Nothing

-- | Execute an 'IO' action with an out parameter, returning its value.
out :: Storable a => (Ptr a -> IO ()) -> IO a
out act = alloca $ \ptr -> act ptr >> peek ptr

-- | Convert a peek combinator into one that releases the given memory
-- location.
takeWith :: Storable a => (Ptr a -> IO c) -> (Ptr a -> IO b) -> Ptr a -> IO b
takeWith free' peek' ptr = peek' ptr <* free' ptr

-- | Execute an 'IO' action returning a 'GLXErrorCode', throwing a 'userError'
-- if the return value indicates an error condition.
throwIfErr_ :: String -> IO GLXErrorCode -> IO ()
throwIfErr_ name = throwIf_ (/= 0) showErr
  where
    showErr err = "GLX Error: " ++ name ++ " = " ++ show err

-- | A null X11 display
nullDpy :: Display
nullDpy = Display nullPtr

-- | A null GLX context
nullCtx :: GLXContext
nullCtx = GLXContext nullPtr

-- | A GLX attribute
data Attribute
  = UseGL Bool
  | BufferSize CInt
  | Level CInt
  | RGBA Bool
  | DoubleBuffer Bool
  | Stereo Bool
  | AuxBuffers CInt
  | RedSize CInt
  | GreenSize CInt
  | BlueSize CInt
  | AlphaSize CInt
  | DepthSize CInt
  | StencilSize CInt
  | AccumRedSize CInt
  | AccumGreenSize CInt
  | AccumBlueSize CInt
  | AccumAlphaSize CInt
  | ConfigCaveat GLXConfigCaveat
  | XVisualType GLXXVisualType
  | TransparentType GLXTransparentType
  | TransparentIndexValue CInt
  | TransparentRedValue CInt
  | TransparentGreenValue CInt
  | TransparentBlueValue CInt
  | TransparentAlphaValue CInt
  | DrawableType GLXDrawableTypeMask
  | RenderType GLXRenderTypeMask
  | XRenderable Bool
  | FBConfigID GLXFBConfigID
  | PreservedContents Bool
  | LargestPbuffer Bool
  | PbufferHeight CInt
  | PbufferWidth CInt

-- | An interface to the GLX function 'glXChooseVisual'
chooseVisual
  :: Display -> ScreenNumber -> [Attribute] -> IO (Maybe VisualInfo)
chooseVisual dpy screenNum attrs = do
  infoPtr <- withAttrArray False attrs $ glXChooseVisual dpy screenNum
  maybePeek (takeWith xFree peek) infoPtr

-- | An interface to the GLX function 'glXCreateContext'
createContext :: Display -> VisualInfo -> Maybe GLXContext -> Bool
  -> IO (Maybe GLXContext)
createContext dpy info shareListCtx direct
  = justSat (/= nullCtx) <$> with info
    (\infoPtr -> glXCreateContext dpy infoPtr rawShareListCtx direct)
  where
    rawShareListCtx = fromMaybe nullCtx shareListCtx

-- | An interface to the GLX function 'glXDestroyContext'
destroyContext :: Display -> GLXContext -> IO ()
destroyContext = glXDestroyContext

-- | An interface to the GLX function 'glXMakeCurrent'
makeCurrent :: Display -> GLXDrawable -> GLXContext -> IO Bool
makeCurrent = glXMakeCurrent

-- | An interface to the GLX function 'glXCopyContext'
copyContext :: Display -> GLXContext -> GLXContext -> GLbitfield -> IO ()
copyContext dpy srcCtx dstCtx mask
  = glXCopyContext dpy srcCtx dstCtx $ fromIntegral mask

-- | An interface to the GLX function 'glXSwapBuffers'
swapBuffers :: Display -> GLXDrawable -> IO ()
swapBuffers = glXSwapBuffers

-- | An interface to the GLX function 'glXCreateGLXPixmap'
createGLXPixmap :: Display -> VisualInfo -> Pixmap -> IO GLXPixmap
createGLXPixmap dpy info pixmap = with info $ \infoPtr ->
  glXCreateGLXPixmap dpy infoPtr pixmap

-- | An interface to the GLX function 'glXDestroyGLXPixmap'
destroyGLXPixmap :: Display -> GLXPixmap -> IO ()
destroyGLXPixmap = glXDestroyPixmap

-- | An interface to the GLX function 'glXQueryExtension'
queryExtension :: Display -> IO (Maybe (CInt, CInt))
queryExtension dpy
  = alloca $ \errorBasePtr ->
    alloca $ \eventBasePtr -> do
      res <- glXQueryExtension dpy errorBasePtr eventBasePtr
      if not res
        then return Nothing
        else do
          errorBase <- peek errorBasePtr
          eventBase <- peek eventBasePtr
          return $ Just (errorBase, eventBase)

-- | An interface to the GLX function 'glXQueryVersion'
queryVersion :: Display -> IO (Maybe (CInt, CInt))
queryVersion dpy
  = alloca $ \majorPtr ->
    alloca $ \minorPtr -> do
      res <- glXQueryVersion dpy majorPtr minorPtr
      if not res
        then return Nothing
        else do
          major <- peek majorPtr
          minor <- peek minorPtr
          return $ Just (major, minor)

-- | An interface to the GLX function 'glXIsDirect'
isDirect :: Display -> GLXContext -> IO Bool
isDirect = glXIsDirect

-- | An interface to the GLX function 'glXGetConfig'
getConfig :: Display -> VisualInfo -> GLXAttributeToken -> IO GLXAttribute
getConfig dpy info token = out $ throwIfErr_ "glXGetConfig"
  . (\outPtr -> with info $ \infoPtr -> glXGetConfig dpy infoPtr token outPtr)

-- | An interface to the GLX function 'glXGetCurrentContext'
getCurrentContext :: IO (Maybe GLXContext)
getCurrentContext = justSat (/= nullCtx) <$> glXGetCurrentContext

-- | An interface to the GLX function 'glXGetCurrentDrawable'
getCurrentDrawable :: IO (Maybe GLXDrawable)
getCurrentDrawable = justSat (/= 0) <$> glXGetCurrentDrawable

-- | An interface to the GLX function 'glXWaitGL'
waitGL :: IO ()
waitGL = glXWaitGL

-- | An interface to the GLX function 'glXWaitX'
waitX :: IO ()
waitX = glXWaitX

-- | An interface to the GLX function 'glXUseXFont'
useXFont :: Font -> CInt -> CInt -> GLuint -> IO ()
useXFont font first count listBase
  = glXUseXFont font first count $ fromIntegral listBase

-- | An interface to the GLX function 'glXQueryExtensionsString'
queryExtensionList :: Display -> ScreenNumber -> IO [String]
queryExtensionList dpy screenNum = do
  cstr <- glXQueryExtensionsString dpy screenNum
  str <- peekCString cstr
  return $ words str

-- | An interface to the GLX function 'glXQueryServerString'
queryServerString
  :: Display -> ScreenNumber -> GLXStringName -> IO (Maybe String)
queryServerString dpy screenNum name = do
  cstr <- glXQueryServerString dpy screenNum name
  maybePeek peekCString cstr

-- | An interface to the GLX function 'glXGetClientString'
getClientString :: Display -> GLXStringName -> IO (Maybe String)
getClientString dpy name = do
  cstr <- glXGetClientString dpy name
  maybePeek peekCString cstr

-- | An interface to the GLX function 'glXGetCurrentDisplay'
getCurrentDisplay :: IO (Maybe Display)
getCurrentDisplay = justSat (/= nullDpy) <$> glXGetCurrentDisplay

-- | An interface to the GLX function 'glXChooseFBConfig'
chooseFBConfig :: Display -> ScreenNumber -> [Attribute] -> IO [GLXFBConfig]
chooseFBConfig dpy screenNum attrs = alloca $ \nElemsPtr -> do
  elemsPtr <- withAttrArray True attrs
    $ \attrsPtr -> glXChooseFBConfig dpy screenNum attrsPtr nElemsPtr
  nElems <- peek nElemsPtr
  takeWith xFree (peekArray $ fromIntegral nElems) elemsPtr

-- | An interface to the GLX function 'glXGetFBConfigAttrib'
getFBConfigAttrib
  :: Display -> GLXFBConfig -> GLXAttributeToken -> IO GLXAttribute
getFBConfigAttrib dpy conf token = out
  $ throwIfErr_ "glXGetFBConfigAttrib" . glXGetFBConfigAttrib dpy conf token

-- | An interface to the GLX function 'glXGetVisualFromFBConfig'
getVisualFromFBConfig :: Display -> GLXFBConfig -> IO (Maybe VisualInfo)
getVisualFromFBConfig dpy conf = do
  infoPtr <- glXGetVisualFromFBConfig dpy conf
  maybePeek (takeWith xFree peek) infoPtr

-- | An interface to the GLX function 'glXCreateWindow'
createWindow :: Display -> GLXFBConfig -> Window -> [Attribute] -> IO GLXWindow
createWindow dpy conf win attrs
  = withAttrArray True attrs $ glXCreateWindow dpy conf win

-- | An interface to the GLX function 'glXDestroyWindow'
destroyWindow :: Display -> GLXWindow -> IO ()
destroyWindow = glXDestroyWindow

-- | An interface to the GLX function 'glXCreatePixmap'
createPixmap :: Display -> GLXFBConfig -> Pixmap -> [Attribute] -> IO GLXPixmap
createPixmap dpy conf pixmap attrs
  = withAttrArray True attrs $ glXCreatePixmap dpy conf pixmap

-- | An interface to the GLX function 'glXDestroyPixmap'
destroyPixmap :: Display -> GLXPixmap -> IO ()
destroyPixmap = glXDestroyPixmap

-- | An interface to the GLX function 'glXCreatePbuffer'
createPbuffer :: Display -> GLXFBConfig -> [Attribute] -> IO GLXPbuffer
createPbuffer dpy conf attrs
  = withAttrArray True attrs $ glXCreatePbuffer dpy conf

-- | An interface to the GLX function 'glXDestroyPbuffer'
destroyPbuffer :: Display -> GLXPbuffer -> IO ()
destroyPbuffer = glXDestroyPbuffer

-- | An interface to the GLX function 'glXQueryDrawable'
queryDrawable :: Display -> GLXDrawable -> GLXAttributeToken -> IO GLXAttribute
queryDrawable dpy drw token = out $ glXQueryDrawable dpy drw token

-- | An interface to the GLX function 'glXCreateNewContext'
createNewContext :: Display -> GLXFBConfig -> GLXRenderTypeMask
  -> Maybe GLXContext -> Bool -> IO (Maybe GLXContext)
createNewContext dpy conf renderType shareListCtx direct
  = justSat (/= nullCtx)
  <$> glXCreateNewContext dpy conf renderType rawShareListCtx direct
  where
    rawShareListCtx = fromMaybe nullCtx shareListCtx

-- | An interface to the GLX function 'glXMakeContextCurrent'
makeContextCurrent
  :: Display -> GLXDrawable -> GLXDrawable -> GLXContext -> IO Bool
makeContextCurrent = glXMakeContextCurrent

-- | An interface to the GLX function 'glXGetCurrentReadDrawable'
getCurrentReadDrawable :: IO (Maybe GLXDrawable)
getCurrentReadDrawable = justSat (/= 0) <$> glXGetCurrentReadDrawable

-- | An interface to the GLX function 'glXQueryContext'
queryContext :: Display -> GLXContext -> GLXAttributeToken -> IO GLXAttribute
queryContext dpy ctx token
  = out $ throwIfErr_ "glXQueryContext" . glXQueryContext dpy ctx token

-- | An interface to the GLX function 'glXSelectEvent'
selectEvent :: Display -> GLXDrawable -> GLXEventMask -> IO ()
selectEvent = glXSelectEvent

-- | An interface to the GLX function 'glXSelectedEvent'
getSelectedEvent :: Display -> GLXDrawable -> IO GLXEventMask
getSelectedEvent dpy drw = out $ glXGetSelectedEvent dpy drw

-- | Temporarily store a list of encoded attributes in memory
withAttrArray :: Bool -> [Attribute] -> (Ptr GLXAttribute -> IO a) -> IO a
withAttrArray newEnc attrs f
  = if null attrs
      then f nullPtr -- GLX accepts NULL as an empty attribute array
      else withArray0 (fromIntegral none) rawAttrs f
  where
    rawAttrs = foldr (encodeAttr newEnc) [] attrs

-- | Encode an attribute using the given (old or new) encoding
encodeAttr :: Bool -> Attribute -> [GLXAttribute] -> [GLXAttribute]
encodeAttr newEnc attr = case attr of
  UseGL x -> encodeBoolAttr glx_USE_GL x
  BufferSize x -> encodeIntAttr glx_BUFFER_SIZE x
  Level x -> encodeIntAttr glx_LEVEL x
  RGBA x -> encodeBoolAttr glx_RGBA x
  DoubleBuffer x -> encodeBoolAttr glx_DOUBLEBUFFER x
  Stereo x -> encodeBoolAttr glx_STEREO x
  AuxBuffers x -> encodeIntAttr glx_AUX_BUFFERS x
  RedSize x -> encodeIntAttr glx_RED_SIZE x
  GreenSize x -> encodeIntAttr glx_GREEN_SIZE x
  BlueSize x -> encodeIntAttr glx_BLUE_SIZE x
  AlphaSize x -> encodeIntAttr glx_ALPHA_SIZE x
  DepthSize x -> encodeIntAttr glx_DEPTH_SIZE x
  StencilSize x -> encodeIntAttr glx_STENCIL_SIZE x
  AccumRedSize x -> encodeIntAttr glx_ACCUM_RED_SIZE x
  AccumGreenSize x -> encodeIntAttr glx_ACCUM_GREEN_SIZE x
  AccumBlueSize x -> encodeIntAttr glx_ACCUM_BLUE_SIZE x
  AccumAlphaSize x -> encodeIntAttr glx_ACCUM_ALPHA_SIZE x
  ConfigCaveat x -> encodeIntAttr glx_CONFIG_CAVEAT x
  XVisualType x -> encodeIntAttr glx_X_VISUAL_TYPE x
  TransparentType x -> encodeIntAttr glx_TRANSPARENT_TYPE x
  TransparentIndexValue x -> encodeIntAttr glx_TRANSPARENT_INDEX_VALUE x
  TransparentRedValue x -> encodeIntAttr glx_TRANSPARENT_RED_VALUE x
  TransparentGreenValue x -> encodeIntAttr glx_TRANSPARENT_GREEN_VALUE x
  TransparentBlueValue x -> encodeIntAttr glx_TRANSPARENT_BLUE_VALUE x
  TransparentAlphaValue x -> encodeIntAttr glx_TRANSPARENT_ALPHA_VALUE x
  DrawableType x -> encodeIntAttr glx_DRAWABLE_TYPE x
  RenderType x -> encodeIntAttr glx_RENDER_TYPE x
  XRenderable x -> encodeBoolAttr glx_X_RENDERABLE x
  FBConfigID x -> encodeIntAttr glx_FBCONFIG_ID x
  PreservedContents x -> encodeBoolAttr glx_PRESERVED_CONTENTS x
  LargestPbuffer x -> encodeBoolAttr glx_LARGEST_PBUFFER x
  PbufferHeight x -> encodeIntAttr glx_PBUFFER_HEIGHT x
  PbufferWidth x -> encodeIntAttr glx_PBUFFER_WIDTH x
  where
    encodeBoolAttr = if newEnc then encodeBoolAttr10 else encodeBoolAttr13

-- | Encode a boolean attribute using the old (GLX 1.0) encoding
encodeBoolAttr10
  :: GLXAttributeToken -> Bool -> [GLXAttribute] -> [GLXAttribute]
encodeBoolAttr10 token val attrs
  | val = token : attrs
  | otherwise = attrs

-- | Encode a boolean attribute using the new (GLX 1.3) encoding
encodeBoolAttr13
  :: GLXAttributeToken -> Bool -> [GLXAttribute] -> [GLXAttribute]
encodeBoolAttr13 token val attrs = token : fromBool val : attrs

-- | Encode an integral attribute
encodeIntAttr
  :: Integral a => GLXAttributeToken -> a -> [GLXAttribute] -> [GLXAttribute]
encodeIntAttr token val attrs = token : fromIntegral val : attrs
