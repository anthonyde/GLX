/*
 * Haskell FFI support macros for GLX extension modules
 *
 * Copyright (c) 2013 Anthony DeRossi <ajderossi@gmail.com>
 * Copyright (c) 2009 Sven Panne <sven.panne@aedion.de>
 *
 * This file is released under a BSD-style license (see the file LICENSE).
 *
 * The original version of this file was distributed with version 1.2.0.0 of
 * the OpenGLRaw package as include/HsOpenGLRaw.h.
 */

#ifndef HSGLX_H
#define HSGLX_H

import qualified Foreign.Ptr
import qualified System.IO.Unsafe
import qualified Graphics.X11.GLX.Raw.Extension

/* Create a FFI definition for a dynamically-loaded extension procedure */
#define EXTENSION_PROC(_proc, _ty) \
_proc :: (_ty); \
_proc = dyn_/**/_proc ptr_/**/_proc; \
\
foreign import ccall unsafe "dynamic" dyn_/**/_proc \
  :: Foreign.Ptr.FunPtr (_ty) -> _ty; \
\
ptr_/**/_proc :: Foreign.Ptr.FunPtr a; \
ptr_/**/_proc = System.IO.Unsafe.unsafePerformIO \
  $ Graphics.X11.GLX.Raw.Extension.getProcAddress "_proc"; \
{-# NOINLINE ptr_/**/_proc #-}

#endif
