{-# LANGUAGE ForeignFunctionInterface #-}
module IdleTime
    ( IdleTimeQuerier
    , createIdleTimeQuerier
    , destroyIdleTimeQuerier
    , idleTimeQuery
    ) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (xFree)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import Foreign.C (CULong)

#include "X11/extensions/scrnsaver.h"

data IdleTimeQuerier = IdleTimeQuerier Display (Ptr ())

createIdleTimeQuerier :: IO IdleTimeQuerier
createIdleTimeQuerier = do
    dpy <- openDisplay ""
    info <- xScreenSaverAllocInfo
    return (IdleTimeQuerier dpy info)

destroyIdleTimeQuerier :: IdleTimeQuerier -> IO ()
destroyIdleTimeQuerier (IdleTimeQuerier dpy info) = do
    _ <- xFree info
    closeDisplay dpy

idleTimeQuery :: IdleTimeQuerier -> IO Integer
idleTimeQuery (IdleTimeQuerier dpy info) = do
    _ <- xScreenSaverQueryInfo dpy (defaultRootWindow dpy) info
    idle <- peekInfoIdleField info
    return (toInteger idle)

peekInfoIdleField :: Ptr a -> IO CULong
peekInfoIdleField = #{peek XScreenSaverInfo, idle}

foreign import ccall unsafe "X11/extensions/scrnsaver.h XScreenSaverAllocInfo"
    xScreenSaverAllocInfo :: IO (Ptr a)

foreign import ccall unsafe "X11/extensions/scrnsaver.h XScreenSaverQueryInfo"
    xScreenSaverQueryInfo :: Display -> Drawable -> Ptr a -> IO Status
