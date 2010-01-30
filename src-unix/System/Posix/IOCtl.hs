{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      : $Header$
Description : ioctl wrapping
Copyright   : (c) Maciej Piechotka
License     : BSD3

Stability   : none
Portability : POSIX

The module wrapps the ioctl system call. Possibly should be in separate module.
-}
module System.Posix.IOCtl
  (
    IOControl(..),
    ioctl,
    ioctl_,
    ioctl',
  )
where

import Foreign
import Foreign.C
import System.Posix

-- | Combines the request with data.
class Storable d => IOControl req d | req -> d where
    -- | Converts request to integer
    ioctlReq :: req -- ^ The request. Should be lazy in argument.
             -> CInt -- ^ The request code.

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

c_ioctl' :: IOControl req d => Fd -> req -> Ptr d -> IO ()
c_ioctl' f req p =
    throwErrnoIfMinus1_ "ioctl" $
        c_ioctl (fromIntegral f) (ioctlReq req) (castPtr p)

-- | Calls a ioctl reading the structure after the call
ioctl :: IOControl req d
      => Fd -- ^ The file descriptor 
      -> req -- ^ The request
      -> d -- ^ The data
      -> IO d -- ^ The data after the call
ioctl f req d = with d $ \p -> c_ioctl' f req p >> peek p

-- | Call a ioctl ignoring the result
ioctl_ :: IOControl req d
       => Fd -- ^ The file descriptor
       -> req -- ^ The request
       -> d -- ^ The data
       -> IO ()
ioctl_ f req d = with d $ \p -> c_ioctl' f req p

-- | Call a ioctl with uninitialized data
ioctl' :: IOControl req d
       => Fd -- ^ The file descriptor
       -> req -- ^ The request
       -> IO d -- ^ The data
ioctl' f req = alloca $ \p -> c_ioctl' f req p >> peek p
