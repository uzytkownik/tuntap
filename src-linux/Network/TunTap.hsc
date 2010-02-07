{-# LANGUAGE ForeignFunctionInterface #-}
{- |
Module      : $Header$
Description : Basic operation on tuntap
Copyright   : (c) Maciej Piechotka
License     : BSD3

Stability   : none
Portability : POSIX

Basic operations for tuntap
-}
module Network.TunTap
  (
    mkTun,
    mkTap,
  )
where
import Control.Exception
import Data.Maybe
import Foreign.C
import Network.Interface.Internal
import System.Posix.IO
import System.Posix.IOCtl
import System.IO

#include <sys/ioctl.h>
#include <net/if.h>
#include <linux/if_tun.h>

data TunSetIff = TunSetIff

instance IOControl TunSetIff IfReqFlags where
    ioctlReq _ = #const TUNSETIFF

mk_ :: CShort -> Maybe String -> IO (Handle, Interface)
mk_ f n = do fd <- openFd "/dev/net/tun" ReadWrite Nothing defaultFileFlags
             (IfReqFlags i _) <- ioctl fd TunSetIff t `onException` closeFd fd
             h <- fdToHandle fd
             return $! (h, i)
          where t = IfReqFlags (Interface (fromMaybe "" n)) f

mkTun :: Maybe String -> IO (Handle, Interface)
mkTun = mk_ #const (IFF_TUN | IFF_NO_PI)

mkTap :: Maybe String -> IO (Handle, Interface)
mkTap = mk_ #const (IFF_TUN | IFF_NO_PI)
