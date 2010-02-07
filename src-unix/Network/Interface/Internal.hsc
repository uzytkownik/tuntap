{-# LANGUAGE ForeignFunctionInterface #-}
{- |
Module      : $Header$
Description : Basic operation on iterfaces
Copyright   : (c) Maciej Piechotka
License     : BSD3

Stability   : none
Portability : POSIX

Basic operations on interface
-}
module Network.Interface.Internal where
import Control.Applicative
import Foreign
import Foreign.C
import Network.Socket
import Network.Socket.Internal
import System.Posix
import System.Posix.IOCtl

#include <net/if.h>
#include <sys/ioctl.h>
#include <stddef.h>

-- | Represents an interface
newtype Interface = Interface {interfaceName :: String}

instance Storable Interface where
    alignment _ = 4
    sizeOf _ = #const IF_NAMESIZE
    peek = fmap Interface . peekCString . castPtr
    poke ptr = pokeArray0 0 (castPtr ptr) . take (#const IF_NAMESIZE - 1) .
               map castCharToCChar . interfaceName

-- Flags
data IfReqFlags = IfReqFlags Interface CShort

instance Storable IfReqFlags where
    alignment _ = 4
    sizeOf _ = #size struct ifreq
    peek ptr = IfReqFlags <$> (#peek struct ifreq, ifr_name) ptr
                          <*> (#peek struct ifreq, ifr_flags) ptr
    poke ptr (IfReqFlags i f) = (#poke struct ifreq, ifr_name) ptr i >>
                                (#poke struct ifreq, ifr_flags) ptr f

data GetIfFlags = GetIfFlags
instance IOControl GetIfFlags IfReqFlags where
    ioctlReq _ = #const SIOCGIFFLAGS

data SetIfFlags = SetIfFlags
instance IOControl SetIfFlags IfReqFlags where
    ioctlReq _ = #const SIOCSIFFLAGS

setFlag :: Int -> Interface -> IO ()
setFlag g i = do let ifreq = IfReqFlags i 0
                 (IfReqFlags _ f) <- ioctl sock GetIfFlags ifreq
                 let f' = f .|. fromIntegral g
                 ioctl_ sock SetIfFlags $ IfReqFlags i f'

unsetFlag :: Int -> Interface -> IO ()
unsetFlag g i = do let ifreq = IfReqFlags i 0
                   (IfReqFlags _ f) <- ioctl sock GetIfFlags ifreq
                   let f' = f `xor` fromIntegral g
                   ioctl_ sock SetIfFlags $ IfReqFlags i f'

-- | Brings up the interface
bringUp :: Interface -> IO ()
bringUp = setFlag #const IFF_UP

-- | Brings down the interface
bringDown :: Interface -> IO ()
bringDown = unsetFlag #const IFF_UP

-- MTU
data IfReqMTU = IfReqMTU Interface CInt

instance Storable IfReqMTU where
    alignment _ = 4
    sizeOf _ = #size struct ifreq
    peek ptr = IfReqMTU <$> (#peek struct ifreq, ifr_name) ptr
                        <*> (#peek struct ifreq, ifr_mtu) ptr
    poke ptr (IfReqMTU i mtu) = (#poke struct ifreq, ifr_name) ptr i >>
                                (#poke struct ifreq, ifr_mtu) ptr mtu

data GetIfMTU = GetIfMTU
instance IOControl GetIfMTU IfReqMTU where
    ioctlReq _ = #const SIOCGIFMTU

data SetIfMTU = SetIfMTU
instance IOControl SetIfMTU IfReqMTU where
    ioctlReq _ = #const SIOCSIFMTU

-- | Sets the MTU 
setMTU :: Interface -> Int -> IO ()
setMTU i mtu = ioctl_ sock SetIfMTU $ IfReqMTU i $ fromIntegral mtu

-- | Gets the MTU
getMTU :: Interface -> IO Int
getMTU i = (\(IfReqMTU _ mtu) -> fromIntegral mtu) <$>
           (ioctl sock GetIfMTU $ IfReqMTU i 0)

-- SockAddr
data IfReqAddr = IfReqAddr Interface SockAddr
instance Storable IfReqAddr where
    alignment _ = 4
    sizeOf _ = #size struct ifreq
    peek ptr = IfReqAddr <$> (#peek struct ifreq, ifr_name) ptr
                         <*> peekSockAddr (ptr `plusPtr` offset)
               where offset = #offset struct ifreq, ifr_addr
    poke ptr (IfReqAddr i a) = (#poke struct ifreq, ifr_name) ptr i >>
                               pokeSockAddr (ptr `plusPtr` offset) a
                               where offset = #offset struct ifreq, ifr_addr

data GetIfAddress = GetIfAddress
instance IOControl GetIfAddress IfReqAddr where
    ioctlReq _ = #const SIOCGIFADDR

data SetIfAddress = SetIfAddress
instance IOControl SetIfAddress IfReqAddr where
    ioctlReq _ = #const SIOCSIFADDR

data GetIfMask = GetIfMask
instance IOControl GetIfMask IfReqAddr where
    ioctlReq _ = #const SIOCSIFNETMASK

data SetIfMask = SetIfMask
instance IOControl SetIfMask IfReqAddr where
    ioctlReq _ = #const SIOCGIFNETMASK

-- | Sets IPv4 address and mask
setIPv4 :: Interface -> (HostAddress, HostAddress) -> IO ()
setIPv4 i (h, m) = do ioctl_ sock SetIfAddress $ IfReqAddr i ha
                      ioctl_ sock SetIfMask $ IfReqAddr i ma
                   where ha = SockAddrInet 0 h
                         ma = SockAddrInet 0 m

-- | Gets IPv4 address and mask
getIPv4 :: Interface -> IO (Maybe (HostAddress, HostAddress))
--getIPv4 i = undefined i
getIPv4 i = do (IfReqAddr _ (SockAddrInet _ h)) <- ioctl sock GetIfAddress n
               (IfReqAddr _ (SockAddrInet _ m)) <- ioctl sock GetIfMask n
               return $! Just $! (h, m)
            where n = IfReqAddr i $ SockAddrInet 0 0
-- Helper
sock :: Fd
sock = Fd $ fdSocket $ unsafePerformIO $ socket AF_INET Datagram 0
{-# NOINLINE sock #-}
