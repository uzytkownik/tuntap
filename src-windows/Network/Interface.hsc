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
module Network.Interface where
import Network.Socket

-- | Represents an interface
newtype Interface = Interface Int

-- | Brings up the interface
bringUp :: Interface -> IO ()
bringUp _ = undefined

-- | Brings down the interface
bringDown :: Interface -> IO ()
bringDown _ = undefined

-- | Sets the MTU 
setMTU :: Interface -> Int -> IO ()
setMTU _ _ = undefined

-- | Gets the MTU
getMTU :: Interface -> IO Int
getMTU _ = undefined

-- | Sets IPv4 address and mask
setIPv4 :: Interface -> (HostAddress, HostAddress) -> IO ()
setIPv4 _ _ = undefined

-- | Gets IPv4 address and mask
getIPv4 :: Interface -> IO (Maybe (HostAddress, HostAddress))
getIPv4 _ = undefined
