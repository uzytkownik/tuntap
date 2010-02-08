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
import Network.Interface
import System.IO

mkTun :: Maybe String -> IO (Handle, Interface)
mkTun _ = undefined

mkTap :: Maybe String -> IO (Handle, Interface)
mkTap _ = undefined
