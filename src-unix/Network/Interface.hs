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
module Network.Interface
  (
    Interface(..),
    bringUp,
    bringDown,
    setMTU,
    getMTU,
    setIPv4,
    getIPv4,
  )
where
import Network.Interface.Internal