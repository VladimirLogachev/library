{-# LANGUAGE OverloadedStrings #-}

module Connect where

import Database.PostgreSQL.Typed.Protocol
import qualified Network.Socket as Net

db :: PGDatabase
db = PGDatabase
  { pgDBAddr = Right $ Net.SockAddrInet 5432 (Net.tupleToHostAddress (127,0,0,1))
  , pgDBName = "library"
  , pgDBUser = "admin"
  , pgDBPass = "admin"
  , pgDBParams = []
  , pgDBDebug = False
  , pgDBLogMessage = print
  , pgDBTLS = TlsDisabled
  }
