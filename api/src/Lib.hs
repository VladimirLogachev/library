{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables, QuasiQuotes #-}

module Lib where

import Control.Monad
import Data.Int (Int32)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query

import Connect

useTPGDatabase db -- compile time connection

someFunc :: IO ()
someFunc = do
  putStrLn "Connecting to Database..."
  c <- pgConnect db -- runtime connection
  putStrLn "Connected."
  print <=< pgQuery c $ bookIdByTitle "The Book 3"
  print <=< pgQuery c $ bookTitles
  pgDisconnect c
  putStrLn "Done."

bookIdByTitle :: String -> PGSimpleQuery Int32
bookIdByTitle title = [pgSQL| SELECT id FROM books WHERE title = ${title} |]

bookTitles :: PGPreparedQuery String
bookTitles = [pgSQL|$ SELECT title FROM books |]