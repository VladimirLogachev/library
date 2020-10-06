{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TemplateHaskell       #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language QuasiQuotes           #-}

module Lib where

import Control.Monad
import Data.Int (Int32)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query

import Connect

import           Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.List

import           Mu.GraphQL.Quasi
import           Mu.GraphQL.Server
import           Mu.Schema
import           Mu.Server

useTPGDatabase db -- compile time connection

staticFilesUrl :: Text
staticFilesUrl = "https://vladimirlogachev.github.io/"

coverImagesDirectory :: Text
coverImagesDirectory = "images/library/"

runSingleQuery :: PGSimpleQuery a -> ServerErrorIO [a]
runSingleQuery query = alwaysOk $ do
  connection <- pgConnect db -- runtime connection
  x <- pgQuery connection query
  pgDisconnect connection
  pure x

graphql "ServiceDefinition" "../schema/schema.graphql" -- compile time schema introspection

serverMain :: IO ()
serverMain = do
  putStrLn "starting GraphQL server on port 8080"
  runGraphQLAppQuery 8080 server (Proxy @"Query")


type ServiceMapping = '[
    "Book"   ':-> (Text, Text, Text)
  , "Author" ':-> Text
  ]

server :: ServerT ServiceMapping ServiceDefinition ServerErrorIO _
server = resolver
  ( object @"Book"
    ( field  @"title"  bookTitle
    , field  @"coverImageUrl"  bookCoverImage
    , field  @"author" bookAuthor )
  , object @"Author"
    ( field  @"name"  authorName
    , field  @"books" authorBooks )
  , object @"Query"
    ( method @"authors" allAuthors
    , method @"books"   allBooks )
  )
  where
    bookTitle :: (Text, Text, Text) -> ServerErrorIO Text
    bookTitle (_, title, _) = pure title

    bookCoverImage :: (Text, Text, Text) -> ServerErrorIO Text
    bookCoverImage (_, _, coverImage) = pure $ staticFilesUrl <> coverImagesDirectory <> coverImage

    bookAuthor :: (Text, Text, Text) -> ServerErrorIO Text
    bookAuthor (authorName, _, _) = pure authorName

    authorName :: Text -> ServerErrorIO Text
    authorName = pure

    authorBooks :: Text -> ServerErrorIO [(Text, Text, Text)]
    authorBooks name = runSingleQuery [pgSQL| 
      SELECT name, title, cover_image_filename
      FROM books INNER JOIN authors ON authors.id = books.author_id
      WHERE name = ${name}
    |]

    allAuthors :: ServerErrorIO [Text]
    allAuthors = runSingleQuery [pgSQL| 
      SELECT name FROM authors
    |]

    allBooks :: ServerErrorIO [(Text, Text, Text)]
    allBooks = runSingleQuery [pgSQL| 
      SELECT name, title, cover_image_filename
      FROM books INNER JOIN authors ON authors.id = books.author_id
    |]
