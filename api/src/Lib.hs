{-# language DataKinds             #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TemplateHaskell       #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language QuasiQuotes           #-}

module Lib where

import Control.Monad
import Data.Int (Int32)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import GHC.Generics

import Connect
import Schema

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

serverMain :: IO ()
serverMain = do
  putStrLn "starting GraphQL server on port 8080"
  runGraphQLApp 8080 server  
    (Proxy @('Just "Query"))
    (Proxy @('Just "Mutation"))
    (Proxy @Nothing)

type ServiceMapping = '[
    "Book"   ':-> (Text, Text, Text)
  , "Author" ':-> Text
  ]

server :: ServerT ServiceMapping Library ServerErrorIO _
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
  , object @"Mutation"
    ( method @"createAuthor" createAuthor
    , method @"createBook" createBook )
  )
  where
    {- Authors -}

    authorName :: Text -> ServerErrorIO Text
    authorName = pure

    authorBooks :: Text -> ServerErrorIO [(Text, Text, Text)]
    authorBooks name = runSingleQuery [pgSQL|
      SELECT name, title, cover_image_source_path
      FROM books INNER JOIN authors ON authors.id = books.author_id
      WHERE name = ${name}
      ORDER BY title
    |]
    
    allAuthors :: ServerErrorIO [Text]
    allAuthors = runSingleQuery [pgSQL|
      SELECT name FROM authors
      ORDER BY name
    |]

    createAuthor :: AuthorInput -> ServerErrorIO Bool
    createAuthor AuthorInput { name = name } = do
      runSingleQuery [pgSQL|
        INSERT INTO authors (name) VALUES (${name})
      |]
      return True

    {- Books -}

    bookTitle :: (Text, Text, Text) -> ServerErrorIO Text
    bookTitle (_, title, _) = pure title

    bookCoverImage :: (Text, Text, Text) -> ServerErrorIO Text
    bookCoverImage (_, _, coverImage) = pure $ staticFilesUrl <> coverImagesDirectory <> coverImage

    bookAuthor :: (Text, Text, Text) -> ServerErrorIO Text
    bookAuthor (authorName, _, _) = pure authorName

    allBooks :: ServerErrorIO [(Text, Text, Text)]
    allBooks = runSingleQuery [pgSQL| 
      SELECT name, title, cover_image_source_path
      FROM books INNER JOIN authors ON authors.id = books.author_id
      ORDER BY title
    |]

    createBook :: BookInput -> ServerErrorIO Bool
    createBook b = do
      runSingleQuery [pgSQL|
        INSERT INTO books (title, cover_image_source_path, author_id)
        VALUES (${title b}, ${coverImageSourcePath b}, ${fromIntegral $ authorId b :: Int32})
      |]
      return True
    