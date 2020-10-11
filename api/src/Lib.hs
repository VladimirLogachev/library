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
    "Book"   ':-> (Integer, Text, Text, Text)
  , "Author" ':-> Text
  ]

toGraphqlBook :: (Int32, Text, Text, Text) -> (Integer, Text, Text, Text)
toGraphqlBook (id, title, coverImage, authorName) = (toInteger id, title, coverImage, authorName)

server :: ServerT ServiceMapping Library ServerErrorIO _
server = resolver
  ( object @"Book"
    ( field  @"id"  bookId
    , field  @"title"  bookTitle
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
    {- Author -}

    authorName :: Text -> ServerErrorIO Text
    authorName = pure

    authorBooks :: Text -> ServerErrorIO [(Integer, Text, Text, Text)]
    authorBooks name = fmap toGraphqlBook <$> runSingleQuery [pgSQL|
      SELECT books.id, title, cover_image_source_path, authors.name
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

    {- Book -}

    bookId :: (Integer, Text, Text, Text) -> ServerErrorIO Integer
    bookId (id, title, coverImage, authorName) = pure id

    bookTitle :: (Integer, Text, Text, Text) -> ServerErrorIO Text
    bookTitle (id, title, coverImage, authorName) = pure title

    bookCoverImage :: (Integer, Text, Text, Text) -> ServerErrorIO Text
    bookCoverImage (id, title, coverImage, authorName) = pure $ staticFilesUrl <> coverImagesDirectory <> coverImage

    bookAuthor :: (Integer, Text, Text, Text) -> ServerErrorIO Text
    bookAuthor (id, title, coverImage, authorName) = pure authorName

    allBooks :: ServerErrorIO [(Integer, Text, Text, Text)]
    allBooks = fmap toGraphqlBook <$> runSingleQuery [pgSQL| 
      SELECT books.id, title, cover_image_source_path, authors.name
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
    
