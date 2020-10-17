{-# language DataKinds             #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TemplateHaskell       #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language QuasiQuotes           #-}

module Lib where

import Data.Int (Int32)
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query

import Connect
import Schema

import           Data.Proxy
import qualified Data.Text as T

import           Mu.GraphQL.Server
import           Mu.Schema
import           Mu.Server

useTPGDatabase db -- compile time connection

staticFilesUrl :: T.Text
staticFilesUrl = "https://raw.githubusercontent.com/"

coverImagesDirectory :: T.Text
coverImagesDirectory = "VladimirLogachev/vladimirlogachev.github.io/dev/static/images/library/"

runSingleQuery :: PGSimpleQuery a -> ServerErrorIO [a]
runSingleQuery query = alwaysOk $ do
  conn <- pgConnect db -- runtime connection
  x <- pgQuery conn query
  pgDisconnect conn
  pure x

serverMain :: IO ()
serverMain = do
  putStrLn "starting GraphQL server on port 8080"
  runGraphQLApp 8080 server  
    (Proxy @('Just "Query"))
    (Proxy @('Just "Mutation"))
    (Proxy @Nothing)

type ServiceMapping = '[
    "Book"   ':-> (Integer, T.Text, T.Text, T.Text)
  , "Author" ':-> T.Text
  ]

toGraphqlBook :: (Int32, T.Text, T.Text, T.Text) -> (Integer, T.Text, T.Text, T.Text)
toGraphqlBook (id, title, coverImage, authorName) = (toInteger id, title, coverImage, authorName)

server :: ServerT ServiceMapping Library ServerErrorIO _
server = resolver
  ( object @"Book"
    ( field  @"id"  bookId
    , field  @"title"  bookTitle
    , field  @"coverImageUrl"  bookCoverImageSourcePath
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

    authorName :: T.Text -> ServerErrorIO T.Text
    authorName = pure

    authorBooks :: T.Text -> ServerErrorIO [(Integer, T.Text, T.Text, T.Text)]
    authorBooks name = fmap toGraphqlBook <$> runSingleQuery [pgSQL|
      SELECT book.id, title, cover_image_source_path, author.name
      FROM book INNER JOIN author ON author.id = book.author_id
      WHERE name = ${name}
      ORDER BY title
    |]
    
    allAuthors :: ServerErrorIO [T.Text]
    allAuthors = runSingleQuery [pgSQL|
      SELECT name FROM author
      ORDER BY name
    |]

    createAuthor :: AuthorInput -> ServerErrorIO Integer
    createAuthor AuthorInput { name = name } = do
      Just x:_ <- runSingleQuery [pgSQL|
        INSERT INTO author (name) VALUES (${name})
        RETURNING author.id
      |] :: ServerErrorIO [Maybe Int32]
      return $ toInteger x

    {- Book -}

    bookId :: (Integer, T.Text, T.Text, T.Text) -> ServerErrorIO Integer
    bookId (id, title, coverImage, authorName) = pure id

    bookTitle :: (Integer, T.Text, T.Text, T.Text) -> ServerErrorIO T.Text
    bookTitle (id, title, coverImage, authorName) = pure title

    bookCoverImageSourcePath :: (Integer, T.Text, T.Text, T.Text) -> ServerErrorIO T.Text
    bookCoverImageSourcePath (id, title, coverImage, authorName) = 
      pure $ staticFilesUrl <> coverImagesDirectory <> coverImage

    bookAuthor :: (Integer, T.Text, T.Text, T.Text) -> ServerErrorIO T.Text
    bookAuthor (id, title, coverImage, authorName) = pure authorName

    allBooks :: ServerErrorIO [(Integer, T.Text, T.Text, T.Text)]
    allBooks = fmap toGraphqlBook <$> runSingleQuery [pgSQL| 
      SELECT book.id, title, cover_image_source_path, author.name
      FROM book INNER JOIN author ON author.id = book.author_id
      ORDER BY title
    |]

    createBook :: BookInput -> ServerErrorIO Integer
    createBook b = do
      Just x:_ <- runSingleQuery [pgSQL|
        INSERT INTO book (title, cover_image_source_path, author_id)
        VALUES (${title b}, ${coverImageSourcePath b}, ${fromIntegral $ authorId b :: Int32})
        RETURNING book.id
      |] :: ServerErrorIO [Maybe Int32]
      return $ toInteger x
    
