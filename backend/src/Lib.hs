{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Lib where

import Connect
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Int (Int32)
import Data.Proxy
import qualified Data.Text as T
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query
import Mu.GraphQL.Server
import Mu.Schema
import Mu.Server
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Schema

useTPGDatabase db -- compile time connection

staticFilesUrl :: T.Text
staticFilesUrl = "https://raw.githubusercontent.com/VladimirLogachev/vladimirlogachev.github.io/dev/static/"

coverImagesDirectory :: T.Text
coverImagesDirectory = "images/library/"

runQuery :: PGConnection -> PGSimpleQuery a -> ServerErrorIO [a]
runQuery conn query = alwaysOk $ pgQuery conn query

serverMain :: IO ()
serverMain = do
  let hm =
        addHeaders
          [ ("Access-Control-Allow-Origin", "*"),
            ("Access-Control-Allow-Headers", "Content-Type")
          ]
  runStdoutLoggingT $ do
    conn <- liftIO $ pgConnect db
    liftIO $ putStrLn "starting GraphQL server on port 8080"
    liftIO $
      run 8080 . hm $
        graphQLApp
          (server conn)
          (Proxy @( 'Just "Query"))
          (Proxy @( 'Just "Mutation"))
          (Proxy @Nothing)

type ServiceMapping =
  '[ "Book" ':-> (Integer, T.Text, T.Text, Integer),
     "Author" ':-> (Integer, T.Text)
   ]

server :: PGConnection -> ServerT ServiceMapping Library ServerErrorIO _
server conn =
  resolver
    ( object @"Book"
        ( field @"id" bookId,
          field @"title" bookTitle,
          field @"coverImageUrl" bookCoverImageSourcePath,
          field @"author" bookAuthor
        ),
      object @"Author"
        ( field @"id" authorId,
          field @"name" authorName,
          field @"books" authorBooks
        ),
      object @"Query"
        ( method @"authors" allAuthors,
          method @"books" allBooks
        ),
      object @"Mutation"
        ( method @"createAuthor" createAuthor,
          method @"createBook" createBook
        )
    )
  where
    {- Author -}

    authorId :: (Integer, T.Text) -> ServerErrorIO Integer
    authorId (id, name) = pure id

    authorName :: (Integer, T.Text) -> ServerErrorIO T.Text
    authorName (id, name) = pure name

    authorBooks :: (Integer, T.Text) -> ServerErrorIO [(Integer, T.Text, T.Text, Integer)]
    authorBooks (id, name) =
      fmap toGraphqlBook
        <$> runQuery
          conn
          [pgSQL|
      SELECT book.id, title, cover_image_source_path, author.id
      FROM book INNER JOIN author ON author.id = book.author_id
      WHERE name = ${name}
      ORDER BY title
    |]

    allAuthors :: ServerErrorIO [(Integer, T.Text)]
    allAuthors =
      fmap toGraphqlAuthor
        <$> runQuery
          conn
          [pgSQL|
      SELECT id, name FROM author
      ORDER BY name
    |]

    createAuthor :: AuthorInput -> ServerErrorIO Integer
    createAuthor AuthorInput {name = name} = do
      Just x : _ <-
        runQuery
          conn
          [pgSQL|
        INSERT INTO author (name) VALUES (${name})
        RETURNING author.id
      |] ::
          ServerErrorIO [Maybe Int32]
      return $ toInteger x

    {- Book -}

    bookId :: (Integer, T.Text, T.Text, Integer) -> ServerErrorIO Integer
    bookId (id, title, coverImage, authorId) = pure id

    bookTitle :: (Integer, T.Text, T.Text, Integer) -> ServerErrorIO T.Text
    bookTitle (id, title, coverImage, authorId) = pure title

    bookCoverImageSourcePath :: (Integer, T.Text, T.Text, Integer) -> ServerErrorIO T.Text
    bookCoverImageSourcePath (id, title, coverImage, authorId) =
      pure $ staticFilesUrl <> coverImagesDirectory <> coverImage

    bookAuthor :: (Integer, T.Text, T.Text, Integer) -> ServerErrorIO (Integer, T.Text)
    bookAuthor (id, title, coverImage, authorId) = do
      x : _ <-
        fmap toGraphqlAuthor
          <$> runQuery
            conn
            [pgSQL|
        SELECT id, name FROM author
        WHERE id = ${fromIntegral $ authorId :: Int32}
      |]
      return x

    allBooks :: ServerErrorIO [(Integer, T.Text, T.Text, Integer)]
    allBooks =
      fmap toGraphqlBook
        <$> runQuery
          conn
          [pgSQL|
      SELECT book.id, title, cover_image_source_path, author.id
      FROM book INNER JOIN author ON author.id = book.author_id
      ORDER BY title
    |]

    createBook :: BookInput -> ServerErrorIO Integer
    createBook BookInput {title = title, coverImageSourcePath = coverImageSourcePath, authorId = authorId} = do
      Just x : _ <-
        runQuery
          conn
          [pgSQL|
        INSERT INTO book (title, cover_image_source_path, author_id)
        VALUES (${title}, ${coverImageSourcePath}, ${fromIntegral $ authorId :: Int32})
        RETURNING book.id
      |] ::
          ServerErrorIO [Maybe Int32]
      return $ toInteger x
