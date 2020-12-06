{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Lib where

import Connect
import Control.Exception (try)
import Control.Monad.Except
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32)
import qualified Data.Map.Lazy as Map
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
import Prelude hiding (id)

useTPGDatabase db -- compile time connection

staticFilesUrl :: T.Text
staticFilesUrl = "https://raw.githubusercontent.com/VladimirLogachev/vladimirlogachev.github.io/dev/static/"

coverImagesDirectory :: T.Text
coverImagesDirectory = "images/library/"

defaultError :: ServerError
defaultError = ServerError Internal "Something went wrong"

includesText :: BSC.ByteString -> PGError -> Bool
includesText subStr = BSC.isInfixOf subStr . extractMessage

extractMessage :: PGError -> BSC.ByteString
extractMessage = Map.findWithDefault "" 'M' . pgErrorFields

serverMain :: IO ()
serverMain = do
  let hm =
        addHeaders
          [ ("Access-Control-Allow-Origin", "*"),
            ("Access-Control-Allow-Headers", "Content-Type")
          ]
  runStdoutLoggingT $ do
    conn <- liftIO $ pgConnect db
    logInfoN "starting GraphQL server on port 8080"
    liftIO $
      run 8080 . hm $
        graphQLApp
          (server conn)
          (Proxy @( 'Just "Query"))
          (Proxy @( 'Just "Mutation"))
          (Proxy @Nothing)

type ObjectMapping =
  '[ "Book" ':-> Book,
     "Author" ':-> Author
   ]

server :: PGConnection -> ServerT ObjectMapping i Library ServerErrorIO _
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
          field @"name" authorName
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
    runQuery :: PGSimpleQuery a -> ExceptT PGError IO [a]
    runQuery = ExceptT . try . pgQuery conn

    simpleQuery :: PGSimpleQuery a -> ExceptT ServerError IO [a]
    simpleQuery = withExceptT (const defaultError) . runQuery

    {- Author -}

    authorId :: Author -> ServerErrorIO Integer
    authorId a = pure $ id (a :: Author)

    authorName :: Author -> ServerErrorIO T.Text
    authorName a = pure $ name (a :: Author)

    allAuthors :: ServerErrorIO [Author]
    allAuthors = fmap toGraphqlAuthor <$> simpleQuery [pgSQL| SELECT id, name FROM author ORDER BY name |]

    createAuthor :: AuthorInput -> ServerErrorIO Integer
    createAuthor (AuthorInput name) =
      let errorHandler e
            | includesText "unique_author_name" e = ServerError Invalid "Author with such name already exists."
            | otherwise = defaultError
       in do
            Just authorId : _ <-
              withExceptT errorHandler $
                runQuery
                  [pgSQL| INSERT INTO author (name) VALUES (${name}) RETURNING author.id |] ::
                ServerErrorIO [Maybe Int32]
            return $ toInteger authorId

    {- Book -}

    bookId :: Book -> ServerErrorIO Integer
    bookId b = pure $ id (b :: Book)

    bookTitle :: Book -> ServerErrorIO T.Text
    bookTitle b = pure $ title (b :: Book)

    bookCoverImageSourcePath :: Book -> ServerErrorIO T.Text
    bookCoverImageSourcePath b =
      pure $ staticFilesUrl <> coverImagesDirectory <> coverImage (b :: Book)

    bookAuthor :: Book -> ServerErrorIO Author
    bookAuthor b = pure $ author (b :: Book)

    allBooks :: ServerErrorIO [Book]
    allBooks =
      fmap toGraphqlBook
        <$> simpleQuery
          [pgSQL|
            SELECT book.id, title, cover_image_source_path, author.id, author.name
            FROM book INNER JOIN author ON author.id = book.author_id
            ORDER BY title
          |]

    createBook :: BookInput -> ServerErrorIO Integer
    createBook (BookInput title coverImageSourcePath authorId) =
      let errorHandler e
            | includesText "unique_book_title_per_author" e = ServerError Invalid "Book with such title already exists for this author."
            | otherwise = defaultError
       in do
            Just bookId : _ <-
              withExceptT errorHandler $
                runQuery
                  [pgSQL|
            INSERT INTO book (title, cover_image_source_path, author_id)
            VALUES (${title}, ${coverImageSourcePath}, ${fromIntegral $ authorId :: Int32})
            RETURNING book.id
          |] ::
                ServerErrorIO [Maybe Int32]
            pure $ toInteger bookId
