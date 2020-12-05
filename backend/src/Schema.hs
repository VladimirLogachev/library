{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics
import Mu.GraphQL.Quasi
import Mu.Schema

graphql "Library" "../schema/schema.graphql" -- compile time schema introspection

{- Author -}

newtype AuthorInput = AuthorInput {name :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema LibrarySchema "AuthorInput")

data Author = Author {id :: Integer, name :: T.Text}
  deriving stock (Eq, Show, Generic)

toGraphqlAuthor :: (Int32, T.Text) -> Author
toGraphqlAuthor (id, name) = Author {id = toInteger id, name = name}

{- Book -}

data BookInput = BookInput
  { title :: T.Text,
    coverImageSourcePath :: T.Text,
    authorId :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema LibrarySchema "BookInput")

data Book = Book
  { id :: Integer,
    title :: T.Text,
    coverImage :: T.Text,
    authorId :: Integer
  }
  deriving stock (Eq, Show, Generic)

toGraphqlBook :: (Int32, T.Text, T.Text, Int32) -> Book
toGraphqlBook (id, title, coverImage, authorId) =
  Book
    { id = toInteger id,
      title = title,
      coverImage = coverImage,
      authorId = toInteger authorId
    }
