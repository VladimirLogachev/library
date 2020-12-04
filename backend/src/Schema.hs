{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Schema where

import Data.Int (Int32, Int64)
import qualified Data.Text as T
import GHC.Generics
import Mu.GraphQL.Quasi
import Mu.Schema

graphql "Library" "../schema/schema.graphql" -- compile time schema introspection

newtype AuthorInput = AuthorInput {name :: T.Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema LibrarySchema "AuthorInput")

data BookInput = BookInput
  { title :: T.Text,
    coverImageSourcePath :: T.Text,
    authorId :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema LibrarySchema "BookInput")

toGraphqlBook :: (Int32, T.Text, T.Text, Int32) -> (Integer, T.Text, T.Text, Integer)
toGraphqlBook (id, title, coverImage, authorId) = (toInteger id, title, coverImage, toInteger authorId)

toGraphqlAuthor :: (Int32, T.Text) -> (Integer, T.Text)
toGraphqlAuthor (id, name) = (toInteger id, name)
