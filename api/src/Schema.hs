{-# language DataKinds                  #-}
{-# language DeriveAnyClass             #-}
{-# language DeriveGeneric              #-}
{-# language DerivingStrategies         #-}
{-# language TemplateHaskell            #-}
module Schema where

import           Data.Int                (Int32, Int64)
import qualified Data.Text               as T
import           GHC.Generics
import           Mu.GraphQL.Quasi
import           Mu.Schema

graphql "Library" "../schema/schema.graphql" -- compile time schema introspection

newtype AuthorInput = AuthorInput { name :: T.Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema LibrarySchema "AuthorInput")