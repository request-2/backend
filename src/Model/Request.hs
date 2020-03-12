{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Request where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import Data.Text (pack, toLower)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import DateTime
import GHC.Generics
import WithID

-- TODO Ask which statuses should be kept
data Status = Requested | WIP | Done deriving (Show, Eq)

instance FromField Status where
  fromField f = case fieldData f of
    SQLText "Requested" -> return Requested
    SQLText "WIP" -> return Requested
    SQLText "Done" -> return Requested
    _ -> returnError Incompatible f "Expected Requested | WIP | Done"

instance FromJSON Status where
  parseJSON = withText "Status field" $ \v ->
    case toLower v of
      "requested" -> return Requested
      "wip" -> return WIP
      "done" -> return Done
      _ -> prependFailure "Can't parse status, " (fail $ "incorrect argument: " ++ show v)

instance ToJSON Status where
  toJSON = String . pack . show

instance ToField Status where
  toField = toField . show

data Request
  = Request
      { authorID :: ID,
        teamID :: ID,
        status :: Status,
        requestType :: String, -- named `type` in db
        created :: DateTime
      }
  deriving (Show, Eq, Generic)

instance FromJSON Request

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions

instance FromRow (WithID Request) where
  fromRow = WithID <$> field <*> (Request <$> field <*> field <*> field <*> field <*> field)

instance ToRow Request where
  toRow Request {..} = toRow (authorID, teamID, dateAdded)
