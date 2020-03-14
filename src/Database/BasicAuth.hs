{-# LANGUAGE OverloadedStrings #-}

module Database.BasicAuth where

import Database.SQLite.Simple
import Model.User
import UserInfo

findApiKeyUser :: Connection -> APIKey -> IO (Maybe UserInfo)
findApiKeyUser db key = do
  res <-
    query
      db
      "SELECT Users.user_id, Users.roles \
    \ FROM ApiKeys JOIN Users ON ApiKeys.user_id=Users.user_id \
    \ WHERE api_key = ?"
      (Only key)
  case res of
    [(user, roles)] ->
      return . Just $ UserInfo user key (stringToRoles roles)
    _ -> return Nothing
