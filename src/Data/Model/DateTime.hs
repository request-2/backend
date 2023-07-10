{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Model.DateTime where

import Data.Aeson
import Data.Text (unpack)
import Data.Time
import Data.Time.Clock.POSIX
import Database.Selda
import Database.Selda.SqlType (
    Lit (LCustom, LInt64),
    SqlTypeRep (TInt64),
    SqlValue (SqlInt64),
 )


newtype DateTime = DateTime UTCTime
    deriving (Show, Eq, Ord, FormatTime, SqlOrd)


instance SqlType DateTime where
    mkLit (DateTime ut) =
        LCustom TInt64
            . LInt64
            . round
            . nominalDiffTimeToSeconds
            . utcTimeToPOSIXSeconds
            $ ut
    sqlType _ = TInt64
    fromSql (SqlInt64 s) =
        DateTime
            . posixSecondsToUTCTime
            . secondsToNominalDiffTime
            . fromIntegral
            $ s
    fromSql v = error $ "fromSql: DateTime column with non-int value: " ++ show v
    defaultValue = LCustom TInt64 $ LInt64 def


instance FromJSON DateTime where
    parseJSON =
        withScientific "UnixTime" $
            return
                . DateTime
                . posixSecondsToUTCTime
                . secondsToNominalDiffTime
                . realToFrac


instance ToJSON DateTime where
    toJSON (DateTime dt) =
        Number
            . realToFrac
            . nominalDiffTimeToSeconds
            . utcTimeToPOSIXSeconds
            $ dt


now :: IO DateTime
now = DateTime <$> getCurrentTime


parseDateTime :: Text -> Maybe DateTime
parseDateTime = fmap DateTime . parseTimeM True defaultTimeLocale "%Y-%-m-%-d" . unpack
