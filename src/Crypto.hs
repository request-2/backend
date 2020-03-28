module Crypto where

import Config
import Data.ByteString.Lazy.UTF8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as LU (fromString)
import Data.ByteString.UTF8 (fromString, toString)
import Data.ByteString.Lazy (toStrict)
import Data.Digest.Pure.SHA (sha256, sha512, showDigest)
import Data.Text
import System.Entropy (getEntropy)
import System.POSIX.Crypt.SHA512
import Text.Email.Validate (isValid)

type Hash = Text

newHash :: Text -> IO Hash
newHash password =
  pack . toString . cryptSHA512' (Just 31337) (fromString $ unpack password) <$>
  getEntropy 128

checkHash :: Text -> Hash -> Bool
checkHash password hashStr =
  Just hash == cryptSHA512 (fromString $ unpack password) hash
  where
    hash = fromString $ unpack hashStr

-- TODO make sure your system's prng is not dim-witted
newApiKey :: IO Text
newApiKey = pack . toString . encode64 <$> getEntropy 32

pathHash :: Text -> Hash
pathHash = pack . showDigest . sha256 . LU.fromString . unpack

pathHash' :: ByteString -> Hash
pathHash' = pack . showDigest . sha256

regToken :: Text -> ServerConfig -> Maybe Text
regToken mail config
  | isValid (toStrict bmail) = Just token
  | otherwise = Nothing
  where
    bs = LU.fromString . unpack
    bmail = bs mail
    padding = bs $ _regTokenSecret config
    token =
      pack . showDigest . sha512 $ padding <> "\0" <> bmail <> "\0" <> padding
