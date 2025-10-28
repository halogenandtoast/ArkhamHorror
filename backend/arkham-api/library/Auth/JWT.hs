{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auth.JWT (lookupToken, jsonToToken, tokenToJson) where

import Control.Lens ((?~))
import Control.Monad.Time
import Crypto.JWT hiding (jwk)
import Data.Aeson
import Data.Aeson.KeyMap qualified as M
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isSpace)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Relude
import Yesod.Core

data Super = Super {jwtClaims :: ClaimsSet, jwt :: Value}

instance HasClaimsSet Super where
  claimsSet f s = fmap (\a' -> s {jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON Super where
  parseJSON = withObject "Super" $ \o ->
    Super
      <$> parseJSON (Object o)
      <*> (o .: "jwt")

instance ToJSON Super where
  toJSON s =
    ins "jwt" (jwt s) (toJSON (jwtClaims s))
   where
    ins k v (Object o) = Object $ M.insert k (toJSON v) o
    ins _ _ a = a

-- | Try to lookup token from the Authorization header
lookupToken :: MonadHandler m => m (Maybe Text)
lookupToken =
  lookupHeader "Authorization" >>= maybe (lookupGetParam "token") (pure . extractToken . decodeUtf8)

-- | sign
jsonToToken :: (MonadTime m, MonadRandom m) => Text -> Value -> m Text
jsonToToken jwtSecret userId = do
  now <- currentTime
  let jwk = fromOctets (encodeUtf8 @Text @BSL.ByteString jwtSecret)
  let
    claims =
      Super
        { jwtClaims =
            emptyClaimsSet
              & (claimIss ?~ "arkham")
              & (claimIat ?~ NumericDate now)
        , jwt = userId
        }

  res <- runJOSE $ signJWT jwk (newJWSHeader ((), HS256)) claims
  case res of
    Left (err :: JWTError) -> error $ show err
    Right tkn -> pure $ TL.toStrict $ TL.decodeUtf8 $ encodeCompact tkn

-- | verify
tokenToJson :: MonadTime m => Text -> Text -> m (Maybe Value)
tokenToJson jwtSecret token = do
  res <- runJOSE do
    let jwk = fromOctets (encodeUtf8 @Text @BSL.ByteString jwtSecret)
    let audCheck = const True -- should be a proper audience check
    jwt <- decodeCompact $ TL.encodeUtf8 $ TL.fromStrict token
    verifyJWT (defaultJWTValidationSettings audCheck) jwk jwt
  pure $ case res of
    Left (err :: JWTError) -> error $ show err
    Right super -> Just (jwt super)

extractToken :: Text -> Maybe Text
extractToken auth
  | T.toLower x == "token" = Just $ T.dropWhile isSpace y
  | otherwise = Nothing
 where
  (x, y) = T.break isSpace auth
