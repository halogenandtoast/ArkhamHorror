{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.JWT
  ( lookupToken
  , jsonToToken
  , tokenToJson
  )
where

import ClassyPrelude.Yesod
import Data.Char (isSpace)
import Data.Map as Map (fromList, (!?))
import Web.JWT as JWT

-- | Try to lookup token from the Authorization header
lookupToken :: MonadHandler m => m (Maybe Text)
lookupToken = do
  mAuth <- lookupHeader "Authorization"
  return $ extractToken . decodeUtf8 =<< mAuth

-- | Create a token out of a given JSON 'Value'
jsonToToken :: Text -> Value -> Text
jsonToToken jwtSecret userId = encodeSigned
  (JWT.hmacSecret jwtSecret)
  mempty
  (mempty { unregisteredClaims = ClaimsMap $ Map.fromList [(jwtKey, userId)] })

-- | Extract a JSON 'Value' out of a token
tokenToJson :: Text -> Text -> Maybe Value
tokenToJson jwtSecret token = do
  jwt <- JWT.decodeAndVerifySignature (JWT.hmacSecret jwtSecret) token
  unClaimsMap (JWT.unregisteredClaims (JWT.claims jwt)) !? jwtKey

jwtKey :: Text
jwtKey = "jwt"

extractToken :: Text -> Maybe Text
extractToken auth
  | toLower x == "token" = Just $ dropWhile isSpace y
  | otherwise = Nothing
  where (x, y) = break isSpace auth
