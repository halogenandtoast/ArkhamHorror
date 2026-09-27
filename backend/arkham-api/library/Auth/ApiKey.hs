{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Scoped API keys: minting them, recognising them on a request, and the scopes
they can carry.

Separate from "Auth.JWT" because the two answer different questions. A JWT says
"this is the account owner, acting as themselves" and authorises everything.
A key says "this is something the owner handed a specific, revocable ability to",
and the whole point is that it authorises less.
-}
module Auth.ApiKey (
  MintedKey (..),
  mintKey,
  digestOf,
  extractKey,
  lookupApiKeyHeader,
  Scope,
  cardsRead,
  cardsWrite,
  allScopes,
  parseScopes,
  renderScopes,
  writeLimitPerHour,
) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isSpace)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Relude
import Yesod.Core

-- | A capability a key may carry. Space-separated on the row, as OAuth writes them.
type Scope = Text

cardsRead :: Scope
cardsRead = "cards:read"

cardsWrite :: Scope
cardsWrite = "cards:write"

{- | Every scope a key may be granted. Deliberately short: a key exists to do
less than the account can, so a scope is only added when something needs it.
-}
allScopes :: [Scope]
allScopes = [cardsRead, cardsWrite]

parseScopes :: Text -> [Scope]
parseScopes = filter (`elem` allScopes) . T.words

renderScopes :: [Scope] -> Text
renderScopes = T.unwords . ordNub . filter (`elem` allScopes)

{- | How many writes an hour one key may make.

A cap on the key rather than the account, so one runaway agent cannot spend the
allowance of the person's other tools, and a constant rather than a setting
because the number that matters is "enough for any real session, far short of
enough to fill a disk". Card writes are small; art upload is the expensive one
and is capped at 1MB an image by 'storeArt'.
-}
writeLimitPerHour :: Int
writeLimitPerHour = 300

-- | A freshly minted key: the plaintext to show once, and what is stored.
data MintedKey = MintedKey
  { mintedPlaintext :: Text
  -- ^ Shown to the user once and never recoverable afterwards.
  , mintedPrefix :: Text
  , mintedDigest :: Text
  }

keyPrefixTag :: Text
keyPrefixTag = "ak_"

{- | 32 random bytes, base64url, behind a recognisable tag.

The bytes are the raw forms of two version-4 UUIDs, which are drawn from the
system CSPRNG and are already a dependency here: 244 bits of entropy for no new
package, and no clash with @Control.Monad.Random@'s @MonadRandom@, which is a
different class of the same name that this module would otherwise have to
disambiguate.

Raw bytes via 'UUID.toByteString', not 'UUID.toASCIIBytes' -- the ASCII form is
the 36-character dashed text, so concatenating two of those and taking 32 would
take part of the first one only and throw the second away entirely.
-}
mintKey :: MonadIO m => m MintedKey
mintKey = do
  left <- liftIO UUID.nextRandom
  right <- liftIO UUID.nextRandom
  let
    bytes = raw left <> raw right
    secret = decodeUtf8 $ B64URL.encodeUnpadded bytes
    plaintext = keyPrefixTag <> secret
  pure
    MintedKey
      { mintedPlaintext = plaintext
      , -- Enough to tell four keys apart in a list, far too little to guess the rest.
        mintedPrefix = T.take (T.length keyPrefixTag + 6) plaintext
      , mintedDigest = digestOf plaintext
      }
 where
  raw = BSL.toStrict . UUID.toByteString

-- | What is stored, and what a presented key is looked up by.
digestOf :: Text -> Text
digestOf = decodeUtf8 . B16.encode . SHA256.hash . encodeUtf8

{- | The key out of an @Authorization@ header, if it holds one.

@Bearer@, deliberately not @Token@: "Arkham.Auth.JWT.extractToken" owns @Token@,
and keeping the two schemes apart means a JWT can never be mistaken for a key or
the other way round. A caller who sends the wrong scheme gets a 401 that says
which to use rather than a confusing partial success.
-}
extractKey :: Text -> Maybe Text
extractKey header
  | T.toLower scheme == "bearer", keyPrefixTag `T.isPrefixOf` value = Just value
  | otherwise = Nothing
 where
  (scheme, rest) = T.break isSpace header
  value = T.dropWhile isSpace rest

lookupApiKeyHeader :: MonadHandler m => m (Maybe Text)
lookupApiKeyHeader = do
  header <- lookupHeader "Authorization"
  pure $ extractKey . decodeUtf8 =<< header
