{- | Player-authored cards created at runtime from the debug menu.

The engine's card defs and entity builders are compile-time maps keyed by card
code, so a card invented while a game is running has nowhere to live. A custom
card sidesteps that with a process-global registry: the def is stored on the
game (see @gameCustomCards@) and re-registered here whenever that game is
deserialized, and every def/builder lookup falls back to this registry for a
card code carrying the custom prefix.

Codes are minted per card and never reused, so the registry is additive and
games cannot collide with one another's cards.
-}
module Arkham.Card.CustomCard where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Prelude
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.IO.Unsafe (unsafePerformIO)

{- | A runtime card: its printed def plus the art to render it with (a URL, or a
data URI for an image the player dropped in). Art is served separately from
the game state so it never rides the websocket payload.
-}
data CustomCard = CustomCard
  { customCardDef :: CardDef
  , customCardArt :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Data)

instance ToJSON CustomCard where
  toJSON c = object ["def" .= customCardDef c, "art" .= customCardArt c]

instance FromJSON CustomCard where
  parseJSON = withObject "CustomCard" \o -> CustomCard <$> o .: "def" <*> o .:? "art"

customCardPrefix :: Text
customCardPrefix = "*"

isCustomCardCode :: HasCardCode a => a -> Bool
isCustomCardCode (toCardCode -> CardCode t) = customCardPrefix `T.isPrefixOf` t

{- | 'Eq CardCode' treats a trailing a/b/c/d as a side designator, so two
distinct custom codes ending in complementary letters would compare equal.
Minted codes end in a digit; anything else gets one appended.
-}
sanitizeCustomCardCode :: CardCode -> CardCode
sanitizeCustomCardCode (CardCode t) = case T.unsnoc t of
  Just (_, c) | c `elem` ("abcd" :: String) -> CardCode (t <> "0")
  _ -> CardCode t

{-# NOINLINE customCardRegistry #-}
customCardRegistry :: IORef (Map CardCode CustomCard)
customCardRegistry = unsafePerformIO (newIORef mempty)

registerCustomCards :: MonadIO m => Map CardCode CustomCard -> m ()
registerCustomCards cards =
  unless (null cards) $ atomicModifyIORef' customCardRegistry \existing -> (cards <> existing, ())

{- | Registration as a pure side effect, for the 'FromJSON Game' path where the
defs must be live before the entities that reference them are parsed.
-}
registerCustomCardsPure :: Map CardCode CustomCard -> ()
registerCustomCardsPure = unsafePerformIO . registerCustomCards

lookupCustomCard :: HasCardCode a => a -> Maybe CustomCard
lookupCustomCard (toCardCode -> cardCode)
  | not (isCustomCardCode cardCode) = Nothing
  | otherwise = unsafePerformIO $ Map.lookup cardCode <$> readIORef customCardRegistry

lookupCustomCardDef :: HasCardCode a => a -> Maybe CardDef
lookupCustomCardDef = fmap customCardDef . lookupCustomCard

{- | Stats that live on the entity rather than the card def -- a location's
shroud and clue value, an asset's health and sanity -- are carried in
'cdMeta' so a custom card can supply them without widening 'CardDef'. Absent
or unparseable keys fall back to the default.
-}
customMeta :: FromJSON a => Text -> a -> CardDef -> a
customMeta k fallback = fromMaybe fallback . customMetaMaybe k

-- | As 'customMeta', where absent and present-but-unparseable are both Nothing.
customMetaMaybe :: FromJSON a => Text -> CardDef -> Maybe a
customMetaMaybe k def = do
  v <- Map.lookup k (cdMeta def)
  parseMaybe parseJSON v
