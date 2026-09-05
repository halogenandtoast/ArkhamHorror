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
import Arkham.Card.CardType
import Arkham.Id (InvestigatorId (..))
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

{- | The def as the engine should see it, with the signature restriction the
investigator implies.

Being a signature is recorded on the investigator, which lists what it brings.
Deriving the restriction here rather than storing it on the card means a card
added to an investigator's signatures is theirs at once, instead of only after
it is next saved.
-}
lookupCustomCardDef :: HasCardCode a => a -> Maybe CardDef
lookupCustomCardDef = fmap (withSignatureRestriction . customCardDef) . lookupCustomCard

withSignatureRestriction :: CardDef -> CardDef
withSignatureRestriction def
  | isSignature def = def
  | otherwise = case customSignatureOwner def of
      Nothing -> def
      Just owner -> def {cdDeckRestrictions = Signature (coerce owner) : cdDeckRestrictions def}

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
  parseMaybe parseJSON (substituteDefBindings def v)

-- | As 'customMetaMaybe', without substituting; for keys the bindings read.
rawMetaMaybe :: FromJSON a => Text -> CardDef -> Maybe a
rawMetaMaybe k def = Map.lookup k (cdMeta def) >>= parseMaybe parseJSON

{- | The @$name@ bindings a def knows about itself.

An entity's own bindings (@$id@, @$source@, whatever a query bound) are put in
by "Arkham.Custom.Ability" when an ability runs. Meta the card is /built/ from
-- an enemy's prey, where it spawns -- is read before any entity exists, so the
only binding it can have is the one the def alone knows: whose signature it is.
-}
defBindings :: CardDef -> [(Text, Value)]
defBindings def = [("investigator", toJSON iid) | iid <- take 1 (declared <> listed)]
 where
  declared = [iid | Signature iid <- cdDeckRestrictions def]
  listed = coerce (maybeToList (customSignatureOwner def))

substituteDefBindings :: CardDef -> Value -> Value
substituteDefBindings def = go
 where
  env = defBindings def
  go = \case
    String t | Just name <- T.stripPrefix "$" t, Just v <- lookup name env -> v
    Object o -> Object (fmap go o)
    Array xs -> Array (fmap go xs)
    v -> v

{- | The custom investigator that lists this card among its signatures.

The investigator names its signatures, not the other way round, so a card only
learns whose it is by asking. Used to bind @$investigator@ for a signature
card's own abilities.
-}
customSignatureOwner :: HasCardCode a => a -> Maybe CardCode
customSignatureOwner (toCardCode -> cardCode) = unsafePerformIO do
  registry <- readIORef customCardRegistry
  pure
    $ listToMaybe
      [ ownerCode
      | (ownerCode, card) <- Map.toList registry
      , let def = customCardDef card
      , cdCardType def == InvestigatorType
      , cardCode `elem` signatureCodes def
      ]
 where
  -- Raw: substitution asks who the owner is, which is what this answers.
  signatureCodes def =
    maybe [] (map sanitizeCustomCardCode) (rawMetaMaybe "_signatures" def)
