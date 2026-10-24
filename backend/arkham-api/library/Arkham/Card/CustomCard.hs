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
import Arkham.Custom.SetDefs (withKeywordEffects)
import Arkham.Id (InvestigatorId (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Name (Name)
import Arkham.Prelude
import Data.Aeson.KeyMap qualified as KeyMap
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

{- | A stand-in for a custom card whose definition is not to hand.

A custom def lives on the game and in a process-global registry, so a game can
outlive the thing that describes its cards: the card is deleted from its
author's library, or the process restarts holding a game that never recorded its
own copy. The def is then simply gone, and the entity cannot be built.

Crashing there takes the whole game down -- @error "invalid assets"@ turned into
a 500 on every load, with nothing saying which card. A card that exists and does
nothing loses only that card, and can be pointed at the builder to be restored.

The card keeps its code, so the client can look it up and offer to open or
recreate it, and carries 'missingCustomCardTag' to say plainly that this is not
what the author wrote.
-}
missingCustomCardTag :: Text
missingCustomCardTag = "missing-custom-card"

missingCustomCardDef :: CardType -> CardCode -> CardDef
missingCustomCardDef cardType cardCode =
  (emptyCardDef cardCode ("Missing card" :: Name) cardType)
    { cdCardTraits = mempty
    , cdTags = [missingCustomCardTag]
    , -- Building an investigator reads its class, so a stand-in without one
      -- throws rather than losing the one card this exists to lose.
      cdClassSymbols = singleton #neutral
    }

{- | The def for a custom code, or a stand-in when it is not to hand.

Used where an entity has to be built: a missing def costs that one card rather
than the game. A code that is not a custom card at all still yields 'Nothing',
so a missing *printed* card keeps failing loudly -- that is a bug in the engine,
not a card someone deleted.
-}
lookupCustomCardDefOrMissing :: HasCardCode a => CardType -> a -> Maybe CardDef
lookupCustomCardDefOrMissing cardType (toCardCode -> cardCode) =
  lookupCustomCardDef cardCode
    <|> (missingCustomCardDef cardType cardCode <$ guard (isCustomCardCode cardCode))

-- | Whether a def is one of those stand-ins rather than a real card.
isMissingCustomCard :: HasCardDef a => a -> Bool
isMissingCustomCard = elem missingCustomCardTag . cdTags . toCardDef

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

{- | The ids arkham.build names a custom card by: a dashed UUID, or the bare
32-hex and short 8-hex forms a pack's cards come through with.

Kept beside the derivation below because the two are one question asked twice --
what counts as an arkham.build id, and what code does it become. The frontend
asks the same pair in @customCards.ts@ (@isArkhamBuildCardId@ /
@arkhamBuildCustomCardCode@) and the two sides have to agree exactly: a deck
naming a card by an id one side translates and the other does not is a deck that
fails validation as 'UnimplementedCard' with the card sitting in the library.

Printed codes are five or six digits and are none of these.
-}
isArkhamBuildCardId :: Text -> Bool
isArkhamBuildCardId t = bare || dashed
 where
  hexOfLength n s = T.length s == n && T.all (`elem` ("0123456789abcdefABCDEF" :: String)) s
  bare = hexOfLength 32 t || hexOfLength 8 t
  dashed = case T.splitOn "-" t of
    [a, b, c, d, e] -> and $ zipWith hexOfLength [8, 4, 4, 4, 12] [a, b, c, d, e]
    _ -> False

{- | The code an import of an arkham.build pack gives the card with this id.

Deterministic, because the card is imported once and named by a deck later; a
minted code would never match up with itself. Ends in a digit for the reason
'sanitizeCustomCardCode' gives.
-}
arkhamBuildCustomCardCode :: Text -> CardCode
arkhamBuildCustomCardCode t =
  CardCode $ customCardPrefix <> T.toLower (T.filter (/= '-') t) <> "0"

{- | arkham.build's "Investigator Placeholders" pack.

Johnny Anybody and Jane Doe: blank investigators the site ships so a deck can be
built without one. They print no ability, -2 in every skill, and "this front is
left intentionally blank" -- they are not meant to be played there either. A deck
built on one is a deck with no investigator of its own, waiting for an overlay to
say whose it is (see "Arkham.Custom.Overlay"), which is how a deck for a card
builder investigator gets built on a site that cannot know them.

Recognized by pack wherever the deck carries one, so a card the pack gains later
counts without being listed here; the two ids are what a deck arriving without
that meta is recognized by. Mirrored in @customCards.ts@ for the same reason
'arkhamBuildCustomCardCode' is.
-}
arkhamBuildPlaceholderPack :: Text
arkhamBuildPlaceholderPack = "d12a4d9c-8c65-4df0-be0c-c2e14af65a21"

arkhamBuildPlaceholderCodes :: Set CardCode
arkhamBuildPlaceholderCodes =
  setFromList
    $ map
      arkhamBuildCustomCardCode
      [ "347ff5d0-8521-4d9b-a0fe-90c06114057d" -- Johnny Anybody
      , "c13c4114-0769-410c-864b-e43f5a596c0d" -- Jane Doe
      ]

isArkhamBuildPlaceholder :: HasCardCode a => a -> Bool
isArkhamBuildPlaceholder (toCardCode -> cardCode) =
  sanitizeCustomCardCode cardCode `member` arkhamBuildPlaceholderCodes

-- | The meta key a card records 'arkhamBuildIdOf' under.
arkhamBuildIdKey :: Text
arkhamBuildIdKey = "arkhamBuildId"

{- | The arkham.build card this one stands in for, when its author said so.

A card imported from an arkham.build pack needs none of this: its code is
derived from the id, so a deck naming the id lands on it. A card built here has
a code of its own and no way to be named by a deck built there -- which is the
whole of the problem when the same card exists in both places, as a set
published to arkham.build and rebuilt here. The id is the author's claim that
the two are the same card; nothing checks it, because nothing here can.

Validated on the way out rather than on the way in, so a mistyped id is a card
that answers to one code instead of a card that mints a nonsense second one.
-}
arkhamBuildIdOf :: CardDef -> Maybe Text
arkhamBuildIdOf def = do
  String t <- Map.lookup arkhamBuildIdKey (cdMeta def)
  guard $ isArkhamBuildCardId t
  pure t

{- | Every code a custom card answers to: its own, and the one an arkham.build
deck would name it by.

The first is the card's identity and the rest are aliases, so a caller building
a map keyed by code should keep this order and let the real code win a tie.
-}
customCardCodes :: CustomCard -> [CardCode]
customCardCodes card =
  cdCardCode def : map arkhamBuildCustomCardCode (maybeToList $ arkhamBuildIdOf def)
 where
  def = customCardDef card

{- | A library keyed by every code its cards answer to, ready for the registry.

Aliases underneath, so a card's own code always wins one. Importing the
arkham.build pack gives its card a code derived from the same id a hand-built
card can claim, and in that collision the imported card is the one the deck
means. Left-biased union rather than row order, which is `updatedAt` and says
nothing about which card is which.

One function because every path that loads a deck has to resolve the same codes:
keying it by hand at each of them drifted once already, and a deck naming a card
by the id it claims then missed it (#5891).
-}
customCardsByCode :: [CustomCard] -> Map CardCode CustomCard
customCardsByCode cards =
  Map.fromList [(cdCardCode (customCardDef c), c) | c <- cards]
    `Map.union` Map.fromList [(code, c) | c <- cards, code <- drop 1 (customCardCodes c)]

{- | The registry, with the one thing that can only be known from all of it.

'registrySignatureOwners' maps a signature card's code to the investigator that
lists it. Derived here, where the registry changes, rather than by searching for
it where it is asked: the question is asked of every custom def the engine
reads, thousands of times a turn, and answering each by walking the registry
made a game's cost grow with the number of custom cards the whole /process/ had
ever loaded -- every def of every game deserialized and every card of every
library registered since the last restart.
-}
data CustomCardRegistry = CustomCardRegistry
  { registryCards :: Map CardCode CustomCard
  , registrySignatureOwners :: Map CardCode CardCode
  , registryGeneration :: Int
  -- ^ Moves when, and only when, the cards above do. See 'customCardRegistryGeneration'.
  }

{-# NOINLINE customCardRegistry #-}
customCardRegistry :: IORef CustomCardRegistry
customCardRegistry = unsafePerformIO (newIORef (CustomCardRegistry mempty mempty 0))

{- | A counter that moves whenever the registry does.

Everything derived from a def -- not only the def itself -- stays good for
exactly as long as this stands still, so a cache anywhere in the engine can
hold its own entries and drop all of them at once without this module having to
know that it exists.
-}
customCardRegistryGeneration :: MonadIO m => m Int
customCardRegistryGeneration = registryGeneration <$> readIORef customCardRegistry

{- | Who claims each signature card, keyed by the code the card answers to.

Keyed and queried through 'sanitizeCustomCardCode' so the map's 'Ord' lookup
decides exactly what the 'elem' it replaced decided. 'Eq CardCode' treats a
trailing a/b/c/d as a side designator, which 'Ord' knows nothing about; a
sanitized code ends in a digit, has no side, and so compares structurally.

The registry key rather than the def's own code, and the earliest of them on a
tie, because that is the order a scan of the map would have found them in.
-}
signatureOwners :: Map CardCode CustomCard -> Map CardCode CardCode
signatureOwners cards =
  Map.fromListWith (\_later earlier -> earlier)
    $ [ (sigCode, ownerCode)
      | (ownerCode, card) <- Map.toList cards
      , let def = customCardDef card
      , cdCardType def == InvestigatorType
      , sigCode <- signatureCodes def
      ]
 where
  -- Raw: substitution asks who the owner is, which is what this answers.
  signatureCodes def = maybe [] (map sanitizeCustomCardCode) (rawMetaMaybe "_signatures" def)

{- | Add these cards to the registry, if they are not already exactly in it.

Registration happens twice per answer -- once for the game's own snapshot as it
is deserialized, once for the library of whoever is playing -- and almost every
time it is handed back precisely what it was handed last time. Noticing that is
the whole point: a registration that changes nothing leaves the generation
where it is, so every def and everything derived from one survives the answer
instead of being rebuilt from meta on the next message that asks for it.
-}
registerCustomCards :: MonadIO m => Map CardCode CustomCard -> m ()
registerCustomCards cards = unless (null cards) do
  changed <- atomicModifyIORef' customCardRegistry \existing ->
    let
      alreadyThere new old = if new == old then Nothing else Just new
      incoming = Map.differenceWith alreadyThere cards (registryCards existing)
     in
      if null incoming
        then (existing, False)
        else
          let !merged = incoming <> registryCards existing
           in (CustomCardRegistry merged (signatureOwners merged) (registryGeneration existing + 1), True)
  -- A derived def carries the signature restriction the rest of the registry
  -- implies, so any change to any card invalidates every one of them.
  when changed $ atomicModifyIORef' customCardDefCache (const (mempty, ()))

{- | Registration as a pure side effect, for the 'FromJSON Game' path where the
defs must be live before the entities that reference them are parsed.
-}
registerCustomCardsPure :: Map CardCode CustomCard -> ()
registerCustomCardsPure = unsafePerformIO . registerCustomCards

lookupCustomCard :: HasCardCode a => a -> Maybe CustomCard
lookupCustomCard (toCardCode -> cardCode)
  | not (isCustomCardCode cardCode) = Nothing
  | otherwise = unsafePerformIO $ Map.lookup cardCode . registryCards <$> readIORef customCardRegistry

{- | The def as the engine should see it, with the signature restriction the
investigator implies.

Being a signature is recorded on the investigator, which lists what it brings.
Deriving the restriction here rather than storing it on the card means a card
added to an investigator's signatures is theirs at once, instead of only after
it is next saved.
-}
lookupCustomCardDef :: HasCardCode a => a -> Maybe CardDef
lookupCustomCardDef (toCardCode -> cardCode)
  | not (isCustomCardCode cardCode) = Nothing
  | otherwise = unsafePerformIO do
      cached <- Map.lookup cardCode <$> readIORef customCardDefCache
      case cached of
        Just def -> pure (Just def)
        Nothing -> case deriveCustomCardDef <$> lookupCustomCard cardCode of
          Nothing -> pure Nothing
          Just def -> do
            atomicModifyIORef' customCardDefCache \m -> (Map.insert cardCode def m, ())
            pure (Just def)

{- | Derived defs, held until the registry they were derived from changes.

Deriving one parses and rebuilds the whole of a card's meta -- kilobytes of
ability DSL for an ordinary card -- and 'toCardDef' is a lookup as far as every
caller is concerned, so it is called freely and the derivation was being redone
on each of them. Caching the record is enough: its fields stay thunks, so a
caller still pays only for what it reads, and pays it once.
-}
{-# NOINLINE customCardDefCache #-}
customCardDefCache :: IORef (Map CardCode CardDef)
customCardDefCache = unsafePerformIO (newIORef mempty)

deriveCustomCardDef :: CustomCard -> CardDef
deriveCustomCardDef =
  withRevelationAbility
    . withAbilityZones
    . withSignatureRestriction
    -- Before the zones are derived, because a keyword's result may grant an
    -- ability with a zone of its own.
    . withKeywordEffects
    . customCardDef

{- | Whether the def prints the hidden keyword.

Only an enemy or a treachery can: being hidden is a placement in your hand, and
the other card types have nowhere to be put there. A card type that cannot be
hidden keeps the keyword as printed text and nothing more.
-}
isHiddenCustomCard :: CardDef -> Bool
isHiddenCustomCard def = printsHidden && canBeHidden
 where
  printsHidden = Keyword.Hidden `elem` cdKeywords def
  canBeHidden =
    cdCardType def `elem` [EnemyType, PlayerEnemyType, TreacheryType, PlayerTreacheryType]

-- | The meta key holding what a card's revelation does.
revelationMetaKey :: Text
revelationMetaKey = "_onRevelation"

-- | The meta key holding where a card's revelation leaves it.
revelationPlacementMetaKey :: Text
revelationPlacementMetaKey = "_revelationPlacement"

{- | A card that was given a revelation ability has one.

Read off what the def says rather than off its card type: steps under
@_onRevelation@, a @_revelationPlacement@ that puts the card somewhere, and the
hidden keyword -- which "secretly adds that card to your hand", and is the whole
of what a hidden card's revelation does -- each are a revelation ability. Any
card type can be given one, so an asset or an event with revelation steps counts
exactly as a treachery does.

Derived rather than asked for, because a def that reaches the engine by a path
other than the builder -- a set import, an export from another library, a def
written by hand or by the MCP -- has nothing to set the flag beside the ability,
and the two disagreeing is silent: 'hasRevelation' gates the draw, so the card
is drawn, the ability never runs, and the card stays in hand.
-}
withRevelationAbility :: CardDef -> CardDef
withRevelationAbility def
  | hasRevelationAbility, cdRevelation def == NoRevelation = def {cdRevelation = IsRevelation}
  | otherwise = def
 where
  hasRevelationAbility = isHiddenCustomCard def || placesItself || notNull steps
  steps = fromMaybe [] (rawMetaMaybe revelationMetaKey def) :: [Value]
  -- "none" is the builder's way of saying the steps place it, or nothing does.
  placesItself = maybe False (/= ("none" :: Text)) (customMetaMaybe revelationPlacementMetaKey def)

withSignatureRestriction :: CardDef -> CardDef
withSignatureRestriction def
  | isSignature def = def
  | otherwise = case customSignatureOwner def of
      Nothing -> def
      Just owner -> def {cdDeckRestrictions = Signature (coerce owner) : cdDeckRestrictions def}

{- | The zones a card's own abilities need it to exist in.

A card out of play is only built as an entity when its def asks for it, so an
ability that says it works from your hand has to reach the def too. Derived from
the abilities rather than set beside them, so the two cannot drift apart.
-}
withAbilityZones :: CardDef -> CardDef
withAbilityZones def = def {cdOutOfPlayEffects = nub (cdOutOfPlayEffects def <> derived)}
 where
  derived = mapMaybe zoneOf $ fromMaybe [] $ rawMetaMaybe "_abilities" def
  zoneOf = \case
    Object o -> case KeyMap.lookup "zone" o of
      Just (String "hand") -> Just InHandEffect
      Just (String "discard") -> Just InDiscardEffect
      Just (String "search") -> Just InSearchEffect
      Just (String "topOfDeck") -> Just OnTopOfDeckEffect
      Just (String "committed") -> Just CommittedEffect
      _ -> Nothing
    _ -> Nothing

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
  Map.lookup (sanitizeCustomCardCode cardCode)
    . registrySignatureOwners
    <$> readIORef customCardRegistry
