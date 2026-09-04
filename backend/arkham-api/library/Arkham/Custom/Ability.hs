{- | Data-driven behaviour for debug-authored custom cards.

A custom card has no Haskell behind it, so what it does is carried as JSON in
the card def's meta:

* @_abilities@ -- abilities the card offers. Each declares itself (its
  'AbilityType', criteria, limit) and the steps to run when it resolves.
* @_handlers@ -- messages the card listens for. Each names a message tag and
  runs steps when a message with that tag is addressed to this card.
* @_modifiers@ -- modifiers the card hands out while it is in play. Each names
  what to match and the modifiers to give whatever it matches. Matching @card@
  rather than an entity targets the card itself, so the modifier is already
  there when the engine reads it at draw or spawn time.

Both run the same tiny step language, which does exactly two things:

* @query@ -- run a matcher and bind the result to a name.
* @push@ -- push a message.

The JSON is written before the card exists, so it cannot name the entity it
belongs to. Instead it refers to values by @$name@: the entity's own serialized
fields (@$id@, @$placement@, …), @$source@ and @$target@, @$iid@ for whoever
used an ability, @$message@ and @$0@, @$1@, … for the fields of a handled
message, and anything a @query@ step has bound. Those are substituted into the
JSON before it is decoded, so the decoder only ever sees ordinary, fully
applied JSON.
-}
module Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  runCustomAbility,
  runCustomHandlers,
  abilitiesMetaKey,
  handlersMetaKey,
  modifiersMetaKey,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query

-- Brings the Query instances into scope the way card modules get them, without
-- this module taking its own hs-boot edge on Arkham.Game (which pulls other
-- modules into the cycle and onto their boot interfaces).

import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Helpers.Query ()
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Modifier (ModifierType)
import Arkham.Prelude
import Arkham.Query (QueryElement)
import Arkham.Source
import Arkham.Target
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

abilitiesMetaKey :: Text
abilitiesMetaKey = "_abilities"

handlersMetaKey :: Text
handlersMetaKey = "_handlers"

modifiersMetaKey :: Text
modifiersMetaKey = "_modifiers"

type Env = KeyMap.KeyMap Value

-- * Specs

data AbilitySpec = AbilitySpec
  { specType :: Value
  , specCriteria :: Maybe Value
  , specLimit :: Maybe Value
  , specTooltip :: Maybe Text
  , specSteps :: [Value]
  }

instance FromJSON AbilitySpec where
  parseJSON = withObject "AbilitySpec" \o ->
    AbilitySpec
      <$> o
      .: "type"
      <*> o
      .:? "criteria"
      <*> o
      .:? "limit"
      <*> o
      .:? "tooltip"
      -- "effect" is the older name for a bare list of messages to push.
      <*> (o .:? "steps" >>= maybe (map pushStep <$> o .:? "effect" .!= []) pure)

pushStep :: Value -> Value
pushStep msg = object ["push" .= msg]

data ModifierSpec = ModifierSpec
  { modKind :: Text
  , modMatcher :: Value
  , modTypes :: [Value]
  }

instance FromJSON ModifierSpec where
  parseJSON = withObject "ModifierSpec" \o ->
    ModifierSpec <$> o .: "kind" <*> o .: "matcher" <*> o .:? "modifiers" .!= []

data HandlerSpec = HandlerSpec
  { handlerOn :: Text
  , handlerSteps :: [Value]
  }

instance FromJSON HandlerSpec where
  parseJSON = withObject "HandlerSpec" \o ->
    HandlerSpec <$> o .: "on" <*> o .:? "steps" .!= []

metaSpecs :: FromJSON a => Text -> CardDef -> [a]
metaSpecs key def = fromMaybe [] do
  v <- Map.lookup key (cdMeta def)
  parseMaybe parseJSON v

-- * Substitution

{- | Replace every @"$name"@ string with the bound value, anywhere in the
structure. A whole string is replaced by a whole value, so a binding keeps its
type instead of being spliced into text.
-}
substitute :: Env -> Value -> Value
substitute env = go
 where
  go = \case
    String t | Just name <- T.stripPrefix "$" t -> fromMaybe (String t) (KeyMap.lookup (Key.fromText name) env)
    Object o -> Object (fmap go o)
    Array xs -> Array (fmap go xs)
    v -> v

decodeWith :: FromJSON a => Env -> Value -> Maybe a
decodeWith env = parseMaybe parseJSON . substitute env

type CustomEntity a = (HasCardDef a, Sourceable a, Targetable a, HasCardCode a, ToJSON a)

{- | The entity's own serialized fields, so @$id@ and friends resolve, plus the
source and target it is addressed by (which are not fields of the attrs).
-}
bindings :: CustomEntity a => a -> Env
bindings a =
  KeyMap.fromList [("source", toJSON (toSource a)), ("target", toJSON (toTarget a))] <> fields
 where
  fields = case toJSON a of
    Object o -> o
    _ -> mempty

-- * Abilities

-- | Abilities are numbered from 1, in the order they are written.
customAbilities :: CustomEntity a => a -> [Ability]
customAbilities a =
  [ applySpec spec (mkAbility a idx abilityType)
  | (idx, spec) <- zip [1 ..] (metaSpecs abilitiesMetaKey (toCardDef a))
  , Just abilityType <- [decodeWith env (specType spec)]
  ]
 where
  env = bindings a
  applySpec spec ab =
    ab
      { abilityCriteria = fromMaybe (abilityCriteria ab) (decodeWith env =<< specCriteria spec)
      , abilityLimit = fromMaybe (abilityLimit ab) (decodeWith env =<< specLimit spec)
      , abilityTooltip = specTooltip spec <|> abilityTooltip ab
      }

{- | Run ability @idx@. @$iid@ is bound here rather than in 'customAbilities'
because it is only known once someone uses the ability.
-}
runCustomAbility :: (CustomEntity a, ReverseQueue m) => a -> InvestigatorId -> Int -> m ()
runCustomAbility a iid idx =
  case drop (idx - 1) (metaSpecs @AbilitySpec abilitiesMetaKey (toCardDef a)) of
    spec : _ -> runSteps (KeyMap.insert "iid" (toJSON iid) (bindings a)) (specSteps spec)
    [] -> pure ()

-- * Handlers

{- | Run any handler listening for this message.

A handler fires when the message's tag matches and the message mentions this
entity -- by its target, its source, or its id. Without that check a handler
would fire for every copy of the card and for messages aimed at other entities
entirely.
-}
runCustomHandlers :: (CustomEntity a, ReverseQueue m) => a -> Message -> m ()
runCustomHandlers a msg = case toJSON msg of
  Object o -> do
    let tag = KeyMap.lookup "tag" o
        mentioned = any (`isSubValue` Object o) [toJSON (toTarget a), toJSON (toSource a)]
    when mentioned $ for_ (metaSpecs @HandlerSpec handlersMetaKey (toCardDef a)) \handler ->
      when (tag == Just (String (handlerOn handler)))
        $ runSteps (messageBindings o <> bindings a) (handlerSteps handler)
  _ -> pure ()
 where
  messageBindings o =
    KeyMap.fromList
      $ ("message", Object o)
      : case KeyMap.lookup "contents" o of
        Just (Array xs) -> [(Key.fromText (tshow i), x) | (i :: Int, x) <- zip [0 ..] (toList xs)]
        Just v -> [("0", v)]
        Nothing -> []

-- | Does this value appear anywhere inside that one?
isSubValue :: Value -> Value -> Bool
isSubValue needle haystack = go haystack
 where
  go v
    | v == needle = True
    | otherwise = case v of
        Object o -> any go (KeyMap.elems o)
        Array xs -> any go xs
        _ -> False

-- * Steps

{- | Steps run in order, each seeing what the ones before it bound. A step is
either a query that binds its result or a message to push.
-}
runSteps :: ReverseQueue m => Env -> [Value] -> m ()
runSteps env0 = void . foldM step env0
 where
  step env v = case v of
    Object o
      | Just q <- KeyMap.lookup "query" o -> do
          result <- runQueryStep env q (KeyMap.lookup "mode" o)
          pure $ case (KeyMap.lookup "bind" o, result) of
            (Just (String name), Just value) -> KeyMap.insert (Key.fromText name) value env
            _ -> env
      | Just m <- KeyMap.lookup "push" o -> do
          for_ (decodeWith env m) push
          pure env
    _ -> pure env

{- | @mode@ decides what a query binds: the whole list (the default), just the
first element, or how many there were.
-}
runQueryStep :: HasGame m => Env -> Value -> Maybe Value -> m (Maybe Value)
runQueryStep env q mode = fmap (fmap shape) (runQuery env q)
 where
  shape results = case mode of
    Just (String "first") -> fromMaybe Null (headMay results)
    Just (String "count") -> toJSON (length results)
    _ -> toJSON results

-- | Dispatch a @{"kind": …, "matcher": …}@ query onto the matcher it names.
runQuery :: HasGame m => Env -> Value -> m (Maybe [Value])
runQuery env v = case v of
  Object o -> case (KeyMap.lookup "kind" o, KeyMap.lookup "matcher" o) of
    (Just (String kind), Just matcher) -> case kind of
      "enemy" -> run @EnemyMatcher matcher
      "location" -> run @LocationMatcher matcher
      "investigator" -> run @InvestigatorMatcher matcher
      "asset" -> run @AssetMatcher matcher
      "treachery" -> run @TreacheryMatcher matcher
      "event" -> run @EventMatcher matcher
      "skill" -> run @SkillMatcher matcher
      "story" -> run @StoryMatcher matcher
      "act" -> run @ActMatcher matcher
      "agenda" -> run @AgendaMatcher matcher
      "card" -> run @ExtendedCardMatcher matcher
      _ -> pure Nothing
    _ -> pure Nothing
  _ -> pure Nothing
 where
  run
    :: forall a m'
     . (HasGame m', FromJSON a, Query a, ToJSON (QueryElement a))
    => Value
    -> m' (Maybe [Value])
  run matcher = case decodeWith env matcher of
    Nothing -> pure Nothing
    Just m -> Just . map toJSON <$> select (m :: a)

-- * Modifiers

{- | Modifiers the card hands out while it is in play, to whatever its matcher
selects. This is how a custom card reaches other cards -- giving an enemy a
keyword, say -- rather than only acting on itself.
-}
customModifiers :: (CustomEntity a, HasModifiersM m) => a -> m ()
customModifiers a = for_ (metaSpecs @ModifierSpec modifiersMetaKey (toCardDef a)) \spec ->
  case modKind spec of
    "enemy" -> apply @EnemyMatcher spec
    "location" -> apply @LocationMatcher spec
    "investigator" -> apply @InvestigatorMatcher spec
    "asset" -> apply @AssetMatcher spec
    "treachery" -> apply @TreacheryMatcher spec
    "event" -> apply @EventMatcher spec
    "skill" -> apply @SkillMatcher spec
    -- Cards rather than entities: this reaches a card before it is in play,
    -- which is the only way to change something the engine reads at draw or
    -- spawn time (a keyword deciding how an enemy enters play, say).
    "card" -> apply @ExtendedCardMatcher spec
    _ -> pure ()
 where
  env = bindings a
  apply
    :: forall q m'
     . ( HasModifiersM m'
       , FromJSON q
       , Query q
       , Targetable (QueryElement q)
       )
    => ModifierSpec
    -> m' ()
  apply spec = for_ (decodeWith env (modMatcher spec)) \matcher ->
    for_ (traverse (decodeWith @ModifierType env) (modTypes spec))
      $ modifySelect a (matcher :: q)
