{- | Data-driven behaviour for debug-authored custom cards.

A custom card has no Haskell behind it, so what it does is carried as JSON in
the card def's meta:

* @_abilities@ -- abilities the card offers. Each declares itself (its
  'AbilityType', criteria, limit) and the steps to run when it resolves.
* @_handlers@ -- messages the card listens for. Each names a message tag and
  runs steps when a message with that tag is addressed to this card. A handler
  may also require fields of the message to match ("the source has to be me"),
  which a card written by hand would do by pattern matching.
* @_onRevelation@ -- what the card does when it is revealed, plus where it puts
  itself (@_revelationPlacement@). A revelation is not an ability anyone
  activates, so it gets a key rather than an entry in @_abilities@.
* @_modifiers@ -- modifiers the card hands out while it is in play. Each names
  what to match and the modifiers to give whatever it matches. Matching @card@
  rather than an entity targets the card itself, so the modifier is already
  there when the engine reads it at draw or spawn time.

Both run the same small step language:

* @query@ -- run a matcher and bind the result to a name.
* @let@ -- bind an expression over what is already bound (see "Arkham.Custom.Expr").
* @push@ -- push a message.
* @if@ -- branch on a criterion, or on whether a matcher found anything.
* @case@ -- the first branch whose condition holds, with an optional fallback.
* @forEach@ -- run steps once per thing a matcher finds.
* @choose@ -- offer the player named options, each running steps of its own.
* @chooseFrom@ -- offer one option per thing a matcher finds, binding it.
* @playCard@ -- play a card from hand, paying its cost, optionally discounted.
* @fight@ -- fight an enemy, with modifiers for that attack.
* @investigate@, @evade@, @parley@ -- start the matching skill test. Each binds
  the test it started as @$sid@, so a later step can scope a modifier to it, and
  each takes an @onReveal@ saying what happens if a named chaos token turns up
  during it, immediately or -- with @whenPassed@ -- once the test is known to
  have succeeded.
* @attack@ -- an enemy attacks an investigator.
* @ready@ -- ready a card.
* @draw@ -- draw cards, however many an expression works out to.
* @gather@ -- shuffle a card from an encounter set into a deck.
* @customize@ -- mark a checkbox on a customizable card you own.
* @useAbility@ -- resolve one of this card's own abilities, cost and all.
* @cancelBatch@ -- stop what a @would@ window is about to do, for "instead".

Scoped modifiers need no step of their own: @CreateWindowModifierEffect@ is an
ordinary message, so a @push@ covers "for this attack" and friends. A modifier
on a test the card itself starts is better said with that step's @modifiers@, or
against the @$sid@ it binds.

The JSON is written before the card exists, so it cannot name the entity it
belongs to. Instead it refers to values by @$name@: the entity's own serialized
fields (@$id@, @$placement@, …), @$source@ and @$target@, @$iid@ for whoever
used an ability, @$payment@ for what its cost took, @$window@
and @$w0@, @$w1@, … for the window that ability triggered on, @$message@ and
@$0@, @$1@, … for the fields of a handled message, and anything a @query@ or
@let@ step has bound. Those are substituted into the JSON
before it is decoded, so the decoder only ever sees ordinary, fully applied
JSON.
-}
module Arkham.Custom.Ability (
  customAbilities,
  customModifiers,
  runCustomAbility,
  isCustomAbility,
  pattern ZonedUseThisAbility,
  runCustomHandlers,
  runCustomSteps,
  customSteps,
  runCustomRevelation,
  abilitiesMetaKey,
  handlersMetaKey,
  modifiersMetaKey,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.GameLogger (HasGameLogger)
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.Query

-- Brings the Query instances into scope the way card modules get them, without
-- this module taking its own hs-boot edge on Arkham.Game (which pulls other
-- modules into the cycle and onto their boot interfaces).

import Arkham.Custom.Env
import Arkham.Custom.Steps
import Arkham.Helpers.Modifiers (ModifierType, modifySelect)
import Arkham.Helpers.Window (windowMatches)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted.Placement (Placeable, Placement (InPlayArea, InThreatArea), place)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Query (QueryElement)
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window, windowType)
import Control.Monad.Extra (findM)
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

revelationMetaKey :: Text
revelationMetaKey = "_onRevelation"

revelationPlacementMetaKey :: Text
revelationPlacementMetaKey = "_revelationPlacement"

data AbilitySpec = AbilitySpec
  { specType :: Value
  , specCriteria :: Maybe Value
  , specLimit :: Maybe Value
  , specTooltip :: Maybe Text
  , specZone :: Maybe Text
  {- ^ Where the card has to be for the ability to be live: @hand@, @discard@,
  @search@, @topOfDeck@, or in play when absent. A card only exists as an
  entity out of play if its def says so, which is why the def's zone list is
  derived from this rather than set beside it.
  -}
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
      <*> o
      .:? "zone"
      -- "effect" is the older name for a bare list of messages to push.
      <*> (o .:? "steps" >>= maybe (map pushStep <$> o .:? "effect" .!= []) pure)

pushStep :: Value -> Value
pushStep msg = object ["push" .= msg]

data ModifierSpec = ModifierSpec
  { modKind :: Text
  , modMatcher :: Value
  , modTypes :: [Value]
  , modCondition :: Maybe Value
  {- ^ Gate on the situation. Modifiers are gathered while reading the game, not
  while changing it, so a condition here can ask questions but cannot do
  anything.
  -}
  , modRequires :: [(Value, Value)]
  {- ^ Pairs that must be equal once substituted, as a handler's @requires@ is.
  Unlike @if@ this asks nothing of the game, so it is the way to gate on where
  the card is -- a card with an in-hand ability is an entity in hand /and/ once
  committed, and a query to tell those apart would ask for modifiers while
  modifiers are being collected.
  -}
  }

instance FromJSON ModifierSpec where
  parseJSON = withObject "ModifierSpec" \o ->
    ModifierSpec
      <$> o
      .: "kind"
      <*> o
      .: "matcher"
      <*> o
      .:? "modifiers"
      .!= []
      <*> o
      .:? "if"
      <*> o
      .:? "requires"
      .!= []

data HandlerSpec = HandlerSpec
  { handlerOn :: Text
  , handlerRequires :: [(Value, Value)]
  {- ^ Pairs that must be equal once substituted, so a handler can say which
  field of the message has to be this card.
  -}
  , handlerGlobal :: Bool
  {- ^ Listen even though the message does not name this card. The mention
  check keeps a handler from firing for messages aimed elsewhere, but a
  message about the game itself -- setup ending, the game ending -- names
  nobody, so it can only ever be heard by asking for it.
  -}
  , handlerSteps :: [Value]
  }

instance FromJSON HandlerSpec where
  parseJSON = withObject "HandlerSpec" \o ->
    HandlerSpec
      <$> o
      .: "on"
      <*> (map toPair <$> o .:? "requires" .!= [])
      <*> o
      .:? "global"
      .!= False
      <*> o
      .:? "steps"
      .!= []
   where
    toPair = \case
      [a, b] -> (a, b)
      xs -> (toJSON xs, toJSON xs)

metaSpecs :: FromJSON a => Text -> CardDef -> [a]
metaSpecs key def = fromMaybe [] do
  v <- Map.lookup key (cdMeta def)
  parseMaybe parseJSON v

type CustomEntity a = (HasCardDef a, Sourceable a, Targetable a, HasCardCode a, ToJSON a)

{- | The entity's own serialized fields, so @$id@ and friends resolve, plus the
source and target it is addressed by (which are not fields of the attrs) and, for
a signature card, the investigator it belongs to as @$investigator@.
-}
bindings :: CustomEntity a => a -> Env
bindings a = KeyMap.fromList (own <> signatureOf) <> fields
 where
  own = [("source", toJSON (toSource a)), ("target", toJSON (toTarget a))]
  -- The card may carry the restriction, or the investigator may simply list it.
  signatureOf = case declared <> listed of
    iid : _ -> [("investigator", toJSON iid)]
    [] -> []
  declared = [iid | Signature iid <- cdDeckRestrictions (toCardDef a)]
  listed = coerce (maybeToList (customSignatureOwner a))
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
      { abilityCriteria =
          zoneCriterion (specZone spec)
            <> fromMaybe (abilityCriteria ab) (decodeWith env =<< specCriteria spec)
      , abilityLimit = fromMaybe (abilityLimit ab) (decodeWith env =<< specLimit spec)
      , abilityTooltip = specTooltip spec <|> abilityTooltip ab
      }

{- | Where the card must be. Search and the top of the deck are zones the card
is /put/ into rather than states it can be asked about, so they add nothing.
-}
zoneCriterion :: Maybe Text -> Criterion
zoneCriterion = \case
  Just "hand" -> InYourHand
  Just "discard" -> InYourDiscard
  _ -> NoRestriction

{- | An ability use, however it arrived.

A card that is not in play is handed its messages wrapped in where it lives, so
an ability with a @zone@ never sees a bare 'UseThisAbility'. Matching through
the wrapper keeps the runners from having to know which zones exist.

The windows come along because an ability that says "that many" is talking about
the one it triggered on, and only the message still knows them.
-}
pattern ZonedUseThisAbility
  :: InvestigatorId -> Source -> Int -> [Window] -> Payment -> Message
pattern ZonedUseThisAbility iid source idx ws payment <-
  (zonedAbilityUse -> Just (iid, source, idx, ws, payment))

zonedAbilityUse :: Message -> Maybe (InvestigatorId, Source, Int, [Window], Payment)
zonedAbilityUse = \case
  InHand _ m -> zonedAbilityUse m
  InDiscard _ m -> zonedAbilityUse m
  InSearch m -> zonedAbilityUse m
  UseCardAbility iid source idx ws payment -> Just (iid, source, idx, ws, payment)
  _ -> Nothing

{- | Whether an ability index is one of this card's own @_abilities@.

A custom card's abilities are extended onto the ones its attrs already provide,
and those keep their own indices -- an enemy's basic fight is 'AbilityAttack'.
Only the card's own are ours to run; the rest must fall through to the attrs
runner that resolves them, or using one spends the action and does nothing.
-}
isCustomAbility :: HasCardDef a => a -> Int -> Bool
isCustomAbility a idx =
  idx >= 1 && idx <= length (metaSpecs @AbilitySpec abilitiesMetaKey (toCardDef a))

{- | Run ability @idx@. @$iid@ and the window are bound here rather than in
'customAbilities' because neither is known until someone uses the ability.
-}
runCustomAbility
  :: (CustomEntity a, HasGameLogger m, ReverseQueue m)
  => a -> InvestigatorId -> Int -> [Window] -> Payment -> m ()
runCustomAbility a iid idx ws payment =
  case drop (idx - 1) (metaSpecs @AbilitySpec abilitiesMetaKey (toCardDef a)) of
    spec : _ -> do
      windowEnv <- triggeringWindow a iid ws (specType spec)
      let env = windowEnv <> paymentBindings payment <> KeyMap.insert "iid" (toJSON iid) (bindings a)
      runSteps env (specSteps spec)
    [] -> pure ()

{- | What paying for the ability cost gave up, as @$payment@.

What was actually /taken/ -- the cards, the targets exhausted, the resources --
is read off it with an @apply@ expression, because which of those a cost yields
depends on the cost. Binding one of them eagerly would offer every ability a
list of discarded cards, empty for the ones that discarded nothing.
-}
paymentBindings :: Payment -> Env
paymentBindings payment = KeyMap.singleton "payment" (toJSON payment)

{- | The window the ability triggered on, as @$window@, with its fields as
@$w0@, @$w1@, … the way a handler binds a message's.

Several windows are open whenever an ability is offered, so the one meant is
found by asking the ability's own matcher which of them it accepts -- the same
question the engine asked in order to offer the ability at all. An ability with
no window of its own (an action, a fast ability) matches none and binds nothing.
-}
triggeringWindow
  :: (CustomEntity a, HasGame m) => a -> InvestigatorId -> [Window] -> Value -> m Env
triggeringWindow a iid ws specType' = case decodeWith (bindings a) specType' of
  Nothing -> pure mempty
  Just abilityType -> do
    let matcher = defaultAbilityWindow abilityType
    mwindow <- findM (\w -> windowMatches iid (toSource a) w matcher) ws
    pure $ case mwindow of
      Nothing -> mempty
      Just w -> KeyMap.fromList $ ("window", toJSON w) : windowFields w

-- | A window's own fields, positionally, as its serialized form carries them.
windowFields :: Window -> [(Key.Key, Value)]
windowFields w = case toJSON (windowType w) of
  Object o -> case KeyMap.lookup "contents" o of
    Just (Array xs) -> [(Key.fromText ("w" <> tshow i), x) | (i :: Int, x) <- zip [0 ..] (toList xs)]
    Just v -> [("w0", v)]
    Nothing -> []
  _ -> []

{- | Run the steps stored under a named meta key.

Used for the odd hook that is not an ability or a message handler -- an
investigator's elder sign, which resolves as part of the token rather than as
something anyone activates.
-}
runCustomSteps
  :: (CustomEntity a, HasGameLogger m, ReverseQueue m) => a -> InvestigatorId -> Text -> m ()
runCustomSteps a iid key = runSteps (KeyMap.insert "iid" (toJSON iid) (bindings a)) (customSteps a key)

-- | The steps under a meta key, so a caller can tell "does nothing" from "does something".
customSteps :: HasCardDef a => a -> Text -> [Value]
customSteps a key = fromMaybe [] (Map.lookup key (cdMeta (toCardDef a)) >>= parseMaybe parseJSON)

{- | What a card does when its revelation resolves.

Placement comes first, and is its own key rather than a step, because a card
that stays on the table has to put itself there before anything else runs: the
engine discards a treachery still in limbo once its revelation is over, and an
asset that placed itself nowhere is left in play with no home. "Put this into
play in your threat area" is most of what weaknesses say, so it should need
nothing written.
-}
runCustomRevelation
  :: (CustomEntity a, HasGameLogger m, Placeable a, ReverseQueue m) => a -> InvestigatorId -> m ()
runCustomRevelation a iid = do
  for_ (customRevelationPlacement (toCardDef a) iid) (place a)
  runCustomSteps a iid revelationMetaKey

customRevelationPlacement :: CardDef -> InvestigatorId -> Maybe Placement
customRevelationPlacement def iid = case customMetaMaybe revelationPlacementMetaKey def of
  Just ("threatArea" :: Text) -> Just (InThreatArea iid)
  Just "playArea" -> Just (InPlayArea iid)
  _ -> Nothing

-- * Handlers

{- | Run any handler listening for this message.

A handler fires when the message's tag matches and the message mentions this
entity -- by its target, its source, or its id. Without that check a handler
would fire for every copy of the card and for messages aimed at other entities
entirely.
-}
runCustomHandlers :: (CustomEntity a, HasGameLogger m, ReverseQueue m) => a -> Message -> m ()
runCustomHandlers a msg = case toJSON msg of
  Object o -> do
    let mentioned = any (`isSubValue` Object o) [toJSON (toTarget a), toJSON (toSource a)]
    for_ (metaSpecs @HandlerSpec handlersMetaKey (toCardDef a)) \handler ->
      when (mentioned || handlerGlobal handler) $ for_ (matched (handlerOn handler) o) \fields -> do
        let env = messageBindings o fields <> bindings a
            holds (l, r) = sameValue (substitute env l) (substitute env r)
        when (all holds (handlerRequires handler)) $ runSteps env (handlerSteps handler)
  _ -> pure ()
 where
  {- Many messages sit inside a grouping constructor -- @Defeated@ is really
  @DefeatMessage (Defeated_ ...)@ -- and the constructor inside carries a
  trailing underscore. A handler names the message the way the engine does, and
  the fields it binds come from whichever object actually holds them. -}
  matched name o = case KeyMap.lookup "tag" o of
    Just (String t) | t == name -> Just o
    _ -> case KeyMap.lookup "contents" o of
      Just (Object inner) | tagged name inner -> Just inner
      _ -> Nothing

  tagged name inner = case KeyMap.lookup "tag" inner of
    Just (String t) -> t == name || T.dropWhileEnd (== '_') t == name
    _ -> False

  messageBindings o fields =
    KeyMap.fromList
      $ ("message", Object o)
      : case KeyMap.lookup "contents" fields of
        Just (Array xs) -> [(Key.fromText (tshow i), x) | (i :: Int, x) <- zip [0 ..] (toList xs)]
        Just v -> [("0", v)]
        Nothing -> []

{- | Modifiers the card hands out while it is in play, to whatever its matcher
selects. This is how a custom card reaches other cards -- giving an enemy a
keyword, say -- rather than only acting on itself.
-}
customModifiers :: (CustomEntity a, HasModifiersM m) => a -> m ()
customModifiers a = for_ (metaSpecs @ModifierSpec modifiersMetaKey (toCardDef a)) \spec -> do
  let holds (l, r) = sameValue (substitute env l) (substitute env r)
  applies <-
    if all holds (modRequires spec)
      then maybe (pure True) (runReadCondition env) (modCondition spec)
      else pure False
  when applies $ case modKind spec of
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
