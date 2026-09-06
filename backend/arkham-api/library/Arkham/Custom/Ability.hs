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

Scoped modifiers need no step of their own: @CreateWindowModifierEffect@ is an
ordinary message, so a @push@ covers "for this attack" and friends. A modifier
on a test the card itself starts is better said with that step's @modifiers@, or
against the @$sid@ it binds.

The JSON is written before the card exists, so it cannot name the entity it
belongs to. Instead it refers to values by @$name@: the entity's own serialized
fields (@$id@, @$placement@, …), @$source@ and @$target@, @$iid@ for whoever
used an ability, @$window@ and @$w0@, @$w1@, … for the window that ability
triggered on, @$message@ and @$0@, @$1@, … for the fields of a handled message,
and anything a @query@ step has bound. Those are substituted into the JSON
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
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck

-- Brings the Query instances into scope the way card modules get them, without
-- this module taking its own hs-boot edge on Arkham.Game (which pulls other
-- modules into the cycle and onto their boot interfaces).

import Arkham.Aspect (InsteadOf (..), IsAspect)
import Arkham.Calculation (GameCalculation (Fixed))
import Arkham.Card.PlayerCard (lookupPlayerCard)
import Arkham.Custom.Expr (evalExpr, exprInt)
import Arkham.Customization (CustomizationChoice (..))
import Arkham.Evade (mkChooseEvade, mkChooseEvadeMatch)
import Arkham.Evade qualified as Evade
import Arkham.Fight (ChooseFight (..))
import Arkham.Helpers.Ability (getCanPerformAbility)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Customization (
  CustomizationChoiceType (..),
  cardRemainingCheckMarks,
  choicesRequired,
  customizationKey,
  hasCustomization_,
 )
import Arkham.Helpers.Message (drawCards)
import Arkham.Helpers.Modifiers (
  ModifierType (ReduceCostOf),
  modifySelect,
  toModifiers,
  withModifiers,
 )
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest qualified as SkillTest
import Arkham.Helpers.Window (windowMatches)
import Arkham.Homebrew.Defs (allTraits)
import Arkham.Id
import Arkham.Investigate (mkInvestigate, mkInvestigateLocation)
import Arkham.Investigate qualified as Investigate
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted (
  aspect,
  chooseFightEnemyEdit,
  initiateEnemyAttack,
  reduceCostOf,
  skillTestModifiers,
 )
import Arkham.Message.Lifted.Base (capture)
import Arkham.Message.Lifted.Card (playCardPayingCost)
import Arkham.Message.Lifted.Placement (Placeable, Placement (InPlayArea, InThreatArea), place)
import Arkham.Message.Lifted.Prompt qualified as Prompt
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Name (toTitle)
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Prelude
import Arkham.Query (QueryElement)
import Arkham.SkillType (SkillType (SkillWillpower))
import Arkham.Source
import Arkham.Target
import Arkham.Trait (displayTrait)
import Arkham.Window (Window, defaultWindows, windowType)
import Control.Monad.Extra (findM)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Function (on)
import Data.List (nubBy)
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

type Env = KeyMap.KeyMap Value

-- * Specs

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

{- | Equality for a @requires@ pair, with the two spellings of a nullary
constructor treated as one.

Aeson's tagged encoding leaves @contents@ off a constructor that has no fields,
so a placement comes back as @{"tag": "Limbo"}@, while the editor and anyone
writing the JSON by hand reach for @{"tag": "Limbo", "contents": []}@. Compared
raw, a requirement that looks exactly right never holds.
-}
sameValue :: Value -> Value -> Bool
sameValue a b = normalize a == normalize b
 where
  normalize = \case
    Object o
      | Just (Array xs) <- KeyMap.lookup "contents" o
      , null xs
      , KeyMap.member "tag" o ->
          Object (fmap normalize (KeyMap.delete "contents" o))
    Object o -> Object (fmap normalize o)
    Array xs -> Array (fmap normalize xs)
    v -> v

decodeWith :: FromJSON a => Env -> Value -> Maybe a
decodeWith env = parseMaybe parseJSON . substitute env

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
pattern ZonedUseThisAbility :: InvestigatorId -> Source -> Int -> [Window] -> Message
pattern ZonedUseThisAbility iid source idx ws <- (zonedAbilityUse -> Just (iid, source, idx, ws))

zonedAbilityUse :: Message -> Maybe (InvestigatorId, Source, Int, [Window])
zonedAbilityUse = \case
  InHand _ m -> zonedAbilityUse m
  InDiscard _ m -> zonedAbilityUse m
  InSearch m -> zonedAbilityUse m
  UseCardAbility iid source idx ws _ -> Just (iid, source, idx, ws)
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
  :: (CustomEntity a, ReverseQueue m) => a -> InvestigatorId -> Int -> [Window] -> m ()
runCustomAbility a iid idx ws =
  case drop (idx - 1) (metaSpecs @AbilitySpec abilitiesMetaKey (toCardDef a)) of
    spec : _ -> do
      windowEnv <- triggeringWindow a iid ws (specType spec)
      runSteps (windowEnv <> KeyMap.insert "iid" (toJSON iid) (bindings a)) (specSteps spec)
    [] -> pure ()

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
runCustomSteps :: (CustomEntity a, ReverseQueue m) => a -> InvestigatorId -> Text -> m ()
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
  :: (CustomEntity a, Placeable a, ReverseQueue m) => a -> InvestigatorId -> m ()
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
runCustomHandlers :: (CustomEntity a, ReverseQueue m) => a -> Message -> m ()
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

{- | Fight an enemy.

Two different things wear the word "fight". A card with the Fight action on it
/is/ a fight action when you play it, and the attack it then makes is just an
attack -- that is this step, and its source is the card. A /basic/ fight action
is the enemy's own attack ability, which no card can ever be; @basic@ offers
that instead, granted so that "immediately" does not cost an action.

Modifiers given here go on the attack's skill test, which is what "for this
attack" means. They have no home on a basic fight action, which makes its own
test.
-}
runFight :: ReverseQueue m => Env -> Value -> m Env
runFight env spec = do
  iid <- stepInvestigator env
  let
    o = specObject spec
    source = stepSource env
    matcher = KeyMap.lookup "matcher" o >>= decodeWith env
    isBasic = KeyMap.lookup "basic" o == Just (Bool True)
  if isBasic
    then env <$ runBasicFight source iid matcher
    else beginTest env spec \sid _ _ ->
      chooseFightEnemyEdit sid iid source \cf ->
        cf {chooseFightEnemyMatcher = fromMaybe (chooseFightEnemyMatcher cf) matcher}

{- | The scaffolding every step that starts a skill test shares.

The test's id is minted here and bound as @$sid@, which is the point: "for this
investigation" is a modifier scoped to a test that did not exist when the card
was written, so the only way to say it is for the step that starts the test to
hand the id to the steps after it. @modifiers@ is the common case and is applied
here; anything richer is a @push@ against @$sid@.
-}
beginTest
  :: ReverseQueue m
  => Env
  -> Value
  -> (SkillTestId -> InvestigatorId -> Source -> m ())
  -> m Env
beginTest env spec f = do
  iid <- stepInvestigator env
  let
    o = specObject spec
    source = stepSource env
    mods = fromMaybe [] (KeyMap.lookup "modifiers" o >>= decodeWith env)
  sid <- getRandom
  let env' = KeyMap.insert "sid" (toJSON sid) env
  unless (null mods) $ skillTestModifiers sid source iid mods
  -- Registered before the test starts, the way a printed card does it: the
  -- effect has to be watching by the time tokens are revealed.
  for_ (KeyMap.lookup "onReveal" o) (runOnReveal env' sid source)
  f sid iid source
  pure env'

{- | "If such a chaos token is revealed during this test, …".

Part of the step that starts the test rather than an ability of its own, because
that is what the card says: one ability, with a rider on the test it just began.
It also could not be an ability of its own -- a separate ability never saw the
@$sid@ this one minted, and so has no way to say /this/ test.
-}
runOnReveal :: ReverseQueue m => Env -> SkillTestId -> Source -> Value -> m ()
runOnReveal env sid source spec = do
  let
    o = specObject spec
    matcher = fromMaybe AnyChaosToken (KeyMap.lookup "tokens" o >>= decodeWith env)
    target = fromMaybe (toTarget sid) (KeyMap.lookup "target" env >>= parseMaybe parseJSON)
    -- A choice that only matters on a success should be offered once the result
    -- is known, not the moment the token turns up: otherwise you are asked to
    -- spend something before you know whether it buys anything.
    deferred = KeyMap.lookup "whenPassed" o == Just (Bool True)
  msgs <- capture $ runSteps env (maybe [] subSteps (KeyMap.lookup "steps" o))
  let
    queued
      | deferred = [SkillTest.onSucceedByEffect sid AnyValue source target msgs]
      | otherwise = msgs
  unless (null msgs) $ push $ CreateOnRevealChaosTokenEffect sid matcher source target queued

{- | Which skill a test uses.

@skill@ alone sets it outright. Paired with @insteadOf@ it goes through the
aspect instead, which substitutes only when the test would have used the skill
being replaced and honours @CanIgnoreAspect@ -- the difference between "uses
willpower" and "uses willpower instead of intellect".
-}
withTestSkill
  :: (ReverseQueue m, IsAspect InsteadOf a, IsMessage a)
  => Env
  -> KeyMap.KeyMap Value
  -> InvestigatorId
  -> Source
  -> (SkillType -> a -> a)
  -> a
  -> m ()
withTestSkill env o iid source setSkill action =
  case (field "skill", field "insteadOf") of
    (Just using, Just replaced) -> aspect iid source (using `InsteadOf` replaced) (pure action)
    (Just using, Nothing) -> push $ toMessage (setSkill using action)
    _ -> push $ toMessage action
 where
  field k = KeyMap.lookup k o >>= decodeWith @SkillType env

{- | Investigate.

Defaults to where you are; @location@ names somewhere else, which is what a card
that investigates a connecting location needs.
-}
runInvestigate :: ReverseQueue m => Env -> Value -> m Env
runInvestigate env spec = beginTest env spec \sid iid source -> do
  let o = specObject spec
  investigation <- case KeyMap.lookup "location" o >>= decodeWith env of
    Just lid -> mkInvestigateLocation sid iid source (lid :: LocationId)
    Nothing -> mkInvestigate sid iid source
  withTestSkill env o iid source Investigate.withSkillType investigation

-- | Evade an enemy, by default any you could evade.
runEvade :: ReverseQueue m => Env -> Value -> m Env
runEvade env spec = beginTest env spec \sid iid source -> do
  let o = specObject spec
  evasion <- case KeyMap.lookup "matcher" o >>= decodeWith env of
    Just matcher -> mkChooseEvadeMatch sid iid source (matcher :: EnemyMatcher)
    Nothing -> mkChooseEvade sid iid source
  withTestSkill env o iid source Evade.withSkillType evasion

{- | Parley against something.

Unlike the others this has nothing to derive its test from -- there is no
"parley action" the engine builds for you -- so the target, the skill and the
difficulty are all the card's to name.
-}
runParley :: ReverseQueue m => Env -> Value -> m Env
runParley env spec = beginTest env spec \sid iid source -> do
  let o = specObject spec
  for_ (KeyMap.lookup "target" o >>= decodeWith env) \target -> do
    let
      sType = fromMaybe SkillWillpower (KeyMap.lookup "skill" o >>= decodeWith env)
      difficulty = fromMaybe (Fixed 0) (KeyMap.lookup "difficulty" o >>= decodeWith env)
    push $ SkillTest.parley sid iid source (target :: Target) sType difficulty

specObject :: Value -> KeyMap.KeyMap Value
specObject = \case
  Object o -> o
  _ -> mempty

-- | The card the step belongs to, which is what the test is sourced from.
stepSource :: Env -> Source
stepSource env = fromMaybe GameSource (KeyMap.lookup "source" env >>= parseMaybe parseJSON)

{- | The enemy's own attack ability, at no action cost.

Asked for the way the engine asks -- @select@ over the ability list -- rather
than by sweeping the action bar. The two disagree for an enemy-location: its
basic actions are folded into the game ability list separately, and they are
sourced from the /location/, so an enemy matcher has to be asked both ways or
the option is silently missing (#5603).
-}
runBasicFight :: ReverseQueue m => Source -> InvestigatorId -> Maybe EnemyMatcher -> m ()
runBasicFight source iid matcher = do
  let
    ws = defaultWindows iid
    onTarget = case matcher of
      Nothing -> AnyAbility
      Just m -> AbilityOneOf [AbilityOnEnemy m, AbilityOnLocation (LocationWithEnemy m)]
  -- Granted, so "immediately take a basic fight action" does not cost one.
  abilities <-
    map (`decreaseAbilityActionCost` 1)
      <$> selectMap (setRequestor source) (BasicAbility <> #fight <> onTarget)
  fightable <- filterM (getCanPerformAbility iid ws) abilities
  unless (null fightable)
    $ Prompt.chooseOne iid [AbilityLabel iid ab ws [] [] | ab <- fightable]

{- | Mark a checkbox on an upgrade sheet for a customizable card you own.

The flow the printed cards use, written here rather than shared with them: pick
one of your customizable cards that still has a box free, and pick which
customization to mark. A customization whose last box asks for something (a
trait, a card, a skill) asks for it in the same breath -- a step has nowhere to
defer to, so it cannot re-enter itself the way a card with its own handlers can.
-}
runCustomize :: ReverseQueue m => Env -> Value -> m ()
runCustomize env spec = do
  let
    o = case spec of
      Object o' -> o'
      _ -> mempty
  -- Named explicitly when a loop is doing this once per investigator; the
  -- loop binds its own name, not @iid@.
  iid <- maybe (stepInvestigator env) pure (KeyMap.lookup "iid" o >>= decodeWith env)
  cards <- select $ OwnedBy (InvestigatorWithId iid) <> basic CardWithAvailableCustomization
  let choosable = filter (notNull . available) (nubBy ((==) `on` toCardCode) cards)
  options <- for choosable \card -> do
    msgs <- capture $ chooseCustomization iid card
    pure $ Label (toTitle card) msgs
  let declined =
        [ Label (textField env o "declineLabel" "Skip") [] | KeyMap.lookup "optional" o /= Just (Bool False)
        ]
  unless (null options) $ Prompt.chooseOne iid (options <> declined)
 where
  -- The card first, then which of its boxes -- so the second prompt's labels can
  -- be the customization's own name key rather than text built around it.
  chooseCustomization iid card = do
    options <- for (available card) \customization -> do
      msgs <- capture $ mark iid card customization
      pure $ Label (customizationKey customization) msgs
    unless (null options) $ Prompt.chooseOne iid options

  available card =
    let cardCustomizations = cdCustomizations (toCardDef card)
     in case card of
          PlayerCard pc ->
            filter
              (not . hasCustomization_ cardCustomizations (pcCustomizations pc))
              (keys cardCustomizations)
          _ -> []

  mark iid card customization = do
    let increase = IncreaseCustomization iid (toCardCode card) customization
    case (cardRemainingCheckMarks card customization, choicesRequired customization) of
      (Just 1, choice : _) -> Prompt.chooseOneDropDown iid (map (second (increase . pure)) (offers choice))
      _ -> push (increase [])

  offers = \case
    CustomizationTraitChoice -> [(displayTrait t, ChosenTrait t) | t <- allTraits]
    CustomizationSkillChoice -> [(tshow st, ChosenSkill st) | st <- [minBound ..]]
    CustomizationIndexChoice zs -> [(tshow z, ChosenIndex n) | (n, z) <- withIndex zs]
    CustomizationCardChoice matcher ->
      [ (title, ChosenCard title)
      | title <-
          sort
            $ nub
            $ map toTitle
            $ filter ((`cardMatch` matcher) . (`lookupPlayerCard` nullCardId)) (toList allPlayerCards)
      ]

{- | Gather a card from an encounter set into a deck.

"Gather X from the Y encounter set" is a setup instruction, and setup is over
by the time a card in play can act, so the nearest honest equivalent is to
shuffle the card in where it would have ended up.
-}
runGather :: ReverseQueue m => Env -> Value -> m ()
runGather env spec = do
  let
    o = case spec of
      Object o' -> o'
      _ -> mempty
    into = fromMaybe Deck.EncounterDeck (KeyMap.lookup "into" o >>= decodeWith env)
  for_ (KeyMap.lookup "cardCode" o >>= decodeWith env) \cardCode ->
    for_ (lookupCardDef (cardCode :: CardCode)) \def -> do
      card <- genEncounterCard def
      push $ ShuffleCardsIntoDeck into [toCard card]

{- | Ready a card.

@Ready@ is a pattern synonym, so it is not a constructor the editor's schema
knows and a raw push of it shows up blank. As a step it reads as what it is, and
defaults to this card.
-}
runReady :: ReverseQueue m => Env -> Value -> m ()
runReady env spec = do
  let
    o = case spec of
      Object o' -> o'
      _ -> mempty
    target =
      (KeyMap.lookup "target" o >>= decodeWith env)
        <|> (KeyMap.lookup "target" env >>= parseMaybe parseJSON)
  for_ target $ push . Ready

{- | An enemy attacks.

Defaults to this card attacking whoever triggered the ability, which is what
"it makes an immediate attack against you" means on an enemy.
-}
runAttack :: ReverseQueue m => Env -> Value -> m ()
runAttack env spec = do
  iid <- stepInvestigator env
  let
    o = case spec of
      Object o' -> o'
      _ -> mempty
    source = fromMaybe GameSource (KeyMap.lookup "source" env >>= parseMaybe parseJSON)
    target = fromMaybe (toTarget iid) (KeyMap.lookup "target" o >>= decodeWith env)
    enemy =
      (KeyMap.lookup "enemy" o >>= decodeWith env)
        <|> (KeyMap.lookup "id" env >>= parseMaybe parseJSON)
  for_ enemy \eid -> initiateEnemyAttack (eid :: EnemyId) source target

{- | Play a card from hand, paying its cost.

A discount has to be decided before the choice is offered, not after: a card is
only playable if you can afford it, so the cost reduction must be in effect while
playability is worked out. 'withModifiers' does that as a question -- what would
be playable if this discount applied -- and the reduction is then really applied
on the branch the player takes.
-}
runPlayCard :: ReverseQueue m => Env -> Value -> m ()
runPlayCard env spec = case spec of
  Object o -> do
    iid <- stepInvestigator env
    -- The card this ability belongs to, which is what pays for and plays it.
    let source = fromMaybe GameSource (KeyMap.lookup "source" env >>= parseMaybe parseJSON)
    discount <- do
      let amount = fromMaybe 0 (KeyMap.lookup "discount" o >>= parseMaybe parseJSON)
      case KeyMap.lookup "discountIf" o of
        Nothing -> pure amount
        Just condition -> bool 0 amount <$> runReadCondition env condition

    let matcher = KeyMap.lookup "matcher" o >>= decodeWith @CardMatcher env
    cards <-
      withModifiers iid (toModifiers source [ReduceCostOf AnyCard discount]) do
        playable <- getPlayableCards source iid (UnpaidCost NoAction) (defaultWindows iid)
        pure $ maybe playable (\m -> filter (`cardMatch` m) playable) matcher

    -- One option per card, shown as the card itself: two different events both
    -- reading "Play an event" is no choice at all.
    labels <- for cards \card -> do
      msgs <- capture do
        when (discount > 0) $ reduceCostOf source card discount
        playCardPayingCost iid card
      pure $ targetLabel card msgs

    let declined =
          [ Label (textField env o "declineLabel" "Do not") [] | KeyMap.lookup "optional" o == Just (Bool True)
          ]
    unless (null labels && null declined) $ Prompt.chooseOne iid (labels <> declined)
  _ -> pure ()

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

{- | Steps run in order, each seeing what the ones before it bound.

A @query@ and a @let@ add to the environment; everything else acts on it. A branch or
a choice runs its own steps against the same environment, so a binding made
before the branch is still there inside it.
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
      | Just (String name) <- KeyMap.lookup "let" o -> do
          value <- evalExpr env (fromMaybe Null (KeyMap.lookup "be" o))
          pure $ KeyMap.insert (Key.fromText name) value env
      | Just m <- KeyMap.lookup "push" o -> do
          for_ (decodeWith env m) push
          pure env
      | Just condition <- KeyMap.lookup "if" o -> do
          taken <- runReadCondition env condition
          runSteps env $ branch o taken
          pure env
      | Just branches <- KeyMap.lookup "case" o -> do
          runCase env (subSteps branches) (fromMaybe [] (subSteps <$> KeyMap.lookup "else" o))
          pure env
      | Just spec <- KeyMap.lookup "forEach" o -> do
          runForEach env spec
          pure env
      | Just spec <- KeyMap.lookup "choose" o -> do
          runChoose env spec
          pure env
      | Just spec <- KeyMap.lookup "chooseFrom" o -> do
          runChooseFrom env spec
          pure env
      | Just spec <- KeyMap.lookup "playCard" o -> do
          runPlayCard env spec
          pure env
      | Just spec <- KeyMap.lookup "fight" o -> runFight env spec
      | Just spec <- KeyMap.lookup "investigate" o -> runInvestigate env spec
      | Just spec <- KeyMap.lookup "evade" o -> runEvade env spec
      | Just spec <- KeyMap.lookup "parley" o -> runParley env spec
      | Just spec <- KeyMap.lookup "attack" o -> do
          runAttack env spec
          pure env
      | Just spec <- KeyMap.lookup "ready" o -> do
          runReady env spec
          pure env
      | Just spec <- KeyMap.lookup "draw" o -> do
          runDraw env spec
          pure env
      | Just spec <- KeyMap.lookup "gather" o -> do
          runGather env spec
          pure env
      | Just spec <- KeyMap.lookup "customize" o -> do
          runCustomize env spec
          pure env
    _ -> pure env

  branch o taken = fromMaybe [] do
    steps <- KeyMap.lookup (if taken then "then" else "else") o
    parseMaybe parseJSON steps

{- | A condition is either a criterion or a matcher.

A criterion covers what a card says about the situation ("if you succeeded by 2
or more"); a matcher covers what is on the table. Both read as @if@ in the
editor.
-}
runReadCondition :: HasGame m => Env -> Value -> m Bool
runReadCondition env condition = case condition of
  Object o | Just criteria <- KeyMap.lookup "criteria" o -> case decodeWith env criteria of
    Nothing -> pure False
    Just criterion -> do
      iid <- stepInvestigator env
      let source = fromMaybe (GameSource) (KeyMap.lookup "source" env >>= parseMaybe parseJSON)
      passesCriteria iid Nothing source source [] criterion
  _ -> notNull . fromMaybe [] <$> runQuery env condition

subSteps :: Value -> [Value]
subSteps v = fromMaybe [] (parseMaybe parseJSON v)

{- | Who to prompt. An ability knows (@$iid@ is bound when it resolves); a
handler does not, so it falls back to whoever the card belongs to, and only then
to the lead investigator answering for the table.
-}
stepInvestigator :: HasGame m => Env -> m InvestigatorId
stepInvestigator env =
  case asum [KeyMap.lookup k env >>= parseMaybe parseJSON | k <- ["iid", "controller", "owner"]] of
    Just iid -> pure iid
    Nothing -> getLead

textField :: Env -> KeyMap.KeyMap Value -> Key.Key -> Text -> Text
textField env o key fallback = case substitute env <$> KeyMap.lookup key o of
  Just (String t) -> t
  _ -> fallback

{- | The first branch whose condition holds. Nothing runs if none do, unless a
fallback is given.
-}
runCase :: ReverseQueue m => Env -> [Value] -> [Value] -> m ()
runCase env branches fallback = go branches
 where
  go [] = runSteps env fallback
  go (b : rest) = case b of
    Object o -> do
      taken <- maybe (pure True) (runReadCondition env) (KeyMap.lookup "if" o)
      if taken
        then runSteps env (maybe [] subSteps (KeyMap.lookup "steps" o))
        else go rest
    _ -> go rest

{- | Steps once per thing the matcher finds, with it bound each time. The loop
is over what the query saw when it ran, so steps that change the board do not
change what is still to come.
-}
runForEach :: ReverseQueue m => Env -> Value -> m ()
runForEach env spec = case spec of
  Object o -> do
    found <- maybe (pure Nothing) (runQuery env) (KeyMap.lookup "query" o)
    let
      name = case KeyMap.lookup "bind" o of
        Just (String n) -> Key.fromText n
        _ -> "each"
      steps = maybe [] subSteps (KeyMap.lookup "steps" o)
    for_ (fromMaybe [] found) \value -> runSteps (KeyMap.insert name value env) steps
  _ -> pure ()

-- | Named options, each running its own steps.
runChoose :: ReverseQueue m => Env -> Value -> m ()
runChoose env spec = case spec of
  Object o -> do
    iid <- stepInvestigator env
    let options = maybe [] subSteps (KeyMap.lookup "options" o)
    labels <- for options \option -> case option of
      Object opt -> do
        msgs <- capture $ runSteps env (maybe [] subSteps (KeyMap.lookup "steps" opt))
        pure [Label (textField env opt "label" "Choose") msgs]
      _ -> pure []
    unless (null (concat labels)) $ Prompt.chooseOne iid (concat labels)
  _ -> pure ()

{- | Draw cards.

@amount@ is an expression, so "one card for each unique icon committed" is the
count that expression works out to rather than a number written in advance.
-}
runDraw :: ReverseQueue m => Env -> Value -> m ()
runDraw env spec = case spec of
  Object o -> do
    iid <- maybe (stepInvestigator env) pure (KeyMap.lookup "iid" o >>= decodeWith env)
    let source = fromMaybe GameSource (KeyMap.lookup "source" env >>= parseMaybe parseJSON)
    n <- maybe (pure 1) (exprInt env) (KeyMap.lookup "amount" o)
    when (n > 0) $ push $ drawCards iid source n
  _ -> pure ()

{- | One option per thing the matcher finds, with the found thing bound so the
steps can act on it. @optional@ adds a way to decline.
-}
runChooseFrom :: ReverseQueue m => Env -> Value -> m ()
runChooseFrom env spec = case spec of
  Object o -> do
    iid <- stepInvestigator env
    found <- maybe (pure Nothing) (runQuery env) (KeyMap.lookup "query" o)
    let
      name = case KeyMap.lookup "bind" o of
        Just (String n) -> Key.fromText n
        _ -> "chosen"
      steps = maybe [] subSteps (KeyMap.lookup "steps" o)
      kind = case KeyMap.lookup "query" o of
        Just (Object q) | Just (String k) <- KeyMap.lookup "kind" q -> k
        _ -> ""
    -- Shown as the thing itself. Three cards all labelled "Choose" is no
    -- choice at all, the same way two events both reading "Play an event"
    -- were not.
    labels <- for (fromMaybe [] found) \value -> do
      msgs <- capture $ runSteps (KeyMap.insert name value env) steps
      pure $ case chosenTarget kind value of
        Just t -> targetLabel t msgs
        Nothing -> Label (textField env o "label" "Choose") msgs
    let
      declined =
        [ Label (textField env o "declineLabel" "Do not") [] | KeyMap.lookup "optional" o == Just (Bool True)
        ]
    unless (null labels && null declined) $ Prompt.chooseOne iid (labels <> declined)
  _ -> pure ()

{- | What an option stands for, from the kind its query named. A card is bound
as the whole card, everything else as the id the matcher selected.
-}
chosenTarget :: Text -> Value -> Maybe Target
chosenTarget kind value = case kind of
  "enemy" -> EnemyTarget <$> decoded
  "location" -> LocationTarget <$> decoded
  "investigator" -> InvestigatorTarget <$> decoded
  "asset" -> AssetTarget <$> decoded
  "treachery" -> TreacheryTarget <$> decoded
  "event" -> EventTarget <$> decoded
  "skill" -> SkillTarget <$> decoded
  "story" -> StoryTarget <$> decoded
  "act" -> ActTarget <$> decoded
  "agenda" -> AgendaTarget <$> decoded
  "card" -> CardIdTarget . toCardId <$> (decoded :: Maybe Card)
  _ -> Nothing
 where
  decoded :: FromJSON a => Maybe a
  decoded = parseMaybe parseJSON value

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
