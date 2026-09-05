{- | Data-driven behaviour for debug-authored custom cards.

A custom card has no Haskell behind it, so what it does is carried as JSON in
the card def's meta:

* @_abilities@ -- abilities the card offers. Each declares itself (its
  'AbilityType', criteria, limit) and the steps to run when it resolves.
* @_handlers@ -- messages the card listens for. Each names a message tag and
  runs steps when a message with that tag is addressed to this card. A handler
  may also require fields of the message to match ("the source has to be me"),
  which a card written by hand would do by pattern matching.
* @_modifiers@ -- modifiers the card hands out while it is in play. Each names
  what to match and the modifiers to give whatever it matches. Matching @card@
  rather than an entity targets the card itself, so the modifier is already
  there when the engine reads it at draw or spawn time.

Both run the same small step language:

* @query@ -- run a matcher and bind the result to a name.
* @push@ -- push a message.
* @if@ -- branch on a criterion, or on whether a matcher found anything.
* @case@ -- the first branch whose condition holds, with an optional fallback.
* @forEach@ -- run steps once per thing a matcher finds.
* @choose@ -- offer the player named options, each running steps of its own.
* @chooseFrom@ -- offer one option per thing a matcher finds, binding it.
* @playCard@ -- play a card from hand, paying its cost, optionally discounted.
* @fight@ -- fight an enemy, with modifiers for that attack.
* @attack@ -- an enemy attacks an investigator.
* @ready@ -- ready a card.
* @gather@ -- shuffle a card from an encounter set into a deck.
* @customize@ -- mark a checkbox on a customizable card you own.

Scoped modifiers need no step of their own: @CreateWindowModifierEffect@ is an
ordinary message, so a @push@ covers "for this attack" and friends.

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
  isCustomAbility,
  runCustomHandlers,
  runCustomSteps,
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

import Arkham.Card.PlayerCard (lookupPlayerCard)
import Arkham.Customization (CustomizationChoice (..))
import Arkham.Fight (ChooseFight (..))
import Arkham.Helpers.Ability (getCanPerformAbility)
import Arkham.Helpers.Criteria (passesCriteria)
import Arkham.Helpers.Customization (
  CustomizationChoiceType (..),
  cardRemainingCheckMarks,
  choicesRequired,
  hasCustomization_,
 )
import Arkham.Helpers.Modifiers (
  ModifierType (ReduceCostOf),
  modifySelect,
  toModifiers,
  withModifiers,
 )
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.Defs (allTraits)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted (
  chooseFightEnemyEdit,
  initiateEnemyAttack,
  reduceCostOf,
  skillTestModifiers,
 )
import Arkham.Message.Lifted.Base (capture)
import Arkham.Message.Lifted.Card (playCardPayingCost)
import Arkham.Message.Lifted.Prompt qualified as Prompt
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Name (toTitle)
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Prelude
import Arkham.Query (QueryElement)
import Arkham.Source
import Arkham.Target
import Arkham.Trait (displayTrait)
import Arkham.Window (defaultWindows)
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
  , modCondition :: Maybe Value
  {- ^ Gate on the situation. Modifiers are gathered while reading the game, not
  while changing it, so a condition here can ask questions but cannot do
  anything.
  -}
  }

instance FromJSON ModifierSpec where
  parseJSON = withObject "ModifierSpec" \o ->
    ModifierSpec <$> o .: "kind" <*> o .: "matcher" <*> o .:? "modifiers" .!= [] <*> o .:? "if"

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
      { abilityCriteria = fromMaybe (abilityCriteria ab) (decodeWith env =<< specCriteria spec)
      , abilityLimit = fromMaybe (abilityLimit ab) (decodeWith env =<< specLimit spec)
      , abilityTooltip = specTooltip spec <|> abilityTooltip ab
      }

{- | Whether an ability index is one of this card's own @_abilities@.

A custom card's abilities are extended onto the ones its attrs already provide,
and those keep their own indices -- an enemy's basic fight is 'AbilityAttack'.
Only the card's own are ours to run; the rest must fall through to the attrs
runner that resolves them, or using one spends the action and does nothing.
-}
isCustomAbility :: HasCardDef a => a -> Int -> Bool
isCustomAbility a idx =
  idx >= 1 && idx <= length (metaSpecs @AbilitySpec abilitiesMetaKey (toCardDef a))

{- | Run ability @idx@. @$iid@ is bound here rather than in 'customAbilities'
because it is only known once someone uses the ability.
-}
runCustomAbility
  :: (CustomEntity a, ReverseQueue m) => a -> InvestigatorId -> Int -> m ()
runCustomAbility a iid idx =
  case drop (idx - 1) (metaSpecs @AbilitySpec abilitiesMetaKey (toCardDef a)) of
    spec : _ -> runSteps (KeyMap.insert "iid" (toJSON iid) (bindings a)) (specSteps spec)
    [] -> pure ()

{- | Run the steps stored under a named meta key.

Used for the odd hook that is not an ability or a message handler -- an
investigator's elder sign, which resolves as part of the token rather than as
something anyone activates.
-}
runCustomSteps :: (CustomEntity a, ReverseQueue m) => a -> InvestigatorId -> Text -> m ()
runCustomSteps a iid key =
  runSteps (KeyMap.insert "iid" (toJSON iid) (bindings a))
    $ fromMaybe [] (Map.lookup key (cdMeta (toCardDef a)) >>= parseMaybe parseJSON)

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
            holds (l, r) = substitute env l == substitute env r
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
runFight :: ReverseQueue m => Env -> Value -> m ()
runFight env spec = do
  iid <- stepInvestigator env
  let
    o = case spec of
      Object o' -> o'
      _ -> mempty
    source = fromMaybe GameSource (KeyMap.lookup "source" env >>= parseMaybe parseJSON)
    matcher = KeyMap.lookup "matcher" o >>= decodeWith env
    isBasic = KeyMap.lookup "basic" o == Just (Bool True)
  if isBasic
    then runBasicFight source iid matcher
    else do
      let mods = fromMaybe [] (KeyMap.lookup "modifiers" o >>= decodeWith env)
      sid <- getRandom
      unless (null mods) $ skillTestModifiers sid source iid mods
      chooseFightEnemyEdit sid iid source \cf ->
        cf {chooseFightEnemyMatcher = fromMaybe (chooseFightEnemyMatcher cf) matcher}

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
  options <- for (available cards) \(card, customization) -> do
    msgs <- capture $ mark iid card customization
    pure $ Label (toTitle card <> ": " <> tshow customization) msgs
  let declined =
        [ Label (textField env o "declineLabel" "Do not") [] | KeyMap.lookup "optional" o /= Just (Bool False)
        ]
  unless (null options) $ Prompt.chooseOne iid (options <> declined)
 where
  -- One entry per box still free, so both choices are made at once.
  available cards = do
    card <- nubBy ((==) `on` toCardCode) cards
    let cardCustomizations = cdCustomizations (toCardDef card)
    case card of
      PlayerCard pc -> do
        customization <- keys cardCustomizations
        guard $ not (hasCustomization_ cardCustomizations (pcCustomizations pc) customization)
        pure (card, customization)
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

Only a @query@ adds to the environment; everything else acts on it. A branch or
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
      | Just spec <- KeyMap.lookup "fight" o -> do
          runFight env spec
          pure env
      | Just spec <- KeyMap.lookup "attack" o -> do
          runAttack env spec
          pure env
      | Just spec <- KeyMap.lookup "ready" o -> do
          runReady env spec
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
    labels <- for (fromMaybe [] found) \value -> do
      msgs <- capture $ runSteps (KeyMap.insert name value env) steps
      pure $ Label (textField env o "label" "Choose") msgs
    let
      declined =
        [ Label (textField env o "declineLabel" "Do not") [] | KeyMap.lookup "optional" o == Just (Bool True)
        ]
    unless (null labels && null declined) $ Prompt.chooseOne iid (labels <> declined)
  _ -> pure ()

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
  applies <- maybe (pure True) (runReadCondition env) (modCondition spec)
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
