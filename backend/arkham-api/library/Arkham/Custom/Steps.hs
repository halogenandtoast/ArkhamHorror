{- | The step language a custom card's abilities and handlers are written in.

Each step is one JSON object naming what it does. "Arkham.Custom.Ability" owns
what a card /has/ -- its abilities, handlers and modifiers -- and calls into
'runSteps' to run any of them; this module owns what a step /does/.
-}
module Arkham.Custom.Steps where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.GameLogger (HasGameLogger, sendCustomCardIssue)
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck

-- Brings the Query instances into scope the way card modules get them, without
-- this module taking its own hs-boot edge on Arkham.Game (which pulls other
-- modules into the cycle and onto their boot interfaces).

import Arkham.Aspect (InsteadOf (..), IsAspect)
import Arkham.Calculation (GameCalculation (Fixed))
import Arkham.Card.PlayerCard (lookupPlayerCard)
import Arkham.Custom.Env
import Arkham.Custom.Expr (evalExpr, exprInt, runQuery, runQueryStep, valueList)
import Arkham.Customization (CustomizationChoice (..))
import Arkham.EffectMetadata (EffectMetadata (EffectModifiers))
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
import Arkham.Helpers.Location (Locateable, getLocationOf)
import Arkham.Helpers.Message (drawCards)
import Arkham.Helpers.Modifiers (
  ModifierType (ReduceCostOf),
  toModifiers,
  withModifiers,
 )
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest qualified as SkillTest
import Arkham.Helpers.Window (allWindows)
import Arkham.Homebrew.Defs (allTraits)
import Arkham.Id
import Arkham.Investigate (mkInvestigate, mkInvestigateLocation)
import Arkham.Investigate qualified as Investigate
import Arkham.Matcher
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted (
  aspect,
  cancelBatch,
  chooseFightEnemyEdit,
  initiateEnemyAttack,
  reduceCostOf,
  skillTestModifiers,
 )
import Arkham.Message.Lifted.Base (capture)
import Arkham.Message.Lifted.Card (playCardPayingCost)
import Arkham.Message.Lifted.Prompt qualified as Prompt
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Modifier (Modifier (Modifier))
import Arkham.Name (toTitle)
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Prelude
import Arkham.SkillType (SkillType (SkillWillpower))
import Arkham.Source
import Arkham.Target
import Arkham.Trait (displayTrait)
import Arkham.Window (defaultWindows, windowBatchId)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Function (on)
import Data.List (nubBy)

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
runFight :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m Env
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
  :: (HasGameLogger m, ReverseQueue m)
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
runOnReveal :: (HasGameLogger m, ReverseQueue m) => Env -> SkillTestId -> Source -> Value -> m ()
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
runInvestigate :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m Env
runInvestigate env spec = beginTest env spec \sid iid source -> do
  let o = specObject spec
  investigation <- case KeyMap.lookup "location" o >>= decodeWith env of
    Just lid -> mkInvestigateLocation sid iid source (lid :: LocationId)
    Nothing -> mkInvestigate sid iid source
  withTestSkill env o iid source Investigate.withSkillType investigation

-- | Evade an enemy, by default any you could evade.
runEvade :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m Env
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
runParley :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m Env
runParley env spec = beginTest env spec \sid iid source -> do
  let o = specObject spec
  for_ (KeyMap.lookup "target" o >>= decodeWith env) \target -> do
    let
      sType = fromMaybe SkillWillpower (KeyMap.lookup "skill" o >>= decodeWith env)
      difficulty = fromMaybe (Fixed 0) (KeyMap.lookup "difficulty" o >>= decodeWith env)
    push $ SkillTest.parley sid iid source (target :: Target) sType difficulty

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
runBasicFight
  :: (HasGameLogger m, ReverseQueue m) => Source -> InvestigatorId -> Maybe EnemyMatcher -> m ()
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
runCustomize :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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
runGather :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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
runReady :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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
runAttack :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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
runPlayCard :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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

    let
      matcher = KeyMap.lookup "matcher" o >>= decodeWith @CardMatcher env
      -- "Play the card you discarded" names one card outright, so there is
      -- nothing to search for and nothing to choose between.
      named = KeyMap.lookup "card" o >>= decodeWith @Card env
      free = KeyMap.lookup "free" o == Just (Bool True)
    cards <- case named of
      Just card -> pure [card]
      Nothing ->
        withModifiers iid (toModifiers source [ReduceCostOf AnyCard discount]) do
          playable <- getPlayableCards source iid (UnpaidCost NoAction) (defaultWindows iid)
          pure $ maybe playable (\m -> filter (`cardMatch` m) playable) matcher

    -- One option per card, shown as the card itself: two different events both
    -- reading "Play an event" is no choice at all.
    labels <- for cards \card -> do
      msgs <-
        capture
          $ if free
            -- Straight to playing it, which is the stage after paying.
            then push $ Msg.PlayCard iid card Nothing NoPayment (defaultWindows iid) False
            else do
              when (discount > 0) $ reduceCostOf source card discount
              playCardPayingCost iid card
      pure $ targetLabel card msgs

    let declined =
          [ Label (textField env o "declineLabel" "Do not") [] | KeyMap.lookup "optional" o == Just (Bool True)
          ]
    unless (null labels && null declined) $ Prompt.chooseOne iid (labels <> declined)
  _ -> pure ()

{- | Steps run in order, each seeing what the ones before it bound.

A @query@ and a @let@ add to the environment; everything else acts on it. A branch or
a choice runs its own steps against the same environment, so a binding made
before the branch is still there inside it.
-}
runSteps :: (HasGameLogger m, ReverseQueue m) => Env -> [Value] -> m ()
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
          case decodeWith env m of
            Just msg -> push msg
            -- A payload that will not decode is the card doing nothing at all,
            -- which looks exactly like a card that had nothing to do. Say so.
            Nothing -> reportBadPayload env "push" (substitute env m)
          pure env
      | Just condition <- KeyMap.lookup "if" o -> do
          taken <- runReadCondition env condition
          runSteps env $ branch o taken
          pure env
      {- An @if@ with nothing on the other side. The same thing, said without an
         empty branch to read past -- most conditions in a card have no else. -}
      | Just condition <- KeyMap.lookup "when" o -> do
          taken <- runReadCondition env condition
          when taken $ runSteps env (maybe [] subSteps (KeyMap.lookup "then" o))
          pure env
      | Just branches <- KeyMap.lookup "case" o -> do
          runCase env (subSteps branches) (maybe [] subSteps $ KeyMap.lookup "else" o)
          pure env
      | Just spec <- KeyMap.lookup "forEach" o -> do
          runForEach env spec
          pure env
      | Just spec <- KeyMap.lookup "modify" o -> do
          runModify env spec
          pure env
      | Just spec <- KeyMap.lookup "withSkillTest" o -> do
          runWithSkillTest env spec
          pure env
      | Just spec <- KeyMap.lookup "withLocationOf" o -> do
          runWithLocationOf env spec
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
      | Just (Bool True) <- KeyMap.lookup "cancelBatch" o -> do
          runCancelBatch env
          pure env
      | Just spec <- KeyMap.lookup "useAbility" o -> do
          runUseAbility env spec
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
      let source = fromMaybe GameSource (KeyMap.lookup "source" env >>= parseMaybe parseJSON)
      passesCriteria iid Nothing source source [] criterion
  _ -> notNull . fromMaybe [] <$> runQuery env condition

{- | Who to prompt. An ability knows (@$iid@ is bound when it resolves); a
handler does not, so it falls back to whoever the card belongs to, and only then
to the lead investigator answering for the table.
-}
stepInvestigator :: HasGame m => Env -> m InvestigatorId
stepInvestigator env =
  maybe getLead pure
    $ asum [KeyMap.lookup k env >>= parseMaybe parseJSON | k <- ["iid", "controller", "owner"]]

{- | The first branch whose condition holds. Nothing runs if none do, unless a
fallback is given.
-}
runCase :: (HasGameLogger m, ReverseQueue m) => Env -> [Value] -> [Value] -> m ()
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
runForEach :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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

{- | Give something modifiers for as long as a window lasts.

The message underneath is @CreateWindowModifierEffect@, whose payload is four
levels of wrapping around what a card actually says: a window, an effect holding
a list of modifiers, the source, and the target. Three of those never vary --
the source is this card, the modifier carries no card of its own, and nothing a
card does is active during setup -- so the step asks for the three that do.

The window defaults to the test being resolved, which is what almost every
"+2 for this test" is.
-}
runModify :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
runModify env spec = case spec of
  Object o -> do
    let types = maybe [] valueList (KeyMap.lookup "modifiers" o)
    let mWindow = decodeWith env (fromMaybe thisSkillTest (KeyMap.lookup "window" o))
    let mTarget = decodeWith env =<< KeyMap.lookup "target" o
    case (mWindow, mTarget, traverse (decodeWith @ModifierType env) types) of
      (Just window, Just target, Just modifiers)
        | notNull modifiers ->
            push
              $ CreateWindowModifierEffect
                window
                (EffectModifiers [Modifier source modifier False Nothing | modifier <- modifiers])
                source
                target
      _ -> reportBadPayload env "modify" spec
  _ -> reportBadPayload env "modify" spec
 where
  source = stepSource env
  -- What "for this test" means, which is the window nearly every modifier wants.
  thisSkillTest =
    object
      [ "tag" .= ("EffectSkillTestMatchingWindow" :: Text)
      , "contents" .= object ["tag" .= ("AnySkillTest" :: Text), "contents" .= ([] :: [Value])]
      ]

{- | The steps inside, with the skill test being resolved bound for them.

A block rather than a binding on every step: there may be no test, and a step
that reads @$skillTestId@ when there is none has nothing sensible to do. Running
the inner steps only when there is one says that once, where a card would say it
once -- "during a skill test".
-}
runWithSkillTest :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
runWithSkillTest env spec = case spec of
  Object o -> do
    SkillTest.getSkillTestId >>= traverse_ \sid ->
      runSteps (KeyMap.insert (bindingName o "skillTestId") (toJSON sid) env) (subStepsOf o)
  _ -> pure ()

{- | The steps inside, with the location of something bound for them.

Which kind of thing is being located has to be said: every id is a bare uuid, so
an @EnemyId@ and an @AssetId@ are the same JSON and the 'Locateable' instance
cannot be chosen from the value. Nothing runs when it is nowhere, which is what
@withLocationOf@ in "Arkham.Helpers.Location" already does.
-}
runWithLocationOf :: forall m. (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
runWithLocationOf env spec = case spec of
  Object o -> do
    subject <- evalExpr env (fromMaybe Null (KeyMap.lookup "of" o))
    let run :: forall a. (FromJSON a, Locateable a) => m ()
        run = case parseMaybe parseJSON subject of
          Nothing -> pure ()
          Just x ->
            getLocationOf (x :: a) >>= traverse_ \lid ->
              runSteps (KeyMap.insert (bindingName o "location") (toJSON lid) env) (subStepsOf o)
    case KeyMap.lookup "kind" o of
      Just (String "enemy") -> run @EnemyId
      Just (String "asset") -> run @AssetId
      Just (String "treachery") -> run @TreacheryId
      _ -> run @InvestigatorId
  _ -> pure ()

-- | The name a block binds under, which the author may rename.
bindingName :: KeyMap.KeyMap Value -> Text -> Key.Key
bindingName o fallback = case KeyMap.lookup "bind" o of
  Just (String n) | not (null n) -> Key.fromText n
  _ -> Key.fromText fallback

subStepsOf :: KeyMap.KeyMap Value -> [Value]
subStepsOf o = maybe [] subSteps (KeyMap.lookup "steps" o)

-- | Named options, each running its own steps.
runChoose :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
runChoose env spec = case spec of
  Object o -> do
    iid <- stepInvestigator env
    let options = maybe [] subSteps (KeyMap.lookup "options" o)
    labels <- for options \case
      Object opt -> do
        msgs <- capture $ runSteps env (maybe [] subSteps (KeyMap.lookup "steps" opt))
        pure [Label (textField env opt "label" "Choose") msgs]
      _ -> pure []
    unless (all null labels) $ Prompt.chooseOne iid (concat labels)
  _ -> pure ()

{- | Tell whoever is playing that a step could not be used.

The step language decodes into engine types at the last moment, so a wrong tag
or a missing field is not an error anywhere -- the value simply fails to parse
and the step is skipped. For a card being written that silence is the worst
possible answer, so it goes to the table with the fragment at fault and the card
to look at.
-}
reportBadPayload :: HasGameLogger m => Env -> Text -> Value -> m ()
reportBadPayload env stepName payload =
  sendCustomCardIssue
    (fromMaybe "" (KeyMap.lookup "cardCode" env >>= parseMaybe parseJSON))
    (stepName <> " could not be read as a message")
    payload

{- | Stop the thing the ability is reacting to.

A @would@ window opens a batch that carries the messages it is about to run, and
cancelling that batch is how "instead" is said -- a card that replaces its own
discard has to stop the discard, not merely act after it. The batch is the one
on the window the ability triggered on, which is what @$window@ holds.
-}
runCancelBatch :: (HasGameLogger m, ReverseQueue m) => Env -> m ()
runCancelBatch env =
  for_ (KeyMap.lookup "window" env >>= parseMaybe parseJSON) \w ->
    for_ (windowBatchId w) cancelBatch

{- | Resolve one of this card's own abilities.

An elder sign that reads "you may resolve the above reaction" is asking for the
ability the card already has, on the usual terms -- so it is offered rather than
run, and the cost is paid the way using it normally would. @ignoreLimit@ is for
the wording that says so explicitly; @optional@ for "you may".
-}
runUseAbility :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
runUseAbility env spec = case spec of
  Object o -> do
    iid <- stepInvestigator env
    let
      -- Whose ability. This card's unless another is named, which is how a
      -- signature reaches something on the investigator who owns it.
      source = fromMaybe (stepSource env) (KeyMap.lookup "source" o >>= decodeWith env)
      idx = fromMaybe 1 (KeyMap.lookup "index" o >>= parseMaybe parseJSON)
      unlimited = KeyMap.lookup "ignoreLimit" o == Just (Bool True)
    -- The window the ability wants is long gone by the time an elder sign
    -- resolves, so it is handed whatever is open now rather than nothing.
    ws <- allWindows
    abilities <- select (AbilityIs source idx)
    for_ (headMay abilities) \ab -> do
      let ab' = if unlimited then ab {abilityLimit = NoLimit} else ab
          declined =
            [ Label (textField env o "declineLabel" "Do not") []
            | KeyMap.lookup "optional" o == Just (Bool True)
            ]
      Prompt.chooseOne iid $ AbilityLabel iid ab' ws [] [] : declined
  _ -> pure ()

{- | Draw cards.

@amount@ is an expression, so "one card for each unique icon committed" is the
count that expression works out to rather than a number written in advance.
-}
runDraw :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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
runChooseFrom :: (HasGameLogger m, ReverseQueue m) => Env -> Value -> m ()
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

-- * Modifiers
