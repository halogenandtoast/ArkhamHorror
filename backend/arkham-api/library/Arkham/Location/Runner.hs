{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Location.Runner (
  module Arkham.Location.Runner,
  module X,
) where

import Arkham.Ability as X hiding (PaidCost)
import Arkham.Calculation as X
import Arkham.Card.CardDef as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Helpers.Ability as X
import Arkham.Helpers.Effect as X
import Arkham.Helpers.Location as X
import Arkham.Helpers.Message as X hiding (
  DiscoverClues,
  EnemyDamage,
  EnemyDefeated,
  EnemyEvaded,
  MoveAction,
  RevealLocation,
 )
import Arkham.Helpers.Query as X
import Arkham.Helpers.SkillTest as X
import Arkham.Id as X
import Arkham.Location.Types as X
import Arkham.LocationSymbol as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Action qualified as Action
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Constants
import Arkham.Direction
import Arkham.Discover
import Arkham.Enemy.Types (Field (..))
import Arkham.Exception
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (checkAfter, checkWhen, checkWindows, frame, windows, wouldDoEach)
import Arkham.Helpers.Window qualified as Helpers
import Arkham.History
import Arkham.I18n
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.BreachStatus qualified as Breach
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.Location.Helpers
import Arkham.Matcher (
  EnemyMatcher (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  accessibleTo,
  enemyAt,
  investigatorAt,
  noModifier,
 )
import Arkham.Message (Message (MoveAction, RevealLocation))
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Name (display, toName)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Spawn
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window
import Data.Function (on)
import Data.List (nubBy)
import Data.Map.Strict qualified as Map

pattern AfterFailedInvestigate :: InvestigatorId -> Target -> Message
pattern AfterFailedInvestigate iid target <-
  After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)

pattern UseResign :: InvestigatorId -> Source -> Message
pattern UseResign iid source <- UseCardAbility iid source 99 _ _

pattern UseDrawCardUnderneath :: InvestigatorId -> Source -> Message
pattern UseDrawCardUnderneath iid source <- UseCardAbility iid source 100 _ _

withRevealedAbilities :: LocationAttrs -> [Ability] -> [Ability]
withRevealedAbilities attrs other = withBaseAbilities attrs $ guard (locationRevealed attrs) *> other

extendRevealed :: LocationAttrs -> [Ability] -> [Ability]
extendRevealed = withRevealedAbilities

extendRevealed1 :: LocationAttrs -> Ability -> [Ability]
extendRevealed1 attrs ability = extendRevealed attrs [ability]

getModifiedRevealClueCount :: HasGame m => LocationAttrs -> m Int
getModifiedRevealClueCount attrs = do
  mods <- getModifiers attrs
  getModifiedRevealClueCountWithMods mods attrs

getModifiedRevealClueCountWithMods :: HasGame m => [ModifierType] -> LocationAttrs -> m Int
getModifiedRevealClueCountWithMods mods attrs =
  if CannotPlaceClues `elem` mods
    then pure 0
    else do
      base <- getPlayerCountValue (locationRevealClues attrs)
      pure $ foldl' applyModifier base mods
 where
  applyModifier base ReduceStartingCluesByHalf = (base + 1) `div` 2
  applyModifier base _ = base

instance RunMessage LocationAttrs where
  runMessage msg a@LocationAttrs {..} = case msg of
    UseAbility _ ab _ | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg
      pure a
    InSearch msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    InDiscard _ msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    InHand _ msg'@(UseAbility _ ab _) | isSource a ab.source || isProxySource a ab.source -> do
      push $ Do msg'
      pure a
    SetGlobal target key v | isTarget a target -> pure $ a & globalMetaL %~ insertMap key v
    UpdateLocation lid upd | lid == locationId -> do
      -- TODO: we may want life cycles around this, generally this might just be a bad idea
      pure $ updateLocation [upd] a
    FlipClues target n | isTarget a target -> do
      let clueCount = max 0 $ subtract n $ locationClues a
      pure $ a & tokensL %~ flipClues n & withoutCluesL .~ (clueCount == 0)
    FlipDoom target n | isTarget a target -> do
      let flipCount = min n $ locationDoom a
      pure $ a & tokensL %~ flipDoom n & withoutCluesL .~ (flipCount >= 0)
    Investigate investigation | investigation.location == locationId && not investigation.isAction -> do
      let iid = investigation.investigator
      allowed <- getInvestigateAllowed iid a
      when allowed $ do
        let target = maybe (toTarget a) (ProxyTarget (toTarget a)) investigation.target
        push
          $ investigate investigation.skillTest iid investigation.source target investigation.skillType
          $ LocationMaybeFieldCalculation a.id LocationShroud
      pure a
    PassedSkillTest iid (Just Action.Investigate) source (Initiator target) _ n | isTarget a target -> do
      let clues = locationClues a
      let (before, _, after) = frame $ Window.SuccessfullyInvestigateWithNoClues iid $ toId a
      push
        $ SkillTestResultOption ("Discover Clue at " <> display (toName a))
        $ [before | clues == 0]
        <> [ UpdateHistory iid (HistoryItem HistorySuccessfulInvestigations 1)
           , Successful (Action.Investigate, toTarget a) iid source (toTarget a) n
           ]
        <> [after | clues == 0]
      pure a
    PassedSkillTest iid (Just Action.Investigate) source (InitiatorProxy target actual) _ n | isTarget a target -> do
      let clues = locationClues a
      let (before, _, after) = frame $ Window.SuccessfullyInvestigateWithNoClues iid $ toId a
      push
        $ SkillTestResultOption ("Discover Clue at " <> display (toName a))
        $ [before | clues == 0]
        <> [Successful (Action.Investigate, toTarget a) iid source actual n]
        <> [after | clues == 0]
      pure a
    Successful (Action.Investigate, _) iid source target n | isTarget a target -> do
      let lid = toId a
      modifiers' <- getModifiers (LocationTarget lid)
      let (before, _, after) = frame (Window.SuccessfulInvestigation iid lid)
      let alternateSuccessfullInvestigation = mapMaybe (preview _AlternateSuccessfullInvestigation) modifiers'
      when (null alternateSuccessfullInvestigation)
        $ pushAll
          [before, Msg.DiscoverClues iid $ viaInvestigate $ discover lid (toSource a) 1, after]

      for_ alternateSuccessfullInvestigation $ \target' ->
        push $ Successful (Action.Investigate, toTarget lid) iid source target' n
      pure a
    FailedSkillTest iid (Just Action.Investigate) source (Initiator target) _ n | isTarget a target -> do
      push $ Failed (Action.Investigate, toTarget a) iid source (toTarget a) n
      pure a
    FailedSkillTest iid (Just Action.Investigate) source (InitiatorProxy target actual) _ n | isTarget a target -> do
      push $ Failed (Action.Investigate, toTarget a) iid source actual n
      pure a
    PlaceUnderneath (isTarget a -> True) cards -> do
      pure $ a & cardsUnderneathL %~ (nubBy ((==) `on` toCardId) . (<> cards))
    SetLocationLabel lid label' | lid == locationId -> do
      pure $ a & labelL .~ label'
    PlacedLocationDirection lid direction lid2 | lid2 == locationId -> do
      pure $ a & (directionsL %~ Map.insertWith (<>) direction [lid])
    PlacedLocationDirection lid direction lid2 | lid == locationId -> do
      let
        reversedDirection = case direction of
          LeftOf -> RightOf
          RightOf -> LeftOf
          Above -> Below
          Below -> Above

      pure $ a & (directionsL %~ Map.insertWith (<>) reversedDirection [lid2])
    LocationMoved lid | lid == locationId -> do
      pure $ a & (directionsL .~ mempty)
    LocationMoved lid | lid /= locationId -> do
      pure $ a & (directionsL %~ filterMap (/= []) . Map.map (filter (/= lid)))
    PutLocationInFrontOf iid lid | lid == locationId -> do
      pure $ a & inFrontOfL ?~ iid
    PutLocationInCenter lid | lid == locationId -> do
      pure $ a & inFrontOfL .~ Nothing
    Discard _ source target | isTarget a target -> do
      pushAll
        $ windows [Window.WouldBeDiscarded (toTarget a)]
        <> [Discarded (toTarget a) source (toCard a)]
        <> [RemovedFromPlay $ toSource a]
        <> resolve (RemoveLocation $ toId a)
      pure a
    SetConnections lid connections | lid == locationId -> do
      pure
        $ a
        & (connectedMatchersL .~ connections)
        & (revealedConnectedMatchersL .~ connections)
    AddDirectConnection fromLid toLid | fromLid == locationId -> do
      pure
        $ a
        & (revealedConnectedMatchersL <>~ [LocationWithId toLid])
        & (connectedMatchersL <>~ [LocationWithId toLid])
    EnterLocation iid lid | lid == locationId -> do
      unless locationRevealed $ push (RevealLocation (Just iid) lid)
      pure a
    PlaceAsset aid (AtLocation lid) | lid == locationId -> do
      mInVehicle <- selectOne $ ActiveInvestigator <> InvestigatorWithPlacement (InVehicle aid)
      for_ mInVehicle \iid -> do
        afterMoveButBeforeEnemyEngagement <-
          Helpers.checkWindows [Window.mkAfter (Window.MovedButBeforeEnemyEngagement iid lid)]
        enemies <- select $ enemyAt lid
        afterVehicleEnters <-
          Helpers.checkWindows [Window.mkAfter (Window.VehicleEnters aid lid)]
        pushAll
          $ [ WhenWillEnterLocation iid lid
            , EnterLocation iid lid
            , afterMoveButBeforeEnemyEngagement
            ]
          <> map EnemyCheckEngagement enemies
          <> [afterVehicleEnters]
      pure a
    PlaceGrid (GridLocation pos lid) | lid == locationId -> do
      pure $ a & positionL ?~ pos
    SetFlippable lid flippable | lid == locationId -> do
      pure $ a & canBeFlippedL .~ flippable
    RemovePlayerCardFromGame _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= card)
    AddToHand _ cards -> do
      pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
    AddToDiscard _ pc -> do
      pure $ a & cardsUnderneathL %~ filter (/= PlayerCard pc)
    AddToEncounterDiscard ec -> do
      pure $ a & cardsUnderneathL %~ filter (/= EncounterCard ec)
    Will next@(EnemySpawn details) | details.location == Just locationId -> do
      let eid = details.enemy
      whenM (getShouldSpawnNonEliteAtConnectingInstead a) do
        traits' <- field EnemyTraits eid
        unless (Elite `elem` traits') $ do
          activeInvestigatorId <- getActiveInvestigatorId
          connectedLocationIds <- select $ AccessibleFrom $ LocationWithId a.id
          availableLocationIds <-
            flip filterM connectedLocationIds $ \locationId' -> do
              modifiers' <- getModifiers (LocationTarget locationId')
              pure
                . not
                $ flip any modifiers'
                $ \case
                  SpawnNonEliteAtConnectingInstead {} -> True
                  _ -> False
          withQueue_ $ filter (/= next)
          if null availableLocationIds
            then push (toDiscard GameSource eid)
            else do
              player <- getPlayer activeInvestigatorId
              push
                $ chooseOne
                  player
                  [ targetLabel lid' [Will (EnemySpawn details'), EnemySpawn details']
                  | lid' <- availableLocationIds
                  , let details' = details {spawnDetailsSpawnAt = SpawnAtLocation lid'}
                  ]
      pure a
    MoveAllCluesTo source target | not (isTarget a target) -> do
      when (locationClues a > 0) do
        push $ MoveTokens source (toSource a) target Clue (locationClues a)
      pure a
    PlaceCluesUpToClueValue lid source n | lid == locationId -> do
      clueValue <- getPlayerCountValue locationRevealClues
      let n' = min n (clueValue - locationClues a)
      a <$ push (PlaceClues source (toTarget a) n')
    RemoveAllClues _ target | isTarget a target -> do
      pure $ a & tokensL %~ removeAllTokens Clue & withoutCluesL .~ True
    RemoveAllDoom _ target | isTarget a target -> pure $ a & tokensL %~ removeAllTokens Doom
    RemoveAllTokens _ target | isTarget a target -> pure $ a & tokensL %~ mempty
    PlaceTokens source target tType n | isTarget a target -> do
      if tType == Clue
        then do
          modifiers' <- getModifiers a
          let placedCluesWindows = windows [Window.PlacedClues source (toTarget a) n]
          let placedTokensWindows = windows [Window.PlacedToken source (toTarget a) tType n]
          if CannotPlaceClues `elem` modifiers'
            then pure a
            else do
              let clueCount = locationClues a + n
              pushAll $ placedCluesWindows <> placedTokensWindows
              pure $ a & tokensL %~ setTokens Clue clueCount & withoutCluesL .~ (clueCount == 0)
        else do
          pushM $ checkAfter $ Window.PlacedToken source target tType n
          when (tType == Doom && a.doom == 0) do
            pushM $ checkAfter $ Window.PlacedDoomCounterOnTargetWithNoDoom source target n
          when (tType == Damage) do
            pushAll $ windows [Window.PlacedDamage source (toTarget a) n]
          when (tType == Resource) do
            pushAll $ windows [Window.PlacedResources source (toTarget a) n]
          pure $ a & tokensL %~ addTokens tType n
    MoveTokens s source _ tType n | isSource a source -> runMessage (RemoveTokens s (toTarget a) tType n) a
    MoveTokens _s (InvestigatorSource _) target Clue _ | isTarget a target -> pure a
    MoveTokens s _ target tType n | isTarget a target -> runMessage (PlaceTokens s target tType n) a
    ClearTokens (isTarget a -> True) -> pure $ a & tokensL .~ mempty
    RemoveTokens _ target tType n | isTarget a target -> do
      if tType == Clue
        then do
          let clueCount = max 0 $ subtract n $ locationClues a
          pure $ a & tokensL %~ setTokens Clue clueCount & withoutCluesL .~ (clueCount == 0)
        else pure $ a & tokensL %~ subtractTokens tType n
    PlacedLocation _ _ lid | lid == locationId -> do
      selectOne ActiveInvestigator >>= traverse_ \active -> do
        pushM $ checkAfter $ Window.PutLocationIntoPlay active lid
      pushM $ checkAfter $ Window.LocationEntersPlay lid
      if locationRevealed
        then do
          modifiers' <- getModifiers (toTarget a)
          locationClueCount' <-
            if CannotPlaceClues `elem` modifiers'
              then pure 0
              else getPlayerCountValue locationRevealClues
          let locationClueCount =
                if ReduceStartingCluesByHalf `elem` modifiers'
                  then locationClueCount' `div` 2
                  else locationClueCount'
          let currentClues = countTokens Clue locationTokens

          pushAll
            $ [ PlaceClues (toSource a) (toTarget a) locationClueCount
              | locationClueCount > 0
              ]
          pure $ a & withoutCluesL .~ (locationClueCount + currentClues == 0)
        else pure a
    RevealLocation miid lid | lid == locationId && not locationRevealed -> do
      mods <- getModifiers a
      let maxFloodLevel
            | CannotBeFlooded `elem` mods = Unflooded
            | CannotBeFullyFlooded `elem` mods = PartiallyFlooded
            | otherwise = FullyFlooded
      locationClueCount <- getModifiedRevealClueCountWithMods mods a
      revealer <- maybe getLead pure miid
      whenWindowMsg <-
        checkWindows
          [mkWindow Timing.When (Window.RevealLocation revealer lid)]

      afterWindowMsg <-
        checkWindows
          [mkWindow Timing.After (Window.RevealLocation revealer lid)]

      let currentClues = countTokens Clue locationTokens

      pushAll
        $ [whenWindowMsg]
        <> [PlaceClues (toSource a) (toTarget a) locationClueCount | locationClueCount > 0]
        <> [afterWindowMsg]
      pure
        $ a
        & (revealedL .~ True)
        & (withoutCluesL .~ (locationClueCount + currentClues == 0))
        & (floodLevelL %~ maybe Nothing (Just . min maxFloodLevel))
    LookAtRevealed iid source target | isTarget a target -> do
      player <- getPlayer iid
      push $ chooseOne player [Label "Continue" [After (LookAtRevealed iid source $ toTarget a)]]
      pure $ a & revealedL .~ True
    After (LookAtRevealed _ _ target) | isTarget a target -> do
      pure $ a & revealedL .~ False
    PlaceKey (isTarget a -> True) k -> do
      pure $ a & keysL %~ insertSet k
    PlaceKey (isTarget a -> False) k -> do
      pure $ a & keysL %~ deleteSet k
    PlaceSeal (isTarget a -> True) k -> do
      pure $ a & sealsL %~ insertSet k
    PlaceSeal (isTarget a -> False) k -> do
      pure $ a & sealsL %~ deleteSet k
    IncreaseFloodLevel lid | lid == locationId -> do
      mods <- getModifiers a
      let
        newFloodLevel =
          if
            | CannotBeFlooded `elem` mods -> Unflooded
            | CannotBeFullyFlooded `elem` mods -> PartiallyFlooded
            | otherwise -> maybe PartiallyFlooded increaseFloodLevel locationFloodLevel
      runMessage (SetFloodLevel lid newFloodLevel) a
    DecreaseFloodLevel lid | lid == locationId -> do
      runMessage (SetFloodLevel lid $ maybe Unflooded decreaseFloodLevel locationFloodLevel) a
    SetFloodLevel lid level | lid == locationId -> do
      mods <- getModifiers a
      let
        maxFloodLevel =
          if
            | CannotBeFlooded `elem` mods -> Unflooded
            | CannotBeFullyFlooded `elem` mods -> PartiallyFlooded
            | otherwise -> FullyFlooded
        newFloodLevel = min maxFloodLevel level
        currentFloodLevel = fromMaybe Unflooded locationFloodLevel
      when (currentFloodLevel /= newFloodLevel) do
        before <-
          checkWhen (Window.FloodLevelChanged lid (fromMaybe Unflooded locationFloodLevel) newFloodLevel)
        pushAll [before, Do msg]
      pure a
    Do (SetFloodLevel lid level) | lid == locationId -> do
      after <- checkAfter (Window.FloodLevelChanged lid (fromMaybe Unflooded locationFloodLevel) level)
      push after
      pure $ a & floodLevelL ?~ level
    PlaceBreaches (isTarget a -> True) n -> do
      wouldDoEach
        n
        (PlaceBreaches (toTarget a) 1)
        (Window.WouldPlaceBreaches (toTarget a))
        (Window.WouldPlaceBreach (toTarget a))
        (Window.PlacedBreaches (toTarget a))
        (Window.PlacedBreach (toTarget a))
      pure a
    Do (PlaceBreaches (isTarget a -> True) n) -> do
      pure $ a & breachesL %~ Breach.addBreaches n
    RemoveBreaches (isTarget a -> True) n -> do
      pure $ a & breachesL %~ Breach.removeBreaches n
    Incursion lid | lid == toId a -> do
      -- Æ First, remove all breaches on that location.
      -- Æ Second, place 1 doom on that location.
      -- Æ Finally, place 1 breach on each connecting location. This can chain‐react and cause additional incursions to occur, so beware!
      -- Æ Once an incursion is resolved at a location, breaches from other incursions cannot be placed on that location for the remainder of that phase.
      targets <- selectTargets $ ConnectedTo (LocationWithId lid) <> NotLocation LocationWithIncursion
      lead <- getLeadPlayer
      pushAll
        $ PlaceDoom (toSource a) (toTarget a) 1
        : [ chooseOrRunOneAtATime lead [targetLabel target [PlaceBreaches target 1] | target <- targets]
          | notNull targets
          ]
      pure $ a & breachesL ?~ Breach.Incursion 0
    EndPhase -> do
      pure $ a & breachesL %~ fmap Breach.resetIncursion
    UnrevealLocation lid | lid == locationId -> pure $ a & revealedL .~ False
    RemovedLocation lid -> pure $ a & directionsL %~ filterMap (/= []) . Map.map (filter (/= lid))
    UseResign iid source | isSource a source -> a <$ push (Resign iid)
    InvestigatorDrewEncounterCard _iid ec -> do
      pure $ a & cardsUnderneathL %~ filter ((/= ec.id) . (.id))
    InvestigatorDrewEncounterCardFrom _iid ec _ -> do
      pure $ a & cardsUnderneathL %~ filter ((/= ec.id) . (.id))
    InvestigatorDrewPlayerCardFrom _iid pc _ -> do
      pure $ a & cardsUnderneathL %~ filter ((/= pc.id) . (.id))
    UseDrawCardUnderneath iid source | isSource a source ->
      case locationCardsUnderneath of
        (EncounterCard card : rest) -> do
          push (InvestigatorDrewEncounterCard iid card)
          pure $ a & cardsUnderneathL .~ rest
        _ ->
          throwIO
            $ InvalidState
            $ "Not expecting a player card or empty set, but got "
            <> tshow locationCardsUnderneath
    Blanked msg' -> runMessage msg' a
    UseCardAbility iid source 101 _ _ | isSource a source -> do
      let
        triggerSource = case source of
          ProxySource _ s -> s
          IndexedSource _ s -> s
          _ -> a.ability 101
      sid <- getRandom
      pushM $ mkInvestigateLocation sid iid triggerSource (toId a)
      pure a
    UseCardAbility iid source 102 _ _ | isSource a source -> do
      -- free because already paid for by ability
      push $ MoveAction iid locationId Free False
      pure a
    UseCardAbility iid source VeiledAbility _ _ | isSource a source -> do
      push $ Flip iid (toSource a) (toTarget a)
      pure a
    UseCardAbility iid source n _ _ | isSource a source && n >= 500 && n <= 520 -> do
      let k = fromJustNote "missing key" $ setToList locationKeys !!? (n - 500)
      before <- checkWhen $ Window.TakeControlOfKey iid k
      pushAll [before, PlaceKey (InvestigatorTarget iid) k]
      pure a
    RemoveAllCopiesOfEncounterCardFromGame cardMatcher | toCard a `cardMatch` cardMatcher -> do
      push $ RemoveLocation (toId a)
      pure a
    _ -> pure a

locationInvestigatorsWithClues :: HasGame m => LocationAttrs -> m [InvestigatorId]
locationInvestigatorsWithClues attrs =
  filterM (fieldMap InvestigatorClues (> 0)) =<< select (investigatorAt $ toId attrs)

getModifiedShroudValueFor :: (HasCallStack, HasGame m) => LocationAttrs -> m Int
getModifiedShroudValueFor attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure
    $ foldr
      applyPostModifier
      (foldr applyModifier (fromJustNote "Missing shroud" $ locationShroud attrs) modifiers')
      modifiers'
 where
  applyModifier (ShroudModifier m) n = max 0 (n + m)
  applyModifier _ n = n
  applyPostModifier (SetShroud m) _ = m
  applyPostModifier _ n = n

getInvestigateAllowed :: HasGame m => InvestigatorId -> LocationAttrs -> m Bool
getInvestigateAllowed iid attrs = do
  modifiers' <- getCombinedModifiers [toTarget attrs, toTarget iid]
  pure $ none isCannotInvestigate modifiers' && isJust (locationShroud attrs)
 where
  isCannotInvestigate CannotInvestigate {} = True
  isCannotInvestigate (CannotInvestigateLocation lid) = lid == toId attrs
  isCannotInvestigate _ = False

canEnterLocation :: HasGame m => EnemyId -> LocationId -> m Bool
canEnterLocation eid lid = do
  modifiers' <- getModifiers lid
  not <$> flip anyM modifiers' \case
    CannotBeEnteredBy matcher -> eid <=~> matcher
    _ -> pure False

withResignAction
  :: (Entity location, EntityAttrs location ~ LocationAttrs)
  => location
  -> [Ability]
  -> [Ability]
withResignAction x body = do
  let other = withBaseAbilities attrs body
  locationResignAction attrs : other
 where
  attrs = toAttrs x

withDrawCardUnderneathAction
  :: (Entity location, EntityAttrs location ~ LocationAttrs)
  => location
  -> [Ability]
withDrawCardUnderneathAction x =
  withBaseAbilities attrs [drawCardUnderneathAction attrs | attrs.revealed]
 where
  attrs = toAttrs x

instance HasAbilities LocationAttrs where
  getAbilities l =
    [ basicAbility $ investigateAbility l 101 mempty (onLocation l)
    , basicAbility
        $ restrictedAbility
          l
          102
          ( CanMoveTo (LocationWithId l.id)
              <> OnLocation (accessibleTo l)
              <> exists (You <> can.move <> noModifier (CannotEnter l.id))
          )
        $ ActionAbility [#move] moveCost
    ]
      <> [ withI18n
             $ withVar "key" (String $ keyName k)
             $ withI18nTooltip "takeControlOfKey"
             $ restrictedAbility
               l
               (500 + idx)
               (onLocation l <> youExist (not_ $ InvestigatorWithModifier CannotTakeKeys))
             $ FastAbility Free
         | l.revealed
         , l.clues == 0
         , (idx, k) <- withIndex l.keys
         ]
   where
    moveCost =
      if l.revealed
        then ActionCost 1
        else ActionCost 1 <> NonBlankedCost (CostToEnterUnrevealed $ locationCostToEnterUnrevealed l)

getShouldSpawnNonEliteAtConnectingInstead :: HasGame m => LocationAttrs -> m Bool
getShouldSpawnNonEliteAtConnectingInstead attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure $ flip any modifiers' $ \case
    SpawnNonEliteAtConnectingInstead {} -> True
    _ -> False

enemyAtLocation :: HasGame m => EnemyId -> LocationAttrs -> m Bool
enemyAtLocation eid attrs = elem eid <$> select (enemyAt $ toId attrs)

locationEnemiesWithTrait :: HasGame m => LocationAttrs -> Trait -> m [EnemyId]
locationEnemiesWithTrait attrs trait = select $ enemyAt (toId attrs) <> EnemyWithTrait trait

veiled1 :: LocationAttrs -> Ability -> [Ability]
veiled1 attrs ability = veiled attrs [ability]

veiled :: LocationAttrs -> [Ability] -> [Ability]
veiled attrs abilities =
  withRevealedAbilities
    attrs
    ( restrictedAbility
        attrs
        VeiledAbility
        (Here <> exists (LocationWithId (toId attrs) <> LocationCanBeFlipped <> LocationWithoutClues))
        (FastAbility Free)
        : abilities
    )
