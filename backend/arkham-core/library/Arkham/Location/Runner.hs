{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Location.Runner (
  module Arkham.Location.Runner,
  module X,
) where

import Arkham.Prelude

import Arkham.Ability as X hiding (PaidCost)
import Arkham.Card.CardDef as X
import Arkham.Classes as X
import Arkham.GameValue as X
import Arkham.Helpers.Message as X
import Arkham.Helpers.SkillTest as X
import Arkham.Location.Types as X
import Arkham.LocationSymbol as X
import Arkham.Message as X hiding (
  DiscoverClues,
  EnemyDamage,
  EnemyDefeated,
  EnemyEvaded,
  MoveAction,
  RevealLocation,
 )
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Direction
import Arkham.Enemy.Types (Field (..))
import Arkham.Exception
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.BreachStatus qualified as Breach
import Arkham.Location.Helpers
import Arkham.Matcher (
  Be (..),
  EnemyMatcher (..),
  InvestigatorMatcher (..),
  LocationMatcher (..),
  enemyAt,
 )
import Arkham.Message (Message (DiscoverClues, MoveAction, RevealLocation))
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

pattern AfterFailedInvestigate :: InvestigatorId -> Target -> Message
pattern AfterFailedInvestigate iid target <-
  After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)

pattern UseResign :: InvestigatorId -> Source -> Message
pattern UseResign iid source <- UseCardAbility iid source 99 _ _

pattern UseDrawCardUnderneath :: InvestigatorId -> Source -> Message
pattern UseDrawCardUnderneath iid source <- UseCardAbility iid source 100 _ _

withRevealedAbilities :: LocationAttrs -> [Ability] -> [Ability]
withRevealedAbilities attrs other = withBaseAbilities attrs $ guard (locationRevealed attrs) *> other

cluesToDiscover :: HasGame m => InvestigatorId -> Int -> m Int
cluesToDiscover investigatorId startValue = do
  msource <- getSkillTestSource
  case msource of
    Just _ -> do
      modifiers <- getModifiers (InvestigatorTarget investigatorId)
      pure $ foldr applyModifier startValue modifiers
    Nothing -> pure startValue
 where
  applyModifier (DiscoveredClues m) n = n + m
  applyModifier _ n = n

instance RunMessage LocationAttrs where
  runMessage msg a@LocationAttrs {..} = case msg of
    UpdateLocation lid upd | lid == locationId -> do
      -- TODO: we may want life cycles around this, generally this might just be a bad idea
      pure $ updateLocation [upd] a
    FlipClues target n | isTarget a target -> do
      let clueCount = max 0 $ subtract n $ locationClues a
      pure
        $ a
        & (tokensL %~ flipClues n)
        & (withoutCluesL .~ (clueCount == 0))
    FlipDoom target n | isTarget a target -> do
      let flipCount = min n $ locationDoom a
      pure
        $ a
        & (tokensL %~ flipDoom n)
        & (withoutCluesL .~ (flipCount >= 0))
    Investigate iid lid source mTarget skillType False | lid == locationId -> do
      allowed <- getInvestigateAllowed iid a
      when allowed $ do
        shroudValue' <- getModifiedShroudValueFor a
        push
          $ investigate
            iid
            source
            ( maybe (LocationTarget lid) (ProxyTarget (LocationTarget lid)) mTarget
            )
            skillType
            shroudValue'
      pure a
    PassedSkillTest iid (Just Action.Investigate) source (Initiator target) _ n | isTarget a target -> do
      let clues = locationClues a
      (before, _, after) <- frame $ Window.SuccessfullyInvestigateWithNoClues iid $ toId a
      pushAll
        $ [before | clues == 0]
        <> [Successful (Action.Investigate, toTarget a) iid source (toTarget a) n]
        <> [after | clues == 0]
      pure a
    PassedSkillTest iid (Just Action.Investigate) source (InitiatorProxy target actual) _ n | isTarget a target -> do
      let clues = locationClues a
      (before, _, after) <- frame $ Window.SuccessfullyInvestigateWithNoClues iid $ toId a
      pushAll
        $ [before | clues == 0]
        <> [Successful (Action.Investigate, toTarget a) iid source actual n]
        <> [after | clues == 0]
      pure a
    Successful (Action.Investigate, _) iid _ target _ | isTarget a target -> do
      let lid = toId a
      modifiers' <- getModifiers (LocationTarget lid)
      clueAmount <- cluesToDiscover iid 1
      (before, _, after) <- frame (Window.SuccessfulInvestigation iid lid)
      unless (AlternateSuccessfullInvestigation `elem` modifiers') $ do
        pushAll
          [before, InvestigatorDiscoverClues iid lid (toSource a) clueAmount (Just #investigate), after]
      pure a
    PlaceUnderneath target cards | isTarget a target -> do
      pure $ a & cardsUnderneathL <>~ cards
    SetLocationLabel lid label' | lid == locationId -> do
      pure $ a & labelL .~ label'
    PlacedLocationDirection lid direction lid2 | lid2 == locationId -> do
      pure $ a & (directionsL %~ insertMap direction lid)
    PlacedLocationDirection lid direction lid2 | lid == locationId -> do
      let
        reversedDirection = case direction of
          LeftOf -> RightOf
          RightOf -> LeftOf
          Above -> Below
          Below -> Above

      pure $ a & (directionsL %~ insertMap reversedDirection lid2)
    LocationMoved lid | lid == locationId -> do
      pure $ a & (directionsL .~ mempty)
    LocationMoved lid | lid /= locationId -> do
      pure $ a & (directionsL %~ filterMap (/= lid))
    AttachTreachery tid (LocationTarget lid) | lid == locationId -> do
      pure $ a & treacheriesL %~ insertSet tid
    PutLocationInFrontOf iid lid | lid == locationId -> do
      pure $ a & inFrontOfL ?~ iid
    PutLocationInCenter lid | lid == locationId -> do
      pure $ a & inFrontOfL .~ Nothing
    PlaceEvent _ eid placement -> case placement of
      AttachedToLocation lid
        | lid == locationId ->
            pure $ a & eventsL %~ insertSet eid
      AtLocation lid
        | lid == locationId ->
            pure $ a & eventsL %~ insertSet eid
      _ -> pure $ a & eventsL %~ deleteSet eid
    Discarded (AssetTarget aid) _ _ -> pure $ a & assetsL %~ deleteSet aid
    Discard _ (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    Discard _ (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
    Flipped (AssetSource aid) card
      | toCardType card /= AssetType ->
          pure $ a & assetsL %~ deleteSet aid
    RemoveFromGame (AssetTarget aid) -> pure $ a & assetsL %~ deleteSet aid
    RemoveFromGame (TreacheryTarget tid) ->
      pure $ a & treacheriesL %~ deleteSet tid
    RemoveFromGame (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
    Discard source target | isTarget a target -> do
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      pushAll
        $ windows'
        <> [Discarded (toTarget a) source (toCard a)]
        <> [RemovedFromPlay $ toSource a]
        <> resolve (RemoveLocation $ toId a)
      pure a
    AttachAsset aid (LocationTarget lid)
      | lid == locationId ->
          pure $ a & assetsL %~ insertSet aid
    AttachAsset aid _ -> pure $ a & assetsL %~ deleteSet aid
    PlaceAsset aid placement -> case placement of
      AttachedToLocation lid
        | lid == locationId ->
            pure $ a & assetsL %~ insertSet aid
      AtLocation lid | lid == locationId -> pure $ a & assetsL %~ insertSet aid
      _ -> pure $ a & assetsL %~ deleteSet aid
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
    DiscoverCluesAtLocation iid lid source n maction | lid == locationId -> do
      let discoveredClues = min n $ locationClues a
      push $ DiscoverClues iid lid source discoveredClues maction
      pure a
    Do (DiscoverClues iid lid source n _) | lid == locationId -> do
      let lastClue = locationClues a - n <= 0
      let clueCount = max 0 $ subtract n $ locationClues a
      push
        =<< checkWindows
          ( mkWindow Timing.After (Window.DiscoverClues iid lid source n)
              : [ mkWindow Timing.After (Window.DiscoveringLastClue iid lid)
                | lastClue
                ]
          )
      pure $ a & tokensL %~ setTokens Clue clueCount & withoutCluesL .~ (clueCount == 0)
    InvestigatorEliminated iid -> pure $ a & investigatorsL %~ deleteSet iid
    EnterLocation iid lid
      | lid /= locationId && iid `elem` locationInvestigators ->
          pure $ a & investigatorsL %~ deleteSet iid -- TODO: should we broadcast leaving the location
    EnterLocation iid lid | lid == locationId -> do
      push =<< checkWindows [mkWindow Timing.When $ Window.Entering iid lid]
      unless locationRevealed $ push (RevealLocation (Just iid) lid)
      pure $ a & investigatorsL %~ insertSet iid
    SetFlippable lid flippable
      | lid == locationId ->
          pure $ a & canBeFlippedL .~ flippable
    SetLocationAsIf iid lid | lid == locationId -> do
      pure $ a & investigatorsL %~ insertSet iid
    SetLocationAsIf iid lid | lid /= locationId -> do
      pure $ a & investigatorsL %~ deleteSet iid
    RemovePlayerCardFromGame _ card -> do
      pure $ a & cardsUnderneathL %~ filter (/= card)
    AddToHand _ cards -> do
      pure $ a & cardsUnderneathL %~ filter (`notElem` cards)
    AddToDiscard _ pc -> do
      pure $ a & cardsUnderneathL %~ filter (/= PlayerCard pc)
    AddToEncounterDiscard ec -> do
      pure $ a & cardsUnderneathL %~ filter (/= EncounterCard ec)
    Will next@(EnemySpawn miid lid eid) | lid == locationId -> do
      shouldSpawnNonEliteAtConnectingInstead <-
        getShouldSpawnNonEliteAtConnectingInstead a
      when shouldSpawnNonEliteAtConnectingInstead $ do
        traits' <- field EnemyTraits eid
        when (Elite `notElem` traits') $ do
          activeInvestigatorId <- getActiveInvestigatorId
          connectedLocationIds <-
            selectList
              $ AccessibleFrom
              $ LocationWithId
                lid
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
            then push (Discard GameSource (EnemyTarget eid))
            else
              push
                $ chooseOne
                  activeInvestigatorId
                  [ targetLabel
                    lid'
                    [Will (EnemySpawn miid lid' eid), EnemySpawn miid lid' eid]
                  | lid' <- availableLocationIds
                  ]
      pure a
    TakeControlOfAsset _ aid -> pure $ a & assetsL %~ deleteSet aid
    MoveAllCluesTo source target | not (isTarget a target) -> do
      when (locationClues a > 0) (push $ PlaceClues source (toTarget a) (locationClues a))
      pure $ a & tokensL %~ removeAllTokens Clue & withoutCluesL .~ True
    PlaceClues source target n | isTarget a target -> do
      modifiers' <- getModifiers a
      windows' <- windows [Window.PlacedClues source (toTarget a) n]
      if CannotPlaceClues `elem` modifiers'
        then pure a
        else do
          let clueCount = locationClues a + n
          pushAll windows'
          pure $ a & tokensL %~ setTokens Clue clueCount & withoutCluesL .~ (clueCount == 0)
    PlaceCluesUpToClueValue lid source n | lid == locationId -> do
      clueValue <- getPlayerCountValue locationRevealClues
      let n' = min n (clueValue - locationClues a)
      a <$ push (PlaceClues source (toTarget a) n')
    PlaceDoom _ target n | isTarget a target -> pure $ a & tokensL %~ addTokens Doom n
    RemoveDoom _ target n | isTarget a target -> do
      pure $ a & tokensL %~ subtractTokens Doom n
    PlaceResources source target n | isTarget a target -> do
      windows' <- windows [Window.PlacedResources source (toTarget a) n]
      pushAll windows'
      pure $ a & tokensL %~ addTokens Resource n
    RemoveResources _ target n | isTarget a target -> do
      pure $ a & tokensL %~ subtractTokens Resource n
    PlaceHorror _ target n | isTarget a target -> pure $ a & tokensL %~ addTokens Horror n
    RemoveClues _ (LocationTarget lid) n | lid == locationId -> do
      let clueCount = max 0 $ subtract n $ locationClues a
      pure $ a & tokensL %~ setTokens Clue clueCount & withoutCluesL .~ (clueCount == 0)
    RemoveAllClues _ target | isTarget a target -> do
      pure $ a & tokensL %~ removeAllTokens Clue & withoutCluesL .~ True
    RemoveAllDoom _ target | isTarget a target -> pure $ a & tokensL %~ removeAllTokens Doom
    PlacedLocation _ _ lid | lid == locationId -> do
      if locationRevealed
        then do
          modifiers' <- getModifiers (toTarget a)
          locationClueCount <-
            if CannotPlaceClues `elem` modifiers'
              then pure 0
              else getPlayerCountValue locationRevealClues

          pushAll
            [ PlaceClues (toSource a) (toTarget a) locationClueCount
            | locationClueCount > 0
            ]
          pure $ a & withoutCluesL .~ (locationClueCount == 0)
        else pure a
    RevealLocation miid lid | lid == locationId -> do
      modifiers' <- getModifiers (toTarget a)
      locationClueCount <-
        if CannotPlaceClues `elem` modifiers'
          then pure 0
          else getPlayerCountValue locationRevealClues
      revealer <- maybe getLeadInvestigatorId pure miid
      whenWindowMsg <-
        checkWindows
          [mkWindow Timing.When (Window.RevealLocation revealer lid)]

      afterWindowMsg <-
        checkWindows
          [mkWindow Timing.After (Window.RevealLocation revealer lid)]
      pushAll
        $ [whenWindowMsg, afterWindowMsg]
        <> [PlaceClues (toSource a) (toTarget a) locationClueCount | locationClueCount > 0]
      pure $ a & revealedL .~ True & withoutCluesL .~ (locationClueCount == 0)
    LookAtRevealed iid source target | isTarget a target -> do
      push
        $ chooseOne
          iid
          [Label "Continue" [After (LookAtRevealed iid source $ toTarget a)]]
      pure $ a & revealedL .~ True
    After (LookAtRevealed _ _ target)
      | isTarget a target ->
          pure $ a & revealedL .~ False
    PlaceKey (isTarget a -> True) k -> do
      pure $ a & keysL %~ insertSet k
    PlaceKey (isTarget a -> False) k -> do
      pure $ a & keysL %~ deleteSet k
    PlaceBreaches (isTarget a -> True) n -> do
      wouldDoEach
        n
        (PlaceBreaches (toTarget a) 1)
        (Window.WouldPlaceBreaches (toTarget a))
        (Window.WouldPlaceBreach (toTarget a))
        (Window.PlacedBreaches (toTarget a))
        (Window.PlacedBreach (toTarget a))
      pure a
    DoBatch _ (PlaceBreaches (isTarget a -> True) n) -> do
      pure $ a & breachesL %~ Breach.addBreaches n
    RemoveBreaches (isTarget a -> True) n -> do
      pure $ a & breachesL %~ Breach.removeBreaches n
    Incursion lid | lid == toId a -> do
      -- Æ First, remove all breaches on that location.
      -- Æ Second, place 1 doom on that location.
      -- Æ Finally, place 1 breach on each connecting location. This can chain‐react and cause additional incursions to occur, so beware!
      -- Æ Once an incursion is resolved at a location, breaches from other incursions cannot be placed on that location for the remainder of that phase.
      targets <- selectTargets $ ConnectedTo (LocationWithId lid) <> NotLocation LocationWithIncursion
      lead <- getLead
      pushAll
        [ PlaceDoom (toSource a) (toTarget a) 1
        , chooseOrRunOneAtATime lead [targetLabel target [PlaceBreaches target 1] | target <- targets]
        ]
      pure $ a & breachesL ?~ Breach.Incursion 0
    EndPhase -> do
      pure $ a & breachesL %~ fmap Breach.resetIncursion
    UnrevealLocation lid | lid == locationId -> pure $ a & revealedL .~ False
    RemovedLocation lid -> pure $ a & directionsL %~ filterMap (/= lid)
    UseResign iid source | isSource a source -> a <$ push (Resign iid)
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
          _ -> InvestigatorSource iid
      push
        $ Investigate
          iid
          (toId a)
          triggerSource
          Nothing
          locationInvestigateSkill
          False
      pure a
    UseCardAbility iid source 102 _ _ | isSource a source -> do
      push
        $ MoveAction
          iid
          locationId
          Free -- already paid by using ability
          True
      pure a
    UseCardAbility iid source n _ _ | isSource a source && n >= 500 && n <= 520 -> do
      let k = fromJustNote "missing key" $ setToList locationKeys !!? (n - 500)
      push $ PlaceKey (InvestigatorTarget iid) k
      pure a
    _ -> pure a

locationInvestigatorsWithClues :: LocationAttrs -> GameT [InvestigatorId]
locationInvestigatorsWithClues LocationAttrs {locationInvestigators} =
  filterM (fieldMap InvestigatorClues (> 0)) (setToList locationInvestigators)

getModifiedShroudValueFor :: LocationAttrs -> GameT Int
getModifiedShroudValueFor attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure
    $ foldr
      applyPostModifier
      (foldr applyModifier (locationShroud attrs) modifiers')
      modifiers'
 where
  applyModifier (ShroudModifier m) n = max 0 (n + m)
  applyModifier _ n = n
  applyPostModifier (SetShroud m) _ = m
  applyPostModifier _ n = n

getInvestigateAllowed :: InvestigatorId -> LocationAttrs -> GameT Bool
getInvestigateAllowed _iid attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure $ none isCannotInvestigate modifiers'
 where
  isCannotInvestigate CannotInvestigate {} = True
  isCannotInvestigate (CannotInvestigateLocation lid) = lid == toId attrs
  isCannotInvestigate _ = False

canEnterLocation :: EnemyId -> LocationId -> GameT Bool
canEnterLocation eid lid = do
  traits' <- field EnemyTraits eid
  modifiers' <- getModifiers (LocationTarget lid)
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite {} -> Elite `notMember` traits'
    _ -> False

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
  withBaseAbilities
    attrs
    [drawCardUnderneathAction attrs | locationRevealed attrs]
 where
  attrs = toAttrs x

instance HasAbilities LocationAttrs where
  getAbilities l =
    [ restrictedAbility l 101 (OnLocation $ LocationWithId $ toId l)
        $ ActionAbility (Just Action.Investigate) (ActionCost 1)
    , restrictedAbility
        l
        102
        ( OnLocation (AccessibleTo $ LocationWithId $ toId l)
            <> InvestigatorExists
              ( You
                  <> InvestigatorWithoutModifier CannotMove
                  <> InvestigatorWithoutModifier (CannotEnter $ toId l)
              )
        )
        $ ActionAbility (Just Action.Move) moveCost
    ]
      <> [ withTooltip ("Take " <> keyName k <> " key")
          $ restrictedAbility l (500 + idx) (OnLocation $ LocationWithId $ toId l)
          $ FastAbility Free
         | locationRevealed l
         , locationClues l == 0
         , (idx, k) <- withIndex (toList $ locationKeys l)
         ]
   where
    moveCost =
      if not (locationRevealed l)
        then locationCostToEnterUnrevealed l
        else ActionCost 1

getShouldSpawnNonEliteAtConnectingInstead :: LocationAttrs -> GameT Bool
getShouldSpawnNonEliteAtConnectingInstead attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure $ flip any modifiers' $ \case
    SpawnNonEliteAtConnectingInstead {} -> True
    _ -> False

enemyAtLocation :: HasGame m => EnemyId -> LocationAttrs -> m Bool
enemyAtLocation eid attrs = member eid <$> select (enemyAt $ toId attrs)

locationEnemiesWithTrait :: HasGame m => LocationAttrs -> Trait -> m [EnemyId]
locationEnemiesWithTrait attrs trait = selectList $ enemyAt (toId attrs) <> EnemyWithTrait trait

instance Be LocationAttrs LocationMatcher where
  be = LocationWithId . toId
