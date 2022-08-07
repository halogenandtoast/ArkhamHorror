{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Location.Runner
  ( module Arkham.Location.Runner
  , module X
  ) where

import Arkham.Prelude

import Arkham.Card.CardDef as X
import Arkham.Classes as X
import Arkham.Location.Types as X
import Arkham.LocationSymbol as X

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Exception
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Helpers
import Arkham.Matcher
  ( InvestigatorMatcher (..), LocationMatcher (..), locationWithEnemy )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

pattern AfterFailedInvestigate :: InvestigatorId -> Target -> Message
pattern AfterFailedInvestigate iid target <-
  After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)

pattern UseResign :: InvestigatorId -> Source -> Message
pattern UseResign iid source <- UseCardAbility iid source _ 99 _

pattern UseDrawCardUnderneath :: InvestigatorId -> Source -> Message
pattern UseDrawCardUnderneath iid source <- UseCardAbility iid source _ 100 _

cluesToDiscover :: (Monad m, HasGame m) => InvestigatorId -> Int -> m Int
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
    UpdateLocation newAttrs lid | lid == locationId -> do
      pure newAttrs
    Investigate iid lid source mTarget skillType False | lid == locationId -> do
      allowed <- getInvestigateAllowed iid a
      if allowed
        then do
          shroudValue' <- getModifiedShroudValueFor a
          a <$ push
            (BeginSkillTest
              iid
              source
              (maybe
                (LocationTarget lid)
                (ProxyTarget (LocationTarget lid))
                mTarget
              )
              (Just Action.Investigate)
              skillType
              shroudValue'
            )
        else pure a
    PassedSkillTest iid (Just Action.Investigate) source (SkillTestInitiatorTarget target) _ n
      | isTarget a target
      -> a <$ push (Successful (Action.Investigate, target) iid source target n)
    PassedSkillTest iid (Just Action.Investigate) source (SkillTestInitiatorTarget (ProxyTarget target investigationTarget)) _ n
      | isTarget a target
      -> a
        <$ push
             (Successful
               (Action.Investigate, target)
               iid
               source
               investigationTarget
               n
             )
    Successful (Action.Investigate, _) iid _ target _ | isTarget a target -> do
      let lid = toId a
      modifiers' <- getModifiers (LocationTarget lid)
      clueAmount <- cluesToDiscover iid 1
      whenWindowMsg <- checkWindows
        [Window Timing.When (Window.SuccessfulInvestigation iid lid)]
      afterWindowMsg <- checkWindows
        [Window Timing.After (Window.SuccessfulInvestigation iid lid)]
      a <$ unless
        (AlternateSuccessfullInvestigation `elem` modifiers')
        (pushAll
          [ whenWindowMsg
          , InvestigatorDiscoverClues
            iid
            lid
            clueAmount
            (Just Action.Investigate)
          , afterWindowMsg
          ]
        )
    PlaceUnderneath target cards | isTarget a target ->
      pure $ a & cardsUnderneathL <>~ cards
    SetLocationLabel lid label' | lid == locationId ->
      pure $ a & labelL .~ label'
    PlacedLocationDirection lid direction lid2 | lid == locationId ->
      pure $ a & (directionsL %~ insertMap direction lid2)
    PlacedLocationDirection lid direction lid2 | lid2 == locationId -> do
      let
        reversedDirection = case direction of
          LeftOf -> RightOf
          RightOf -> LeftOf
          Above -> Below
          Below -> Above

      pure $ a & (directionsL %~ insertMap reversedDirection lid)
    AttachTreachery tid (LocationTarget lid) | lid == locationId ->
      pure $ a & treacheriesL %~ insertSet tid
    AttachEvent eid (LocationTarget lid) | lid == locationId ->
      pure $ a & eventsL %~ insertSet eid
    Discarded (AssetTarget aid) _ -> pure $ a & assetsL %~ deleteSet aid
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    Discard (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
    Discarded (EnemyTarget eid) _ -> pure $ a & enemiesL %~ deleteSet eid
    PlaceEnemyInVoid eid -> pure $ a & enemiesL %~ deleteSet eid
    Flipped (AssetSource aid) card | toCardType card /= AssetType ->
      pure $ a & assetsL %~ deleteSet aid
    RemoveFromGame (AssetTarget aid) -> pure $ a & assetsL %~ deleteSet aid
    RemoveFromGame (TreacheryTarget tid) ->
      pure $ a & treacheriesL %~ deleteSet tid
    RemoveFromGame (EventTarget eid) -> pure $ a & eventsL %~ deleteSet eid
    RemoveFromGame (EnemyTarget eid) -> pure $ a & enemiesL %~ deleteSet eid
    Discard target | isTarget a target ->
      a <$ pushAll (resolve (RemoveLocation $ toId a))
    AttachAsset aid (LocationTarget lid) | lid == locationId ->
      pure $ a & assetsL %~ insertSet aid
    AttachAsset aid _ -> pure $ a & assetsL %~ deleteSet aid
    AddDirectConnection fromLid toLid | fromLid == locationId -> do
      pure
        $ a
        & revealedConnectedMatchersL
        <>~ [LocationWithId toLid]
        & connectedMatchersL
        <>~ [LocationWithId toLid]
    DiscoverCluesAtLocation iid lid n maction | lid == locationId -> do
      let discoveredClues = min n locationClues
      a <$ push (DiscoverClues iid lid discoveredClues maction)
    Do (DiscoverClues iid lid n _) | lid == locationId -> do
      let lastClue = locationClues - n <= 0
      push =<< checkWindows
        (Window Timing.After (Window.DiscoverClues iid lid n)
        : [ Window Timing.After (Window.DiscoveringLastClue iid lid)
          | lastClue
          ]
        )
      pure $ a & cluesL %~ max 0 . subtract n
    InvestigatorEliminated iid -> pure $ a & investigatorsL %~ deleteSet iid
    EnterLocation iid lid
      | lid /= locationId && iid `elem` locationInvestigators
      -> pure $ a & investigatorsL %~ deleteSet iid -- TODO: should we broadcast leaving the location
    EnterLocation iid lid | lid == locationId -> do
      push =<< checkWindows [Window Timing.When $ Window.Entering iid lid]
      unless locationRevealed $ push (RevealLocation (Just iid) lid)
      pure $ a & investigatorsL %~ insertSet iid
    SetLocationAsIf iid lid | lid == locationId -> do
      pure $ a & investigatorsL %~ insertSet iid
    SetLocationAsIf iid lid | lid /= locationId -> do
      pure $ a & investigatorsL %~ deleteSet iid
    AddToVictory (EnemyTarget eid) -> pure $ a & enemiesL %~ deleteSet eid
    DefeatedAddToVictory (EnemyTarget eid) ->
      pure $ a & enemiesL %~ deleteSet eid
    EnemyEngageInvestigator eid iid -> do
      mlid <- field InvestigatorLocation iid
      if mlid == Just locationId
        then pure $ a & enemiesL %~ insertSet eid
        else pure a
    EnemyMove eid lid | lid == locationId -> do
      willMove <- canEnterLocation eid lid
      pure $ if willMove then a & enemiesL %~ insertSet eid else a
    EnemyMove eid lid -> do
      mLocationId <- selectOne $ locationWithEnemy eid
      if mLocationId == Just locationId
        then do
          willMove <- canEnterLocation eid lid
          pure $ if willMove then a & enemiesL %~ deleteSet eid else a
        else pure a
    EnemyEntered eid lid | lid == locationId -> do
      pure $ a & enemiesL %~ insertSet eid
    EnemyEntered eid lid | lid /= locationId -> do
      pure $ a & enemiesL %~ deleteSet eid
    Will next@(EnemySpawn miid lid eid) | lid == locationId -> do
      shouldSpawnNonEliteAtConnectingInstead <-
        getShouldSpawnNonEliteAtConnectingInstead a
      when shouldSpawnNonEliteAtConnectingInstead $ do
        traits' <- field EnemyTraits eid
        when (Elite `notElem` traits') $ do
          activeInvestigatorId <- getActiveInvestigatorId
          connectedLocationIds <- selectList $ AccessibleFrom $ LocationWithId
            lid
          availableLocationIds <-
            flip filterM connectedLocationIds $ \locationId' -> do
              modifiers' <- getModifiers (LocationTarget locationId')
              pure . not $ flip any modifiers' $ \case
                SpawnNonEliteAtConnectingInstead{} -> True
                _ -> False
          withQueue_ $ filter (/= next)
          if null availableLocationIds
            then push (Discard (EnemyTarget eid))
            else push
              (chooseOne
                activeInvestigatorId
                [ Run
                    [Will (EnemySpawn miid lid' eid), EnemySpawn miid lid' eid]
                | lid' <- availableLocationIds
                ]
              )
      pure a
    EnemySpawn _ lid eid | lid == locationId ->
      pure $ a & enemiesL %~ insertSet eid
    EnemySpawnedAt lid eid | lid == locationId ->
      pure $ a & enemiesL %~ insertSet eid
    RemoveEnemy eid -> pure $ a & enemiesL %~ deleteSet eid
    RemovedFromPlay (EnemySource eid) -> pure $ a & enemiesL %~ deleteSet eid
    TakeControlOfAsset _ aid -> pure $ a & assetsL %~ deleteSet aid
    MoveAllCluesTo target | not (isTarget a target) -> do
      when (locationClues > 0) (push $ PlaceClues target locationClues)
      pure $ a & cluesL .~ 0
    PlaceClues target n | isTarget a target -> do
      modifiers' <- getModifiers (toTarget a)
      windows' <- windows [Window.PlacedClues (toTarget a) n]
      if CannotPlaceClues `elem` modifiers'
        then pure a
        else do
          pushAll windows'
          pure $ a & cluesL +~ n
    PlaceCluesUpToClueValue lid n | lid == locationId -> do
      clueValue <- getPlayerCountValue locationRevealClues
      let n' = min n (clueValue - locationClues)
      a <$ push (PlaceClues (toTarget a) n')
    PlaceDoom target n | isTarget a target -> pure $ a & doomL +~ n
    RemoveDoom target n | isTarget a target ->
      pure $ a & doomL %~ max 0 . subtract n
    PlaceResources target n | isTarget a target -> pure $ a & resourcesL +~ n
    PlaceHorror target n | isTarget a target -> pure $ a & horrorL +~ n
    RemoveClues (LocationTarget lid) n | lid == locationId ->
      pure $ a & cluesL %~ max 0 . subtract n
    RemoveAllClues target | isTarget a target -> pure $ a & cluesL .~ 0
    RemoveAllDoom _ -> pure $ a & doomL .~ 0
    PlacedLocation _ _ lid | lid == locationId -> do
      when locationRevealed $ do
        modifiers' <- getModifiers (toTarget a)
        locationClueCount <- if CannotPlaceClues `elem` modifiers'
          then pure 0
          else getPlayerCountValue locationRevealClues

        pushAll
          [ PlaceClues (toTarget a) locationClueCount | locationClueCount > 0 ]
      pure a
    RevealLocation miid lid | lid == locationId -> do
      modifiers' <- getModifiers (toTarget a)
      locationClueCount <- if CannotPlaceClues `elem` modifiers'
        then pure 0
        else getPlayerCountValue locationRevealClues
      revealer <- maybe getLeadInvestigatorId pure miid
      whenWindowMsg <- checkWindows
        [Window Timing.When (Window.RevealLocation revealer lid)]

      afterWindowMsg <- checkWindows
        [Window Timing.After (Window.RevealLocation revealer lid)]
      pushAll
        $ [whenWindowMsg, afterWindowMsg]
        <> [ PlaceClues (toTarget a) locationClueCount | locationClueCount > 0 ]
      pure $ a & revealedL .~ True
    LookAtRevealed iid source target | isTarget a target -> do
      push (Label "Continue" [After (LookAtRevealed iid source target)])
      pure $ a & revealedL .~ True
    After (LookAtRevealed _ _ target) | isTarget a target ->
      pure $ a & revealedL .~ False
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
    UseCardAbility iid source _ 101 _ | isSource a source -> do
      let
        triggerSource = case source of
          ProxySource _ s -> s
          _ -> InvestigatorSource iid
      a <$ push
        (Investigate iid (toId a) triggerSource Nothing SkillIntellect False)
    UseCardAbility iid source _ 102 _ | isSource a source -> a <$ push
      (MoveAction
        iid
        locationId
        Free -- already paid by using ability
        True
      )
    _ -> pure a

locationEnemiesWithTrait :: LocationAttrs -> Trait -> GameT [EnemyId]
locationEnemiesWithTrait LocationAttrs { locationEnemies } trait =
  filterM (fieldMap EnemyTraits (member trait)) (setToList locationEnemies)

locationInvestigatorsWithClues :: LocationAttrs -> GameT [InvestigatorId]
locationInvestigatorsWithClues LocationAttrs { locationInvestigators } =
  filterM (fieldMap InvestigatorClues (> 0)) (setToList locationInvestigators)

getModifiedShroudValueFor :: LocationAttrs -> GameT Int
getModifiedShroudValueFor attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure $ foldr applyModifier (locationShroud attrs) modifiers'
 where
  applyModifier (ShroudModifier m) n = max 0 (n + m)
  applyModifier _ n = n

getInvestigateAllowed :: InvestigatorId -> LocationAttrs -> GameT Bool
getInvestigateAllowed _iid attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure $ none isCannotInvestigate modifiers'
 where
  isCannotInvestigate CannotInvestigate{} = True
  isCannotInvestigate _ = False

canEnterLocation :: EnemyId -> LocationId -> GameT Bool
canEnterLocation eid lid = do
  traits' <- field EnemyTraits eid
  modifiers' <- getModifiers (LocationTarget lid)
  pure $ not $ flip any modifiers' $ \case
    CannotBeEnteredByNonElite{} -> Elite `notMember` traits'
    _ -> False

withResignAction
  :: (Entity location, EntityAttrs location ~ LocationAttrs)
  => location
  -> [Ability]
  -> [Ability]
withResignAction x body = do
  let other = withBaseAbilities attrs body
  locationResignAction attrs : other
  where attrs = toAttrs x

withDrawCardUnderneathAction
  :: (Entity location, EntityAttrs location ~ LocationAttrs)
  => location
  -> [Ability]
withDrawCardUnderneathAction x = withBaseAbilities
  attrs
  [ drawCardUnderneathAction attrs | locationRevealed attrs ]
  where attrs = toAttrs x

instance HasAbilities LocationAttrs where
  getAbilities l =
    [ restrictedAbility l 101 (OnLocation $ LocationWithId $ toId l)
      $ ActionAbility (Just Action.Investigate) (ActionCost 1)
    , restrictedAbility
        l
        102
        (OnLocation (AccessibleTo $ LocationWithId $ toId l)
        <> InvestigatorExists (You <> InvestigatorWithoutModifier CannotMove)
        )
      $ ActionAbility (Just Action.Move) moveCost
    ]
   where
    moveCost = if not (locationRevealed l)
      then locationCostToEnterUnrevealed l
      else ActionCost 1

getShouldSpawnNonEliteAtConnectingInstead :: LocationAttrs -> GameT Bool
getShouldSpawnNonEliteAtConnectingInstead attrs = do
  modifiers' <- getModifiers (toTarget attrs)
  pure $ flip any modifiers' $ \case
    SpawnNonEliteAtConnectingInstead{} -> True
    _ -> False
