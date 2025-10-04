module Arkham.Scenarios.BeforeTheBlackThrone.Helpers where

import Arkham.Campaigns.TheCircleUndone.I18n
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message (toDiscard)
import Arkham.I18n
import Arkham.Id
import Arkham.Label
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted (capture)
import Arkham.Message.Lifted qualified as Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Projection
import Arkham.Queue
import Arkham.Scenario.Deck
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Source
import Arkham.Target
import Arkham.Trait (Trait (Cultist))

findCosmosPosition :: HasGame m => InvestigatorId -> m (Maybe Pos)
findCosmosPosition iid = do
  cosmos' <- getCosmos
  lid <- getJustLocation iid
  pure $ findInCosmos lid cosmos'

cosmosFail :: HasQueue Message m => LocationAttrs -> m ()
cosmosFail attrs = do
  pushAll
    [ RemoveFromGame (toTarget attrs)
    , ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [toCard attrs]
    ]

getEmptyPositionsInDirections :: HasGame m => Pos -> [GridDirection] -> m [Pos]
getEmptyPositionsInDirections pos directions = do
  cosmos' <- getCosmos
  let adjacents = positionsInDirections pos directions
  pure $ filter (\adj -> isEmpty $ viewCosmos adj cosmos') adjacents

getEmptySpacePositionsInDirections :: HasGame m => Pos -> [GridDirection] -> m [Pos]
getEmptySpacePositionsInDirections pos directions = do
  cosmos' <- getCosmos
  let adjacents = positionsInDirections pos directions
  pure $ filter (\adj -> isEmptySpace $ viewCosmos adj cosmos') adjacents

getLocationInDirection :: HasGame m => Pos -> GridDirection -> m (Maybe LocationId)
getLocationInDirection pos dir = do
  cosmos' <- getCosmos
  pure $ case viewCosmos (updatePosition pos dir) cosmos' of
    Nothing -> Nothing
    Just (EmptySpace _ _) -> Nothing
    Just (CosmosLocation _ lid') -> Just lid'

getCanMoveLocationLeft :: HasGame m => LocationId -> m Bool
getCanMoveLocationLeft lid = do
  cosmos' <- getCosmos
  pure $ case findInCosmos lid cosmos' of
    Nothing -> False -- could be player location, like Luke
    Just pos -> case viewCosmos (updatePosition pos GridLeft) cosmos' of
      Nothing -> True
      Just (EmptySpace _ _) -> True
      Just (CosmosLocation _ _) -> False

commitRitualSuicide :: (ReverseQueue m, Sourceable source) => source -> m ()
commitRitualSuicide (toSource -> source) = do
  cultists <- select $ EnemyWithTrait Cultist
  for_ cultists (push . toDiscard source)

  azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
  doom <- getSum <$> foldMapM (fieldMap EnemyDoom Sum) cultists
  push $ PlaceDoom source (toTarget azathoth) doom

getEmptySpaceCards :: HasGame m => m [Card]
getEmptySpaceCards = cosmosEmptySpaceCards <$> getCosmos

findLocationInCosmos :: HasGame m => LocationId -> m (Maybe Pos)
findLocationInCosmos lid = findInCosmos lid <$> getCosmos

topmostRevealedLocationPositions :: HasGame m => m [Pos]
topmostRevealedLocationPositions = do
  cosmosLocations <- flattenCosmos <$> getCosmos
  revealedCosmosLocations <- flip mapMaybeM cosmosLocations $ \case
    CosmosLocation p@(Pos _ y) lid -> runMaybeT $ guardM (lift $ lid <=~> RevealedLocation) $> (p, y)
    _ -> pure Nothing

  pure $ maxes revealedCosmosLocations

bottommostRevealedLocationPositions :: HasGame m => m [Pos]
bottommostRevealedLocationPositions = do
  cosmosLocations <- flattenCosmos <$> getCosmos
  revealedCosmosLocations <- flip mapMaybeM cosmosLocations $ \case
    CosmosLocation p@(Pos _ y) lid -> runMaybeT $ guardM (lift $ lid <=~> RevealedLocation) $> (p, y)
    _ -> pure Nothing

  pure $ mins revealedCosmosLocations

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "beforeTheBlackThrone" a

placeCosmos
  :: ( ReverseQueue m
     , ToId investigator InvestigatorId
     , ToId location LocationId
     )
  => investigator -> location -> CosmosLocation Card LocationId -> m ()
placeCosmos (asId -> iid) (asId -> lid) = push . PlaceCosmos iid lid

chooseCosmos
  :: (ToId investigator InvestigatorId, ReverseQueue m)
  => LocationAttrs -> investigator -> [Pos] -> [Message] -> m ()
chooseCosmos attrs (asId -> iid) valids msgs = do
  if null valids
    then cosmosFail attrs
    else chooseOneM iid do
      for_ valids \pos'@(Pos x y) ->
        gridLabeled (cosmicLabel pos') do
          placeCosmos iid attrs (CosmosLocation (Pos x y) attrs.id)
          pushAll msgs

handleCosmos
  :: (ReverseQueue m, ToId location LocationId) => location -> CosmosLocation Card LocationId -> m ()
handleCosmos (asId -> lid) cloc = handleCosmosWithHandleEmptySpace lid cloc \iid c -> Lifted.shuffleCardsIntoDeck iid [c]

handleCosmosWithHandleEmptySpace
  :: (ReverseQueue m, ToId location LocationId)
  => location
  -> CosmosLocation Card LocationId
  -> (InvestigatorId -> Card -> QueueT Message m ())
  -> m ()
handleCosmosWithHandleEmptySpace (asId -> lid) cloc f = do
  cosmos' <- getCosmos
  let
    pos = cosmosLocationToPosition cloc
    current = viewCosmos pos cosmos'
    cosmos'' = insertCosmos cloc cosmos'
  mTopLocation <-
    selectOne
      $ IncludeEmptySpace
      $ not_ (be lid)
      <> LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridUp)
  mBottomLocation <-
    selectOne
      $ IncludeEmptySpace
      $ not_ (be lid)
      <> LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridDown)
  mLeftLocation <-
    selectOne
      $ IncludeEmptySpace
      $ not_ (be lid)
      <> LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridLeft)
  mRightLocation <-
    selectOne
      $ IncludeEmptySpace
      $ not_ (be lid)
      <> LocationWithLabel (mkLabel $ cosmicLabel $ updatePosition pos GridRight)
  currentMsgs <- case current of
    Just (EmptySpace _ c) -> case toCardOwner c of
      Nothing -> error "Unhandled EmptySpace with no owner"
      Just iid -> do
        emptySpace <- selectJust $ IncludeEmptySpace $ LocationWithLabel (mkLabel $ cosmicLabel pos)
        handleMsgs <- capture (f iid c)
        pure $ RemoveFromGame (toTarget emptySpace) : handleMsgs
    _ -> pure []
  pushAll
    $ currentMsgs
    <> [ LocationMoved lid
       , SetLocationLabel lid (cosmicLabel pos)
       , SetScenarioMeta (toJSON cosmos'')
       ]
    <> [PlacedLocationDirection lid Below topLocation | topLocation <- maybeToList mTopLocation]
    <> [PlacedLocationDirection lid Above bottomLocation | bottomLocation <- maybeToList mBottomLocation]
    <> [PlacedLocationDirection lid LeftOf rightLocation | rightLocation <- maybeToList mRightLocation]
    <> [PlacedLocationDirection lid RightOf leftLocation | leftLocation <- maybeToList mLeftLocation]
