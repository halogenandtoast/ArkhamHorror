module Arkham.Act.Cards.TheMissingSibling (theMissingSibling) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Cave, Coastal))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype TheMissingSibling = TheMissingSibling ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMissingSibling :: ActCard TheMissingSibling
theMissingSibling = act (1, A) TheMissingSibling Cards.theMissingSibling Nothing

instance HasAbilities TheMissingSibling where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ forced $ DiscoveringLastClue #after Anyone (LocationWithTrait Cave)
    , restricted a 2 (Remembered FoundATornDogLeash <> Remembered FoundASetOfFootprints)
        $ Objective
        $ forced AnyWindow
    ]

getLastClueLocation :: [Window] -> Maybe LocationId
getLastClueLocation =
  asum . map \case
    (windowType -> Window.DiscoveringLastClue _ lid) -> Just lid
    _ -> Nothing

placeCavernCard :: ReverseQueue m => LocationId -> m ()
placeCavernCard lid = do
  deck <- getScenarioDeck CavernsDeck
  for_ (nonEmpty deck) \(card :| rest) -> do
    setScenarioDeck CavernsDeck rest
    pos <- fieldJust LocationPosition lid
    emptyPositions <- filterM (selectNone . LocationInPosition) pos.adjacents
    leadChooseOrRunOneM do
      for_ emptyPositions \emptyPos -> gridLabeled_ emptyPos $ placeLocationInGrid_ emptyPos card

instance RunMessage TheMissingSibling where
  runMessage msg a@(TheMissingSibling attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getLastClueLocation -> Just lid) _ -> do
      placeCavernCard lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      let hasEmptyAdjacent lid = do
            pos <- fieldJust LocationPosition lid
            notNull <$> filterM (selectNone . LocationInPosition) pos.adjacents
      locations <-
        filterM hasEmptyAdjacent
          =<< select (FirstLocation [LocationWithTrait Coastal <> LocationWithoutInvestigators, LocationWithTrait Coastal])
      leadChooseOrRunOneM $ targets locations \chosenLid -> do
        chosenPos <- fieldJust LocationPosition chosenLid
        emptyPositions <- filterM (selectNone . LocationInPosition) chosenPos.adjacents
        leadChooseOrRunOneM do
          for_ emptyPositions \emptyPos ->
            gridLabeled_ emptyPos do
              fungalCaveCard <- getSetAsideCard Locations.fungalCave
              placeLocationInGrid_ emptyPos fungalCaveCard
              occupiedAdjacent <- filterM (selectAny . LocationInPosition) emptyPos.adjacents
              let needed = 4 - length occupiedAdjacent
              emptyAdjacent <- filterM (selectNone . LocationInPosition) emptyPos.adjacents
              deck <- getScenarioDeck CavernsDeck
              let toPlace = zip (take needed emptyAdjacent) deck
              setScenarioDeck CavernsDeck (drop (length toPlace) deck)
              for_ toPlace (uncurry placeLocationInGrid_)
      advanceActDeck attrs
      pure a
    _ -> TheMissingSibling <$> liftRunMessage msg attrs
