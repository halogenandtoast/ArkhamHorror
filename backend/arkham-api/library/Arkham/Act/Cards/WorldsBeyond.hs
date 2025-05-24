module Arkham.Act.Cards.WorldsBeyond (worldsBeyond) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype WorldsBeyond = WorldsBeyond ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

worldsBeyond :: ActCard WorldsBeyond
worldsBeyond = act (1, A) WorldsBeyond Cards.worldsBeyond (groupClueCost $ PerPlayer 2)

instance HasAbilities WorldsBeyond where
  getAbilities (WorldsBeyond a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttemptsToSpawnAt #when AnyEnemy LocationNotInPlay

instance RunMessage WorldsBeyond where
  runMessage msg a@(WorldsBeyond attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      aPocketInTime <- getSetAsideCard Locations.aPocketInTime
      shuffleCardsIntoDeck ExplorationDeck [aPocketInTime]
      advanceActDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      explorationDeck <- getExplorationDeck
      let (nonMatched, remaining) = break (`cardMatch` CardWithType LocationType) explorationDeck
      case remaining of
        [] -> focusCards nonMatched do
          leadChooseOneM $ labeled "No locations found" $ shuffleDeck ExplorationDeck
        (x : _) -> do
          focusCards (nonMatched <> [x]) do
            leadChooseOneM do
              targeting x do
                push $ RemoveCardFromScenarioDeck ExplorationDeck x
                lid <- placeLocation x
                shuffleDeck ExplorationDeck
                forTarget lid msg
      pure a
    ForTarget (LocationTarget lid) (UseCardAbility _ (isSource attrs -> True) 1 (spawnedEnemy -> eid) _) -> do
      changeSpawnAt eid lid
      pure a
    _ -> WorldsBeyond <$> liftRunMessage msg attrs
