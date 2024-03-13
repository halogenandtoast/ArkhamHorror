module Arkham.Act.Cards.ExploringTheMoon (ExploringTheMoon (..), exploringTheMoon) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype ExploringTheMoon = ExploringTheMoon ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exploringTheMoon :: ActCard ExploringTheMoon
exploringTheMoon =
  act
    (2, A)
    ExploringTheMoon
    Cards.exploringTheMoon
    (Just $ GroupClueCost (PerPlayer 1) (locationIs Locations.templeOfTheMoonLizard))

instance RunMessage ExploringTheMoon where
  runMessage msg a@(ExploringTheMoon attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocation_ Locations.cavernsBeneathTheMoonDarkSide
      theBlackCore <- placeSetAsideLocation Locations.theBlackCore
      createSetAsideEnemyWith Enemies.moonLizard theBlackCore \c -> c {enemyCreationExhausted = True}
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> ExploringTheMoon <$> lift (runMessage msg attrs)
