module Arkham.Location.Cards.BearDen (bearDen) where

import Arkham.Ability
import Arkham.Helpers.Enemy
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn

newtype BearDen = BearDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bearDen :: LocationCard BearDen
bearDen = locationWith BearDen Cards.bearDen 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities BearDen where
  getAbilities (BearDen a) =
    extendRevealed a [mkAbility a 1 $ forced (RevealLocation #after You $ be a)]

instance RunMessage BearDen where
  runMessage msg l@(BearDen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- pursuitEnemiesWithHighestFight
      if null enemies
        then drawEncounterCard iid attrs
        else chooseTargetM iid enemies \e -> spawnAt e Nothing (SpawnEngagedWith $ InvestigatorWithId iid)
      pure l
    _ -> BearDen <$> liftRunMessage msg attrs
