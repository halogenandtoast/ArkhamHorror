module Arkham.Location.Cards.BlightedGlade (blightedGlade) where

import Arkham.Ability
import Arkham.Helpers.Enemy
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn

newtype BlightedGlade = BlightedGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blightedGlade :: LocationCard BlightedGlade
blightedGlade = locationWith BlightedGlade Cards.blightedGlade 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities BlightedGlade where
  getAbilities (BlightedGlade a) =
    extendRevealed a [mkAbility a 1 (forced $ RevealLocation #after You $ be a)]

instance RunMessage BlightedGlade where
  runMessage msg l@(BlightedGlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- pursuitEnemiesWithLowestEvade
      chooseTargetM iid enemies \e -> spawnAt e Nothing (SpawnAtLocation attrs.id)
      pure l
    _ -> BlightedGlade <$> liftRunMessage msg attrs
