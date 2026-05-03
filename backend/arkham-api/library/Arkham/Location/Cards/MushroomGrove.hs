module Arkham.Location.Cards.MushroomGrove (mushroomGrove) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheTwistedHollow.Helpers

newtype MushroomGrove = MushroomGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mushroomGrove :: LocationCard MushroomGrove
mushroomGrove = locationWith MushroomGrove Cards.mushroomGrove 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities MushroomGrove where
  getAbilities (MushroomGrove a) =
    extendRevealed a [mkAbility a 1 $ forced (RevealLocation #after You $ be a)]

instance RunMessage MushroomGrove where
  runMessage msg l@(MushroomGrove attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      darkness <- getDarknessLevel
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed darkness)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      validEnemies <- pursuitEnemiesWithHighestEvade
      chooseTargetM iid validEnemies \e -> initiateEnemyAttack e (attrs.ability 1) iid
      pure l
    _ -> MushroomGrove <$> liftRunMessage msg attrs
