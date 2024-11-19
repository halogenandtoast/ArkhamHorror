module Arkham.Location.Cards.BarrierCamp (barrierCamp, BarrierCamp (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BarrierCamp = BarrierCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierCamp :: LocationCard BarrierCamp
barrierCamp =
  symbolLabel
    $ locationWith BarrierCamp Cards.barrierCamp 4 (PerPlayer 3)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 4) YourLocation)

instance HasAbilities BarrierCamp where
  getAbilities (BarrierCamp a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here parleyAction_

instance RunMessage BarrierCamp where
  runMessage msg l@(BarrierCamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      unlessM (hasSupply Dynamite) $ recoverSupply Dynamite
      pure l
    _ -> BarrierCamp <$> liftRunMessage msg attrs
