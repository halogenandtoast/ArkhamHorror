module Arkham.Homebrew.CircusExMortis.Locations.CrowdedRow_051 (crowdedRow_051) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CrowdedRow_051 = CrowdedRow_051 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crowdedRow_051 :: LocationCard CrowdedRow_051
crowdedRow_051 = location CrowdedRow_051 Cards.crowdedRow_051 2 (Static 2)

instance HasAbilities CrowdedRow_051 where
  getAbilities (CrowdedRow_051 a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ freeReaction (EnemyEvaded #after You (EnemyAt (be a)))

instance RunMessage CrowdedRow_051 where
  runMessage msg l@(CrowdedRow_051 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      performActionAction iid (attrs.ability 1) #investigate
      pure l
    _ -> CrowdedRow_051 <$> liftRunMessage msg attrs
