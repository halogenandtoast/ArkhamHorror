module Arkham.Homebrew.CircusExMortis.Locations.CrowdedRow_049 (crowdedRow_049) where

import Arkham.Ability
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype CrowdedRow_049 = CrowdedRow_049 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crowdedRow_049 :: LocationCard CrowdedRow_049
crowdedRow_049 = location CrowdedRow_049 Cards.crowdedRow_049 1 (Static 2)

instance HasAbilities CrowdedRow_049 where
  getAbilities (CrowdedRow_049 a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ EnemyEnters #after (be a) NonEliteEnemy

instance RunMessage CrowdedRow_049 where
  runMessage msg l@(CrowdedRow_049 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (enteringEnemy -> enemy) _ -> do
      roundModifiers (attrs.ability 1) enemy [EnemyFight 1, EnemyEvade 1]
      pure l
    _ -> CrowdedRow_049 <$> liftRunMessage msg attrs
