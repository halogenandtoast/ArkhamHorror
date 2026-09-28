module Arkham.Homebrew.CircusExMortis.Locations.StatuaryGardens (statuaryGardens) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype StatuaryGardens = StatuaryGardens LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

statuaryGardens :: LocationCard StatuaryGardens
statuaryGardens = location StatuaryGardens Cards.statuaryGardens 4 (PerPlayer 1)

instance HasModifiersFor StatuaryGardens where
  getModifiersFor (StatuaryGardens a) = viceShroudReduction a Violence

instance HasAbilities StatuaryGardens where
  getAbilities (StatuaryGardens a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ EnemyDealtDamage #after AnyDamageEffect (enemyAt a) (SourceUsedBy You)

instance RunMessage StatuaryGardens where
  runMessage msg l@(StatuaryGardens attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      parleyBonusAt (attrs.ability 1) iid attrs.id
      pure l
    _ -> StatuaryGardens <$> liftRunMessage msg attrs
