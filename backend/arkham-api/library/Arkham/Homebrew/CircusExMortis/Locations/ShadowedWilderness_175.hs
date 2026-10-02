module Arkham.Homebrew.CircusExMortis.Locations.ShadowedWilderness_175 (
  shadowedWilderness_175,
) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ShadowedWilderness_175 = ShadowedWilderness_175 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shadowedWilderness_175 :: LocationCard ShadowedWilderness_175
shadowedWilderness_175 = location ShadowedWilderness_175 Cards.shadowedWilderness_175 3 (PerPlayer 1)

instance HasModifiersFor ShadowedWilderness_175 where
  getModifiersFor (ShadowedWilderness_175 a) = whenRevealed a do
    modifySelect a (investigatorAt a) [SkillModifier #agility 1]
    modifySelect a (enemyAt a) [AddKeyword Keyword.Alert]

instance RunMessage ShadowedWilderness_175 where
  runMessage msg (ShadowedWilderness_175 attrs) = runQueueT $ ShadowedWilderness_175 <$> liftRunMessage msg attrs
