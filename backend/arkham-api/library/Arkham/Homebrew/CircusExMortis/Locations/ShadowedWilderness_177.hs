module Arkham.Homebrew.CircusExMortis.Locations.ShadowedWilderness_177 (
  shadowedWilderness_177,
) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ShadowedWilderness_177 = ShadowedWilderness_177 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shadowedWilderness_177 :: LocationCard ShadowedWilderness_177
shadowedWilderness_177 = location ShadowedWilderness_177 Cards.shadowedWilderness_177 5 (PerPlayer 1)

instance HasModifiersFor ShadowedWilderness_177 where
  getModifiersFor (ShadowedWilderness_177 a) = whenRevealed a do
    anyEnemies <- selectAny $ enemyAt a
    exhausted <- selectAny $ enemyAt a <> ExhaustedEnemy
    modifySelfWhen a anyEnemies [ShroudModifier (if exhausted then (-2) else (-1))]

instance RunMessage ShadowedWilderness_177 where
  runMessage msg (ShadowedWilderness_177 attrs) = runQueueT $ ShadowedWilderness_177 <$> liftRunMessage msg attrs
