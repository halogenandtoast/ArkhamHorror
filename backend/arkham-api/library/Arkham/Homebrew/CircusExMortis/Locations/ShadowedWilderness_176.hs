module Arkham.Homebrew.CircusExMortis.Locations.ShadowedWilderness_176 (
  shadowedWilderness_176,
) where

import Arkham.Ability
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ShadowedWilderness_176 = ShadowedWilderness_176 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowedWilderness_176 :: LocationCard ShadowedWilderness_176
shadowedWilderness_176 = location ShadowedWilderness_176 Cards.shadowedWilderness_176 4 (PerPlayer 1)

instance HasAbilities ShadowedWilderness_176 where
  getAbilities (ShadowedWilderness_176 a) =
    extendRevealed1 a
      -- The window is #after, so the enemy that entered is already here: "no other enemies"
      -- means it is the only one. Keeping it #after matters, because aloof granted before it
      -- entered would stop it engaging on arrival.
      $ restricted a 1 (EnemyCount (EqualTo $ Static 1) (enemyAt a))
      $ freeReaction (EnemyEnters #after (be a) NonEliteEnemy)

instance RunMessage ShadowedWilderness_176 where
  runMessage msg l@(ShadowedWilderness_176 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (enteringEnemy -> enemy) _ -> do
      roundModifier (attrs.ability 1) enemy (AddKeyword Keyword.Aloof)
      pure l
    _ -> ShadowedWilderness_176 <$> liftRunMessage msg attrs
