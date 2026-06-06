module Arkham.Location.Cards.OozyLakebed (oozyLakebed) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Window (spawnedEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Manifold))

newtype OozyLakebed = OozyLakebed LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oozyLakebed :: LocationCard OozyLakebed
oozyLakebed = locationWith OozyLakebed Cards.oozyLakebed 2 (Static 0) connectsToAdjacent

instance HasAbilities OozyLakebed where
  getAbilities (OozyLakebed a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ EnemySpawns #after (be a) (EnemyWithTrait Manifold)

instance RunMessage OozyLakebed where
  runMessage msg l@(OozyLakebed attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (spawnedEnemy -> enemy) _ -> do
      placeClues (attrs.ability 1) attrs 1
      roundModifier (attrs.ability 1) enemy CannotBeDamaged
      pure l
    _ -> OozyLakebed <$> liftRunMessage msg attrs
