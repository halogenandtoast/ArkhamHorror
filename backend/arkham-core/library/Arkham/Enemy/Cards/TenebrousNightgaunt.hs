module Arkham.Enemy.Cards.TenebrousNightgaunt (tenebrousNightgaunt, TenebrousNightgaunt (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype TenebrousNightgaunt = TenebrousNightgaunt EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenebrousNightgaunt :: EnemyCard TenebrousNightgaunt
tenebrousNightgaunt = enemy TenebrousNightgaunt Cards.tenebrousNightgaunt (4, Static 4, 1) (1, 1)

-- We add a no-op ability here as the scenario TheSearchForKadath will be
-- responsible for triggering the ability, and implementing its effect.
instance HasAbilities TenebrousNightgaunt where
  getAbilities = extend a [mkAbility a 1 $ forced NotAnyWindow]

instance RunMessage TenebrousNightgaunt where
  runMessage msg e@(TenebrousNightgaunt attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure $ e & placementL .~ Unplaced
    _ -> TenebrousNightgaunt <$> runMessage msg attrs
