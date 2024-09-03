module Arkham.Enemy.Cards.NyarlathotepGodOfAThousandForms (
  nyarlathotepGodOfAThousandForms,
  NyarlathotepGodOfAThousandForms (..),
)
where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Placement
import Arkham.Prelude

newtype NyarlathotepGodOfAThousandForms = NyarlathotepGodOfAThousandForms EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepGodOfAThousandForms :: EnemyCard NyarlathotepGodOfAThousandForms
nyarlathotepGodOfAThousandForms =
  enemy
    NyarlathotepGodOfAThousandForms
    Cards.nyarlathotepGodOfAThousandForms
    (2, Static 6, 5)
    (1, 0)

instance RunMessage NyarlathotepGodOfAThousandForms where
  runMessage msg e@(NyarlathotepGodOfAThousandForms attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceEnemy attrs.id (StillInHand iid)
      pure e
    _ -> NyarlathotepGodOfAThousandForms <$> runMessage msg attrs
