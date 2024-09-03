module Arkham.Enemy.Cards.ValentinoRivas (
  valentinoRivas,
  ValentinoRivas (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ValentinoRivas = ValentinoRivas EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinoRivas :: EnemyCard ValentinoRivas
valentinoRivas = enemy ValentinoRivas Cards.valentinoRivas (3, Static 5, 4) (1, 1)

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 (InvestigatorExists $ You <> InvestigatorWithAnyResources)
            $ ForcedAbility
            $ EnemyEngaged Timing.After You
            $ EnemyWithId
            $ toId a
        ]

instance RunMessage ValentinoRivas where
  runMessage msg e@(ValentinoRivas attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ LoseResources iid (toSource attrs) 2
      pure e
    _ -> ValentinoRivas <$> runMessage msg attrs
