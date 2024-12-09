module Arkham.Enemy.Cards.PadmaAmrita (padmaAmrita, PadmaAmrita (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Ancient))

newtype PadmaAmrita = PadmaAmrita EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

padmaAmrita :: EnemyCard PadmaAmrita
padmaAmrita = enemy PadmaAmrita Cards.padmaAmrita (5, PerPlayer 3, 3) (0, 0)

instance HasModifiersFor PadmaAmrita where
  getModifiersFor (PadmaAmrita a) =
    modifySelectWhen a a.ready Anyone [CannotDiscoverCluesAt (LocationWithTrait Ancient)]

instance HasAbilities PadmaAmrita where
  getAbilities (PadmaAmrita a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage PadmaAmrita where
  runMessage msg e@(PadmaAmrita attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasClues <- fieldP InvestigatorClues (> 0) iid
      push
        $ if hasClues
          then FlipClues (InvestigatorTarget iid) 1
          else InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 3
      pure e
    _ -> PadmaAmrita <$> runMessage msg attrs
