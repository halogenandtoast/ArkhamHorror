module Arkham.Enemy.Cards.PadmaAmrita (padmaAmrita) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
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
  runMessage msg e@(PadmaAmrita attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasClues <- fieldP InvestigatorClues (> 0) iid
      if hasClues
        then flipCluesToDoom iid 1
        else assignHorror iid (attrs.ability 1) 3
      pure e
    _ -> PadmaAmrita <$> liftRunMessage msg attrs
