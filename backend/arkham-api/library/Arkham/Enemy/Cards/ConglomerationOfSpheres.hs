module Arkham.Enemy.Cards.ConglomerationOfSpheres (conglomerationOfSpheres) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Ref
import Arkham.Helpers.Window (attackSource)
import Arkham.Matcher
import Arkham.Trait

newtype ConglomerationOfSpheres = ConglomerationOfSpheres EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conglomerationOfSpheres :: EnemyCard ConglomerationOfSpheres
conglomerationOfSpheres =
  enemy ConglomerationOfSpheres Cards.conglomerationOfSpheres (1, Static 6, 4) (1, 1)
    & setPrey (InvestigatorWithLowestSkill #willpower UneliminatedInvestigator)

instance HasAbilities ConglomerationOfSpheres where
  getAbilities (ConglomerationOfSpheres x) =
    extend1 x $ mkAbility x 1 $ forced $ EnemyAttacked #after You (SourceWithTrait Melee) (be x)

instance RunMessage ConglomerationOfSpheres where
  runMessage msg e@(ConglomerationOfSpheres attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (attackSource -> source) _ -> do
      toDiscardBy iid (attrs.ability 1) (sourceToTarget source)
      pure e
    _ -> ConglomerationOfSpheres <$> liftRunMessage msg attrs
