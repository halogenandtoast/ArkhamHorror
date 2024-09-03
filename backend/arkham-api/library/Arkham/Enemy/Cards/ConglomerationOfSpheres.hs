module Arkham.Enemy.Cards.ConglomerationOfSpheres (
  conglomerationOfSpheres,
  ConglomerationOfSpheres (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ConglomerationOfSpheres = ConglomerationOfSpheres EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conglomerationOfSpheres :: EnemyCard ConglomerationOfSpheres
conglomerationOfSpheres =
  enemyWith ConglomerationOfSpheres Cards.conglomerationOfSpheres (1, Static 6, 4) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #willpower UneliminatedInvestigator)

instance HasAbilities ConglomerationOfSpheres where
  getAbilities (ConglomerationOfSpheres x) =
    extend x [mkAbility x 1 $ forced $ EnemyAttacked #after You (SourceWithTrait Melee) (be x)]

instance RunMessage ConglomerationOfSpheres where
  runMessage msg e@(ConglomerationOfSpheres attrs) = case msg of
    UseCardAbility
      iid
      (isSource attrs -> True)
      1
      [windowType -> Window.EnemyAttacked _ attackSource _]
      _ -> do
        push $ toDiscardBy iid (attrs.ability 1) (sourceToTarget attackSource)
        pure e
    _ -> ConglomerationOfSpheres <$> runMessage msg attrs
