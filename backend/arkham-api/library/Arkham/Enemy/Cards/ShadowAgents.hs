module Arkham.Enemy.Cards.ShadowAgents (
  shadowAgents,
  ShadowAgents (..),
)
where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ShadowAgents = ShadowAgents EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowAgents :: EnemyCard ShadowAgents
shadowAgents =
  enemyWith
    ShadowAgents
    Cards.shadowAgents
    (3, Static 3, 5)
    (2, 0)
    (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities ShadowAgents where
  getAbilities (ShadowAgents a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ EnemyEvaded #after Anyone (be a)
      ]

instance HasModifiersFor ShadowAgents where
  getModifiersFor (InvestigatorTarget iid) (ShadowAgents attrs) = do
    affected <- iid <=~> investigatorEngagedWith attrs
    pure $ toModifiers attrs [CannotDiscoverCluesExceptAsResultOfInvestigation Anywhere | affected]
  getModifiersFor _ _ = pure []

instance RunMessage ShadowAgents where
  runMessage msg e@(ShadowAgents attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ toDiscard (attrs.ability 1) attrs
      pure e
    _ -> ShadowAgents <$> runMessage msg attrs
