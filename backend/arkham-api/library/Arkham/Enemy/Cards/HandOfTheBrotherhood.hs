module Arkham.Enemy.Cards.HandOfTheBrotherhood (handOfTheBrotherhood, HandOfTheBrotherhood (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Ancient))

newtype HandOfTheBrotherhood = HandOfTheBrotherhood EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handOfTheBrotherhood :: EnemyCard HandOfTheBrotherhood
handOfTheBrotherhood =
  enemyWith HandOfTheBrotherhood Cards.handOfTheBrotherhood (2, Static 2, 2) (0, 1)
    $ spawnAtL
    ?~ SpawnAt EmptyLocation

instance HasModifiersFor HandOfTheBrotherhood where
  getModifiersFor (HandOfTheBrotherhood a) = do
    modifySelectWhen
      a
      a.ready
      Anyone
      [ CannotTriggerAbilityMatching
          $ AbilityOnLocation (orConnected $ locationWithEnemy a)
          <> AbilityOneOf [AbilityIsActionAbility, AbilityIsReactionAbility]
      ]

instance HasAbilities HandOfTheBrotherhood where
  getAbilities (HandOfTheBrotherhood a) =
    extend1 a
      $ restrictedAbility a 1 (EnemyCriteria $ ThisEnemy ReadyEnemy)
      $ forced
      $ DiscoveringLastClue #after Anyone
      $ LocationWithTrait Ancient

instance RunMessage HandOfTheBrotherhood where
  runMessage msg e@(HandOfTheBrotherhood attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceDoom (attrs.ability 1) (toTarget attrs) 1
      pure e
    _ -> HandOfTheBrotherhood <$> runMessage msg attrs
