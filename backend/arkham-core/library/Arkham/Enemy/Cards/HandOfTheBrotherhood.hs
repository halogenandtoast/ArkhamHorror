module Arkham.Enemy.Cards.HandOfTheBrotherhood
  ( handOfTheBrotherhood
  , HandOfTheBrotherhood(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (Ancient) )

newtype HandOfTheBrotherhood = HandOfTheBrotherhood EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handOfTheBrotherhood :: EnemyCard HandOfTheBrotherhood
handOfTheBrotherhood = enemyWith
  HandOfTheBrotherhood
  Cards.handOfTheBrotherhood
  (2, Static 2, 2)
  (0, 1)
  (spawnAtL ?~ EmptyLocation)

instance HasModifiersFor HandOfTheBrotherhood where
  getModifiersFor (InvestigatorTarget _) (HandOfTheBrotherhood a) = do
    pure $ toModifiers
      a
      [ CannotTriggerAbilityMatching
        $ AbilityOnLocation (locationWithEnemy (toId a))
        <> AbilityOneOf [AbilityIsActionAbility, AbilityIsReactionAbility]
      | not (enemyExhausted a)
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities HandOfTheBrotherhood where
  getAbilities (HandOfTheBrotherhood a) = withBaseAbilities
    a
    [ restrictedAbility a 1 (EnemyCriteria $ ThisEnemy ReadyEnemy)
      $ ForcedAbility
      $ DiscoveringLastClue Timing.After Anyone
      $ LocationWithTrait Ancient
    ]

instance RunMessage HandOfTheBrotherhood where
  runMessage msg e@(HandOfTheBrotherhood attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ 1 _ -> do
      push $ PlaceDoom (toTarget attrs) 1
      pure e
    _ -> HandOfTheBrotherhood <$> runMessage msg attrs
