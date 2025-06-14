module Arkham.Enemy.Cards.HandOfTheBrotherhood (handOfTheBrotherhood) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Ancient))

newtype HandOfTheBrotherhood = HandOfTheBrotherhood EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

handOfTheBrotherhood :: EnemyCard HandOfTheBrotherhood
handOfTheBrotherhood =
  enemy HandOfTheBrotherhood Cards.handOfTheBrotherhood (2, Static 2, 2) (0, 1)
    & setSpawnAt EmptyLocation

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
      $ restricted a 1 (EnemyCriteria $ ThisEnemy ReadyEnemy)
      $ forced
      $ DiscoveringLastClue #after Anyone
      $ LocationWithTrait Ancient

instance RunMessage HandOfTheBrotherhood where
  runMessage msg e@(HandOfTheBrotherhood attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> HandOfTheBrotherhood <$> liftRunMessage msg attrs
