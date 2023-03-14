module Arkham.Enemy.Cards.PriestessOfTheCoven
  ( priestessOfTheCoven
  , PriestessOfTheCoven(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier qualified as Modifier
import Arkham.Trait ( Trait (Witch) )

newtype PriestessOfTheCoven = PriestessOfTheCoven EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

priestessOfTheCoven :: EnemyCard PriestessOfTheCoven
priestessOfTheCoven =
  enemy PriestessOfTheCoven Cards.priestessOfTheCoven (2, Static 3, 2) (2, 0)

instance HasModifiersFor PriestessOfTheCoven where
  getModifiersFor target (PriestessOfTheCoven a) | isTarget a target = do
    witchCount <- length <$> findInDiscard (CardWithTrait Witch)
    pure
      $ toModifiers a
      $ guard (witchCount > 0)
      *> [ Modifier.EnemyFight (min 3 witchCount)
         , Modifier.EnemyEvade (min 3 witchCount)
         ]
  getModifiersFor _ _ = pure []

instance HasAbilities PriestessOfTheCoven where
  getAbilities (PriestessOfTheCoven a) = withBaseAbilities
    a
    [mkAbility a 1 $ ForcedAbility EncounterDeckRunsOutOfCards]

instance RunMessage PriestessOfTheCoven where
  runMessage msg e@(PriestessOfTheCoven attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <- selectList $ InvestigatorAt $ locationWithEnemy $ toId attrs
      pushAll
        $ Ready (toTarget attrs)
        : map (InitiateEnemyAttack . enemyAttack (toId attrs)) iids
      pure e
    _ -> PriestessOfTheCoven <$> runMessage msg attrs
