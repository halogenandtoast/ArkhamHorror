module Arkham.Enemy.Cards.CreatureFromTheAbyss (creatureFromTheAbyss) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype CreatureFromTheAbyss = CreatureFromTheAbyss EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creatureFromTheAbyss :: EnemyCard CreatureFromTheAbyss
creatureFromTheAbyss = enemy CreatureFromTheAbyss Cards.creatureFromTheAbyss

instance HasModifiersFor CreatureFromTheAbyss where
  getModifiersFor (CreatureFromTheAbyss a) = do
    n <- getStrengthOfTheAbyss
    modifySelf a [EnemyFight n]

instance HasAbilities CreatureFromTheAbyss where
  getAbilities (CreatureFromTheAbyss a) =
    extend
      a
      [ restricted a 1 (HasScenarioCount StrengthOfTheAbyss $ atMost 2)
          $ forced
          $ EnemyEntersPlay #after (be a)
      , restricted a 2 (HasScenarioCount StrengthOfTheAbyss $ atLeast 5)
          $ forced
          $ EnemyDefeated #after Anyone ByAny (be a)
      ]

instance RunMessage CreatureFromTheAbyss where
  runMessage msg e@(CreatureFromTheAbyss attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addStrengthOfTheAbyss 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeStrengthOfTheAbyss 1
      pure e
    _ -> CreatureFromTheAbyss <$> liftRunMessage msg attrs
